# Rules engine: research goal + variables -> valid tests, with feasibility checks.
# Deterministic and auditable: the AI layer may phrase these messages, never override them.
#
# spec fields: goal  = "describe" | "difference" | "paired_difference" | "relationship" |
#                      "association" | "prediction" | "reliability"
#              outcome, group, var1, var2, predictors (vector), items (vector), variables (vector)

qc_refs <- list(
  welch_t   = "Delacre, Lakens & Leys (2017), Int. Review of Social Psychology, 30(1)",
  welch_f   = "Delacre et al. (2019), Int. Review of Social Psychology, 32(1)",
  cohen     = "Cohen (1988), Statistical Power Analysis for the Behavioral Sciences",
  green     = "Green (1991), Multivariate Behavioral Research, 26(3)",
  peduzzi   = "Peduzzi et al. (1996), Journal of Clinical Epidemiology, 49(12)",
  cochran   = "Cochran (1954), Biometrics, 10(4)",
  nunnally  = "Nunnally & Bernstein (1994), Psychometric Theory",
  field     = "Field (2018), Discovering Statistics Using IBM SPSS Statistics, 5th ed.",
  openintro = "Diez, Cetinkaya-Rundel & Barr (2019), OpenIntro Statistics, 4th ed. (CC BY-SA)"
)

qc_option <- function(test_id, label, role, why, status = "ok", messages = character(),
                      params = list(), refs = character()) {
  list(test_id = test_id, label = label, role = role, why = why, status = status,
       messages = messages, params = params, references = unlist(qc_refs[refs], use.names = FALSE))
}
qc_block <- function(reason, instead = NULL)
  list(list(test_id = NA, label = "Not possible with this data", role = "blocked",
            why = reason, status = "blocked", messages = instead, params = list(), references = character()))

# Smallest effect detectable with 80% power at alpha = .05
qc_mde_t   <- function(n) stats::power.t.test(n = n, sd = 1, power = .8)$delta
qc_mde_r   <- function(n) tanh((stats::qnorm(.975) + stats::qnorm(.8)) / sqrt(n - 3))
qc_size_label <- function(d, type = "d") {
  cut <- switch(type, d = c(.2, .5, .8), r = c(.1, .3, .5))
  if (d < cut[1]) "very small" else if (d < cut[2]) "small" else if (d < cut[3]) "medium" else "large"
}
qc_non_normal <- function(y) { s <- qc_shapiro(y); !is.na(s$p) && s$p < .05 }
qc_need <- function(data, vars) {
  miss <- setdiff(vars, names(data))
  if (length(miss)) stop("Column(s) not found: ", paste(miss, collapse = ", "))
}

qc_suggest <- function(data, spec) {
  goal <- spec$goal
  switch(goal,
    describe          = qc_rule_describe(data, spec),
    difference        = qc_rule_difference(data, spec),
    paired_difference = qc_rule_paired(data, spec),
    relationship      = qc_rule_relationship(data, spec),
    association       = qc_rule_association(data, spec),
    prediction        = qc_rule_prediction(data, spec),
    reliability       = qc_rule_reliability(data, spec),
    stop("Unknown goal: ", goal))
}

qc_rule_describe <- function(data, spec) {
  qc_need(data, spec$variables)
  list(qc_option("descriptives", "Descriptive statistics", "recommended",
    "Summarises each variable (mean, SD, median, range or counts and percentages). Report these before any test.",
    params = list(VARIABLES_VEC = spec$variables), refs = "openintro"))
}

qc_rule_difference <- function(data, spec) {
  qc_need(data, c(spec$outcome, spec$group))
  d <- qc_complete(data, c(spec$outcome, spec$group))$data
  yt <- qc_vartype(d[[spec$outcome]])
  if (yt %in% c("binary", "categorical"))
    return(c(list(qc_option("chisq_independence", "Chi-square test of independence", "recommended",
      sprintf("'%s' is a category, not a number, so group means cannot be compared. A chi-square test compares the share of each category across groups instead.", spec$outcome),
      params = list(VAR1 = spec$group, VAR2 = spec$outcome), refs = c("cochran", "openintro")))))
  if (yt %in% c("text", "empty", "constant"))
    return(qc_block(sprintf("'%s' has no usable numeric values (%s).", spec$outcome, yt),
                    "Choose a numeric outcome, or recode this column first."))
  g <- factor(d[[spec$group]]); k <- nlevels(g); ns <- table(g)
  if (k < 2) return(qc_block(sprintf("'%s' has only one group in the complete data.", spec$group)))
  if (any(ns < 2)) return(qc_block(sprintf("At least one group has fewer than 2 cases (%s).",
      paste(names(ns), ns, sep = ": ", collapse = ", ")), "Merge small groups or collect more data."))
  msgs <- character(); status <- "ok"
  if (k > 10) { msgs <- c(msgs, sprintf("'%s' has %d groups - check it is really a grouping variable.", spec$group, k)); status <- "warn" }
  y <- as.numeric(d[[spec$outcome]])
  non_normal <- any(tapply(y, g, qc_non_normal)) && min(ns) < 30
  ordinal <- yt == "ordinal"
  p <- list(OUTCOME = spec$outcome, GROUP = spec$group, ALTERNATIVE = if (is.null(spec$alternative)) "two.sided" else spec$alternative)
  if (k == 2) {
    mde <- qc_mde_t(min(ns))
    if (min(ns) < 20) { status <- "warn"; msgs <- c(msgs, sprintf(
      "With %d cases in the smallest group, only %s effects (d >= %s) can be detected reliably (80%% power). A non-significant result would say little.",
      min(ns), qc_size_label(mde), qc_fmt(mde))) }
    param <- qc_option("ttest_independent", "Welch's independent-samples t-test",
      if (non_normal || ordinal) "alternative" else "recommended",
      "Compares the means of two independent groups. Welch's version does not assume equal variances, so it is the safer default.",
      status, msgs, p, c("welch_t", "cohen"))
    nonpar <- qc_option("mann_whitney", "Mann-Whitney U test", if (non_normal || ordinal) "recommended" else "alternative",
      if (ordinal) "The outcome is a rating-scale item (ordinal), so a rank-based test is more appropriate than comparing means."
      else if (non_normal) "Scores are not normally distributed in at least one small group, so a rank-based test is more robust."
      else "Rank-based alternative if you prefer not to assume normality.",
      status, msgs, p, c("field"))
  } else {
    if (min(ns) < 15) { status <- "warn"; msgs <- c(msgs, sprintf(
      "The smallest group has only %d cases; power is low and post-hoc comparisons are unstable.", min(ns))) }
    param <- qc_option("anova_oneway", "Welch's one-way ANOVA + Games-Howell post-hoc",
      if (non_normal || ordinal) "alternative" else "recommended",
      sprintf("Compares the means of %d groups. Welch's ANOVA does not assume equal variances; Games-Howell shows which pairs differ.", k),
      status, msgs, p, c("welch_f", "cohen"))
    nonpar <- qc_option("kruskal_wallis", "Kruskal-Wallis test + Holm-corrected pairwise tests",
      if (non_normal || ordinal) "recommended" else "alternative",
      "Rank-based comparison of 3+ groups; use it for rating-scale items or non-normal small samples.",
      status, msgs, p, c("field"))
  }
  opts <- list(param, nonpar)
  opts[order(sapply(opts, function(o) o$role != "recommended"))]
}

qc_rule_paired <- function(data, spec) {
  qc_need(data, c(spec$var1, spec$var2))
  d <- qc_complete(data, c(spec$var1, spec$var2))$data
  types <- c(qc_vartype(d[[spec$var1]]), qc_vartype(d[[spec$var2]]))
  if (any(!types %in% c("continuous", "ordinal", "binary")) || !all(sapply(d[c(spec$var1, spec$var2)], is.numeric)))
    return(qc_block("Both measurements must be numeric columns (e.g. score_before and score_after).",
                    "If your data are in long format (one row per measurement), reshape to one row per person first."))
  n <- nrow(d)
  if (n < 3) return(qc_block(sprintf("Only %d complete pairs.", n)))
  diffs <- d[[spec$var1]] - d[[spec$var2]]
  non_normal <- qc_non_normal(diffs) && n < 30; ordinal <- "ordinal" %in% types
  msgs <- character(); status <- "ok"
  if (n < 15) { status <- "warn"; msgs <- sprintf("With %d pairs, only effects of d_z >= %s are detectable with 80%% power.",
      n, qc_fmt(stats::power.t.test(n = n, sd = 1, power = .8, type = "paired")$delta)) }
  p <- list(VAR1 = spec$var1, VAR2 = spec$var2, ALTERNATIVE = if (is.null(spec$alternative)) "two.sided" else spec$alternative)
  opts <- list(
    qc_option("ttest_paired", "Paired-samples t-test", if (non_normal || ordinal) "alternative" else "recommended",
      "Compares two measurements of the same people (e.g. before vs after).", status, msgs, p, c("cohen", "openintro")),
    qc_option("wilcoxon_signed", "Wilcoxon signed-rank test", if (non_normal || ordinal) "recommended" else "alternative",
      if (ordinal) "Rating-scale items are ordinal, so a rank-based paired test fits better."
      else "Rank-based alternative when the differences are not normally distributed.", status, msgs, p, "field"))
  opts[order(sapply(opts, function(o) o$role != "recommended"))]
}

qc_rule_relationship <- function(data, spec) {
  qc_need(data, c(spec$var1, spec$var2))
  d <- qc_complete(data, c(spec$var1, spec$var2))$data
  t1 <- qc_vartype(d[[spec$var1]]); t2 <- qc_vartype(d[[spec$var2]])
  cat_types <- c("binary", "categorical")
  if (t1 %in% cat_types && t2 %in% cat_types) return(qc_rule_association(data, spec))
  if (xor(t1 %in% cat_types, t2 %in% cat_types)) {
    grp <- if (t1 %in% cat_types) spec$var1 else spec$var2
    out <- if (t1 %in% cat_types) spec$var2 else spec$var1
    res <- qc_rule_difference(data, list(outcome = out, group = grp))
    res[[1]]$why <- paste0("'", grp, "' is a grouping variable, so the relationship is tested as a group difference. ", res[[1]]$why)
    return(res)
  }
  n <- nrow(d)
  if (n < 4) return(qc_block(sprintf("Only %d complete cases; a correlation needs at least 4.", n)))
  msgs <- character(); status <- "ok"
  if (n < 30) { status <- "warn"; msgs <- sprintf("With n = %d, only correlations of |r| >= %s are detectable with 80%% power.",
      n, qc_fmt_r(qc_mde_r(n))) }
  non_normal <- (qc_non_normal(d[[spec$var1]]) || qc_non_normal(d[[spec$var2]])) && n < 30
  ordinal <- "ordinal" %in% c(t1, t2)
  p <- list(VAR1 = spec$var1, VAR2 = spec$var2)
  opts <- list(
    qc_option("correlation_pearson", "Pearson correlation", if (non_normal || ordinal) "alternative" else "recommended",
      "Measures the strength of a linear relationship between two numeric variables.", status, msgs, p, c("cohen", "openintro")),
    qc_option("correlation_spearman", "Spearman rank correlation", if (non_normal || ordinal) "recommended" else "alternative",
      "Rank-based correlation; suited to rating-scale items, outliers or curved (monotonic) relationships.", status, msgs, p, "field"))
  opts[order(sapply(opts, function(o) o$role != "recommended"))]
}

qc_rule_association <- function(data, spec) {
  v1 <- if (!is.null(spec$var1)) spec$var1 else spec$group
  v2 <- if (!is.null(spec$var2)) spec$var2 else spec$outcome
  qc_need(data, c(v1, v2))
  d <- qc_complete(data, c(v1, v2))$data
  tab <- table(d[[v1]], d[[v2]])
  if (min(dim(tab)) < 2) return(qc_block("Each variable needs at least 2 categories in the complete data."))
  expct <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  msgs <- character(); status <- "ok"
  if (mean(expct < 5) > .2) { status <- "warn"; msgs <- c(msgs,
    "More than 20% of cells have expected counts below 5, so Fisher's exact test will be used instead of the chi-square approximation.") }
  if (any(expct < 1)) msgs <- c(msgs, "Some expected counts are below 1; consider merging rare categories.")
  if (length(tab) > 30) { status <- "warn"; msgs <- c(msgs, sprintf("The table has %d cells; results will be hard to interpret.", length(tab))) }
  list(qc_option("chisq_independence", "Chi-square test of independence (Fisher's exact test if counts are small)",
    "recommended", "Tests whether two categorical variables are related, e.g. whether the share of 'yes' answers differs by department.",
    status, msgs, list(VAR1 = v1, VAR2 = v2), c("cochran", "openintro")))
}

qc_rule_prediction <- function(data, spec) {
  qc_need(data, c(spec$outcome, spec$predictors))
  d <- qc_complete(data, c(spec$outcome, spec$predictors))$data
  n <- nrow(d); k <- length(spec$predictors); yt <- qc_vartype(d[[spec$outcome]])
  # count model terms (a factor with L levels uses L - 1 terms)
  terms <- sum(sapply(spec$predictors, function(v) if (is.numeric(d[[v]])) 1 else max(1, length(unique(d[[v]])) - 1)))
  if (n <= terms + 1) return(qc_block(sprintf("%d complete cases for %d model terms: the model cannot be estimated.", n, terms),
      "Use fewer predictors, or collect more data."))
  p <- list(OUTCOME = spec$outcome, PREDICTORS_VEC = spec$predictors)
  if (yt == "binary") {
    yy <- d[[spec$outcome]]; events <- min(table(yy)); epv <- events / terms
    msgs <- character(); status <- "ok"
    if (epv < 10) { status <- "warn"; msgs <- sprintf(
      "Only %.1f events per predictor term (the rarer outcome occurs %d times). At least 10 are recommended; estimates may be biased.", epv, events) }
    p$POSITIVE <- if (!is.null(spec$positive)) spec$positive else as.character(sort(unique(yy))[2])
    return(list(qc_option("regression_logistic", "Binary logistic regression", "recommended",
      sprintf("'%s' has two outcomes, so logistic regression models the odds of '%s'.", spec$outcome, p$POSITIVE),
      status, msgs, p, "peduzzi")))
  }
  if (yt %in% c("categorical")) return(qc_block(sprintf("'%s' has more than two categories.", spec$outcome),
      "Multinomial regression is planned for a later version. Alternatively, merge categories into two."))
  if (!yt %in% c("continuous", "ordinal")) return(qc_block(sprintf("'%s' is not a usable outcome (%s).", spec$outcome, yt)))
  msgs <- character(); status <- "ok"; need <- 50 + 8 * terms
  if (n < need) { status <- "warn"; msgs <- c(msgs, sprintf(
    "n = %d is below the recommended minimum of %d (50 + 8 x %d predictor terms) for testing the overall model.", n, need, terms)) }
  if (yt == "ordinal") msgs <- c(msgs, "The outcome is a single rating-scale item; a multi-item scale score is a better regression outcome.")
  list(qc_option("regression_linear", "Multiple linear regression", "recommended",
    "Estimates how much each predictor is associated with the outcome while holding the others constant.",
    status, msgs, p, c("green", "field")))
}

qc_rule_reliability <- function(data, spec) {
  qc_need(data, spec$items)
  if (length(spec$items) < 2) return(qc_block("Reliability needs at least 2 items of the same scale."))
  d <- qc_complete(data, spec$items)$data
  if (!all(sapply(d[spec$items], is.numeric))) return(qc_block("All items must be numeric (e.g. 1-5 ratings)."))
  msgs <- character(); status <- "ok"
  if (nrow(d) < 30) { status <- "warn"; msgs <- sprintf("Only %d complete responses; alpha will be imprecise.", nrow(d)) }
  rev <- if (is.null(spec$reverse)) character() else spec$reverse
  rng <- range(unlist(d[spec$items]))
  list(qc_option("reliability_alpha", "Cronbach's alpha + item analysis", "recommended",
    "Checks whether the items measure the same construct consistently before you average them into one scale score.",
    status, msgs, list(ITEMS_VEC = spec$items, REVERSE_VEC = rev,
                       SCALE_MIN = if (is.null(spec$scale_min)) rng[1] else spec$scale_min,
                       SCALE_MAX = if (is.null(spec$scale_max)) rng[2] else spec$scale_max),
    c("nunnally", "field")))
}
