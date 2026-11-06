# Assumption screening -> parametric or non-parametric decision.
# Rules are fixed in advance and documented, so the test choice never depends on the result.
#
# Normality is judged by sample size (Kim, 2013):
#   n < 50     Shapiro-Wilk p >= .05
#   n 50-300   |z| of skewness and kurtosis <= 3.29
#   n > 300    |skewness| <= 2 and |excess kurtosis| <= 7
# Mean-based tests are kept when normality fails but every group has n >= 30 and no extreme
# outliers (central limit theorem; Field, 2018). Rating-scale items always use rank-based tests.

qc_group_name <- function(v, l) { lab <- qc_level_lab(v, l); if (lab != as.character(l)) lab else if (grepl("[A-Za-z]", l)) as.character(l) else paste(qc_lab(v), l) }

qc_rule_refs <- list(
  kim      = "Kim (2013), Restorative Dentistry & Endodontics, 38(1), 52-54",
  field    = "Field (2018), Discovering Statistics Using IBM SPSS Statistics (5th ed.)",
  tukey    = "Tukey (1977), Exploratory Data Analysis",
  welch_t  = "Delacre, Lakens & Leys (2017), International Review of Social Psychology, 30(1)",
  welch_f  = "Delacre et al. (2019), International Review of Social Psychology, 32(1)",
  cochran  = "Cochran (1954), Biometrics, 10(4)",
  hc3      = "Long & Ervin (2000), The American Statistician, 54(3)",
  vif      = "Hair et al. (2019), Multivariate Data Analysis (8th ed.)",
  cook     = "Cook & Weisberg (1982), Residuals and Influence in Regression",
  peduzzi  = "Peduzzi et al. (1996), Journal of Clinical Epidemiology, 49(12)",
  nunnally = "Nunnally & Bernstein (1994), Psychometric Theory (3rd ed.)",
  hayes    = "Hayes (2022), Introduction to Mediation, Moderation, and Conditional Process Analysis (3rd ed.)",
  preacher = "Preacher & Hayes (2008), Behavior Research Methods, 40(3), 879-891")

# Sample skewness G1 and excess kurtosis G2 (same definitions as SPSS) with standard errors
qc_shape <- function(y) {
  n <- length(y); m <- mean(y); s2 <- mean((y - m)^2)
  g1 <- mean((y - m)^3) / s2^1.5; g2 <- mean((y - m)^4) / s2^2 - 3
  G1 <- if (n > 2) g1 * sqrt(n * (n - 1)) / (n - 2) else NA
  G2 <- if (n > 3) ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3)) else NA
  se1 <- if (n > 3) sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))) else NA
  se2 <- if (n > 5) 2 * se1 * sqrt((n^2 - 1) / ((n - 3) * (n + 5))) else NA
  list(n = n, skew = G1, kurt = G2, z_skew = G1 / se1, z_kurt = G2 / se2)
}

qc_check_row <- function(check, method, value, met, meaning)
  data.frame(check = check, method = method, value = value, met = met, meaning = meaning, stringsAsFactors = FALSE)

qc_normality_check <- function(y, what) {
  y <- y[!is.na(y)]; n <- length(y)
  if (n < 3 || stats::var(y) == 0)
    return(qc_check_row(paste("Normality:", what), "Not assessable", sprintf("n = %d", n), NA,
                        "Too few or identical values to judge the distribution."))
  sh <- qc_shape(y)
  if (n < 50) {
    s <- stats::shapiro.test(y); met <- s$p.value >= .05
    qc_check_row(paste("Normality:", what), "Shapiro-Wilk (n < 50)",
      sprintf("W = %s, p %s", qc_fmt(s$statistic, 3), qc_fmt_p(s$p.value)), met,
      if (met) "The scores do not depart significantly from a normal distribution."
      else "The scores depart significantly from a normal distribution.")
  } else if (n <= 300) {
    met <- abs(sh$z_skew) <= 3.29 && abs(sh$z_kurt) <= 3.29
    qc_check_row(paste("Normality:", what), "Skewness/kurtosis z-scores (n 50-300)",
      sprintf("z(skew) = %s, z(kurtosis) = %s", qc_fmt(sh$z_skew), qc_fmt(sh$z_kurt)), met,
      if (met) "Skewness and kurtosis are within the accepted range (|z| <= 3.29)."
      else "Skewness or kurtosis is outside the accepted range (|z| > 3.29).")
  } else {
    met <- abs(sh$skew) <= 2 && abs(sh$kurt) <= 7
    qc_check_row(paste("Normality:", what), "Absolute skewness/kurtosis (n > 300)",
      sprintf("skewness = %s, kurtosis = %s", qc_fmt(sh$skew), qc_fmt(sh$kurt)), met,
      if (met) "Skewness (<= 2) and kurtosis (<= 7) are within the accepted range."
      else "Skewness or kurtosis exceeds the accepted range (2 and 7).")
  }
}

# Extreme outliers: beyond 3 x IQR from the quartiles (Tukey, 1977)
qc_outlier_check <- function(y, what) {
  y <- y[!is.na(y)]; q <- stats::quantile(y, c(.25, .75)); iqr <- q[2] - q[1]
  if (iqr == 0 && length(unique(y)) > 1) return(qc_check_row(paste("Extreme outliers:", what), "Beyond 3 x IQR (Tukey)", "IQR = 0", FALSE,
    sprintf("At least half of the values are identical (%s), so the outlier rule cannot separate outliers. Values this concentrated do not suit mean-based tests.", qc_fmt(q[1]))))
  k <- sum(y < q[1] - 3 * iqr | y > q[2] + 3 * iqr)
  qc_check_row(paste("Extreme outliers:", what), "Beyond 3 x IQR (Tukey)", sprintf("%d case(s)", k), k == 0,
    if (k == 0) "No extreme outliers." else sprintf("%d extreme value(s); means can be distorted by these.", k))
}

qc_decision <- function(test_id, family, reason, refs)
  list(test_id = test_id, family = family, reason = reason,
       references = unlist(qc_rule_refs[refs], use.names = FALSE))

# Shared logic for group designs: returns parametric TRUE/FALSE plus the reason
qc_decide_groups <- function(norm_ok, outl_ok, ns, ordinal, param_name, nonpar_name) {
  if (ordinal) return(list(param = FALSE, reason = sprintf(
    "The outcome is a single rating-scale item (ordinal), so a rank-based test (%s) was used instead of comparing means.", nonpar_name)))
  if (all(norm_ok, na.rm = TRUE) && !any(is.na(norm_ok)))
    return(list(param = TRUE, reason = sprintf(
      "Normality was met in every group, so the parametric %s was used.", param_name)))
  if (min(ns) >= 30 && all(outl_ok))
    return(list(param = TRUE, reason = sprintf(
      "Normality was not met in every group, but each group has at least 30 cases and there are no extreme outliers, so the %s remains robust (central limit theorem) and was used.", param_name)))
  list(param = FALSE, reason = sprintf(
    "Normality was not met%s, so the non-parametric %s was used.",
    if (min(ns) < 30) " and at least one group has fewer than 30 cases" else " and extreme outliers are present",
    nonpar_name))
}

# Main entry: screens assumptions for a hypothesis spec and decides the test.
qc_screen <- function(data, spec) {
  switch(spec$goal,
    difference = qc_screen_groups(data, spec),
    paired_difference = qc_screen_paired(data, spec),
    relationship = qc_screen_relationship(data, spec),
    association = qc_screen_association(data, spec),
    prediction = qc_screen_prediction(data, spec),
    reliability = qc_screen_reliability(data, spec),
    mediation = qc_screen_mediation(data, spec),
    describe = list(checks = qc_check_row("Not needed", "-", "-", TRUE, "Descriptive statistics make no assumptions."),
                    decision = qc_decision("descriptives", "descriptive", "Descriptive statistics were requested.", character())),
    stop("Unknown goal"))
}

qc_screen_groups <- function(data, spec) {
  d <- qc_complete(data, c(spec$outcome, spec$group))$data
  yt <- qc_vartype(d[[spec$outcome]])
  if (yt %in% c("binary", "categorical")) return(qc_screen_association(data, list(var1 = spec$group, var2 = spec$outcome)))
  y <- as.numeric(d[[spec$outcome]]); g <- factor(d[[spec$group]]); k <- nlevels(g)
  checks <- do.call(rbind, c(
    lapply(levels(g), function(l) qc_normality_check(y[g == l], qc_group_name(spec$group, l))),
    lapply(levels(g), function(l) qc_outlier_check(y[g == l], qc_group_name(spec$group, l)))))
  lev <- qc_levene_bf(y, g)
  checks <- rbind(checks, qc_check_row("Equal variances", "Levene's test (Brown-Forsythe)",
    sprintf("F(%d, %d) = %s, p %s", lev$df1, lev$df2, qc_fmt(lev$F), qc_fmt_p(lev$p)), lev$p >= .05,
    paste(if (lev$p >= .05) "Group variances are similar." else "Group variances differ.",
          "Welch's version is used either way, because it does not require equal variances.")))
  ns <- as.vector(table(g)); norm_ok <- checks$met[grepl("^Normality", checks$check)]
  outl_ok <- checks$met[grepl("^Extreme", checks$check)]
  two <- k == 2
  dec <- qc_decide_groups(norm_ok, outl_ok, ns, yt == "ordinal",
    if (two) "Welch's t-test" else "Welch's ANOVA", if (two) "Mann-Whitney U test" else "Kruskal-Wallis test")
  id <- if (two) { if (dec$param) "ttest_independent" else "mann_whitney" } else { if (dec$param) "anova_oneway" else "kruskal_wallis" }
  list(checks = checks, decision = qc_decision(id, if (dec$param) "parametric" else "non-parametric", dec$reason,
       c("kim", "field", "tukey", if (two) "welch_t" else "welch_f")))
}

qc_screen_paired <- function(data, spec) {
  d <- qc_complete(data, c(spec$var1, spec$var2))$data
  diffs <- as.numeric(d[[spec$var1]]) - as.numeric(d[[spec$var2]])
  checks <- rbind(qc_normality_check(diffs, "differences"), qc_outlier_check(diffs, "differences"))
  ordinal <- any(sapply(d[c(spec$var1, spec$var2)], qc_vartype) == "ordinal")
  dec <- qc_decide_groups(checks$met[1], checks$met[2], length(diffs), ordinal, "paired-samples t-test", "Wilcoxon signed-rank test")
  dec$reason <- sub("in every group", "for the differences", sub("each group has", "there are", dec$reason))
  list(checks = checks, decision = qc_decision(if (dec$param) "ttest_paired" else "wilcoxon_signed",
       if (dec$param) "parametric" else "non-parametric", dec$reason, c("kim", "field", "tukey")))
}

qc_screen_relationship <- function(data, spec) {
  d <- qc_complete(data, c(spec$var1, spec$var2))$data
  t1 <- qc_vartype(d[[spec$var1]]); t2 <- qc_vartype(d[[spec$var2]]); cats <- c("binary", "categorical")
  if (t1 %in% cats && t2 %in% cats) return(qc_screen_association(data, spec))
  if (xor(t1 %in% cats, t2 %in% cats)) {
    grp <- if (t1 %in% cats) spec$var1 else spec$var2; out <- if (t1 %in% cats) spec$var2 else spec$var1
    s <- qc_screen_groups(data, list(outcome = out, group = grp))
    s$decision$reason <- paste0("'", grp, "' is a grouping variable, so the relationship is tested as a group difference. ", s$decision$reason)
    s$rerouted <- list(goal = "difference", outcome = out, group = grp); return(s)
  }
  x <- as.numeric(d[[spec$var1]]); y <- as.numeric(d[[spec$var2]])
  quad <- summary(stats::lm(y ~ x + I(x^2)))$coefficients
  p_quad <- if (nrow(quad) == 3) quad[3, 4] else NA
  checks <- rbind(qc_normality_check(x, qc_lab(spec$var1)), qc_normality_check(y, qc_lab(spec$var2)),
                  qc_outlier_check(x, qc_lab(spec$var1)), qc_outlier_check(y, qc_lab(spec$var2)),
    qc_check_row("Linearity", "Quadratic term test", sprintf("p %s", qc_fmt_p(p_quad)), is.na(p_quad) || p_quad >= .05,
      if (is.na(p_quad) || p_quad >= .05) "The relationship looks linear (no significant curve)."
      else "The relationship is curved; a rank-based correlation captures monotonic curves better."))
  ordinal <- "ordinal" %in% c(t1, t2)
  norm_ok <- checks$met[1:2]; outl_ok <- checks$met[3:4]; lin_ok <- checks$met[5]
  param <- !ordinal && lin_ok && (all(norm_ok) || (nrow(d) >= 30 && all(outl_ok)))
  reason <- if (ordinal) "At least one variable is a rating-scale item (ordinal), so Spearman's rank correlation was used."
    else if (!lin_ok) "The relationship is not linear, so Spearman's rank correlation (which only requires a monotonic relationship) was used."
    else if (param && all(norm_ok)) "Both variables are approximately normal and the relationship is linear, so Pearson's correlation was used."
    else if (param) "Normality was not fully met, but n >= 30, there are no extreme outliers and the relationship is linear, so Pearson's correlation remains appropriate."
    else "Normality was not met and the sample is small or contains extreme outliers, so Spearman's rank correlation was used."
  list(checks = checks, decision = qc_decision(if (param) "correlation_pearson" else "correlation_spearman",
       if (param) "parametric" else "non-parametric", reason, c("kim", "field", "tukey")))
}

qc_screen_association <- function(data, spec) {
  v1 <- if (!is.null(spec$var1)) spec$var1 else spec$group; v2 <- if (!is.null(spec$var2)) spec$var2 else spec$outcome
  d <- qc_complete(data, c(v1, v2))$data; tab <- table(d[[v1]], d[[v2]])
  e <- outer(rowSums(tab), colSums(tab)) / sum(tab); low <- mean(e < 5)
  met <- low <= .2 && all(e >= 1)
  checks <- qc_check_row("Expected cell counts", "Cochran's rule", sprintf("%d%% of cells below 5; smallest = %s", round(100 * low), qc_fmt(min(e))),
    met, if (met) "Expected counts are large enough for the chi-square test." else "Expected counts are too small for the chi-square approximation.")
  reason <- if (met) "Expected cell counts are adequate, so Pearson's chi-square test was used."
    else "More than 20% of expected counts are below 5 (or one is below 1), so Fisher's exact test was used."
  list(checks = checks, decision = qc_decision("chisq_independence", if (met) "parametric" else "non-parametric", reason, "cochran"),
       rerouted = list(goal = "association", var1 = v1, var2 = v2))
}

qc_hc3 <- function(model) {
  X <- stats::model.matrix(model); e <- stats::residuals(model); h <- stats::hatvalues(model)
  XtXi <- solve(crossprod(X)); meat <- crossprod(X * (e / (1 - h)))
  sqrt(diag(XtXi %*% meat %*% XtXi))
}

qc_screen_prediction <- function(data, spec) {
  d <- qc_complete(data, c(spec$outcome, spec$predictors))$data
  for (v in spec$predictors) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
  yt <- qc_vartype(d[[spec$outcome]])
  f <- stats::reformulate(sprintf("`%s`", spec$predictors), response = sprintf("`%s`", spec$outcome))
  X <- NULL
  vif_of <- function(X) if (ncol(X) > 1) sapply(seq_len(ncol(X)), function(j) 1 / (1 - summary(stats::lm(X[, j] ~ X[, -j]))$r.squared)) else 1
  if (yt == "binary") {
    yy <- d[[spec$outcome]]; pos <- if (!is.null(spec$positive)) spec$positive else as.character(sort(unique(yy))[2])
    d$.y <- as.integer(as.character(yy) == pos)
    m <- suppressWarnings(stats::glm(stats::reformulate(sprintf("`%s`", spec$predictors), response = ".y"), data = d, family = stats::binomial()))
    X <- stats::model.matrix(m)[, -1, drop = FALSE]; vif <- vif_of(X)
    events <- min(sum(d$.y), nrow(d) - sum(d$.y)); epv <- events / ncol(X)
    sep <- any(stats::fitted(m) < 1e-8 | stats::fitted(m) > 1 - 1e-8) || any(abs(stats::coef(m)) > 15)
    checks <- rbind(
      qc_check_row("Events per predictor", "Peduzzi rule (>= 10)", sprintf("%.1f", epv), epv >= 10,
        if (epv >= 10) "Enough events per predictor term." else "Too few events per predictor; estimates may be biased."),
      qc_check_row("Multicollinearity", "VIF (< 5)", sprintf("max VIF = %s", qc_fmt(max(vif))), max(vif) < 5,
        if (max(vif) < 5) "Predictors are not too strongly related." else "Some predictors overlap strongly; interpret them with care."),
      qc_check_row("Separation", "Fitted probabilities of 0 or 1", if (sep) "detected" else "none", !sep,
        if (!sep) "No perfect prediction." else "A predictor perfectly separates the outcome; estimates are unreliable."))
    return(list(checks = checks, decision = qc_decision("regression_logistic", "parametric",
      paste("The outcome has two categories, so binary logistic regression was used.",
            if (all(checks$met)) "All screening checks were met." else "Some checks were not met; see the warnings."),
      c("peduzzi", "vif")), positive = pos))
  }
  m <- stats::lm(f, data = d); X <- stats::model.matrix(m)[, -1, drop = FALSE]; n <- nrow(d)
  res <- stats::residuals(m); fit <- stats::fitted(m)
  bp <- summary(stats::lm(res^2 ~ X)); bp_p <- stats::pchisq(n * bp$r.squared, ncol(X), lower.tail = FALSE)
  reset <- stats::anova(m, stats::update(m, . ~ . + I(fit^2)))$`Pr(>F)`[2]
  vif <- vif_of(X); cook <- stats::cooks.distance(m); n_inf <- sum(cook > 4 / n)
  checks <- rbind(
    qc_check_row("Linearity", "RESET-type test (fitted^2 term)", sprintf("p %s", qc_fmt_p(reset)), reset >= .05,
      if (reset >= .05) "No evidence of a curved relationship." else "There is some curvature; consider transforming a predictor."),
    qc_normality_check(res, "residuals"),
    qc_check_row("Constant variance (homoscedasticity)", "Breusch-Pagan (Koenker)", sprintf("p %s", qc_fmt_p(bp_p)), bp_p >= .05,
      if (bp_p >= .05) "Residual spread is constant." else "Residual spread is not constant; robust (HC3) standard errors are reported."),
    qc_check_row("Multicollinearity", "VIF (< 5)", sprintf("max VIF = %s", qc_fmt(max(vif))), max(vif) < 5,
      if (max(vif) < 5) "Predictors are not too strongly related." else "Some predictors overlap strongly; interpret them with care."),
    qc_check_row("Influential cases", "Cook's distance > 4/n", sprintf("%d case(s)", n_inf), n_inf <= max(1, .05 * n),
      if (n_inf <= max(1, .05 * n)) "No unusual concentration of influential cases." else "Several influential cases; check them for data errors."))
  robust <- bp_p < .05
  reason <- paste("The outcome is numeric, so multiple linear regression was used (it has no direct non-parametric replacement).",
    if (robust) "Because residual variance was not constant, heteroscedasticity-robust (HC3) standard errors were used for the coefficient tests."
    else "Residual variance was constant, so ordinary standard errors were used.",
    if (!checks$met[2] && n >= 30) "Residuals were not normal, but with n >= 30 the coefficient tests remain robust." else "")
  list(checks = checks, decision = qc_decision("regression_linear", "parametric", trimws(reason),
       c("field", "hc3", "vif", "cook", "kim")), robust = robust)
}

qc_screen_reliability <- function(data, spec) {
  d <- qc_complete(data, spec$items)$data; X <- as.data.frame(lapply(d[spec$items], as.numeric))
  smin <- if (is.null(spec$scale_min)) min(X) else spec$scale_min; smax <- if (is.null(spec$scale_max)) max(X) else spec$scale_max
  for (it in spec$reverse) X[[it]] <- smin + smax - X[[it]]
  tot <- rowSums(X); itr <- sapply(names(X), function(it) stats::cor(X[[it]], tot - X[[it]]))
  checks <- rbind(
    qc_check_row("Item-total correlations", "Corrected r >= .30", sprintf("lowest = %s (%s)", qc_fmt_r(min(itr)), names(itr)[which.min(itr)]),
      min(itr) >= .3, if (min(itr) >= .3) "Every item relates well to the rest of the scale." else "At least one item relates weakly to the rest; consider removing or re-checking it."),
    qc_check_row("Item direction", "All item-total r positive", if (any(itr < 0)) "negative r found" else "all positive", all(itr >= 0),
      if (all(itr >= 0)) "All items point in the same direction." else "An item correlates negatively - it may need reverse-coding."),
    qc_check_row("Sample size", "n >= 30", sprintf("n = %d", nrow(X)), nrow(X) >= 30,
      if (nrow(X) >= 30) "Enough responses for a stable estimate." else "Few responses; alpha will be imprecise."))
  list(checks = checks, decision = qc_decision("reliability_alpha", "reliability",
    "Cronbach's alpha was used to assess internal consistency, with item analysis to check each item.", "nunnally"))
}

# Mediation: both regressions (mediator model and outcome model) are screened like any regression.
# The indirect effect is tested with a bootstrap interval, which does not assume normality.
qc_screen_mediation <- function(data, spec) {
  sm <- qc_screen_prediction(data, list(goal = "prediction", outcome = spec$mediator, predictors = spec$var1))
  sy <- qc_screen_prediction(data, list(goal = "prediction", outcome = spec$outcome, predictors = c(spec$var1, spec$mediator)))
  tag <- function(ch, what) { ch$check <- paste0(ch$check, " - ", what); ch }
  checks <- rbind(tag(sm$checks[sm$checks$check != "Multicollinearity", ], "mediator model"), tag(sy$checks, "outcome model"))
  robust <- isTRUE(sm$robust) || isTRUE(sy$robust)
  reason <- paste("The hypothesis states that the effect works through a mediator, so a regression-based simple mediation analysis was used.",
    "The indirect effect was tested with a percentile bootstrap confidence interval (5,000 resamples), which does not assume that the indirect effect is normally distributed.",
    if (robust) "Because residual variance was not constant in at least one model, the individual path tests use HC3 robust standard errors."
    else "Residual variance was constant, so ordinary standard errors were used for the path tests.")
  list(checks = checks, decision = qc_decision("mediation", "regression-based (bootstrap)", reason, c("hayes", "preacher", "hc3", "vif", "cook")), robust = robust)
}
