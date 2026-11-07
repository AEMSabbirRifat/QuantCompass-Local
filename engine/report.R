# Plain-language sentences, APA tables, explanations, and Word + HTML report writing.

# ---- Friendly names ----
# Parse "0 = not near the river; 1 = near the river" into a named vector
qc_parse_levels <- function(txt) {
  if (is.null(txt) || is.na(txt) || !nzchar(trimws(txt))) return(NULL)
  m <- regmatches(txt, gregexpr("(-?[A-Za-z0-9_.]+)\\s*=\\s*([^;,]+)", txt))[[1]]
  if (!length(m)) return(NULL)
  keys <- trimws(sub("=.*$", "", m)); vals <- trimws(sub("^[^=]*=", "", m))
  stats::setNames(vals, keys)
}

# Read a codebook sheet (columns 'variable', 'meaning', optional 'values') if the workbook has one
qc_read_codebook <- function(path) {
  out <- list(labels = list(), level_labels = list())
  if (!tolower(tools::file_ext(path)) %in% c("xlsx", "xls")) return(out)
  for (s in readxl::excel_sheets(path)) {
    x <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = s)), error = function(e) NULL)
    if (is.null(x)) next
    nm <- tolower(names(x)); iv <- which(nm %in% c("variable", "variables", "column")); im <- which(nm %in% c("meaning", "label", "description"))
    if (!length(iv) || !length(im)) next
    iv2 <- which(nm %in% c("values", "codes", "categories"))
    for (r in seq_len(nrow(x))) {
      v <- as.character(x[r, iv[1]]); if (is.na(v)) next
      out$labels[[v]] <- as.character(x[r, im[1]])
      if (length(iv2)) { lv <- qc_parse_levels(as.character(x[r, iv2[1]])); if (!is.null(lv)) out$level_labels[[v]] <- lv }
    }
  }
  out
}

qc_and <- function(x) if (length(x) <= 1) x else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])

# ---- Hypothesis as a plain sentence ----
qc_sentence <- function(spec, data) {
  L <- qc_lab
  switch(spec$goal,
    difference = {
      lv <- levels(factor(data[[spec$group]]))
      if (length(lv) == 2) sprintf("We will test whether %s differs between %s and %s.", L(spec$outcome),
          qc_group_name(spec$group, lv[1]), qc_group_name(spec$group, lv[2]))
      else sprintf("We will test whether %s differs between the %d groups of %s (%s).", L(spec$outcome), length(lv), L(spec$group),
          qc_and(sapply(lv, function(l) qc_level_lab(spec$group, l))))
    },
    paired_difference = sprintf("We will test whether scores differ between %s and %s (the same cases measured twice).", L(spec$var1), L(spec$var2)),
    relationship = sprintf("We will test whether %s is related to %s.", L(spec$var1), L(spec$var2)),
    association = sprintf("We will test whether %s and %s are associated.", L(spec$var1), L(spec$var2)),
    prediction = sprintf("We will test whether %s %s %s%s.", qc_and(sapply(spec$predictors, L)),
                         if (length(spec$predictors) > 1) "together predict" else "predicts", L(spec$outcome),
                         if (length(spec$controls)) paste(" after controlling for", qc_and(sapply(spec$controls, L))) else ""),
    reliability = sprintf("We will check whether the %d items (%s) form a reliable scale%s.", length(spec$items),
                          qc_and(spec$items), if (length(spec$reverse)) paste0(", with ", qc_and(spec$reverse), " reverse-coded") else ""),
    mediation = sprintf("We will test whether %s mediates the relationship between %s and %s.", L(spec$mediator), L(spec$var1), L(spec$outcome)),
    describe = "We will describe the selected variables.")
}

# ---- Effect-size wording (Cohen, 1988) ----
qc_size <- function(x, type) {
  x <- abs(x); cut <- switch(type, d = c(.2, .5, .8), r = c(.1, .3, .5), eta = c(.01, .06, .14), r2 = c(.02, .13, .26))
  if (is.na(x)) "unknown" else if (x < cut[1]) "negligible" else if (x < cut[2]) "small" else if (x < cut[3]) "medium" else "large"
}

# ---- APA tables per test ----
qc_tab <- function(title, df, note = NULL) list(title = title, df = df, note = note)
qc_result_tables <- function(id, r) {
  f <- qc_fmt; fr <- qc_fmt_r; fp <- function(p) sub("^= ", "", qc_fmt_p(p))
  gl <- function(df, v) sapply(df$group, function(l) qc_level_lab(v, l))
  switch(id,
    ttest_independent = list(
      qc_tab("Descriptive statistics by group", data.frame(Group = gl(r$groups, r$.group), n = r$groups$n, M = f(r$groups$mean), SD = f(r$groups$sd), check.names = FALSE)),
      qc_tab("Welch's t-test", data.frame(t = f(r$t), df = f(r$df), p = fp(r$p), `Mean difference [95% CI]` = sprintf("%s [%s, %s]", f(r$mean_diff), f(r$mean_diff_ci[1]), f(r$mean_diff_ci[2])),
        `Cohen's d [95% CI]` = sprintf("%s [%s, %s]", f(r$cohen_d), f(r$cohen_d_ci[1]), f(r$cohen_d_ci[2])), check.names = FALSE))),
    mann_whitney = list(
      qc_tab("Descriptive statistics by group", data.frame(Group = gl(r$groups, r$.group), n = r$groups$n, Mdn = f(r$groups$median), IQR = f(r$groups$iqr))),
      qc_tab("Mann-Whitney U test", data.frame(U = f(r$U, 1), p = fp(r$p), `Rank-biserial r` = fr(r$rank_biserial), check.names = FALSE))),
    ttest_paired = list(
      qc_tab("Descriptive statistics", data.frame(Measure = sapply(r$measures$measure, qc_lab), n = r$measures$n, M = f(r$measures$mean), SD = f(r$measures$sd))),
      qc_tab("Paired-samples t-test", data.frame(t = f(r$t), df = r$df, p = fp(r$p), `Mean difference [95% CI]` = sprintf("%s [%s, %s]", f(r$mean_diff), f(r$mean_diff_ci[1]), f(r$mean_diff_ci[2])),
        `d_z [95% CI]` = sprintf("%s [%s, %s]", f(r$cohen_dz), f(r$cohen_dz_ci[1]), f(r$cohen_dz_ci[2])), check.names = FALSE))),
    wilcoxon_signed = list(
      qc_tab("Descriptive statistics", data.frame(Measure = sapply(r$measures$measure, qc_lab), n = r$measures$n, Mdn = f(r$measures$median), IQR = f(r$measures$iqr))),
      qc_tab("Wilcoxon signed-rank test", data.frame(V = f(r$V, 1), p = fp(r$p), `Rank-biserial r` = fr(r$rank_biserial), check.names = FALSE))),
    anova_oneway = list(
      qc_tab("Descriptive statistics by group", data.frame(Group = gl(r$groups, r$.group), n = r$groups$n, M = f(r$groups$mean), SD = f(r$groups$sd))),
      qc_tab("Welch's one-way ANOVA", data.frame(F = f(r$welch$F), df1 = r$welch$df1, df2 = f(r$welch$df2), p = fp(r$welch$p), `eta-squared` = fr(r$eta2), `omega-squared` = fr(r$omega2), check.names = FALSE)),
      qc_tab("Games-Howell post-hoc comparisons", data.frame(Comparison = r$posthoc$comparison, `Mean difference` = f(r$posthoc$mean_diff),
        `95% CI` = sprintf("[%s, %s]", f(r$posthoc$ci_low), f(r$posthoc$ci_high)), t = f(r$posthoc$t), df = f(r$posthoc$df), `p (adjusted)` = sapply(r$posthoc$p_adj, fp), check.names = FALSE))),
    kruskal_wallis = list(
      qc_tab("Medians by group", data.frame(Group = gl(r$groups, r$.group), n = r$groups$n, Mdn = f(r$groups$median))),
      qc_tab("Kruskal-Wallis test", data.frame(H = f(r$H), df = r$df, p = fp(r$p), `epsilon-squared` = fr(r$epsilon2), check.names = FALSE)),
      { pm <- r$pairwise_p_holm; idx <- which(!is.na(pm), arr.ind = TRUE)
        qc_tab("Pairwise comparisons (Mann-Whitney, Holm-corrected)", data.frame(Comparison = paste(rownames(pm)[idx[, 1]], "vs", colnames(pm)[idx[, 2]]),
          `p (adjusted)` = sapply(pm[idx], fp), check.names = FALSE)) }),
    correlation_pearson = list(qc_tab("Pearson correlation", data.frame(r = fr(r$r), `95% CI` = sprintf("[%s, %s]", fr(r$r_ci[1]), fr(r$r_ci[2])), t = f(r$t), df = r$df, p = fp(r$p), N = r$n_used, check.names = FALSE))),
    correlation_spearman = list(qc_tab("Spearman rank correlation", data.frame(rho = fr(r$rho), `95% CI` = sprintf("[%s, %s]", fr(r$rho_ci[1]), fr(r$rho_ci[2])), p = fp(r$p), N = r$n_used, check.names = FALSE))),
    chisq_independence = {
      tb <- r$table; pc <- r$row_percent
      ct <- as.data.frame.matrix(matrix(sprintf("%d (%.1f%%)", tb, pc), nrow(tb), dimnames = list(NULL, sapply(colnames(tb), function(l) qc_level_lab(names(dimnames(tb))[2], l)))))
      ct <- cbind(Group = sapply(rownames(tb), function(l) qc_level_lab(names(dimnames(tb))[1], l)), ct)
      list(qc_tab("Cross-tabulation: counts (row %)", ct),
           qc_tab(if (r$used_fisher) "Fisher's exact test" else "Chi-square test of independence",
             data.frame(`Chi-square` = f(r$chisq), df = r$df, p = fp(r$p), `Cramer's V` = fr(r$cramers_v), N = r$n_used, check.names = FALSE)))
    },
    regression_linear = {
      cf <- r$coefficients
      list(qc_tab("Regression coefficients", data.frame(Predictor = cf$label,
        b = f(cf$b), SE = f(cf$se), beta = c("", f(cf$beta[-1])), t = f(cf$t), p = sapply(cf$p, fp),
        `95% CI for b` = sprintf("[%s, %s]", f(cf$ci_low), f(cf$ci_high)), check.names = FALSE),
        note = if (isTRUE(r$robust)) "Standard errors, t and p use HC3 heteroscedasticity-robust estimates." else NULL),
        qc_tab("Model fit", data.frame(`R-squared` = fr(r$r2), `Adjusted R-squared` = fr(r$adj_r2), F = f(r$F), df = sprintf("%d, %d", r$df1, r$df2), p = fp(r$p), N = r$n_used, check.names = FALSE)))
    },
    regression_hierarchical = {
      st <- r$steps; cf <- r$coefficients
      list(qc_tab("Model comparison (hierarchical regression)", data.frame(Step = st$step, `R-squared` = fr(st$R2), `Adjusted R-squared` = fr(st$adj_R2),
          `Change in R-squared` = fr(st$delta_R2, 3), `F change` = c("-", f(st$F_change[2])), `p (change)` = c("-", fp(st$p_change[2])), check.names = FALSE),
          note = sprintf("N = %d (the same cases in both steps).%s", r$n_used, if (isTRUE(r$robust)) " The change is tested with a robust (HC3) Wald test." else "")),
        qc_tab("Coefficients of the final model", data.frame(Predictor = cf$label, Role = cf$step, b = f(cf$b), SE = f(cf$se), beta = c("", f(cf$beta[-1])), t = f(cf$t),
          p = sapply(cf$p, fp), `95% CI for b` = sprintf("[%s, %s]", f(cf$ci_low), f(cf$ci_high)), check.names = FALSE),
          note = if (isTRUE(r$robust)) "Standard errors, t and p use HC3 heteroscedasticity-robust estimates." else NULL))
    },
    regression_logistic = {
      cf <- r$coefficients
      list(qc_tab("Logistic regression coefficients", data.frame(Predictor = cf$label,
        b = f(cf$b), SE = f(cf$se), OR = f(cf$odds_ratio), `95% CI for OR` = sprintf("[%s, %s]", f(cf$or_ci_low), f(cf$or_ci_high)), z = f(cf$z), p = sapply(cf$p, fp), check.names = FALSE)),
        qc_tab("Model fit", data.frame(`Chi-square` = f(r$lr_chisq), df = r$df, p = fp(r$p), `Nagelkerke R-squared` = fr(r$r2_nagelkerke),
          `Correct classification` = sprintf("%.1f%%", 100 * r$accuracy_at_0.5), N = r$n_used, check.names = FALSE)))
    },
    reliability_alpha = list(qc_tab("Item analysis", data.frame(Item = r$items$item, Reversed = ifelse(r$items$reversed, "yes", ""), M = f(r$items$mean), SD = f(r$items$sd),
      `Item-total r` = fr(r$items$corrected_item_total_r), `Alpha if deleted` = if (all(is.na(r$items$alpha_if_deleted))) "" else fr(r$items$alpha_if_deleted), check.names = FALSE),
      note = sprintf("Cronbach's alpha = %s. Scale score: M = %s, SD = %s.", fr(r$alpha), f(r$scale_mean), f(r$scale_sd)))),
    mediation = { pt <- r$paths
      list(qc_tab("Regression paths of the mediation model", data.frame(Path = pt$path, b = f(pt$b), SE = f(pt$se), t = f(pt$t), df = pt$df, p = sapply(pt$p, fp), check.names = FALSE),
             note = if (isTRUE(r$robust)) "SE, t and p use HC3 heteroscedasticity-robust estimates." else NULL),
           qc_tab("Indirect effect", data.frame(Effect = "Indirect effect (a x b)", Estimate = f(r$ab, 3), `95% bootstrap CI` = sprintf("[%s, %s]", f(r$ab_ci[1], 3), f(r$ab_ci[2], 3)),
             `Standardised` = f(r$ab_cs, 3), `Share of total effect` = if (is.na(r$prop_mediated)) "-" else sprintf("%.0f%%", 100 * r$prop_mediated), N = r$n_used, check.names = FALSE),
             note = sprintf("Percentile bootstrap with %s resamples (seed 2026).", format(r$boot, big.mark = ",")))) },
    list())
}

# ---- Plain-language explanation ----
qc_explain <- function(id, r, s) {
  pv <- r[["p"]]; sig <- !is.null(pv) && length(pv) == 1 && !is.na(pv) && pv < .05
  pw <- if (sig) "This difference is unlikely to be due to chance alone (p < .05)." else "The data do not provide enough evidence of a real difference (p >= .05); this does not prove there is no difference."
  switch(id,
    ttest_independent = { hi <- r$groups[which.max(r$groups$mean), ]
      sprintf("Welch's t-test compares the average %s of two groups without assuming they vary equally. %s had the higher average (M = %s). %s The size of the difference is %s (Cohen's d = %s).",
              qc_lab(s$outcome), qc_level_lab(s$group, hi$group), qc_fmt(hi$mean), pw, qc_size(r$cohen_d, "d"), qc_fmt(r$cohen_d)) },
    mann_whitney = { hi <- r$groups[which.max(r$groups$median), ]
      sprintf("The Mann-Whitney U test ranks all values from lowest to highest and checks whether one group tends to have higher ranks. It does not need normally distributed data. %s had the higher median (Mdn = %s). %s The effect is %s (rank-biserial r = %s).",
              qc_level_lab(s$group, hi$group), qc_fmt(hi$median), pw, qc_size(r$rank_biserial, "r"), qc_fmt_r(r$rank_biserial)) },
    ttest_paired = sprintf("The paired t-test compares two measurements of the same cases by looking at each case's difference. On average the difference was %s. %s The effect is %s (d_z = %s).",
                           qc_fmt(r$mean_diff), pw, qc_size(r$cohen_dz, "d"), qc_fmt(r$cohen_dz)),
    wilcoxon_signed = sprintf("The Wilcoxon signed-rank test compares two measurements of the same cases using the ranks of their differences, without assuming normality. %s The effect is %s (r = %s).",
                              pw, qc_size(r$rank_biserial, "r"), qc_fmt_r(r$rank_biserial)),
    anova_oneway = sprintf("Welch's ANOVA compares the averages of several groups without assuming equal variances. %s The groups explain %s of the differences in %s (eta-squared = %s, a %s effect). The post-hoc table shows which pairs of groups differ.",
                           pw, sprintf("%.0f%%", 100 * r$eta2), qc_lab(s$outcome), qc_fmt_r(r$eta2), qc_size(r$eta2, "eta")),
    kruskal_wallis = sprintf("The Kruskal-Wallis test compares several groups using ranks instead of averages, so it does not need normal data. %s The effect is %s (epsilon-squared = %s). The pairwise table shows which groups differ.",
                             pw, qc_size(r$epsilon2, "eta"), qc_fmt_r(r$epsilon2)),
    correlation_pearson = sprintf("Pearson's r measures how closely two numeric variables follow a straight line, from -1 to +1. Here r = %s: a %s, %s relationship. %s Correlation does not show that one variable causes the other.",
                                  qc_fmt_r(r$r), qc_size(r$r, "r"), if (r$r > 0) "positive (both rise together)" else "negative (one rises as the other falls)",
                                  if (sig) "It is statistically significant (p < .05)." else "It is not statistically significant (p >= .05)."),
    correlation_spearman = sprintf("Spearman's rho measures whether two variables rise or fall together, using ranks, so it works for rating scales, outliers and curved relationships. Here rho = %s: a %s, %s relationship. %s Correlation does not show causation.",
                                   qc_fmt_r(r$rho), qc_size(r$rho, "r"), if (r$rho > 0) "positive" else "negative",
                                   if (sig) "It is statistically significant (p < .05)." else "It is not statistically significant (p >= .05)."),
    chisq_independence = sprintf("This test checks whether the distribution of one category depends on another (compare the row percentages in the table). %s The strength of the association is %s (Cramer's V = %s).",
                                 if (sig) "The two variables are associated (p < .05)." else "There is no evidence of an association (p >= .05).", qc_size(r$cramers_v, "r"), qc_fmt_r(r$cramers_v)),
    regression_hierarchical = sprintf("A hierarchical regression asks whether %s still explain%s %s once %s are taken into account. The controls alone explain %.0f%% of the variation; adding the predictor%s raises this to %.0f%% (change = %s percentage points). %s Each b in the final model is the expected change in %s for a one-unit increase in that variable, with all others held constant.",
      qc_and(sapply(s$predictors, qc_lab)), if (length(s$predictors) == 1) "s" else "", qc_lab(s$outcome), qc_and(sapply(s$controls, qc_lab)), 100 * r$r2_step1,
      if (length(s$predictors) > 1) "s" else "", 100 * r$r2, qc_fmt(100 * r$delta_r2, 1),
      if (r$p < .05) "This increase is statistically significant." else "This increase is not statistically significant.", qc_lab(s$outcome)),
    regression_linear = { cf <- r$coefficients[-1, ]; sp <- rownames(cf)[cf$p < .05]
      sprintf("Linear regression estimates how %s changes with each predictor while the other predictors are held constant. Together the predictors explain %.0f%% of the variation in %s (R-squared = %s, a %s effect). %s Each b value is the expected change in %s for a one-unit increase in that predictor.",
              qc_lab(s$outcome), 100 * r$r2, qc_lab(s$outcome), qc_fmt_r(r$r2), qc_size(r$r2, "r2"),
              if (length(sp)) paste0("Significant predictors: ", qc_and(sapply(sp, function(v) sprintf("%s (%s)", cf[v, "label"], if (cf[v, "b"] > 0) "higher values go with higher outcome" else "higher values go with lower outcome"))), ".")
              else "No single predictor was significant.", qc_lab(s$outcome)) },
    regression_logistic = sprintf("Logistic regression estimates how each predictor changes the odds of the outcome. An odds ratio above 1 means higher odds, below 1 lower odds. %s The model classifies %.0f%% of cases correctly.",
                                  if (sig) "The predictors together improve prediction significantly (p < .05)." else "The predictors together do not improve prediction significantly.", 100 * r$accuracy_at_0.5),
    reliability_alpha = sprintf("Cronbach's alpha shows how consistently the items measure the same thing (0 to 1; .70 or higher is usually acceptable). Here alpha = %s. %s",
                                qc_fmt_r(r$alpha), if (r$alpha >= .7) "The items can be averaged into one scale score." else "The items are not consistent enough to average without checking the weak items."),
    mediation = sprintf("A mediation analysis asks whether %s is linked to %s partly because it changes %s, which in turn changes %s. The indirect effect (a x b = %s) is that part. Its 95%% bootstrap interval [%s, %s] %s. The direct effect (c' = %s) is what remains of the link once %s is taken into account. With survey data collected at one time point, this shows a pattern consistent with mediation, not proof of cause and effect.",
                        qc_lab(s$var1), qc_lab(s$outcome), qc_lab(s$mediator), qc_lab(s$outcome), qc_fmt(r$ab, 3), qc_fmt(r$ab_ci[1], 3), qc_fmt(r$ab_ci[2], 3),
                        if (r$excludes_zero) "does not include zero, so the indirect effect is statistically significant" else "includes zero, so there is not enough evidence of an indirect effect",
                        qc_fmt(r$c_prime), qc_lab(s$mediator)),
    "")
}

# ---- Ready-to-paste paragraph ----
qc_paste_paragraph <- function(h, screen, result) {
  ch <- screen$checks; failed <- ch[!is.na(ch$met) & !ch$met, ]
  screening <- if (nrow(failed) == 0) "All assumption checks were met." else paste0("The following assumption checks were not met: ",
    qc_and(sprintf("%s (%s)", sub("^(.)", "\\L\\1", failed$check, perl = TRUE), failed$value)), ".")
  sprintf("To test %s (%s), the assumptions were screened before the analysis was chosen. %s %s %s",
          h$id, sub("\\.$", "", h$text), screening, screen$decision$reason, result$apa)
}

# ---- Writers ----
qc_html_table <- function(t, n) {
  df <- t$df; head <- paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>")
  rows <- paste0(apply(df, 1, function(r) paste0("<tr>", paste0("<td>", htmltools::htmlEscape(r), "</td>", collapse = ""), "</tr>")), collapse = "")
  paste0("<p class='tabno'><b>Table ", n, "</b></p><p class='tabtitle'><i>", htmltools::htmlEscape(t$title), "</i></p><table>", head, rows, "</table>",
         if (!is.null(t$note)) paste0("<p class='note'><i>Note.</i> ", htmltools::htmlEscape(t$note), "</p>") else "")
}

qc_css <- "body{font-family:Georgia,'Times New Roman',serif;max-width:820px;margin:2rem auto;padding:0 1.2rem;color:#1f2933;line-height:1.55}
h1{font-size:1.5rem}h2{font-size:1.15rem;border-bottom:1px solid #e5e7eb;padding-bottom:.25rem;margin-top:2rem}
table{border-collapse:collapse;margin:.4rem 0 .6rem;font-size:.92rem}th{border-top:1.5px solid #1f2933;border-bottom:1px solid #1f2933;padding:.3rem .6rem;text-align:left}
td{padding:.25rem .6rem}tr:last-child td{border-bottom:1.5px solid #1f2933}.tabno,.tabtitle{margin:.8rem 0 0}.note{font-size:.88rem;color:#4b5563}
.paste{background:#f4f8fd;border-left:4px solid #2a78d6;padding:.8rem 1rem}.met{color:#047857}.notmet{color:#b91c1c}
img{max-width:100%}.fig{margin:1rem 0}.cap{font-size:.9rem}pre{background:#f7f7f5;padding:.8rem;font-size:.78rem;overflow-x:auto;white-space:pre-wrap}
.term{border-bottom:1px dotted #2a78d6;cursor:help}#qc-tip{position:fixed;z-index:10;max-width:340px;background:#1f2933;color:#fff;font:13px/1.45 system-ui,sans-serif;padding:.6rem .7rem;border-radius:6px;box-shadow:0 4px 14px rgba(0,0,0,.2);display:none}
.gl{margin:.35rem 0}"

# Glossary helpers: HTML hover notes and Word comments on the first occurrence of each term
qc_html_terms <- function(text, used) {
  r <- qc_term_segments(text, used)
  html <- paste0(vapply(r$segs, function(sg) if (is.na(sg$key)) htmltools::htmlEscape(sg$text) else
    paste0("<span class='term' tabindex='0' data-def='", htmltools::htmlEscape(paste0(sg$key, ": ", qc_glossary[[sg$key]]$def), TRUE), "'>",
           htmltools::htmlEscape(sg$text), "</span>"), character(1)), collapse = "")
  list(html = html, used = r$used)
}
qc_word_terms <- function(text, used, prefix = "") {
  r <- qc_term_segments(text, used)
  runs <- lapply(r$segs, function(sg) if (is.na(sg$key)) officer::ftext(sg$text) else
    officer::run_comment(cmt = officer::block_list(officer::fpar(officer::ftext(paste0(sg$key, ": ", qc_glossary[[sg$key]]$def)))),
                         run = officer::ftext(sg$text), author = "QuantCompass", initials = "QC"))
  list(par = do.call(officer::fpar, c(if (nzchar(prefix)) list(officer::ftext(prefix)), runs)), used = r$used)
}
qc_report_text <- function(sections) paste(unlist(lapply(sections, function(s) lapply(s$items, function(it) switch(it$type,
  p = it$text, paste = it$text, list = it$items, table = c(it$table$title, it$table$note, names(it$table$df), unlist(it$table$df)),
  checks = c(it$checks$check, it$checks$method, it$checks$meaning), figure = it$fig$caption, NULL)))), collapse = " ")

# sections: list of list(heading, items) where items are list(type = "p"|"paste"|"table"|"figure"|"code"|"list", ...)
qc_write_report <- function(title, sections, out_base) {
  dir.create(dirname(out_base), recursive = TRUE, showWarnings = FALSE)
  terms <- qc_terms_in(qc_report_text(sections))
  if (length(terms)) sections[[length(sections) + 1]] <- list(heading = "Key terms in this report", glossary = TRUE, items = c(
    list(list(type = "p", text = "Short, plain-language definitions of the statistical terms used above. In the Word file, the first mention of each term also carries a comment with its definition.", plain = TRUE)),
    lapply(sort(terms), function(k) list(type = "term", key = k))))
  # HTML
  used <- character(); tn <- 0; fn <- 0
  html <- c("<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'><title>", htmltools::htmlEscape(title), "</title><style>", qc_css, "</style></head><body>",
            "<h1>", htmltools::htmlEscape(title), "</h1>")
  for (s in sections) {
    html <- c(html, "<h2>", htmltools::htmlEscape(s$heading), "</h2>")
    for (it in s$items) {
      if (it$type %in% c("p", "paste") && !isTRUE(it$plain)) { r <- qc_html_terms(it$text, used); used <- r$used
        html <- c(html, if (it$type == "p") paste0("<p>", r$html, "</p>") else paste0("<div class='paste'>", r$html, "</div>")); next }
      if (it$type == "list") { li <- character(); for (x in it$items) { r <- qc_html_terms(x, used); used <- r$used; li <- c(li, paste0("<li>", r$html, "</li>")) }
        html <- c(html, "<ul>", li, "</ul>"); next }
      html <- c(html, switch(it$type,
        p = paste0("<p>", htmltools::htmlEscape(it$text), "</p>"),
        term = paste0("<p class='gl'><b>", htmltools::htmlEscape(it$key), "</b> - ", htmltools::htmlEscape(qc_glossary[[it$key]]$def), "</p>"),
        table = { tn <- tn + 1; qc_html_table(it$table, tn) },
        figure = { fn <- fn + 1; paste0("<div class='fig'><p><b>Figure ", fn, "</b></p><img src='data:image/png;base64,",
          jsonlite::base64_enc(readBin(it$fig$file, "raw", file.info(it$fig$file)$size)), "' alt='", htmltools::htmlEscape(it$fig$caption),
          "'><p class='cap'><i>", htmltools::htmlEscape(it$fig$caption), "</i></p></div>") },
        checks = paste0("<table><tr><th>Check</th><th>Method</th><th>Result</th><th>Met?</th><th>What it means</th></tr>",
          paste0(apply(it$checks, 1, function(r) paste0("<tr><td>", htmltools::htmlEscape(r[["check"]]), "</td><td>", htmltools::htmlEscape(r[["method"]]),
            "</td><td>", htmltools::htmlEscape(r[["value"]]), "</td><td class='", if (is.na(as.logical(r[["met"]]))) "" else if (as.logical(r[["met"]])) "met" else "notmet", "'>",
            if (is.na(as.logical(r[["met"]]))) "n/a" else if (as.logical(r[["met"]])) "Yes" else "No", "</td><td>", htmltools::htmlEscape(r[["meaning"]]), "</td></tr>")), collapse = ""), "</table>"),
        code = paste0("<pre>", htmltools::htmlEscape(paste(it$lines, collapse = "\n")), "</pre>")))
    }
  }
  html <- c(html, "<p class='note'>Generated by QuantCompass (open source, GPL-3.0). Results are computed in R; check your design with your supervisor. Hover over or tap underlined terms for a short definition.</p><div id='qc-tip'></div><script>(function(){var t=document.getElementById('qc-tip');function show(e){var el=e.target.closest('.term');if(!el)return;t.textContent=el.getAttribute('data-def');t.style.display='block';var r=el.getBoundingClientRect(),w=t.offsetWidth,h=t.offsetHeight;var x=Math.min(Math.max(8,r.left),window.innerWidth-w-8);var y=r.bottom+6;if(y+h>window.innerHeight-8)y=r.top-h-6;t.style.left=x+'px';t.style.top=y+'px';}function hide(e){if(e.target.closest('.term'))t.style.display='none';}document.addEventListener('mouseover',show);document.addEventListener('focusin',show);document.addEventListener('mouseout',hide);document.addEventListener('focusout',hide);window.addEventListener('scroll',function(){t.style.display='none';});})();</script></body></html>")
  writeLines(paste(html, collapse = ""), paste0(out_base, ".html"), useBytes = TRUE)
  # Word (comments are not placed inside the ready-to-paste paragraph, so it can be copied cleanly)
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  used <- character(); tn <- 0; fn <- 0
  for (s in sections) {
    doc <- officer::body_add_par(doc, s$heading, style = "heading 2")
    for (it in s$items) {
      if (it$type == "paste" || isTRUE(it$plain)) doc <- officer::body_add_par(doc, it$text, style = "Normal")
      else if (it$type == "p") { r <- qc_word_terms(it$text, used); used <- r$used; doc <- officer::body_add_fpar(doc, r$par) }
      else if (it$type == "list") for (x in it$items) { r <- qc_word_terms(x, used, "• "); used <- r$used; doc <- officer::body_add_fpar(doc, r$par) }
      else if (it$type == "term") doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(it$key, officer::fp_text(bold = TRUE)),
        officer::ftext(paste0(" - ", qc_glossary[[it$key]]$def))))
      else if (it$type == "table") { tn <- tn + 1
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(paste("Table", tn), officer::fp_text(bold = TRUE))))
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(it$table$title, officer::fp_text(italic = TRUE))))
        doc <- officer::body_add_table(doc, it$table$df, style = "table_template")
        if (!is.null(it$table$note)) doc <- officer::body_add_par(doc, paste("Note.", it$table$note), style = "Normal")
      } else if (it$type == "checks") {
        ck <- it$checks; ck$met <- ifelse(is.na(ck$met), "n/a", ifelse(ck$met, "Yes", "No"))
        names(ck) <- c("Check", "Method", "Result", "Met?", "What it means")
        doc <- officer::body_add_table(doc, ck, style = "table_template")
      } else if (it$type == "figure") { fn <- fn + 1
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(paste("Figure", fn), officer::fp_text(bold = TRUE))))
        w <- 6; h <- w * it$fig$height / it$fig$width
        doc <- officer::body_add_img(doc, it$fig$file, width = w, height = h)
        doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(it$fig$caption, officer::fp_text(italic = TRUE, font.size = 9))))
      } else if (it$type == "code") {
        for (ln in it$lines) doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(if (nzchar(ln)) ln else " ",
          officer::fp_text(font.family = "Consolas", font.size = 7.5))))
      }
    }
  }
  print(doc, target = paste0(out_base, ".docx"))
  c(html = paste0(out_base, ".html"), docx = paste0(out_base, ".docx"))
}

# ---- Formal hypotheses (H1 / H0) and the decision after the test ----
qc_hypothesis_pair <- function(spec, data, hid = "H1") {
  L <- qc_lab
  one_sided <- !is.null(spec$alternative) && spec$alternative != "two.sided"
  dir <- if (is.null(spec[["direction"]])) NA_character_ else spec[["direction"]]        # "positive" / "negative" (relationships, mediation)
  hi <- if (is.null(spec$expect_higher)) NA_character_ else as.character(spec$expect_higher)   # group expected higher (two groups)
  expect_sign <- NA_real_; expect_text <- NA_character_
  p <- switch(spec$goal,
    difference = {
      lv <- levels(factor(data[[spec$group]]))
      if (length(lv) == 2) { a <- qc_group_name(spec$group, lv[1]); b <- qc_group_name(spec$group, lv[2])
        if (!is.na(hi) && hi %in% lv) { up <- qc_group_name(spec$group, hi); lo <- qc_group_name(spec$group, setdiff(lv, hi))
          expect_sign <- if (hi == lv[1]) 1 else -1; expect_text <- sprintf("higher %s in %s", L(spec$outcome), up)
          list(h1 = sprintf("%s is higher in %s than in %s.", L(spec$outcome), up, lo),
               h0 = if (one_sided) sprintf("%s is not higher in %s than in %s.", L(spec$outcome), up, lo) else sprintf("%s does not differ between %s and %s.", L(spec$outcome), a, b))
        } else list(h1 = sprintf("%s differs between %s and %s.", L(spec$outcome), a, b),
             h0 = sprintf("%s does not differ between %s and %s.", L(spec$outcome), a, b))
      } else if (!is.null(spec$trend) && !is.na(spec$trend)) list(
          h1 = sprintf("%s differs between the %d groups of %s and %s from %s to %s.", L(spec$outcome), length(lv), L(spec$group),
                       if (spec$trend == "increasing") "rises" else "falls", qc_group_name(spec$group, lv[1]), qc_group_name(spec$group, lv[length(lv)])),
          h0 = sprintf("%s is the same in all %d groups of %s (no difference and no trend).", L(spec$outcome), length(lv), L(spec$group)),
          note = sprintf("Tested with a group comparison and a one-sided trend test across the ordered groups (%s). The research hypothesis counts as supported only if both are significant.",
                         paste(vapply(lv, function(l) qc_group_name(spec$group, l), ""), collapse = " < ")))
      else list(h1 = sprintf("%s differs between at least two of the %d groups of %s.", L(spec$outcome), length(lv), L(spec$group)),
                  h0 = sprintf("%s is the same in all %d groups of %s.", L(spec$outcome), length(lv), L(spec$group)),
                  note = if (!is.null(spec$expect_extreme)) sprintf("Tested two-sided at alpha = .05 (all groups at once). The expectation that '%s' has the %s value is checked with the post-hoc comparisons and group means in the report.",
                                                                    qc_group_name(spec$group, spec$expect_extreme$level), spec$expect_extreme$which) else NULL)
    },
    paired_difference = list(h1 = sprintf("Scores differ between %s and %s.", L(spec$var1), L(spec$var2)),
                             h0 = sprintf("There is no difference between %s and %s.", L(spec$var1), L(spec$var2))),
    relationship = if (!is.na(dir)) { expect_sign <- if (dir == "positive") 1 else -1; expect_text <- paste("a", dir, "relationship")
        list(h1 = sprintf("%s is %sly related to %s (the correlation is %s zero).", L(spec$var1), dir, L(spec$var2), if (dir == "positive") "above" else "below"),
             h0 = if (one_sided) sprintf("%s is not %sly related to %s (the correlation is zero or %s).", L(spec$var1), dir, L(spec$var2), if (dir == "positive") "negative" else "positive")
                  else sprintf("%s is not related to %s (the correlation is zero).", L(spec$var1), L(spec$var2)))
      } else list(h1 = sprintf("%s is related to %s (the correlation is not zero).", L(spec$var1), L(spec$var2)),
                  h0 = sprintf("%s is not related to %s (the correlation is zero).", L(spec$var1), L(spec$var2))),
    association = list(h1 = sprintf("%s and %s are associated.", L(spec$var1), L(spec$var2)),
                       h0 = sprintf("%s and %s are independent (not associated).", L(spec$var1), L(spec$var2))),
    prediction = { pr <- qc_and(sapply(spec$predictors, L)); several <- length(spec$predictors) > 1
      dirs <- spec$directions; dirs <- dirs[!is.na(dirs) & names(dirs) %in% spec$predictors]
      dtxt <- if (length(dirs)) paste0("; ", qc_and(vapply(names(dirs), function(v) sprintf("higher %s goes with %s %s", L(v), if (dirs[[v]] == "positive") "higher" else "lower", L(spec$outcome)), ""))) else ""
      if (length(spec$controls)) {
        cl <- qc_and(sapply(spec$controls, L))
        list(h1 = sprintf("%s %s %s beyond %s (the change in R-squared is above zero)%s.", pr, if (several) "add to the prediction of" else "adds to the prediction of", L(spec$outcome), cl, dtxt),
             h0 = sprintf("%s %s %s beyond %s (the change in R-squared is zero).", pr, if (several) "do not add to the prediction of" else "does not add to the prediction of", L(spec$outcome), cl))
      } else list(h1 = sprintf("%s %s %s%s.", pr, if (several) "together predict" else "predicts", L(spec$outcome), dtxt),
           h0 = sprintf("%s %s %s (all coefficients are zero).", pr, if (several) "do not predict" else "does not predict", L(spec$outcome))) },
    reliability = list(h1 = sprintf("The %d items form a reliable scale (Cronbach's alpha of at least .70).", length(spec$items)),
                       h0 = sprintf("The %d items do not form a reliable scale (Cronbach's alpha below .70).", length(spec$items)),
                       note = "Reliability is judged against the conventional .70 benchmark, not with a significance test."),
    mediation = { if (!is.na(dir)) { expect_sign <- if (dir == "positive") 1 else -1; expect_text <- paste("a", dir, "indirect effect") }
      list(h1 = sprintf("%s mediates the relationship between %s and %s (the indirect effect is %s).", L(spec$mediator), L(spec$var1), L(spec$outcome),
                        if (is.na(dir)) "not zero" else dir),
           h0 = sprintf("%s does not mediate the relationship between %s and %s (the indirect effect is zero).", L(spec$mediator), L(spec$var1), L(spec$outcome)),
           note = "Judged by the 95% bootstrap confidence interval of the indirect effect: the null hypothesis is rejected when the interval does not include zero.") },
    list(h1 = "Not applicable.", h0 = "Not applicable."))
  p$h1 <- qc_cap_first(p$h1); p$h0 <- qc_cap_first(p$h0)
  if (is.null(p$note)) p$note <- if (one_sided) "Tested one-sided (the direction was stated before the analysis) at a significance level of alpha = .05."
    else if (!is.na(expect_sign)) sprintf("Tested two-sided at alpha = .05. The research hypothesis counts as supported only if the result is significant and in the stated direction (%s).", expect_text)
    else "Tested two-sided at a significance level of alpha = .05."
  if (spec$goal == "prediction" && length(spec$directions)) { dd <- spec$directions[!is.na(spec$directions) & names(spec$directions) %in% spec$predictors]
    if (length(dd)) { p$pred_dirs <- dd
      p$note <- paste(p$note, "The research hypothesis counts as fully supported only if each predictor with a stated direction is also significant in that direction.") } }
  p$lab1 <- hid; p$lab0 <- paste0("H0", sub("^H", "", hid)); p$expect_sign <- expect_sign; p$expect_text <- expect_text
  if (!is.null(spec$expect_extreme)) p$expect_extreme <- c(spec$expect_extreme, group = spec$group)
  p
}

# Sign of the observed effect (+1 / -1) for tests where a direction can be stated
qc_effect_sign <- function(id, r) {
  e <- switch(id, correlation_pearson = r$r, correlation_spearman = r$rho, ttest_independent = r$mean_diff,
              mann_whitney = r$rank_biserial, mediation = r$ab, NA)
  if (is.null(e) || is.na(e)) NA else sign(e)
}

qc_decision_line <- function(id, r, pair) {
  d <- qc_decision_core(id, r, pair)
  ex <- pair$expect_extreme; g <- r$groups
  if (!is.null(ex) && !is.null(g)) {
    val <- if ("mean" %in% names(g)) g$mean else g$median; what <- if ("mean" %in% names(g)) "mean" else "median"
    k <- if (ex$which == "lowest") which.min(val) else which.max(val); exp_lab <- qc_group_name(ex$group, ex$level)
    got <- as.character(g$group[k]) == as.character(ex$level) || qc_group_name(ex$group, g$group[k]) == exp_lab
    d$text <- paste(d$text, if (got) sprintf("As expected, '%s' has the %s %s (%s); see the post-hoc table for which differences are significant.", exp_lab, ex$which, what, qc_fmt(val[k]))
      else sprintf("Not as expected: the %s %s is in '%s' (%s), not in '%s'.", ex$which, what, qc_group_name(ex$group, g$group[k]), qc_fmt(val[k]), exp_lab))
  }
  d
}

qc_decision_core <- function(id, r, pair) {
  if (id == "reliability_alpha") {
    ok <- r$alpha >= .70
    return(list(rejected = ok, short = if (ok) paste(pair$lab1, "supported") else paste(pair$lab1, "not supported"),
      text = if (ok) sprintf("Decision: %s is supported. Cronbach's alpha = %s reaches the conventional benchmark of .70 (no significance test is involved).", pair$lab1, qc_fmt_r(r$alpha))
             else sprintf("Decision: %s is not supported. Cronbach's alpha = %s is below the conventional benchmark of .70.", pair$lab1, qc_fmt_r(r$alpha))))
  }
  es <- if (is.null(pair$expect_sign)) NA else pair$expect_sign
  wrong_way <- function(evidence) list(rejected = FALSE, short = sprintf("%s not supported (opposite direction)", pair$lab1),
    text = sprintf("Decision: %s, but the effect goes in the opposite direction to %s (which expected %s). %s is therefore not supported.", evidence, pair$lab1, pair$expect_text, pair$lab1))
  if (id == "mediation") {
    ci <- sprintf("95%% bootstrap CI [%s, %s]", qc_fmt(r$ab_ci[1], 3), qc_fmt(r$ab_ci[2], 3))
    if (!r$excludes_zero) return(list(rejected = FALSE, short = sprintf("%s not rejected", pair$lab0),
      text = sprintf("Decision: %s is not rejected. The interval of the indirect effect (ab = %s, %s) includes zero, so the data do not provide enough evidence for %s.", pair$lab0, qc_fmt(r$ab, 3), ci, pair$lab1)))
    if (!is.na(es) && sign(r$ab) != es) return(wrong_way(sprintf("the indirect effect is significant (ab = %s, %s)", qc_fmt(r$ab, 3), ci)))
    return(list(rejected = TRUE, short = sprintf("%s rejected - %s supported", pair$lab0, pair$lab1),
      text = sprintf("Decision: %s is rejected. The interval of the indirect effect (ab = %s, %s) does not include zero. The data support %s: %s", pair$lab0, qc_fmt(r$ab, 3), ci, pair$lab1, pair$h1)))
  }
  p <- r[["p"]]
  if (is.null(p) || is.na(p)) return(list(rejected = NA, short = "No decision", text = "Decision: no significance test was performed."))
  if (p < .05 && length(pair$pred_dirs) && !is.null(r$coefficients)) {
    cf <- r$coefficients; rn <- gsub("`", "", rownames(cf)); bad <- character(); good <- character()
    for (v in names(pair$pred_dirs)) { i <- which(rn == v); if (!length(i)) next
      want <- if (pair$pred_dirs[[v]] == "positive") 1 else -1; lab <- qc_lab(v)
      txt <- sprintf("%s (b = %s, p %s)", lab, qc_fmt(cf$b[i]), qc_fmt_p(cf$p[i]))
      if (cf$p[i] < .05 && sign(cf$b[i]) == want) good <- c(good, txt) else bad <- c(bad, sprintf("%s %s", txt, if (cf$p[i] >= .05) "is not significant" else "goes in the opposite direction")) }
    test <- if (id == "regression_hierarchical") "the added predictors explain significant additional variance" else "the model as a whole is significant"
    if (length(bad)) return(list(rejected = FALSE, short = sprintf("%s partly supported", pair$lab1),
      text = sprintf("Decision: %s is rejected because %s (p %s), but not every expected effect was found: %s.%s %s is therefore only partly supported.",
                     pair$lab0, test, qc_fmt_p(p), paste(bad, collapse = "; "), if (length(good)) sprintf(" As expected: %s.", paste(good, collapse = "; ")) else "", pair$lab1)))
    return(list(rejected = TRUE, short = sprintf("%s rejected - %s supported", pair$lab0, pair$lab1),
      text = sprintf("Decision: %s is rejected (%s, p %s), and each predictor goes in the expected direction: %s. The data support %s: %s",
                     pair$lab0, test, qc_fmt_p(p), paste(good, collapse = "; "), pair$lab1, pair$h1)))
  }
  if (p < .05 && !is.null(r$trend)) {
    tr <- r$trend
    if (tr$p >= .05) return(list(rejected = FALSE, short = sprintf("%s partly supported", pair$lab1),
      text = sprintf("Decision: the groups differ (p %s), but the expected %s trend across the ordered groups is not significant (rho = %s, p %s, one-sided). %s is therefore only partly supported.",
                     qc_fmt_p(p), tr[["direction"]], qc_fmt_r(tr$rho), qc_fmt_p(tr$p), pair$lab1)))
    return(list(rejected = TRUE, short = sprintf("%s rejected - %s supported", pair$lab0, pair$lab1),
      text = sprintf("Decision: %s is rejected. The groups differ (p %s) and the values show the expected %s trend (rho = %s, p %s, one-sided). The data support %s: %s",
                     pair$lab0, qc_fmt_p(p), tr[["direction"]], qc_fmt_r(tr$rho), qc_fmt_p(tr$p), pair$lab1, pair$h1)))
  }
  if (p < .05) {
    s <- qc_effect_sign(id, r)
    if (!is.na(es) && !is.na(s) && s != es) return(wrong_way(sprintf("the result is statistically significant (p %s)", qc_fmt_p(p))))
    list(rejected = TRUE, short = sprintf("%s rejected - %s supported", pair$lab0, pair$lab1),
      text = sprintf("Decision: %s is rejected (p %s, below the significance level of .05). The data support %s: %s", pair$lab0, qc_fmt_p(p), pair$lab1, pair$h1))
  } else list(rejected = FALSE, short = sprintf("%s not rejected", pair$lab0),
    text = sprintf("Decision: %s is not rejected (p %s, not below the significance level of .05). The data do not provide enough evidence for %s. Not rejecting %s does not prove that it is true.", pair$lab0, qc_fmt_p(p), pair$lab1, pair$lab0))
}
