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
    prediction = sprintf("We will test whether %s %s %s.", qc_and(sapply(spec$predictors, L)),
                         if (length(spec$predictors) > 1) "together predict" else "predicts", L(spec$outcome)),
    reliability = sprintf("We will check whether the %d items (%s) form a reliable scale%s.", length(spec$items),
                          qc_and(spec$items), if (length(spec$reverse)) paste0(", with ", qc_and(spec$reverse), " reverse-coded") else ""),
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
    regression_linear = { cf <- r$coefficients[-1, ]; sp <- rownames(cf)[cf$p < .05]
      sprintf("Linear regression estimates how %s changes with each predictor while the other predictors are held constant. Together the predictors explain %.0f%% of the variation in %s (R-squared = %s, a %s effect). %s Each b value is the expected change in %s for a one-unit increase in that predictor.",
              qc_lab(s$outcome), 100 * r$r2, qc_lab(s$outcome), qc_fmt_r(r$r2), qc_size(r$r2, "r2"),
              if (length(sp)) paste0("Significant predictors: ", qc_and(sapply(sp, function(v) sprintf("%s (%s)", cf[v, "label"], if (cf[v, "b"] > 0) "higher values go with higher outcome" else "higher values go with lower outcome"))), ".")
              else "No single predictor was significant.", qc_lab(s$outcome)) },
    regression_logistic = sprintf("Logistic regression estimates how each predictor changes the odds of the outcome. An odds ratio above 1 means higher odds, below 1 lower odds. %s The model classifies %.0f%% of cases correctly.",
                                  if (sig) "The predictors together improve prediction significantly (p < .05)." else "The predictors together do not improve prediction significantly.", 100 * r$accuracy_at_0.5),
    reliability_alpha = sprintf("Cronbach's alpha shows how consistently the items measure the same thing (0 to 1; .70 or higher is usually acceptable). Here alpha = %s. %s",
                                qc_fmt_r(r$alpha), if (r$alpha >= .7) "The items can be averaged into one scale score." else "The items are not consistent enough to average without checking the weak items."),
    "")
}

# ---- Ready-to-paste paragraph ----
qc_paste_paragraph <- function(h, screen, result) {
  ch <- screen$checks; failed <- ch[!is.na(ch$met) & !ch$met, ]
  screening <- if (nrow(failed) == 0) "All assumption checks were met." else paste0("The following assumption checks were not met: ",
    qc_and(sprintf("%s (%s)", tolower(failed$check), failed$value)), ".")
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
img{max-width:100%}.fig{margin:1rem 0}.cap{font-size:.9rem}pre{background:#f7f7f5;padding:.8rem;font-size:.78rem;overflow-x:auto;white-space:pre-wrap}"

# sections: list of list(heading, items) where items are list(type = "p"|"paste"|"table"|"figure"|"code"|"list", ...)
qc_write_report <- function(title, sections, out_base) {
  dir.create(dirname(out_base), recursive = TRUE, showWarnings = FALSE)
  # HTML
  tn <- 0; fn <- 0; html <- c("<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'><title>", htmltools::htmlEscape(title), "</title><style>", qc_css, "</style></head><body>",
                              "<h1>", htmltools::htmlEscape(title), "</h1>")
  for (s in sections) {
    html <- c(html, "<h2>", htmltools::htmlEscape(s$heading), "</h2>")
    for (it in s$items) html <- c(html, switch(it$type,
      p = paste0("<p>", htmltools::htmlEscape(it$text), "</p>"),
      paste = paste0("<div class='paste'>", htmltools::htmlEscape(it$text), "</div>"),
      list = paste0("<ul>", paste0("<li>", htmltools::htmlEscape(it$items), "</li>", collapse = ""), "</ul>"),
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
  html <- c(html, "<p class='note'>Generated by QuantCompass (open source, GPL-3.0). Results are computed in R; check your design with your supervisor.</p></body></html>")
  writeLines(paste(html, collapse = ""), paste0(out_base, ".html"), useBytes = TRUE)
  # Word
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  tn <- 0; fn <- 0
  for (s in sections) {
    doc <- officer::body_add_par(doc, s$heading, style = "heading 2")
    for (it in s$items) {
      if (it$type %in% c("p", "paste")) doc <- officer::body_add_par(doc, it$text, style = "Normal")
      else if (it$type == "list") for (x in it$items) doc <- officer::body_add_par(doc, paste("•", x), style = "Normal")
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
