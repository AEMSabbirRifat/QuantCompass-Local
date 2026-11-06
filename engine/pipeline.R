# The full pipeline: screen assumptions -> choose test -> run -> charts -> report.
# ctx = list(path, sheet, fixes, labels, level_labels, out_dir)
# h   = list(id = "H1", text = "...", spec = list(goal = ..., ...), choice = "auto" | "parametric" | "non-parametric")

qc_set_labels <- function(ctx) {
  assign("qc_labels", if (is.null(ctx$labels)) list() else ctx$labels, envir = globalenv())
  assign("qc_level_labels", if (is.null(ctx$level_labels)) list() else ctx$level_labels, envir = globalenv())
}

qc_alternatives <- c(ttest_independent = "mann_whitney", mann_whitney = "ttest_independent",
  anova_oneway = "kruskal_wallis", kruskal_wallis = "anova_oneway", ttest_paired = "wilcoxon_signed",
  wilcoxon_signed = "ttest_paired", correlation_pearson = "correlation_spearman", correlation_spearman = "correlation_pearson")
qc_parametric_ids <- c("ttest_independent", "anova_oneway", "ttest_paired", "correlation_pearson")

# Data lines for one hypothesis: analyse a subgroup only, or compare only some groups (in a set order)
qc_hyp_data_lines <- function(spec) {
  out <- character(); desc <- character()
  if (!is.null(spec$subset)) {
    cond <- if (is.character(spec$subset$cond)) spec$subset$cond else qc_cond_code(spec$subset$cond)
    lab <- if (is.null(spec$subset$label)) cond else spec$subset$label
    out <- c(out, sprintf("data <- data[(%s) %%in%% TRUE, , drop = FALSE]   # this hypothesis: %s", cond, lab)); desc <- c(desc, lab)
  }
  if (!is.null(spec$compare) && !is.null(spec$group)) {
    g <- spec$group; lv <- spec$compare; num <- suppressWarnings(as.numeric(lv))
    vec <- if (!anyNA(num)) qc_vec(num) else qc_vec(lv)
    out <- c(out, sprintf('data <- data[data[["%s"]] %%in%% %s, , drop = FALSE]   # this hypothesis compares only these groups', g, vec),
             sprintf('data[["%s"]] <- factor(data[["%s"]], levels = %s)   # group order: the first group is group 1', g, g, vec))
    desc <- c(desc, sprintf("only %s compared", qc_and(vapply(lv, function(l) qc_level_lab(g, l), ""))))
  }
  structure(out, desc = desc)
}

qc_run_hypothesis <- function(h, ctx) {
  qc_set_labels(ctx)
  hl <- qc_hyp_data_lines(h$spec); if (length(hl)) ctx$fixes <- c(ctx$fixes, hl)
  data <- qc_read(ctx$path, ctx$sheet, ctx$fixes)
  spec <- h$spec; h$sentence <- qc_sentence(spec, data)
  screen <- qc_screen(data, spec)
  eff <- if (!is.null(screen$rerouted)) screen$rerouted else spec
  if (!is.null(screen$robust)) eff$robust <- screen$robust
  if (!is.null(screen$positive)) eff$positive <- screen$positive
  auto_id <- screen$decision$test_id; id <- auto_id
  choice <- if (is.null(h$choice)) "auto" else h$choice
  if (choice != "auto" && auto_id %in% names(qc_alternatives)) {
    want_param <- choice == "parametric"
    if (want_param != (auto_id %in% qc_parametric_ids)) {
      id <- qc_alternatives[[auto_id]]
      screen$decision$reason <- paste0(screen$decision$reason, " However, the user chose the ", tolower(qc_test_labels[[id]]),
        " instead of the automatically selected ", tolower(qc_test_labels[[auto_id]]), ".")
      screen$decision$test_id <- id; screen$decision$family <- if (want_param) "parametric" else "non-parametric"
    }
  }
  opts <- qc_suggest(data, eff)
  if (opts[[1]]$status == "blocked") return(list(id = h$id, sentence = h$sentence, blocked = TRUE,
    message = paste(opts[[1]]$why, paste(opts[[1]]$messages, collapse = " "))))
  opt <- Filter(function(o) identical(o$test_id, id), opts)
  if (!length(opt)) opt <- list(list(test_id = id, params = opts[[1]]$params))
  opt <- opt[[1]]
  warnings <- unique(unlist(lapply(opts, `[[`, "messages")))
  notes <- c("ASSUMPTION SCREENING (run before the test was chosen):",
    sprintf("  - %s | %s | %s | met: %s", screen$checks$check, screen$checks$method, screen$checks$value,
            ifelse(is.na(screen$checks$met), "n/a", ifelse(screen$checks$met, "yes", "no"))),
    paste("DECISION:", qc_test_labels[[id]]), strwrap(paste("REASON:", screen$decision$reason), 85))
  out <- qc_analyse(ctx$path, ctx$sheet, opt, paste(h$id, "-", h$text), ctx$fixes, ctx$labels, ctx$level_labels, notes)
  r <- out$result; r$.group <- eff$group
  pair <- qc_hypothesis_pair(eff, data, h$id); dec <- qc_decision_line(id, r, pair)
  hdir <- file.path(ctx$out_dir, h$id); figdir <- file.path(hdir, "figures"); dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
  afigs <- tryCatch(qc_save_figures(qc_assumption_figures(data, eff, screen), figdir, h$id, 1), error = function(e) list())
  rfigs <- tryCatch(qc_save_figures(qc_result_figures(data, eff, screen, r), figdir, h$id, length(afigs) + 1), error = function(e) list())
  script_file <- file.path(hdir, paste0(h$id, "_script.R")); writeLines(out$script, script_file)
  tables <- qc_result_tables(id, r); paragraph <- paste(qc_paste_paragraph(h, screen, r), sub("^Decision: ", "", dec$text))
  explanation <- qc_explain(id, r, eff)
  vars <- unique(c(eff$var1, eff$mediator, eff$outcome, eff$group, eff$var2, eff$predictors, eff$items))
  sections <- list(
    list(heading = "Hypotheses", items = list(list(type = "list", items = c(
      sprintf("Your hypothesis (as entered): %s", sub("\\.?$", ".", h$text)),
      sprintf("Research hypothesis (%s): %s", pair$lab1, pair$h1),
      sprintf("Null hypothesis (%s): %s", pair$lab0, pair$h0), pair$note)))),
    list(heading = "1. Data used", items = list(
      list(type = "p", text = h$sentence),
      list(type = "list", items = c(sprintf("File: %s%s", basename(ctx$path), if (!is.null(ctx$sheet)) paste0(" (sheet: ", ctx$sheet, ")") else ""),
        sprintf("Variables: %s", qc_and(sprintf("%s (%s)", sapply(vars, qc_lab), vars))),
        sprintf("Cases analysed: %d; excluded because of missing values: %d", r$n_used, r$n_excluded),
        if (length(attr(hl, "desc"))) sprintf("Cases for this hypothesis: %s", paste(attr(hl, "desc"), collapse = "; ")),
        if (length(ctx$fixes)) sprintf("Data preparation steps applied: %d (listed in the script)", length(ctx$fixes)))))),
    list(heading = "2. Assumption screening", items = c(list(list(type = "checks", checks = screen$checks)),
      if (length(warnings)) list(list(type = "list", items = warnings)))),
    list(heading = "3. Assumption visualisations", items = lapply(afigs, function(f) list(type = "figure", fig = f))),
    list(heading = "4. Decision", items = list(
      list(type = "p", text = sprintf("Selected test: %s (%s).", qc_test_labels[[id]], screen$decision$family)),
      list(type = "p", text = screen$decision$reason),
      list(type = "list", items = c("Rules are fixed in advance and based on:", screen$decision$references)))),
    list(heading = "5. Result (ready to paste)", items = c(list(list(type = "paste", text = paragraph), list(type = "p", text = dec$text)),
      lapply(tables, function(t) list(type = "table", table = t)))),
    list(heading = "6. Result visualisations", items = lapply(rfigs, function(f) list(type = "figure", fig = f))),
    list(heading = "7. What this means (plain language)", items = list(list(type = "p", text = explanation))),
    list(heading = "8. Appendix: exact R script", items = list(
      list(type = "p", text = sprintf("Saved as %s. Running it next to %s reproduces every number above.", basename(script_file), basename(ctx$path))),
      list(type = "code", lines = out$script))))
  files <- qc_write_report(paste0(h$id, ": ", sub("\\.$", "", h$text)), sections, file.path(hdir, paste0(h$id, "_report")))
  list(id = h$id, sentence = h$sentence, blocked = FALSE, test = qc_test_labels[[id]], family = screen$decision$family,
       h1 = pair$h1, h0 = pair$h0, lab1 = pair$lab1, lab0 = pair$lab0, decision = dec, user_text = h$text,
       reason = screen$decision$reason, paragraph = paragraph, explanation = explanation, tables = tables,
       figures = c(afigs, rfigs), result_figures = rfigs, checks = screen$checks, files = c(files, script = script_file),
       references = screen$decision$references, warnings = warnings, result = r)
}

# ---- Describe the whole dataset ----
qc_describe_dataset <- function(ctx, by = NULL) {
  qc_set_labels(ctx)
  data <- qc_read(ctx$path, ctx$sheet, ctx$fixes); prof <- qc_profile(data)
  types <- stats::setNames(prof$columns$type, prof$columns$column)
  vars <- names(data)[!types[names(data)] %in% c("id", "empty", "text") & !qc_is_system_var(names(data)) & !grepl("_r$", names(data))]
  num <- vars[sapply(data[vars], is.numeric) & !types[vars] %in% c("binary")]
  cat_vars <- setdiff(vars, num); cat_vars <- cat_vars[sapply(cat_vars, function(v) length(unique(stats::na.omit(data[[v]])))) <= 20]
  opt <- list(test_id = "descriptives", params = list(VARIABLES_VEC = vars))
  out <- qc_analyse(ctx$path, ctx$sheet, opt, "Descriptive statistics of the whole dataset", ctx$fixes, ctx$labels, ctx$level_labels)
  r <- out$result; ddir <- file.path(ctx$out_dir, "Descriptives"); figdir <- file.path(ddir, "figures")
  f <- qc_fmt; nt <- r$numeric; nt <- nt[nt$variable %in% num, ]
  tables <- list()
  if (!is.null(nt) && nrow(nt)) tables[[length(tables) + 1]] <- qc_tab("Descriptive statistics of numeric variables", data.frame(Variable = sapply(nt$variable, qc_lab),
    n = nt$n, Missing = nt$missing, M = f(nt$mean), SD = f(nt$sd), Mdn = f(nt$median), IQR = f(nt$iqr), Min = f(nt$min), Max = f(nt$max),
    Skewness = f(nt$skewness), Kurtosis = f(nt$kurtosis)))
  ct <- r$categorical; if (!is.null(ct)) ct <- ct[ct$variable %in% cat_vars, ]
  if (!is.null(ct) && nrow(ct)) tables[[length(tables) + 1]] <- qc_tab("Frequencies of categorical variables", data.frame(Variable = sapply(ct$variable, qc_lab),
    Category = mapply(function(v, l) qc_level_lab(v, l), ct$variable, ct$category), n = ct$n, Percent = sprintf("%.1f%%", ct$percent)))
  if (!is.null(by) && by %in% names(data) && length(num)) {
    g <- data[[by]]; lv <- sort(unique(stats::na.omit(g)))
    bt <- do.call(rbind, lapply(setdiff(num, by), function(v) data.frame(Variable = qc_lab(v), t(sapply(lv, function(l) { y <- data[[v]][!is.na(g) & g == l]
      sprintf("%s (%s)", f(mean(y, na.rm = TRUE)), f(stats::sd(y, na.rm = TRUE))) })), check.names = FALSE)))
    names(bt)[-1] <- sapply(lv, function(l) paste0(qc_level_lab(by, l), " (n = ", sum(g == l, na.rm = TRUE), ")"))
    tables[[length(tables) + 1]] <- qc_tab(paste("Means (SD) by", qc_lab(by)), bt)
  }
  chunks <- function(x, k) split(x, ceiling(seq_along(x) / k))
  figs <- list(qc_plot_missing(data[vars]))
  for (ch in chunks(num, 12)) figs[[length(figs) + 1]] <- qc_plot_hist_grid(data, ch)
  for (ch in chunks(cat_vars, 8)) figs[[length(figs) + 1]] <- qc_plot_bar_grid(data, ch)
  if (length(num) >= 2 && length(num) <= 25) figs[[length(figs) + 1]] <- qc_plot_cor_heatmap(data, num)
  figs <- qc_save_figures(figs, figdir, "Descriptives")
  skewed <- if (!is.null(nt) && nrow(nt)) nt[abs(nt$skewness) > 2, ] else NULL
  flags <- c(if (nrow(prof$issues)) paste0(sapply(prof$issues$column, qc_lab), ": ", prof$issues$message),
    if (!is.null(skewed) && nrow(skewed)) sprintf("%s is strongly skewed (skewness = %s); rank-based tests may suit it better.", sapply(skewed$variable, qc_lab), f(skewed$skewness)))
  miss_total <- sum(is.na(data[vars]))
  para <- sprintf("The dataset consisted of %d cases and %d variables (%d numeric, %d categorical). %s%s", nrow(data), length(vars), length(num), length(cat_vars),
    if (miss_total == 0) "No values were missing. " else sprintf("In total, %d values (%.1f%%) were missing. ", miss_total, 100 * miss_total / (nrow(data) * length(vars))),
    if (!is.null(nt) && nrow(nt)) paste0(paste(sprintf("%s ranged from %s to %s (M = %s, SD = %s).", sapply(utils::head(nt$variable, 3), function(v) { l <- qc_lab(v); paste0(toupper(substr(l, 1, 1)), substring(l, 2)) }),
      f(utils::head(nt$min, 3)), f(utils::head(nt$max, 3)), f(utils::head(nt$mean, 3)), f(utils::head(nt$sd, 3))), collapse = " "),
      if (nrow(nt) > 3) " Full statistics for all variables are given in the table." else "") else "")
  script_file <- file.path(ddir, "Descriptives_script.R"); writeLines(out$script, script_file)
  sections <- list(
    list(heading = "1. Overview", items = list(list(type = "list", items = c(sprintf("File: %s%s", basename(ctx$path), if (!is.null(ctx$sheet)) paste0(" (sheet: ", ctx$sheet, ")") else ""),
      sprintf("Cases: %d; variables described: %d", nrow(data), length(vars)),
      if (length(setdiff(names(data), vars))) sprintf("Not described (ID or free-text columns): %s", qc_and(setdiff(names(data), vars))))))),
    list(heading = "2. Ready-to-paste summary", items = list(list(type = "paste", text = para))),
    list(heading = "3. Tables", items = lapply(tables, function(t) list(type = "table", table = t))),
    list(heading = "4. Visualisations", items = lapply(figs, function(f) list(type = "figure", fig = f))),
    list(heading = "5. Points to check", items = list(list(type = "list", items = if (length(flags)) flags else "No problems were detected."))),
    list(heading = "6. Appendix: exact R script", items = list(list(type = "code", lines = out$script))))
  files <- qc_write_report("Descriptive statistics of the dataset", sections, file.path(ddir, "Descriptives_report"))
  list(paragraph = para, tables = tables, figures = figs, flags = flags, files = c(files, script = script_file), sections = sections)
}

# ---- Summary report across all hypotheses ----
qc_summary_report <- function(results, ctx, descriptives = NULL, methods = NULL, table1 = NULL, breakdowns = NULL) {
  qc_set_labels(ctx)
  secs <- list(list(heading = "About this report", items = list(list(type = "p", text = sprintf(
    "This report summarises %d hypothesis test(s) on %s. For each hypothesis, the assumptions were screened first, and the test was chosen from pre-set rules before results were seen. Full details, all charts and the exact R scripts are in the individual hypothesis reports.",
    length(results), basename(ctx$path))))))
  if (!is.null(methods)) secs[[length(secs) + 1]] <- list(heading = "Methods (ready to paste and adapt)", items = list(
    list(type = "p", text = "Sample"), list(type = "paste", text = methods$sample),
    if (nzchar(methods$measures)) list(type = "p", text = "Measures"), if (nzchar(methods$measures)) list(type = "paste", text = methods$measures),
    list(type = "p", text = "Analysis"), list(type = "paste", text = methods$analysis)))
  if (!is.null(methods)) secs[[length(secs)]]$items <- Filter(Negate(is.null), secs[[length(secs)]]$items)
  if (!is.null(table1)) secs[[length(secs) + 1]] <- list(heading = "Table 1: Sample characteristics", items = list(list(type = "table", table = table1)))
  if (length(breakdowns)) secs[[length(secs) + 1]] <- list(heading = "How the hypotheses were broken down", items = list(list(type = "list", items =
    unlist(lapply(breakdowns, function(b) vapply(b$parts, function(pp) sprintf("%s - %s %s", pp$id, b$text, pp$expected), ""))))))
  if (!is.null(descriptives)) secs[[length(secs) + 1]] <- list(heading = "Descriptive statistics", items = c(list(list(type = "paste", text = descriptives$paragraph)),
    lapply(descriptives$tables[1], function(t) list(type = "table", table = t))))
  ok <- Filter(function(x) !isTRUE(x$blocked), results)
  if (length(ok)) secs[[length(secs) + 1]] <- list(heading = "Overview of decisions", items = list(list(type = "table", table = qc_tab("Hypothesis decisions (alpha = .05)",
    data.frame(ID = sapply(ok, `[[`, "id"), `Research hypothesis` = sapply(ok, `[[`, "h1"), Test = sapply(ok, `[[`, "test"),
      Decision = sapply(ok, function(x) x$decision$short), check.names = FALSE)))))
  refs <- character()
  for (res in results) {
    if (isTRUE(res$blocked)) { secs[[length(secs) + 1]] <- list(heading = res$id, items = list(list(type = "p", text = res$sentence),
      list(type = "p", text = paste("Not possible with this data:", res$message)))); next }
    refs <- c(refs, res$references)
    secs[[length(secs) + 1]] <- list(heading = paste0(res$id, ": ", res$test), items = c(
      list(list(type = "list", items = c(paste0("Research hypothesis (", res$lab1, "): ", res$h1), paste0("Null hypothesis (", res$lab0, "): ", res$h0))), list(type = "paste", text = res$paragraph),
           list(type = "p", text = res$decision$text)),
      lapply(res$tables[length(res$tables)], function(t) list(type = "table", table = t)),
      lapply(res$result_figures, function(f) list(type = "figure", fig = f)),
      list(list(type = "p", text = res$explanation))))
  }
  secs[[length(secs) + 1]] <- list(heading = "References for the decision rules", items = list(list(type = "list", items = sort(unique(refs)))))
  secs[[length(secs) + 1]] <- list(heading = "AI and software disclosure (adapt for your declaration)", items = list(list(type = "p", text =
    "Statistical analyses were conducted in R (R Core Team) using the open-source QuantCompass tool, which screened assumptions, selected tests by pre-defined rules and generated the reported statistics, tables and figures. All analysis scripts are provided in the appendix. No generative AI was used to compute any result.")))
  qc_write_report("QuantCompass summary report", secs, file.path(ctx$out_dir, "Summary_report"))
}

# ---- Option B: hypothesis sheet ----
qc_question_types <- c("Compare groups" = "difference", "Before vs after (same cases)" = "paired_difference",
  "Relationship between two variables" = "relationship", "Association between two categories" = "association",
  "Predict an outcome" = "prediction", "Scale reliability" = "reliability")

qc_split_list <- function(x) { if (is.null(x) || is.na(x) || !nzchar(trimws(x))) character() else trimws(strsplit(as.character(x), "[,;]")[[1]]) }

qc_read_hypothesis_sheet <- function(path, data, sheet = "Hypotheses") {
  ext <- tolower(tools::file_ext(path))
  hs <- if (ext %in% c("xlsx", "xls")) as.data.frame(readxl::read_excel(path, sheet = sheet)) else utils::read.csv(path, check.names = FALSE)
  names(hs) <- tolower(gsub("[^a-z]", "", tolower(names(hs))))
  col <- function(pat) { i <- grep(pat, names(hs)); if (length(i)) hs[[i[1]]] else rep(NA, nrow(hs)) }
  ids <- col("^id"); txt <- col("^hypothesis"); typ <- col("^question|^type"); v1 <- col("^outcome|^first"); v2 <- col("^group|^second")
  multi <- col("^predictor|^items"); rev <- col("^reverse")
  out <- list(); problems <- character()
  for (i in seq_len(nrow(hs))) {
    if (all(is.na(c(txt[i], typ[i], v1[i])))) next
    id <- if (!is.na(ids[i])) as.character(ids[i]) else paste0("H", i)
    goal <- qc_question_types[as.character(typ[i])]
    if (is.na(goal)) { problems <- c(problems, sprintf("%s: unknown question type '%s'.", id, typ[i])); next }
    spec <- switch(goal,
      difference = list(goal = goal, outcome = v1[i], group = v2[i]),
      paired_difference = list(goal = goal, var1 = v1[i], var2 = v2[i]),
      relationship = list(goal = goal, var1 = v1[i], var2 = v2[i]),
      association = list(goal = goal, var1 = v1[i], var2 = v2[i]),
      prediction = list(goal = goal, outcome = v1[i], predictors = qc_split_list(multi[i])),
      reliability = list(goal = goal, items = qc_split_list(multi[i]), reverse = qc_split_list(rev[i])))
    used <- unique(unlist(spec[setdiff(names(spec), "goal")]))
    miss <- setdiff(used, names(data))
    if (length(miss)) {
      sugg <- sapply(miss, function(m) { a <- agrep(m, names(data), value = TRUE, max.distance = .3); if (length(a)) paste0(" (did you mean '", a[1], "'?)") else "" })
      problems <- c(problems, sprintf("%s: column '%s' not found in the data%s.", id, miss, sugg)); next
    }
    out[[length(out) + 1]] <- list(id = id, text = if (!is.na(txt[i])) as.character(txt[i]) else qc_sentence(spec, data), spec = spec, choice = "auto")
  }
  list(hypotheses = out, problems = problems)
}

# ---- Table 1 (sample characteristics) and the methods section ----
qc_table1 <- function(data, vars) {
  rows <- list()
  for (v in vars) {
    x <- data[[v]]; if (is.null(x)) next
    lv <- if (exists("qc_level_labels", inherits = TRUE)) get("qc_level_labels", inherits = TRUE)[[v]] else NULL
    if (is.factor(x) || is.character(x) || !is.null(lv) || length(unique(stats::na.omit(x))) <= 2) {
      t <- table(x, useNA = "no"); n <- sum(t)
      rows[[length(rows) + 1]] <- data.frame(Characteristic = qc_cap_first(qc_lab(v)), Value = "", check.names = FALSE)
      for (k in names(t)) rows[[length(rows) + 1]] <- data.frame(Characteristic = paste0("   ", qc_level_lab(v, k)), Value = sprintf("%d (%.1f%%)", t[[k]], 100 * t[[k]] / n), check.names = FALSE)
      if (anyNA(x)) rows[[length(rows) + 1]] <- data.frame(Characteristic = "   Missing", Value = sprintf("%d", sum(is.na(x))), check.names = FALSE)
    } else rows[[length(rows) + 1]] <- data.frame(Characteristic = qc_cap_first(qc_lab(v)),
      Value = sprintf("M = %s (SD = %s), n = %d", qc_fmt(mean(x, na.rm = TRUE)), qc_fmt(stats::sd(x, na.rm = TRUE)), sum(!is.na(x))), check.names = FALSE)
  }
  qc_tab(sprintf("Sample characteristics (N = %d)", nrow(data)), do.call(rbind, rows), note = "Values are n (%) for categories and M (SD) for numeric measures.")
}

qc_methods_text <- function(prep, flow_counts, scales_summary, results, codebook_source = NULL, n_final = NULL) {
  fc <- flow_counts
  steps <- if (nrow(fc) > 1) fc[-1, ] else fc[0, ]
  excl <- if (nrow(steps)) qc_and(sprintf("%d %s", steps$removed, steps$step)) else "no cases"
  p1 <- sprintf("The data file contained %d cases. In this order, we removed %s, leaving a final sample of N = %d.",
                fc$n[1], excl, if (is.null(n_final)) fc$n[nrow(fc)] else n_final)
  p2 <- if (!is.null(codebook_source)) sprintf("Variables, answer codes and survey filter rules were taken from the codebook (%s); codes marking unanswered questions were treated as missing values.", codebook_source) else ""
  sc <- scales_summary
  p3 <- if (!is.null(sc) && nrow(sc)) paste(vapply(seq_len(nrow(sc)), function(i) {
      s <- prep$scales[[i]]; how <- switch(s$method, who5 = "scored as the WHO-5 index (0-100; Topp et al., 2015)", sum = "summed", "averaged")
      nm <- qc_cap_first(if (nzchar(sc$label[i])) sc$label[i] else sc$scale[i])
      if (sc$items[i] == 1) return(sprintf("%s was measured with a single item (%s)%s.", nm, s$items[1],
        if (length(s$reverse)) sprintf(", reverse-coded (%s + %s - answer) so that higher values mean more of %s", s$min, s$max, tolower(nm)) else ""))
      sprintf("%s was measured with %d item%s (%s)%s%s.", nm, sc$items[i], if (sc$items[i] > 1) "s" else "",
              how, if (length(s$reverse)) sprintf(", with %s reverse-coded", qc_and(s$reverse)) else "",
              if (!is.na(sc$alpha[i])) sprintf("; Cronbach's alpha = %s", qc_fmt_r(sc$alpha[i])) else "") }, ""), collapse = " ") else ""
  ids <- unique(unlist(lapply(Filter(function(x) !isTRUE(x$blocked), results), `[[`, "test")))
  p4 <- paste("Before each test, its assumptions were screened with fixed rules: normality by Shapiro-Wilk below n = 50, skewness and kurtosis z-scores (|z| <= 3.29) for n = 50-300 and absolute skewness (<= 2) and kurtosis (<= 7) above 300 (Kim, 2013); extreme outliers beyond 3 x IQR (Tukey, 1977); equality of variances with the Brown-Forsythe test.",
    "Group means were compared with Welch's tests, which do not assume equal variances (Delacre et al., 2017); rank-based tests were used for ordinal outcomes or when normality failed in small groups. Correlations used Pearson's r for linear, normally distributed variables and Spearman's rho otherwise.",
    if (any(grepl("Mediation", ids))) "The mediation hypothesis was tested with a regression-based simple mediation model and a percentile bootstrap confidence interval (5,000 resamples) for the indirect effect (Hayes, 2022)." else "",
    sprintf("The tests used were: %s. The significance level was alpha = .05; tests were two-sided unless a direction was stated in advance.", qc_and(ids)),
    sprintf("All analyses were run in %s with QuantCompass; the full R scripts are provided.", R.version.string))
  list(sample = trimws(paste(p1, p2)), measures = p3, analysis = gsub("\\s+", " ", p4))
}
