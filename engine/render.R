# Fill a vetted template, save it, run that exact file, return results + the same script.

qc_root <- function() getOption("qc.root", ".")

qc_test_labels <- c(
  descriptives = "Descriptive statistics", ttest_independent = "Welch's independent-samples t-test",
  mann_whitney = "Mann-Whitney U test", ttest_paired = "Paired-samples t-test",
  wilcoxon_signed = "Wilcoxon signed-rank test", anova_oneway = "Welch's one-way ANOVA",
  kruskal_wallis = "Kruskal-Wallis test", correlation_pearson = "Pearson correlation",
  correlation_spearman = "Spearman rank correlation", chisq_independence = "Chi-square test of independence",
  regression_linear = "Multiple linear regression", regression_logistic = "Binary logistic regression",
  reliability_alpha = "Cronbach's alpha")

qc_esc <- function(x) gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", as.character(x)))
qc_r_vec <- function(x) if (length(x) == 0) "character(0)" else
  paste0("c(", paste0('"', qc_esc(x), '"', collapse = ", "), ")")

qc_render <- function(test_id, params, load_code, hypothesis = "") {
  tpl_file <- file.path(qc_root(), "templates", paste0(test_id, ".R"))
  if (!file.exists(tpl_file)) stop("No template for test '", test_id, "'.")
  tpl <- readLines(tpl_file, warn = FALSE)
  helpers <- readLines(file.path(qc_root(), "engine", "helpers.R"), warn = FALSE)
  header <- c(
    "# =====================================================================",
    sprintf("# QuantCompass analysis script - %s", qc_test_labels[[test_id]]),
    if (nzchar(hypothesis)) sprintf("# Hypothesis: %s", hypothesis),
    sprintf("# Generated: %s | QuantCompass engine 0.1.0 | %s", format(Sys.Date()), R.version.string),
    "# This is the exact script that produced your results. To reproduce them,",
    "# put it in the same folder as your data file and run it in R.",
    "# =====================================================================")
  out <- character()
  for (line in tpl) {
    if (trimws(line) == "{{HEADER}}") out <- c(out, header)
    else if (trimws(line) == "{{HELPERS}}") out <- c(out, helpers)
    else if (trimws(line) == "{{DATA_LOAD}}") out <- c(out, load_code)
    else out <- c(out, line)
  }
  for (key in names(params)) {
    val <- if (grepl("_VEC$", key)) qc_r_vec(params[[key]]) else qc_esc(params[[key]])
    out <- gsub(paste0("{{", key, "}}"), val, out, fixed = TRUE)
  }
  left <- regmatches(out, regexpr("\\{\\{[A-Z_]+\\}\\}", out))
  if (length(left)) stop("Missing parameter(s): ", paste(unique(left), collapse = ", "))
  out
}

# Runs the script file itself (not a copy), from the data folder, in a clean environment.
qc_run <- function(script, data_dir, script_path = file.path(tempdir(), "quantcompass_script.R")) {
  writeLines(script, script_path)
  env <- new.env(parent = globalenv())
  old <- setwd(data_dir); on.exit(setwd(old))
  console <- utils::capture.output(source(script_path, local = env, echo = FALSE))
  list(result = env$result, console = console, script = readLines(script_path), script_path = script_path)
}

# One call from file to result: used by the apps and the examples.
qc_analyse <- function(path, sheet = NULL, option, hypothesis = "") {
  load_code <- qc_load_code(path, sheet)
  script <- qc_render(option$test_id, option$params, load_code, hypothesis)
  qc_run(script, dirname(normalizePath(path)))
}
