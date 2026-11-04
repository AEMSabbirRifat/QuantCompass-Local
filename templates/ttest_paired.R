{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select the two measurements (one row per person) ----
var1 <- "{{VAR1}}"
var2 <- "{{VAR2}}"
cc <- qc_complete(data, c(var1, var2)); d <- cc$data
x1 <- as.numeric(d[[var1]]); x2 <- as.numeric(d[[var2]]); diffs <- x1 - x2
n <- length(diffs)

# ---- 3. Assumption check: normality of the differences ----
normality <- qc_shapiro(diffs)

# ---- 4. Paired-samples t-test ----
tt <- stats::t.test(x1, x2, paired = TRUE, alternative = "{{ALTERNATIVE}}")

# ---- 5. Effect size: Cohen's d_z = mean difference / SD of differences ----
dz <- mean(diffs) / stats::sd(diffs)
se_dz <- sqrt(1 / n + dz^2 / (2 * n))
dz_ci <- dz + c(-1, 1) * stats::qnorm(.975) * se_dz

# ---- 6. Results ----
sig <- if (tt$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf(paste0(
  "A paired-samples t-test showed %s difference between %s (M = %s, SD = %s) and %s (M = %s, SD = %s), ",
  "t(%d) = %s, p %s, d_z = %s, 95%% CI [%s, %s], n = %d pairs."),
  sig, var1, qc_fmt(mean(x1)), qc_fmt(stats::sd(x1)), var2, qc_fmt(mean(x2)), qc_fmt(stats::sd(x2)),
  as.integer(tt$parameter), qc_fmt(tt$statistic), qc_fmt_p(tt$p.value),
  qc_fmt(dz), qc_fmt(dz_ci[1]), qc_fmt(dz_ci[2]), n)
result <- list(
  test = "ttest_paired", n_used = cc$n_used, n_excluded = cc$n_excluded,
  t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
  mean_diff = mean(diffs), mean_diff_ci = as.numeric(tt$conf.int), cohen_dz = dz, cohen_dz_ci = dz_ci,
  assumptions = list(normality_of_differences = normality), apa = apa, session = qc_session())
cat(apa, "\n")
