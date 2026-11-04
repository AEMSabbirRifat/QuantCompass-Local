{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
var1 <- "{{VAR1}}"
var2 <- "{{VAR2}}"
cc <- qc_complete(data, c(var1, var2)); d <- cc$data
tab <- table(d[[var1]], d[[var2]], dnn = c(var1, var2)); N <- sum(tab)

# ---- 3. Assumption check: expected counts (Cochran, 1954: no more than 20% below 5) ----
chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
expected <- chi$expected
share_low <- mean(expected < 5)
use_fisher <- share_low > .20

# ---- 4. Test: Pearson chi-square, or Fisher's exact test when expected counts are small ----
set.seed(2026)  # only used if Fisher's test needs simulation for a large table
fisher <- if (use_fisher) tryCatch(stats::fisher.test(tab),
  error = function(e) stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000)) else NULL
p_value <- if (use_fisher) fisher$p.value else chi$p.value

# ---- 5. Effect size: Cramer's V ----
cramers_v <- sqrt(unname(chi$statistic) / (N * (min(dim(tab)) - 1)))

# ---- 6. Results ----
sig <- if (p_value < .05) "a statistically significant" else "no statistically significant"
apa <- if (use_fisher) sprintf(
  "Because %s%% of expected cell counts were below 5, Fisher's exact test was used. It showed %s association between %s and %s, p %s, Cramer's V = %s, N = %d.",
  round(100 * share_low), sig, var1, var2, qc_fmt_p(p_value), qc_fmt_r(cramers_v), N) else sprintf(
  "A chi-square test of independence showed %s association between %s and %s, chi-square(%d, N = %d) = %s, p %s, Cramer's V = %s.",
  sig, var1, var2, as.integer(chi$parameter), N, qc_fmt(chi$statistic), qc_fmt_p(p_value), qc_fmt_r(cramers_v))
result <- list(test = "chisq_independence", n_used = cc$n_used, n_excluded = cc$n_excluded,
  table = tab, row_percent = prop.table(tab, 1) * 100, expected = expected,
  chisq = unname(chi$statistic), df = unname(chi$parameter), p_chisq = chi$p.value,
  used_fisher = use_fisher, p = p_value, cramers_v = cramers_v,
  std_residuals = chi$stdres, apa = apa, session = qc_session())
cat(apa, "\n"); print(tab)
