{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select the two measurements (one row per person) ----
var1 <- "{{VAR1}}"
var2 <- "{{VAR2}}"
cc <- qc_complete(data, c(var1, var2)); d <- cc$data
x1 <- as.numeric(d[[var1]]); x2 <- as.numeric(d[[var2]]); diffs <- x1 - x2

# ---- 3. Wilcoxon signed-rank test (zero differences are dropped, as is standard) ----
ws <- stats::wilcox.test(x1, x2, paired = TRUE, alternative = "{{ALTERNATIVE}}", exact = FALSE, correct = TRUE)
V <- unname(ws$statistic)

# ---- 4. Effect size: matched-pairs rank-biserial correlation (Kerby, 2014) ----
nz <- diffs[diffs != 0]; rk <- rank(abs(nz))
r_rb <- (sum(rk[nz > 0]) - sum(rk[nz < 0])) / sum(rk)

# ---- 5. Results ----
sig <- if (ws$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf(paste0(
  "A Wilcoxon signed-rank test showed %s difference between %s (Mdn = %s) and %s (Mdn = %s), ",
  "V = %s, p %s, matched-pairs rank-biserial r = %s, n = %d pairs (%d with non-zero differences)."),
  sig, var1, qc_fmt(stats::median(x1)), var2, qc_fmt(stats::median(x2)),
  qc_fmt(V, 1), qc_fmt_p(ws$p.value), qc_fmt_r(r_rb), length(diffs), length(nz))
result <- list(test = "wilcoxon_signed", n_used = cc$n_used, n_excluded = cc$n_excluded,
  V = V, p = ws$p.value, rank_biserial = r_rb, n_nonzero = length(nz), apa = apa, session = qc_session())
cat(apa, "\n")
