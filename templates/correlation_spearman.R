{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
var1 <- "{{VAR1}}"
var2 <- "{{VAR2}}"
cc <- qc_complete(data, c(var1, var2)); d <- cc$data
x <- as.numeric(d[[var1]]); y <- as.numeric(d[[var2]]); N <- length(x)

# ---- 3. Spearman rank correlation (asymptotic p-value, valid with ties) ----
ct <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
rho <- unname(ct$estimate)
# Approximate 95% CI via Fisher z with the Bonett & Wright (2000) standard error
se <- sqrt((1 + rho^2 / 2) / (N - 3))
rho_ci <- tanh(atanh(rho) + c(-1, 1) * stats::qnorm(.975) * se)

# ---- 4. Results ----
sig <- if (ct$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf("A Spearman rank correlation showed %s association between %s and %s, rs(%d) = %s, 95%% CI [%s, %s], p %s.",
  sig, var1, var2, N - 2L, qc_fmt_r(rho), qc_fmt_r(rho_ci[1]), qc_fmt_r(rho_ci[2]), qc_fmt_p(ct$p.value))
result <- list(test = "correlation_spearman", n_used = cc$n_used, n_excluded = cc$n_excluded,
  rho = rho, rho_ci = rho_ci, p = ct$p.value, apa = apa, session = qc_session())
cat(apa, "\n")
