{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
var1 <- "{{VAR1}}"
var2 <- "{{VAR2}}"
cc <- qc_complete(data, c(var1, var2)); d <- cc$data
x <- as.numeric(d[[var1]]); y <- as.numeric(d[[var2]])

# ---- 3. Assumption checks: normality of each variable; inspect the scatterplot for linearity ----
normality <- list(qc_shapiro(x), qc_shapiro(y)); names(normality) <- c(var1, var2)

# ---- 4. Pearson correlation with 95% CI (Fisher z) ----
ct <- stats::cor.test(x, y, method = "pearson")

# ---- 5. Results ----
sig <- if (ct$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf("There was %s correlation between %s and %s, r(%d) = %s, 95%% CI [%s, %s], p %s, N = %d.",
  sig, var1, var2, as.integer(ct$parameter), qc_fmt_r(ct$estimate),
  qc_fmt_r(ct$conf.int[1]), qc_fmt_r(ct$conf.int[2]), qc_fmt_p(ct$p.value), length(x))
result <- list(test = "correlation_pearson", n_used = cc$n_used, n_excluded = cc$n_excluded,
  r = unname(ct$estimate), r_ci = as.numeric(ct$conf.int), t = unname(ct$statistic),
  df = unname(ct$parameter), p = ct$p.value, assumptions = list(normality = normality),
  apa = apa, session = qc_session())
cat(apa, "\n")
