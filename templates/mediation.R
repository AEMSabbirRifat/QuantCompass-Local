{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
x_var  <- "{{VAR1}}"       # predictor (X)
m_var  <- "{{MEDIATOR}}"   # mediator (M)
y_var  <- "{{OUTCOME}}"    # outcome (Y)
robust <- {{ROBUST}}       # TRUE = heteroscedasticity-robust (HC3) standard errors for the path tests
cc <- qc_complete(data, c(x_var, m_var, y_var))
d <- data.frame(x = as.numeric(cc$data[[x_var]]), m = as.numeric(cc$data[[m_var]]), y = as.numeric(cc$data[[y_var]]))
N <- nrow(d)

# ---- 3. The three regressions of a simple mediation model (Hayes, 2022, model 4) ----
fit_a <- stats::lm(m ~ x, data = d)       # path a:  X -> M
fit_b <- stats::lm(y ~ x + m, data = d)   # path b:  M -> Y (controlling for X), and direct effect c'
fit_c <- stats::lm(y ~ x, data = d)       # total effect c:  X -> Y
hc3 <- function(fit) { X <- stats::model.matrix(fit); e <- stats::residuals(fit); h <- stats::hatvalues(fit)
  XtXi <- solve(crossprod(X)); sqrt(diag(XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi)) }   # Long & Ervin (2000)
path <- function(fit, term) {
  est <- stats::coef(fit)[[term]]
  se <- if (robust) hc3(fit)[[term]] else summary(fit)$coefficients[term, "Std. Error"]
  t <- est / se; df <- fit$df.residual
  c(b = est, se = se, t = t, df = df, p = 2 * stats::pt(-abs(t), df))
}
a <- path(fit_a, "x"); b <- path(fit_b, "m"); c_direct <- path(fit_b, "x"); c_total <- path(fit_c, "x")

# ---- 4. Indirect effect a*b with a percentile bootstrap 95% CI (5,000 resamples; Preacher & Hayes, 2008) ----
set.seed(2026)                     # fixed seed: the same data always give the same interval
B <- 5000
X1 <- cbind(1, d$x); X2 <- cbind(1, d$x, d$m)
boot_ab <- replicate(B, {
  i <- sample.int(N, N, replace = TRUE)
  .lm.fit(X1[i, , drop = FALSE], d$m[i])$coefficients[2] * .lm.fit(X2[i, , drop = FALSE], d$y[i])$coefficients[3]
})
ab <- unname(a["b"] * b["b"])
ab_ci <- unname(stats::quantile(boot_ab, c(.025, .975)))
ab_cs <- ab * stats::sd(d$x) / stats::sd(d$y)        # completely standardised indirect effect
excludes_zero <- ab_ci[1] > 0 || ab_ci[2] < 0
prop_med <- if (sign(ab) == sign(c_total["b"]) && abs(c_total["b"]) > 1e-12) unname(ab / c_total["b"]) else NA

# ---- 5. Results ----
pv <- function(p) qc_fmt_p(p)
apa <- sprintf(paste0(
  "A simple mediation analysis (Hayes, 2022, model 4) with %s percentile bootstrap resamples tested whether %s mediated the relationship between %s and %s (N = %d). ",
  "%s was %s associated with %s (path a: b = %s, SE = %s, p %s), and %s was %s associated with %s when %s was controlled (path b: b = %s, SE = %s, p %s). ",
  "The indirect effect was ab = %s, 95%% bootstrap CI [%s, %s] (completely standardised ab = %s), so it was %s. ",
  "The total effect of %s on %s was c = %s, p %s, and the direct effect was c' = %s, p %s.%s"),
  format(B, big.mark = ","), qc_lab(m_var), qc_lab(x_var), qc_lab(y_var), N,
  qc_cap_first(qc_lab(x_var)), if (a["p"] < .05) "significantly" else "not significantly", qc_lab(m_var), qc_fmt(a["b"]), qc_fmt(a["se"]), pv(a["p"]),
  qc_lab(m_var), if (b["p"] < .05) "significantly" else "not significantly", qc_lab(y_var), qc_lab(x_var), qc_fmt(b["b"]), qc_fmt(b["se"]), pv(b["p"]),
  qc_fmt(ab, 3), qc_fmt(ab_ci[1], 3), qc_fmt(ab_ci[2], 3), qc_fmt(ab_cs, 3),
  if (excludes_zero) "statistically significant (the interval does not include zero)" else "not statistically significant (the interval includes zero)",
  qc_lab(x_var), qc_lab(y_var), qc_fmt(c_total["b"]), pv(c_total["p"]), qc_fmt(c_direct["b"]), pv(c_direct["p"]),
  if (robust) " Path tests use HC3 robust standard errors." else "")
paths <- data.frame(path = c("a (X -> M)", "b (M -> Y, controlling X)", "c' direct (X -> Y, controlling M)", "c total (X -> Y)"),
                    rbind(a, b, c_direct, c_total), row.names = NULL)
result <- list(test = "mediation", n_used = cc$n_used, n_excluded = cc$n_excluded, paths = paths,
  a = unname(a["b"]), b = unname(b["b"]), c = unname(c_total["b"]), c_prime = unname(c_direct["b"]),
  ab = ab, ab_ci = ab_ci, ab_cs = ab_cs, excludes_zero = excludes_zero, prop_mediated = prop_med, boot = B,
  p = NA, robust = robust, r2_m = summary(fit_a)$r.squared, r2_y = summary(fit_b)$r.squared,
  apa = apa, session = qc_session())
cat(apa, "\n")
