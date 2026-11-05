{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
outcome    <- "{{OUTCOME}}"
predictors <- {{PREDICTORS_VEC}}
robust     <- {{ROBUST}}         # TRUE = heteroscedasticity-robust (HC3) standard errors
cc <- qc_complete(data, c(outcome, predictors)); d <- cc$data
for (v in predictors) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])  # categories -> dummy variables
f <- stats::reformulate(sprintf("`%s`", predictors), response = sprintf("`%s`", outcome))

# ---- 3. Fit the model ----
model <- stats::lm(f, data = d)
s <- summary(model)
coefs <- as.data.frame(s$coefficients); names(coefs) <- c("b", "se", "t", "p")
X <- stats::model.matrix(model)
# HC3 robust standard errors (Long & Ervin, 2000)
e <- stats::residuals(model); h <- stats::hatvalues(model); XtXi <- solve(crossprod(X))
se_hc3 <- sqrt(diag(XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi))
if (robust) {
  coefs$se <- se_hc3; coefs$t <- coefs$b / se_hc3
  coefs$p <- 2 * stats::pt(abs(coefs$t), model$df.residual, lower.tail = FALSE)
}
crit <- stats::qt(.975, model$df.residual)
coefs$ci_low <- coefs$b - crit * coefs$se; coefs$ci_high <- coefs$b + crit * coefs$se
# Standardised betas: b * SD(x) / SD(y), with CIs scaled the same way
sx <- c(NA, apply(X[, -1, drop = FALSE], 2, stats::sd)); sy <- stats::sd(d[[outcome]])
coefs$beta <- coefs$b * sx / sy; coefs$beta_ci_low <- coefs$ci_low * sx / sy; coefs$beta_ci_high <- coefs$ci_high * sx / sy
# Readable names for model terms, e.g. "remote work (remote workers vs on-site workers)"
term_lab <- function(r) { r <- gsub("`", "", r); if (r == "(Intercept)") return(r)
  for (v in predictors) if (is.factor(d[[v]]) && startsWith(r, v) && r != v) {
    l <- substring(r, nchar(v) + 1); return(sprintf("%s (%s vs %s)", qc_lab(v), qc_level_lab(v, l), qc_level_lab(v, levels(d[[v]])[1]))) }
  qc_lab(r) }
coefs$label <- sapply(rownames(coefs), term_lab)
fs <- s$fstatistic; p_model <- stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

# ---- 4. Assumption checks ----
Xp <- X[, -1, drop = FALSE]
normality <- qc_shapiro(e)                         # normality of residuals
bp_fit <- summary(stats::lm(e^2 ~ Xp))             # Breusch-Pagan (Koenker) for constant variance
bp <- list(LM = nrow(Xp) * bp_fit$r.squared, df = ncol(Xp),
           p = stats::pchisq(nrow(Xp) * bp_fit$r.squared, ncol(Xp), lower.tail = FALSE))
vif <- if (ncol(Xp) > 1) sapply(seq_len(ncol(Xp)), function(j)
  1 / (1 - summary(stats::lm(Xp[, j] ~ Xp[, -j]))$r.squared)) else 1
names(vif) <- colnames(Xp)                         # multicollinearity: VIF > 5 is a warning sign

# ---- 5. Results ----
sig <- if (p_model < .05) "statistically significant" else "not statistically significant"
sig_preds <- rownames(coefs)[-1][coefs$p[-1] < .05]
apa <- sprintf("A multiple linear regression predicting %s from %s was %s, F(%d, %d) = %s, p %s, R-squared = %s, adjusted R-squared = %s, N = %d.%s%s",
  qc_lab(outcome), paste(sapply(predictors, qc_lab), collapse = ", "), sig, as.integer(fs[2]), as.integer(fs[3]), qc_fmt(fs[1]),
  qc_fmt_p(p_model), qc_fmt_r(s$r.squared), qc_fmt_r(s$adj.r.squared), nrow(d),
  paste0(sapply(rownames(coefs)[-1], function(r) {
    i <- which(rownames(coefs) == r)
    sprintf(" For %s, b = %s, beta = %s, t = %s, p %s.", coefs$label[i], qc_fmt(coefs$b[i]), qc_fmt(coefs$beta[i]),
            qc_fmt(coefs$t[i]), qc_fmt_p(coefs$p[i])) }), collapse = ""),
  if (robust) " Coefficient tests use HC3 robust standard errors." else "")
result <- list(test = "regression_linear", n_used = cc$n_used, n_excluded = cc$n_excluded,
  coefficients = coefs, r2 = s$r.squared, adj_r2 = s$adj.r.squared, robust = robust,
  F = unname(fs[1]), df1 = unname(fs[2]), df2 = unname(fs[3]), p = unname(p_model),
  assumptions = list(normality_of_residuals = normality, breusch_pagan = bp, vif = vif),
  apa = apa, session = qc_session())
cat(apa, "\n"); print(coefs, digits = 3)
