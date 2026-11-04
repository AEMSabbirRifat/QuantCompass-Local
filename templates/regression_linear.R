{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
outcome    <- "{{OUTCOME}}"
predictors <- {{PREDICTORS_VEC}}
cc <- qc_complete(data, c(outcome, predictors)); d <- cc$data
for (v in predictors) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])  # categories -> dummy variables
f <- stats::reformulate(sprintf("`%s`", predictors), response = sprintf("`%s`", outcome))

# ---- 3. Fit the model ----
model <- stats::lm(f, data = d)
s <- summary(model)
coefs <- as.data.frame(s$coefficients); names(coefs) <- c("b", "se", "t", "p")
ci <- stats::confint(model); coefs$ci_low <- ci[, 1]; coefs$ci_high <- ci[, 2]
# Standardised betas (numeric predictors only): b * SD(x) / SD(y)
X <- stats::model.matrix(model)[, -1, drop = FALSE]
coefs$beta <- c(NA, coefs$b[-1] * apply(X, 2, stats::sd) / stats::sd(d[[outcome]]))
fs <- s$fstatistic; p_model <- stats::pf(fs[1], fs[2], fs[3], lower.tail = FALSE)

# ---- 4. Assumption checks ----
res <- stats::residuals(model)
normality <- qc_shapiro(res)                       # normality of residuals
bp_fit <- summary(stats::lm(res^2 ~ X))            # Breusch-Pagan (Koenker) for constant variance
bp <- list(LM = nrow(X) * bp_fit$r.squared, df = ncol(X),
           p = stats::pchisq(nrow(X) * bp_fit$r.squared, ncol(X), lower.tail = FALSE))
vif <- if (ncol(X) > 1) sapply(seq_len(ncol(X)), function(j)
  1 / (1 - summary(stats::lm(X[, j] ~ X[, -j]))$r.squared)) else 1
names(vif) <- colnames(X)                          # multicollinearity: VIF > 5 is a warning sign

# ---- 5. Results ----
sig <- if (p_model < .05) "statistically significant" else "not statistically significant"
apa <- sprintf("A multiple linear regression predicting %s from %s was %s, F(%d, %d) = %s, p %s, R-squared = %s, adjusted R-squared = %s, N = %d.",
  outcome, paste(predictors, collapse = ", "), sig, as.integer(fs[2]), as.integer(fs[3]), qc_fmt(fs[1]),
  qc_fmt_p(p_model), qc_fmt_r(s$r.squared), qc_fmt_r(s$adj.r.squared), nrow(d))
result <- list(test = "regression_linear", n_used = cc$n_used, n_excluded = cc$n_excluded,
  coefficients = coefs, r2 = s$r.squared, adj_r2 = s$adj.r.squared,
  F = unname(fs[1]), df1 = unname(fs[2]), df2 = unname(fs[3]), p = unname(p_model),
  assumptions = list(normality_of_residuals = normality, breusch_pagan = bp, vif = vif),
  apa = apa, session = qc_session())
cat(apa, "\n"); print(round(coefs, 3))
