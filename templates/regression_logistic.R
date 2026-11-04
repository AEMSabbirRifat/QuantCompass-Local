{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
outcome    <- "{{OUTCOME}}"
positive   <- "{{POSITIVE}}"     # the outcome category modelled as "1"
predictors <- {{PREDICTORS_VEC}}
cc <- qc_complete(data, c(outcome, predictors)); d <- cc$data
if (length(unique(d[[outcome]])) != 2) stop("The outcome must have exactly 2 categories.")
d$.y <- as.integer(as.character(d[[outcome]]) == positive)
for (v in predictors) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
f <- stats::reformulate(sprintf("`%s`", predictors), response = ".y")

# ---- 3. Fit the model ----
model <- stats::glm(f, data = d, family = stats::binomial())
s <- summary(model)
coefs <- as.data.frame(s$coefficients); names(coefs) <- c("b", "se", "z", "p")
wald <- stats::confint.default(model)                  # Wald 95% CIs
coefs$odds_ratio <- exp(coefs$b); coefs$or_ci_low <- exp(wald[, 1]); coefs$or_ci_high <- exp(wald[, 2])

# ---- 4. Model fit ----
n <- nrow(d); ll1 <- as.numeric(stats::logLik(model))
null <- stats::glm(.y ~ 1, data = d, family = stats::binomial()); ll0 <- as.numeric(stats::logLik(null))
lr <- 2 * (ll1 - ll0); lr_df <- length(stats::coef(model)) - 1
p_model <- stats::pchisq(lr, lr_df, lower.tail = FALSE)
r2_cs <- 1 - exp(2 * (ll0 - ll1) / n)                 # Cox & Snell
r2_nag <- r2_cs / (1 - exp(2 * ll0 / n))              # Nagelkerke
events <- min(sum(d$.y), n - sum(d$.y)); epv <- events / lr_df
pred <- as.integer(stats::fitted(model) >= .5); accuracy <- mean(pred == d$.y)

# ---- 5. Results ----
sig <- if (p_model < .05) "statistically significant" else "not statistically significant"
apa <- sprintf("A binary logistic regression predicting %s = '%s' from %s was %s, chi-square(%d) = %s, p %s, Nagelkerke R-squared = %s, N = %d (%d events, %.1f events per predictor term).",
  outcome, positive, paste(predictors, collapse = ", "), sig, as.integer(lr_df), qc_fmt(lr),
  qc_fmt_p(p_model), qc_fmt_r(r2_nag), n, sum(d$.y), epv)
result <- list(test = "regression_logistic", n_used = cc$n_used, n_excluded = cc$n_excluded,
  coefficients = coefs, lr_chisq = lr, df = lr_df, p = p_model, r2_cox_snell = r2_cs,
  r2_nagelkerke = r2_nag, events_per_term = epv, accuracy_at_0.5 = accuracy,
  apa = apa, session = qc_session())
cat(apa, "\n"); print(round(coefs, 3))
