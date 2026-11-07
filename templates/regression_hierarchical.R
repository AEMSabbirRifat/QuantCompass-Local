{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
outcome    <- "{{OUTCOME}}"
controls   <- {{CONTROLS_VEC}}     # step 1: control variables
predictors <- {{PREDICTORS_VEC}}   # step 2: the predictor(s) the hypothesis is about
robust     <- {{ROBUST}}           # TRUE = heteroscedasticity-robust (HC3) tests
allv <- c(controls, predictors)
cc <- qc_complete(data, c(outcome, allv)); d <- cc$data       # the same cases in both steps
for (v in allv) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])   # categories -> dummy variables
f1 <- stats::reformulate(sprintf("`%s`", controls), response = sprintf("`%s`", outcome))
f2 <- stats::reformulate(sprintf("`%s`", allv), response = sprintf("`%s`", outcome))

# ---- 3. Step 1 (controls) and step 2 (controls + predictors) ----
m1 <- stats::lm(f1, data = d); m2 <- stats::lm(f2, data = d)
s1 <- summary(m1); s2 <- summary(m2)
delta_r2 <- s2$r.squared - s1$r.squared
cmp <- stats::anova(m1, m2)                       # F-change test for the added predictors
F_change <- cmp$F[2]; df1 <- cmp$Df[2]; df2 <- cmp$Res.Df[2]; p_change <- cmp[["Pr(>F)"]][2]
# HC3 robust standard errors (Long & Ervin, 2000)
X <- stats::model.matrix(m2); e <- stats::residuals(m2); h <- stats::hatvalues(m2); XtXi <- solve(crossprod(X))
V_hc3 <- XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi
added <- setdiff(colnames(X), colnames(stats::model.matrix(m1)))
if (robust) {                                     # robust Wald test of the added terms (replaces the F-change test)
  b_add <- stats::coef(m2)[added]; W <- as.numeric(t(b_add) %*% solve(V_hc3[added, added, drop = FALSE]) %*% b_add)
  F_change <- W / length(added); df1 <- length(added); df2 <- m2$df.residual; p_change <- stats::pf(F_change, df1, df2, lower.tail = FALSE)
}
coefs <- as.data.frame(s2$coefficients); names(coefs) <- c("b", "se", "t", "p")
if (robust) { coefs$se <- sqrt(diag(V_hc3)); coefs$t <- coefs$b / coefs$se; coefs$p <- 2 * stats::pt(abs(coefs$t), m2$df.residual, lower.tail = FALSE) }
crit <- stats::qt(.975, m2$df.residual)
coefs$ci_low <- coefs$b - crit * coefs$se; coefs$ci_high <- coefs$b + crit * coefs$se
sx <- c(NA, apply(X[, -1, drop = FALSE], 2, stats::sd)); sy <- stats::sd(d[[outcome]])
coefs$beta <- coefs$b * sx / sy; coefs$beta_ci_low <- coefs$ci_low * sx / sy; coefs$beta_ci_high <- coefs$ci_high * sx / sy
term_lab <- function(r) { r <- gsub("`", "", r); if (r == "(Intercept)") return(r)
  for (v in allv) if (is.factor(d[[v]]) && startsWith(r, v) && r != v) {
    l <- substring(r, nchar(v) + 1); return(sprintf("%s (%s vs %s)", qc_lab(v), qc_level_lab(v, l), qc_level_lab(v, levels(d[[v]])[1]))) }
  qc_lab(r) }
rownames(coefs) <- gsub("`", "", rownames(coefs))
coefs$label <- sapply(rownames(coefs), term_lab)
coefs$step <- ifelse(rownames(coefs) %in% gsub("`", "", added), "added in step 2", ifelse(rownames(coefs) == "(Intercept)", "", "control"))
fs1 <- s1$fstatistic; fs2 <- s2$fstatistic
p1 <- stats::pf(fs1[1], fs1[2], fs1[3], lower.tail = FALSE); p2 <- stats::pf(fs2[1], fs2[2], fs2[3], lower.tail = FALSE)
steps <- data.frame(step = c("Step 1: controls", "Step 2: + predictors"), R2 = c(s1$r.squared, s2$r.squared),
                    adj_R2 = c(s1$adj.r.squared, s2$adj.r.squared), F = c(fs1[1], fs2[1]), df1 = c(fs1[2], fs2[2]), df2 = c(fs1[3], fs2[3]),
                    p = c(p1, p2), delta_R2 = c(s1$r.squared, delta_r2), F_change = c(NA, F_change), p_change = c(NA, p_change))

# ---- 4. Assumption checks (final model) ----
Xp <- X[, -1, drop = FALSE]
normality <- qc_shapiro(e)
bp_fit <- summary(stats::lm(e^2 ~ Xp))
bp <- list(LM = nrow(Xp) * bp_fit$r.squared, df = ncol(Xp), p = stats::pchisq(nrow(Xp) * bp_fit$r.squared, ncol(Xp), lower.tail = FALSE))
vif <- if (ncol(Xp) > 1) sapply(seq_len(ncol(Xp)), function(j) 1 / (1 - summary(stats::lm(Xp[, j] ~ Xp[, -j]))$r.squared)) else 1
names(vif) <- colnames(Xp)

# ---- 5. Results ----
focal <- rownames(coefs)[coefs$step == "added in step 2"]
apa <- sprintf(paste0("A hierarchical multiple regression predicted %s (N = %d). Step 1 entered %s and explained R-squared = %s, F(%d, %d) = %s, p %s. ",
  "Adding %s in step 2 increased R-squared by %s (to %s), %s(%d, %d) = %s, p %s, so the added predictor%s explained %s additional variance.%s%s"),
  qc_lab(outcome), nrow(d), paste(sapply(controls, qc_lab), collapse = ", "), qc_fmt_r(s1$r.squared), as.integer(fs1[2]), as.integer(fs1[3]), qc_fmt(fs1[1]), qc_fmt_p(p1),
  paste(sapply(predictors, qc_lab), collapse = " and "), qc_fmt_r(delta_r2, 3), qc_fmt_r(s2$r.squared), if (robust) "robust Wald F" else "F-change",
  as.integer(df1), as.integer(df2), qc_fmt(F_change), qc_fmt_p(p_change), if (length(predictors) > 1) "s" else "",
  if (p_change < .05) "statistically significant" else "no statistically significant",
  paste0(sapply(focal, function(r) { i <- which(rownames(coefs) == r)
    sprintf(" In the final model, %s: b = %s, beta = %s, t = %s, p %s.", coefs$label[i], qc_fmt(coefs$b[i]), qc_fmt(coefs$beta[i]), qc_fmt(coefs$t[i]), qc_fmt_p(coefs$p[i])) }), collapse = ""),
  if (robust) " Because residual variance was not constant, all tests use HC3 robust standard errors." else "")
result <- list(test = "regression_hierarchical", n_used = cc$n_used, n_excluded = cc$n_excluded, steps = steps, coefficients = coefs,
  r2_step1 = s1$r.squared, r2 = s2$r.squared, adj_r2 = s2$adj.r.squared, delta_r2 = delta_r2, F_change = F_change, df1 = df1, df2 = df2,
  p = p_change, robust = robust, focal = focal, assumptions = list(normality_of_residuals = normality, breusch_pagan = bp, vif = vif),
  apa = apa, session = qc_session())
cat(apa, "\n"); print(steps, digits = 3); print(coefs[, c("b", "se", "t", "p", "beta", "step")], digits = 3)
