{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
x_var  <- "{{VAR1}}"        # predictor (X)
w_var  <- "{{MODERATOR}}"   # moderator (W)
y_var  <- "{{OUTCOME}}"     # outcome (Y)
robust <- {{ROBUST}}        # TRUE = heteroscedasticity-robust (HC3) tests
cc <- qc_complete(data, c(x_var, w_var, y_var)); d0 <- cc$data
w_raw <- d0[[w_var]]
w_cat <- is.factor(w_raw) || is.character(w_raw) || length(unique(w_raw)) <= 2    # categories: one slope per group
d <- data.frame(y = as.numeric(d0[[y_var]]), x = as.numeric(d0[[x_var]]))
d$xc <- d$x - mean(d$x)                                     # mean-centred predictor (Aiken & West, 1991)
if (w_cat) d$w <- factor(w_raw) else { d$w <- as.numeric(w_raw); d$wc <- d$w - mean(d$w) }
N <- nrow(d)

# ---- 3. Model without and with the interaction (Hayes, 2022, model 1) ----
m0 <- if (w_cat) stats::lm(y ~ xc + w, data = d) else stats::lm(y ~ xc + wc, data = d)
m1 <- if (w_cat) stats::lm(y ~ xc * w, data = d) else stats::lm(y ~ xc * wc, data = d)
hc3_vcov <- function(fit) { X <- stats::model.matrix(fit); e <- stats::residuals(fit); h <- stats::hatvalues(fit)
  XtXi <- solve(crossprod(X)); XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi }       # Long & Ervin (2000)
V <- if (robust) hc3_vcov(m1) else stats::vcov(m1)
b <- stats::coef(m1); df2 <- m1$df.residual
int_terms <- grep(":", names(b), value = TRUE); q <- length(int_terms)
# interaction test: F-change of the added product term(s), or a robust Wald test
F_int <- if (robust) as.numeric(t(b[int_terms]) %*% solve(V[int_terms, int_terms, drop = FALSE]) %*% b[int_terms]) / q else stats::anova(m0, m1)$F[2]
p_int <- stats::pf(F_int, q, df2, lower.tail = FALSE)
r2_0 <- summary(m0)$r.squared; r2_1 <- summary(m1)$r.squared; delta_r2 <- r2_1 - r2_0
se <- sqrt(diag(V)); crit <- stats::qt(.975, df2)
coefs <- data.frame(term = names(b), b = unname(b), se = unname(se), t = unname(b / se), p = unname(2 * stats::pt(-abs(b / se), df2)), row.names = names(b))
coefs$ci_low <- coefs$b - crit * coefs$se; coefs$ci_high <- coefs$b + crit * coefs$se
coefs$label <- vapply(rownames(coefs), function(r) {
  if (r == "(Intercept)") return(r)
  parts <- strsplit(r, ":", fixed = TRUE)[[1]]
  lab1 <- function(p) if (p == "xc") sprintf("%s (centred)", qc_lab(x_var)) else if (p == "wc") sprintf("%s (centred)", qc_lab(w_var))
    else sprintf("%s: %s vs %s", qc_lab(w_var), qc_level_lab(w_var, substring(p, 2)), qc_level_lab(w_var, levels(d$w)[1]))
  paste(vapply(parts, lab1, ""), collapse = " x ") }, "")

# ---- 4. Simple slopes: the effect of X on Y at chosen values of W ----
slope_at <- function(a) { est <- sum(a * b); s <- sqrt(as.numeric(t(a) %*% V %*% a)); c(slope = est, se = s, t = est / s, p = 2 * stats::pt(-abs(est / s), df2), ci_low = est - crit * s, ci_high = est + crit * s) }
if (w_cat) {
  lv <- levels(d$w)
  slopes <- do.call(rbind, lapply(lv, function(l) { a <- stats::setNames(rep(0, length(b)), names(b)); a["xc"] <- 1
    tn <- paste0("xc:w", l); if (tn %in% names(b)) a[tn] <- 1
    data.frame(at = qc_level_lab(w_var, l), level = l, w_value = NA, t(slope_at(a)), n = sum(d$w == l)) }))
} else {
  sdw <- stats::sd(d$w); pts <- c(-1, 0, 1) * sdw
  slopes <- do.call(rbind, lapply(1:3, function(i) { a <- stats::setNames(rep(0, length(b)), names(b)); a["xc"] <- 1; a["xc:wc"] <- pts[i]
    data.frame(at = c("Low (-1 SD)", "Mean", "High (+1 SD)")[i], level = c("low", "mean", "high")[i], w_value = mean(d$w) + pts[i], t(slope_at(a)), n = NA) }))
}
rownames(slopes) <- NULL

# ---- 5. Results ----
pv <- function(p) qc_fmt_p(p)
slope_txt <- paste(sprintf("%s%s: b = %s, p %s", if (w_cat) "" else paste0(qc_lab(w_var), " "), sub("^(.)", "\\L\\1", slopes$at, perl = TRUE), qc_fmt(slopes$slope), vapply(slopes$p, pv, "")), collapse = "; ")
int_txt <- if (q == 1) { sprintf("b = %s, SE = %s, t(%d) = %s, p %s", qc_fmt(b[[int_terms]]), qc_fmt(se[[int_terms]]), df2, qc_fmt(b[[int_terms]] / se[[int_terms]]), pv(p_int))
  } else sprintf("F(%d, %d) = %s, p %s", q, df2, qc_fmt(F_int), pv(p_int))
apa <- sprintf(paste0("A moderation analysis (Hayes, 2022, model 1) with a mean-centred predictor tested whether %s moderated the relationship between %s and %s (N = %d). ",
  "The interaction %s x %s was %s, %s, and it added %s to the explained variance (Delta R-squared = %s; R-squared = %s). ",
  "Simple slopes of %s on %s: %s.%s"),
  qc_lab(w_var), qc_lab(x_var), qc_lab(y_var), N,
  qc_lab(x_var), qc_lab(w_var), if (p_int < .05) "statistically significant" else "not statistically significant", int_txt,
  sprintf("%.1f percentage points", 100 * delta_r2), qc_fmt_r(delta_r2, 3), qc_fmt_r(r2_1),
  qc_lab(x_var), qc_lab(y_var), slope_txt, if (robust) " Tests use HC3 robust standard errors." else "")
result <- list(test = "moderation", n_used = cc$n_used, n_excluded = cc$n_excluded, p = p_int, F_int = F_int, df1 = q, df2 = df2,
  b_int = if (q == 1) unname(b[[int_terms]]) else NA, se_int = if (q == 1) unname(se[[int_terms]]) else NA,
  r2 = r2_1, r2_step1 = r2_0, delta_r2 = delta_r2, coefficients = coefs, slopes = slopes, w_categorical = w_cat,
  w_mean = if (w_cat) NA else mean(d$w), w_sd = if (w_cat) NA else stats::sd(d$w), x_mean = mean(d$x), robust = robust,
  apa = apa, session = qc_session())
cat(apa, "\n")
