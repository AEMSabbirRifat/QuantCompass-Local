# ---- QuantCompass helpers (copied into every script so it runs on its own) ----
qc_fmt   <- function(x, d = 2) formatC(x, format = "f", digits = d)
qc_fmt_p <- function(p) {
  if (is.na(p)) return("= NA")
  if (p < .001) return("< .001")
  paste0("= ", sub("^0", "", qc_fmt(p, 3)))
}
# Values that cannot exceed 1 (r, p, alpha) are reported without a leading zero (APA 7)
qc_fmt_r <- function(x, d = 2) sub("^(-?)0\\.", "\\1.", qc_fmt(x, d))
qc_complete <- function(data, vars) {
  keep <- stats::complete.cases(data[, vars, drop = FALSE])
  list(data = data[keep, , drop = FALSE], n_used = sum(keep), n_excluded = sum(!keep))
}
# Brown-Forsythe (median-centred) version of Levene's test for equal variances
qc_levene_bf <- function(y, g) {
  g <- factor(g)
  dev <- abs(y - stats::ave(y, g, FUN = stats::median))
  a <- stats::anova(stats::lm(dev ~ g))
  list(F = a[["F value"]][1], df1 = a[["Df"]][1], df2 = a[["Df"]][2], p = a[["Pr(>F)"]][1])
}
# Shapiro-Wilk normality test (only defined for 3 to 5000 non-constant values)
qc_shapiro <- function(y) {
  if (length(y) >= 3 && length(y) <= 5000 && stats::var(y) > 0) {
    s <- stats::shapiro.test(y)
    list(W = unname(s$statistic), p = s$p.value)
  } else list(W = NA, p = NA)
}
qc_session <- function() list(engine = "QuantCompass engine 0.1.0",
                              r_version = R.version.string, date = format(Sys.Date()))
# ---- end of helpers ----
