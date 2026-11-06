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
qc_session <- function() list(engine = "QuantCompass engine 0.3.0",
                              r_version = R.version.string, date = format(Sys.Date()))
# Friendly names for variables and categories (set by QuantCompass; empty = use column names)
qc_lab <- function(v) {
  l <- if (exists("qc_labels", inherits = TRUE)) get("qc_labels", inherits = TRUE)[[v]] else NULL
  if (is.null(l) || !nzchar(l)) v else l
}
qc_level_lab <- function(v, level) {
  m <- if (exists("qc_level_labels", inherits = TRUE)) get("qc_level_labels", inherits = TRUE)[[v]] else NULL
  key <- as.character(level)
  if (!is.null(m) && key %in% names(m) && nzchar(m[[key]])) m[[key]] else key
}
# Read any text data file as UTF-8 text (handles UTF-16 exports such as SoSci Survey, BOMs, Latin-1)
qc_text_file <- function(f, encoding) {
  b <- readBin(f, "raw", file.info(f)$size)
  bom <- list("UTF-16LE" = as.raw(c(0xff, 0xfe)), "UTF-16BE" = as.raw(c(0xfe, 0xff)), "UTF-8" = as.raw(c(0xef, 0xbb, 0xbf)))
  if (length(b) >= length(bom[[encoding]]) && identical(b[seq_along(bom[[encoding]])], bom[[encoding]])) b <- b[-seq_along(bom[[encoding]])]
  txt <- iconv(list(b), from = encoding, to = "UTF-8")
  gsub("\r\n?", "\n", txt)
}
# Convert columns read as text to numbers where every value is a number
qc_type_convert <- function(d) {
  d[] <- lapply(d, function(x) if (is.character(x)) utils::type.convert(x, as.is = TRUE, na.strings = c("", "NA")) else x)
  d
}
# Cronbach's alpha for a matrix of complete item answers
qc_alpha <- function(m) { m <- as.matrix(m); k <- ncol(m); (k / (k - 1)) * (1 - sum(apply(m, 2, stats::var)) / stats::var(rowSums(m))) }
qc_cap_first <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
# ---- end of helpers ----
