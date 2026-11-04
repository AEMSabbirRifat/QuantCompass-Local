{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
outcome <- "{{OUTCOME}}"
group   <- "{{GROUP}}"
cc <- qc_complete(data, c(outcome, group)); d <- cc$data
y <- as.numeric(d[[outcome]]); g <- factor(d[[group]])
if (nlevels(g) != 2) stop("The grouping variable must have exactly 2 groups; found ", nlevels(g), ".")
lv <- levels(g); y1 <- y[g == lv[1]]; y2 <- y[g == lv[2]]
n1 <- length(y1); n2 <- length(y2)

# ---- 3. Mann-Whitney U test (normal approximation with continuity correction if ties) ----
mw <- stats::wilcox.test(y1, y2, alternative = "{{ALTERNATIVE}}", exact = FALSE, correct = TRUE)
U <- unname(mw$statistic)

# ---- 4. Effect size: rank-biserial correlation r = 1 - 2U / (n1 * n2) (Kerby, 2014) ----
r_rb <- 1 - 2 * U / (n1 * n2)
# sign convention: positive r means group 1 tends to have higher values
r_rb <- -r_rb

# ---- 5. Results ----
sig <- if (mw$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf(paste0(
  "A Mann-Whitney U test showed %s difference in %s between %s (Mdn = %s, n = %d) and %s (Mdn = %s, n = %d), ",
  "U = %s, p %s, rank-biserial r = %s."),
  sig, outcome, lv[1], qc_fmt(stats::median(y1)), n1, lv[2], qc_fmt(stats::median(y2)), n2,
  qc_fmt(U, 1), qc_fmt_p(mw$p.value), qc_fmt_r(r_rb))
result <- list(
  test = "mann_whitney", n_used = cc$n_used, n_excluded = cc$n_excluded,
  groups = data.frame(group = lv, n = c(n1, n2), median = c(stats::median(y1), stats::median(y2)),
                      iqr = c(stats::IQR(y1), stats::IQR(y2))),
  U = U, p = mw$p.value, rank_biserial = r_rb, apa = apa, session = qc_session())
cat(apa, "\n")
