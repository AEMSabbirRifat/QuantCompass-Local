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

# ---- 3. Assumption checks ----
normality <- setNames(list(qc_shapiro(y1), qc_shapiro(y2)), lv)   # Shapiro-Wilk per group
levene <- qc_levene_bf(y, g)                                      # equal variances (Brown-Forsythe)

# ---- 4. Welch's t-test (does not assume equal variances) ----
tt <- stats::t.test(y ~ g, var.equal = FALSE, alternative = "{{ALTERNATIVE}}")

# ---- 5. Effect size: Cohen's d (pooled SD) with approximate 95% CI (Hedges & Olkin, 1985) ----
sp <- sqrt(((n1 - 1) * stats::var(y1) + (n2 - 1) * stats::var(y2)) / (n1 + n2 - 2))
cohen_d <- (mean(y1) - mean(y2)) / sp
se_d <- sqrt((n1 + n2) / (n1 * n2) + cohen_d^2 / (2 * (n1 + n2)))
d_ci <- cohen_d + c(-1, 1) * stats::qnorm(.975) * se_d

# ---- 6. Results ----
sig <- if (tt$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf(paste0(
  "A Welch's independent-samples t-test showed %s difference in %s between %s (M = %s, SD = %s, n = %d) ",
  "and %s (M = %s, SD = %s, n = %d), t(%s) = %s, p %s, d = %s, 95%% CI [%s, %s]."),
  sig, outcome, lv[1], qc_fmt(mean(y1)), qc_fmt(stats::sd(y1)), n1,
  lv[2], qc_fmt(mean(y2)), qc_fmt(stats::sd(y2)), n2,
  qc_fmt(tt$parameter), qc_fmt(tt$statistic), qc_fmt_p(tt$p.value),
  qc_fmt(cohen_d), qc_fmt(d_ci[1]), qc_fmt(d_ci[2]))
result <- list(
  test = "ttest_independent", n_used = cc$n_used, n_excluded = cc$n_excluded,
  groups = data.frame(group = lv, n = c(n1, n2), mean = c(mean(y1), mean(y2)), sd = c(stats::sd(y1), stats::sd(y2))),
  t = unname(tt$statistic), df = unname(tt$parameter), p = tt$p.value,
  mean_diff = unname(-diff(tt$estimate)), mean_diff_ci = as.numeric(tt$conf.int),
  cohen_d = cohen_d, cohen_d_ci = d_ci,
  assumptions = list(normality = normality, levene = levene),
  apa = apa, session = qc_session())
cat(apa, "\n")
