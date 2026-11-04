{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
outcome <- "{{OUTCOME}}"
group   <- "{{GROUP}}"
cc <- qc_complete(data, c(outcome, group)); d <- cc$data
y <- as.numeric(d[[outcome]]); g <- factor(d[[group]]); k <- nlevels(g); N <- length(y)
if (k < 3) stop("Use a t-test for 2 groups; this template needs 3 or more groups.")

# ---- 3. Assumption checks ----
normality <- lapply(split(y, g), qc_shapiro)
levene <- qc_levene_bf(y, g)

# ---- 4. Welch's ANOVA (primary; robust to unequal variances) and classic ANOVA (reference) ----
welch <- stats::oneway.test(y ~ g, var.equal = FALSE)
classic <- stats::anova(stats::lm(y ~ g))
ss_b <- classic[["Sum Sq"]][1]; ss_w <- classic[["Sum Sq"]][2]; ms_w <- classic[["Mean Sq"]][2]
eta2 <- ss_b / (ss_b + ss_w)
omega2 <- (ss_b - (k - 1) * ms_w) / (ss_b + ss_w + ms_w)

# ---- 5. Games-Howell post-hoc comparisons (no equal-variance assumption) ----
m <- tapply(y, g, mean); v <- tapply(y, g, stats::var); n <- tapply(y, g, length); lv <- levels(g)
pairs <- utils::combn(k, 2)
posthoc <- do.call(rbind, lapply(seq_len(ncol(pairs)), function(j) {
  a <- pairs[1, j]; b <- pairs[2, j]
  diff <- m[[b]] - m[[a]]; se <- sqrt(v[[a]] / n[[a]] + v[[b]] / n[[b]])
  df <- (v[[a]] / n[[a]] + v[[b]] / n[[b]])^2 /
        ((v[[a]] / n[[a]])^2 / (n[[a]] - 1) + (v[[b]] / n[[b]])^2 / (n[[b]] - 1))
  crit <- stats::qtukey(.95, k, df) / sqrt(2)
  data.frame(comparison = paste(lv[b], "-", lv[a]), mean_diff = diff,
             ci_low = diff - crit * se, ci_high = diff + crit * se, t = diff / se, df = df,
             p_adj = stats::ptukey(abs(diff / se) * sqrt(2), k, df, lower.tail = FALSE))
}))

# ---- 6. Results ----
sig <- if (welch$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf("A Welch's one-way ANOVA showed %s difference in %s across the %d groups of %s, F(%d, %s) = %s, p %s, eta-squared = %s, omega-squared = %s.",
  sig, outcome, k, group, as.integer(welch$parameter[1]), qc_fmt(welch$parameter[2]),
  qc_fmt(welch$statistic), qc_fmt_p(welch$p.value), qc_fmt_r(eta2), qc_fmt_r(omega2))
result <- list(test = "anova_oneway", n_used = cc$n_used, n_excluded = cc$n_excluded,
  groups = data.frame(group = lv, n = as.vector(n), mean = as.vector(m), sd = sqrt(as.vector(v))),
  welch = list(F = unname(welch$statistic), df1 = unname(welch$parameter[1]), df2 = unname(welch$parameter[2]), p = welch$p.value),
  classic = list(F = classic[["F value"]][1], df1 = classic[["Df"]][1], df2 = classic[["Df"]][2], p = classic[["Pr(>F)"]][1]),
  eta2 = eta2, omega2 = omega2, posthoc = posthoc,
  assumptions = list(normality = normality, levene = levene), apa = apa, session = qc_session())
cat(apa, "\n"); print(posthoc, digits = 3, row.names = FALSE)
