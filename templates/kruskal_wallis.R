{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select variables and remove incomplete cases ----
outcome <- "{{OUTCOME}}"
group   <- "{{GROUP}}"
cc <- qc_complete(data, c(outcome, group)); d <- cc$data
y <- as.numeric(d[[outcome]]); g <- factor(d[[group]]); N <- length(y); k <- nlevels(g)

# ---- 3. Kruskal-Wallis rank-sum test ----
kw <- stats::kruskal.test(y ~ g)
H <- unname(kw$statistic)

# ---- 4. Effect size: epsilon-squared = H / ((N^2 - 1) / (N + 1)) (Tomczak & Tomczak, 2014) ----
eps2 <- H / ((N^2 - 1) / (N + 1))

# ---- 5. Pairwise Mann-Whitney tests with Holm correction ----
pw <- stats::pairwise.wilcox.test(y, g, p.adjust.method = "holm", exact = FALSE)

# ---- 6. Results ----
sig <- if (kw$p.value < .05) "a statistically significant" else "no statistically significant"
apa <- sprintf("A Kruskal-Wallis test showed %s difference in %s across the %d groups of %s, H(%d) = %s, p %s, epsilon-squared = %s, N = %d.",
  sig, outcome, k, group, as.integer(kw$parameter), qc_fmt(H), qc_fmt_p(kw$p.value), qc_fmt_r(eps2), N)
result <- list(test = "kruskal_wallis", n_used = cc$n_used, n_excluded = cc$n_excluded,
  groups = data.frame(group = levels(g), n = as.vector(table(g)), median = as.vector(tapply(y, g, stats::median))),
  H = H, df = unname(kw$parameter), p = kw$p.value, epsilon2 = eps2,
  pairwise_p_holm = pw$p.value, apa = apa, session = qc_session())
cat(apa, "\n"); print(pw)
