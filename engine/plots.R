# Charts for assumption checks, results and dataset descriptives (ggplot2).
# Style: one accent colour, recessive grid, text in ink colours; identity on axes, not colour.

qc_col <- list(accent = "#2a78d6", accent2 = "#eb6834", ink = "#1f2933", muted = "#6b7280",
               grid = "#e5e7eb", ref = "#9ca3af", neutral = "#f0efec", neg = "#e34948",
               cat = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"))

qc_theme <- function() ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(
  text = ggplot2::element_text(colour = qc_col$ink), axis.text = ggplot2::element_text(colour = qc_col$muted),
  panel.grid.minor = ggplot2::element_blank(), panel.grid.major = ggplot2::element_line(colour = qc_col$grid, linewidth = .3),
  plot.title = ggplot2::element_text(face = "bold", size = 11), plot.title.position = "plot",
  strip.text = ggplot2::element_text(face = "bold", colour = qc_col$ink), legend.position = "top")

qc_cap1 <- function(x) if (is.null(x) || !nzchar(x)) x else paste0(toupper(substr(x, 1, 1)), substring(x, 2))
qc_fig <- function(plot, caption, width = 6.5, height = 4) {
  plot$labels$title <- qc_cap1(plot$labels$title); plot$labels$x <- qc_cap1(plot$labels$x); plot$labels$y <- qc_cap1(plot$labels$y)
  list(plot = plot, caption = qc_cap1(caption), width = width, height = height)
}
qc_grp <- function(group, g) factor(sapply(as.character(g), function(l) qc_level_lab(group, l)),
                                     levels = sapply(levels(factor(g)), function(l) qc_level_lab(group, l)))

# ---- Assumption charts ----
qc_plot_normality <- function(y, g = NULL, what) {
  df <- data.frame(y = y, g = if (is.null(g)) "All cases" else g)
  stats_df <- do.call(rbind, lapply(split(df, df$g), function(s) {
    xs <- seq(min(s$y), max(s$y), length.out = 100); bw <- diff(range(df$y)) / 30
    data.frame(g = s$g[1], x = xs, dens = stats::dnorm(xs, mean(s$y), stats::sd(s$y)) * nrow(s) * bw) }))
  bw <- diff(range(df$y)) / 30
  h <- ggplot2::ggplot(df, ggplot2::aes(y)) +
    ggplot2::geom_histogram(binwidth = bw, fill = qc_col$accent, colour = "white", linewidth = .2, alpha = .85) +
    ggplot2::geom_line(data = stats_df, ggplot2::aes(x, dens), colour = qc_col$ink, linewidth = .6) +
    ggplot2::facet_wrap(~g, scales = "free_y") + ggplot2::labs(title = paste("Distribution of", what), x = what, y = "Count") + qc_theme()
  q <- ggplot2::ggplot(df, ggplot2::aes(sample = y)) + ggplot2::stat_qq(colour = qc_col$accent, size = 1.2, alpha = .7) +
    ggplot2::stat_qq_line(colour = qc_col$ink, linewidth = .5) + ggplot2::facet_wrap(~g, scales = "free") +
    ggplot2::labs(title = paste("Normal Q-Q plot of", what), x = "Theoretical quantiles", y = "Observed values") + qc_theme()
  list(qc_fig(h, sprintf("Histogram of %s with a fitted normal curve. A bell shape that follows the curve indicates approximate normality.", what)),
       qc_fig(q, sprintf("Normal Q-Q plot of %s. Points close to the line indicate approximate normality.", what)))
}

qc_plot_box <- function(y, g, ylab, xlab) {
  df <- data.frame(y = y, g = g)
  p <- ggplot2::ggplot(df, ggplot2::aes(g, y)) +
    ggplot2::geom_boxplot(fill = "#cde2fb", colour = qc_col$accent, width = .5, outlier.shape = NA, linewidth = .5) +
    ggplot2::geom_jitter(width = .12, height = 0, colour = qc_col$ink, alpha = .35, size = 1.1) +
    ggplot2::labs(title = paste(ylab, "by", xlab), x = xlab, y = ylab) + qc_theme()
  qc_fig(p, sprintf("Boxplots of %s by %s with individual cases. Box = middle 50%%, line = median; similar box heights suggest similar variances, isolated points far from the box are outliers.", ylab, xlab))
}

qc_plot_scatter <- function(x, y, xlab, ylab) {
  df <- data.frame(x = x, y = y)
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_point(colour = qc_col$accent, alpha = .5, size = 1.4) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = qc_col$ink, fill = qc_col$grid, linewidth = .7) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = qc_col$accent2, linewidth = .7, linetype = "dashed") +
    ggplot2::labs(title = paste(ylab, "versus", xlab), x = xlab, y = ylab) + qc_theme()
  qc_fig(p, sprintf("Scatterplot of %s against %s. Solid line = straight-line fit with 95%% band; dashed line = smooth curve. If the two lines agree, the relationship is linear.", ylab, xlab))
}

qc_plot_residuals <- function(model) {
  df <- data.frame(fit = stats::fitted(model), res = stats::residuals(model), std = stats::rstandard(model),
                   cook = stats::cooks.distance(model), id = seq_along(stats::fitted(model)))
  n <- nrow(df)
  a <- ggplot2::ggplot(df, ggplot2::aes(fit, res)) + ggplot2::geom_hline(yintercept = 0, colour = qc_col$ref) +
    ggplot2::geom_point(colour = qc_col$accent, alpha = .5, size = 1.2) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = qc_col$accent2, linewidth = .7) +
    ggplot2::labs(title = "Residuals versus fitted values", x = "Fitted values", y = "Residuals") + qc_theme()
  b <- ggplot2::ggplot(df, ggplot2::aes(fit, sqrt(abs(std)))) + ggplot2::geom_point(colour = qc_col$accent, alpha = .5, size = 1.2) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = qc_col$accent2, linewidth = .7) +
    ggplot2::labs(title = "Scale-location", x = "Fitted values", y = "sqrt(|standardised residual|)") + qc_theme()
  c <- ggplot2::ggplot(df, ggplot2::aes(id, cook)) + ggplot2::geom_col(fill = qc_col$accent, width = .8) +
    ggplot2::geom_hline(yintercept = 4 / n, colour = qc_col$neg, linetype = "dashed") +
    ggplot2::labs(title = "Cook's distance", x = "Case number", y = "Cook's distance") + qc_theme()
  list(qc_fig(a, "Residuals versus fitted values. A flat, even band around zero indicates linearity; a curve indicates a non-linear pattern."),
       qc_fig(b, "Scale-location plot. A flat line indicates constant residual variance (homoscedasticity); a rising or falling line indicates it is not constant."),
       qc_fig(c, "Cook's distance for each case. Bars above the dashed line (4/n) mark influential cases worth checking."))
}

qc_plot_vif <- function(vif, X) {
  cm <- stats::cor(X); df <- expand.grid(a = colnames(cm), b = colnames(cm)); df$r <- as.vector(cm)
  h <- ggplot2::ggplot(df, ggplot2::aes(a, b, fill = r)) + ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sub("^(-?)0\\.", "\\1.", sprintf("%.2f", r))), size = 3, colour = qc_col$ink) +
    ggplot2::scale_fill_gradient2(low = qc_col$neg, mid = qc_col$neutral, high = qc_col$accent, midpoint = 0, limits = c(-1, 1)) +
    ggplot2::labs(title = "Correlations between predictors", x = NULL, y = NULL, fill = "r") + qc_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  v <- ggplot2::ggplot(data.frame(p = names(vif), vif = vif), ggplot2::aes(stats::reorder(p, vif), vif)) +
    ggplot2::geom_col(fill = qc_col$accent, width = .6) + ggplot2::geom_hline(yintercept = 5, colour = qc_col$neg, linetype = "dashed") +
    ggplot2::coord_flip() + ggplot2::labs(title = "Variance inflation factors", x = NULL, y = "VIF") + qc_theme()
  list(qc_fig(h, "Correlations between predictors. Values close to -1 or 1 indicate predictors that overlap strongly."),
       qc_fig(v, "Variance inflation factor (VIF) per predictor. Values above the dashed line (5) indicate problematic multicollinearity.", height = 3))
}

qc_plot_stacked <- function(a, b, alab, blab) {
  tab <- prop.table(table(a, b), 1) * 100; df <- as.data.frame(tab)
  df$b <- factor(df$b); if (nlevels(df$b) > 8) stop("Too many categories to chart")
  p <- ggplot2::ggplot(df, ggplot2::aes(a, Freq, fill = b)) + ggplot2::geom_col(width = .6, colour = "white", linewidth = .5) +
    ggplot2::scale_fill_manual(values = qc_col$cat[seq_len(nlevels(df$b))]) +
    ggplot2::labs(title = paste(blab, "by", alab), x = alab, y = "Percent within group", fill = blab) + qc_theme()
  qc_fig(p, sprintf("Percentage of each %s category within each %s group. Bars with similar splits suggest no association.", blab, alab))
}

qc_plot_paired <- function(x1, x2, l1, l2) {
  df <- data.frame(id = rep(seq_along(x1), 2), when = factor(rep(c(l1, l2), each = length(x1)), levels = c(l1, l2)), y = c(x1, x2))
  p <- ggplot2::ggplot(df, ggplot2::aes(when, y, group = id)) + ggplot2::geom_line(colour = qc_col$ref, alpha = .4) +
    ggplot2::geom_point(colour = qc_col$accent, size = 1.3, alpha = .7) +
    ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line", colour = qc_col$ink, linewidth = 1.1) +
    ggplot2::labs(title = paste(l1, "versus", l2), x = NULL, y = "Score") + qc_theme()
  qc_fig(p, sprintf("Each grey line is one case measured at %s and %s; the dark line connects the means.", l1, l2))
}

# ---- Result charts ----
qc_plot_means_ci <- function(y, g, ylab, xlab) {
  df <- data.frame(y = y, g = g)
  s <- do.call(rbind, lapply(split(df$y, df$g), function(v) { m <- mean(v); se <- stats::sd(v) / sqrt(length(v))
    data.frame(m = m, lo = m - stats::qt(.975, length(v) - 1) * se, hi = m + stats::qt(.975, length(v) - 1) * se) }))
  s$g <- factor(rownames(s), levels = levels(df$g))
  p <- ggplot2::ggplot() + ggplot2::geom_jitter(data = df, ggplot2::aes(g, y), width = .15, height = 0, colour = qc_col$ref, alpha = .4, size = 1) +
    ggplot2::geom_errorbar(data = s, ggplot2::aes(g, ymin = lo, ymax = hi), width = .15, colour = qc_col$ink, linewidth = .7) +
    ggplot2::geom_point(data = s, ggplot2::aes(g, m), colour = qc_col$accent, size = 3) +
    ggplot2::labs(title = paste("Mean", ylab, "by", xlab), x = xlab, y = ylab) + qc_theme()
  qc_fig(p, sprintf("Mean %s by %s with 95%% confidence intervals (dots and bars) and individual cases (grey).", ylab, xlab))
}

qc_plot_coef <- function(coefs, ylab) {
  df <- coefs[-1, ]; df$term <- df$label
  p <- ggplot2::ggplot(df, ggplot2::aes(beta, stats::reorder(term, beta))) + ggplot2::geom_vline(xintercept = 0, colour = qc_col$ref) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = beta_ci_low, xmax = beta_ci_high), height = .15, colour = qc_col$ink) +
    ggplot2::geom_point(colour = qc_col$accent, size = 3) +
    ggplot2::labs(title = paste("Standardised effects on", ylab), x = "Standardised coefficient (beta) with 95% CI", y = NULL) + qc_theme()
  qc_fig(p, sprintf("Standardised regression coefficients for %s with 95%% confidence intervals. Intervals that do not cross zero are statistically significant.", ylab), height = 1.8 + .35 * nrow(df))
}

qc_plot_or <- function(coefs, ylab) {
  df <- coefs[-1, ]; df$term <- df$label
  p <- ggplot2::ggplot(df, ggplot2::aes(odds_ratio, stats::reorder(term, odds_ratio))) + ggplot2::geom_vline(xintercept = 1, colour = qc_col$ref) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = or_ci_low, xmax = or_ci_high), height = .15, colour = qc_col$ink) +
    ggplot2::geom_point(colour = qc_col$accent, size = 3) + ggplot2::scale_x_log10() +
    ggplot2::labs(title = paste("Odds ratios for", ylab), x = "Odds ratio with 95% CI (log scale)", y = NULL) + qc_theme()
  qc_fig(p, sprintf("Odds ratios for %s with 95%% confidence intervals. Values above 1 increase the odds, below 1 decrease them; intervals crossing 1 are not significant.", ylab), height = 1.8 + .35 * nrow(df))
}

qc_plot_items <- function(items_df) {
  p <- ggplot2::ggplot(items_df, ggplot2::aes(corrected_item_total_r, stats::reorder(item, corrected_item_total_r))) +
    ggplot2::geom_col(fill = qc_col$accent, width = .6) + ggplot2::geom_vline(xintercept = .3, colour = qc_col$neg, linetype = "dashed") +
    ggplot2::labs(title = "Corrected item-total correlations", x = "r with the rest of the scale", y = NULL) + qc_theme()
  qc_fig(p, "Corrected item-total correlation per item. Items below the dashed line (.30) relate weakly to the rest of the scale.", height = 1.8 + .35 * nrow(items_df))
}

# ---- Dataset description charts ----
qc_plot_hist_grid <- function(data, vars) {
  long <- do.call(rbind, lapply(vars, function(v) data.frame(var = qc_lab(v), value = as.numeric(data[[v]]))))
  long <- long[!is.na(long$value), ]
  p <- ggplot2::ggplot(long, ggplot2::aes(value)) + ggplot2::geom_histogram(bins = 25, fill = qc_col$accent, colour = "white", linewidth = .2) +
    ggplot2::facet_wrap(~var, scales = "free", ncol = 3) + ggplot2::labs(title = "Distributions of numeric variables", x = NULL, y = "Count") + qc_theme()
  qc_fig(p, "Histograms of all numeric variables.", height = 1.6 + 1.7 * ceiling(length(vars) / 3))
}

qc_plot_bar_grid <- function(data, vars) {
  long <- do.call(rbind, lapply(vars, function(v) { t <- table(data[[v]])
    data.frame(var = qc_lab(v), cat = sapply(names(t), function(l) qc_level_lab(v, l)), n = as.vector(t)) }))
  p <- ggplot2::ggplot(long, ggplot2::aes(n, cat)) + ggplot2::geom_col(fill = qc_col$accent, width = .6) +
    ggplot2::facet_wrap(~var, scales = "free", ncol = 2) + ggplot2::labs(title = "Categorical variables", x = "Count", y = NULL) + qc_theme()
  qc_fig(p, "Counts per category for each categorical variable.", height = 1.6 + 1.8 * ceiling(length(vars) / 2))
}

qc_plot_missing <- function(data) {
  df <- data.frame(var = sapply(names(data), qc_lab), pct = 100 * colMeans(is.na(data)))
  p <- ggplot2::ggplot(df, ggplot2::aes(pct, stats::reorder(var, pct))) + ggplot2::geom_col(fill = qc_col$accent, width = .6) +
    ggplot2::labs(title = "Missing values per variable", x = "Percent missing", y = NULL) + qc_theme()
  qc_fig(p, "Percentage of missing values in each variable.", height = 1.6 + .25 * ncol(data))
}

qc_plot_cor_heatmap <- function(data, vars) {
  cm <- stats::cor(data[vars], use = "pairwise.complete.obs"); labs <- sapply(vars, qc_lab)
  df <- expand.grid(a = labs, b = labs); df$r <- as.vector(cm)
  p <- ggplot2::ggplot(df, ggplot2::aes(a, b, fill = r)) + ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sub("^(-?)0\\.", "\\1.", sprintf("%.2f", r))), size = 2.6, colour = qc_col$ink) +
    ggplot2::scale_fill_gradient2(low = qc_col$neg, mid = qc_col$neutral, high = qc_col$accent, midpoint = 0, limits = c(-1, 1)) +
    ggplot2::labs(title = "Correlations between numeric variables", x = NULL, y = NULL, fill = "r") + qc_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 40, hjust = 1))
  qc_fig(p, "Pearson correlations between numeric variables (blue = positive, red = negative).", width = 7, height = 2 + .35 * length(vars))
}

# ---- Which charts belong to which test ----
qc_assumption_figures <- function(data, spec, screen) {
  id <- screen$decision$test_id; s <- if (!is.null(screen$rerouted)) screen$rerouted else spec
  if (id %in% c("ttest_independent", "mann_whitney", "anova_oneway", "kruskal_wallis")) {
    d <- qc_complete(data, c(s$outcome, s$group))$data; y <- as.numeric(d[[s$outcome]]); g <- qc_grp(s$group, d[[s$group]])
    return(c(qc_plot_normality(y, g, qc_lab(s$outcome)), list(qc_plot_box(y, g, qc_lab(s$outcome), qc_lab(s$group)))))
  }
  if (id %in% c("ttest_paired", "wilcoxon_signed")) {
    d <- qc_complete(data, c(s$var1, s$var2))$data; diffs <- as.numeric(d[[s$var1]]) - as.numeric(d[[s$var2]])
    return(c(qc_plot_normality(diffs, NULL, sprintf("differences (%s - %s)", qc_lab(s$var1), qc_lab(s$var2))),
             list(qc_plot_paired(d[[s$var1]], d[[s$var2]], qc_lab(s$var1), qc_lab(s$var2)))))
  }
  if (id %in% c("correlation_pearson", "correlation_spearman")) {
    d <- qc_complete(data, c(s$var1, s$var2))$data
    return(c(qc_plot_normality(as.numeric(d[[s$var1]]), NULL, qc_lab(s$var1)), qc_plot_normality(as.numeric(d[[s$var2]]), NULL, qc_lab(s$var2)),
             list(qc_plot_scatter(d[[s$var1]], d[[s$var2]], qc_lab(s$var1), qc_lab(s$var2)))))
  }
  if (id == "chisq_independence") {
    d <- qc_complete(data, c(s$var1, s$var2))$data
    return(list(qc_plot_stacked(qc_grp(s$var1, d[[s$var1]]), qc_grp(s$var2, d[[s$var2]]), qc_lab(s$var1), qc_lab(s$var2))))
  }
  if (id %in% c("regression_linear", "regression_logistic", "regression_hierarchical")) {
    allp <- c(s$controls, s$predictors)
    d <- qc_complete(data, c(s$outcome, allp))$data
    for (v in allp) if (!is.numeric(d[[v]])) d[[v]] <- factor(d[[v]])
    X <- stats::model.matrix(stats::reformulate(sprintf("`%s`", allp)), d)[, -1, drop = FALSE]
    colnames(X) <- sapply(gsub("`", "", colnames(X)), qc_lab)
    vif <- if (ncol(X) > 1) sapply(seq_len(ncol(X)), function(j) 1 / (1 - summary(stats::lm(X[, j] ~ X[, -j]))$r.squared)) else 1
    names(vif) <- colnames(X)
    figs <- if (ncol(X) > 1) qc_plot_vif(vif, X) else list()
    if (id %in% c("regression_linear", "regression_hierarchical")) {
      m <- stats::lm(stats::reformulate(sprintf("`%s`", allp), response = sprintf("`%s`", s$outcome)), data = d)
      figs <- c(qc_plot_normality(stats::residuals(m), NULL, "residuals"), qc_plot_residuals(m), figs)
    }
    return(figs)
  }
  if (id == "mediation") {
    d <- qc_complete(data, c(s$var1, s$mediator, s$outcome))$data
    m <- stats::lm(stats::reformulate(sprintf("`%s`", c(s$var1, s$mediator)), response = sprintf("`%s`", s$outcome)), data = d)
    return(c(qc_plot_normality(stats::residuals(m), NULL, "residuals of the outcome model"), qc_plot_residuals(m)))
  }
  if (id == "reliability_alpha") {
    d <- qc_complete(data, s$items)$data
    return(list(qc_plot_hist_grid(d, s$items)))
  }
  list()
}

qc_result_figures <- function(data, spec, screen, result) {
  id <- screen$decision$test_id; s <- if (!is.null(screen$rerouted)) screen$rerouted else spec
  if (id %in% c("ttest_independent", "anova_oneway")) {
    d <- qc_complete(data, c(s$outcome, s$group))$data
    return(list(qc_plot_means_ci(as.numeric(d[[s$outcome]]), qc_grp(s$group, d[[s$group]]), qc_lab(s$outcome), qc_lab(s$group))))
  }
  if (id %in% c("mann_whitney", "kruskal_wallis")) {
    d <- qc_complete(data, c(s$outcome, s$group))$data
    f <- qc_plot_box(as.numeric(d[[s$outcome]]), qc_grp(s$group, d[[s$group]]), qc_lab(s$outcome), qc_lab(s$group))
    f$caption <- sprintf("Distribution of %s by %s (boxplots with medians and individual cases); the rank-based test compares these distributions.", qc_lab(s$outcome), qc_lab(s$group))
    return(list(f))
  }
  if (id %in% c("ttest_paired", "wilcoxon_signed")) {
    d <- qc_complete(data, c(s$var1, s$var2))$data
    return(list(qc_plot_paired(d[[s$var1]], d[[s$var2]], qc_lab(s$var1), qc_lab(s$var2))))
  }
  if (id %in% c("correlation_pearson", "correlation_spearman")) {
    d <- qc_complete(data, c(s$var1, s$var2))$data
    return(list(qc_plot_scatter(d[[s$var1]], d[[s$var2]], qc_lab(s$var1), qc_lab(s$var2))))
  }
  if (id == "chisq_independence") {
    d <- qc_complete(data, c(s$var1, s$var2))$data
    return(list(qc_plot_stacked(qc_grp(s$var1, d[[s$var1]]), qc_grp(s$var2, d[[s$var2]]), qc_lab(s$var1), qc_lab(s$var2))))
  }
  if (id %in% c("regression_linear", "regression_hierarchical")) return(list(qc_plot_coef(result$coefficients, qc_lab(s$outcome))))
  if (id == "regression_logistic") return(list(qc_plot_or(result$coefficients, qc_lab(s$outcome))))
  if (id == "reliability_alpha") return(list(qc_plot_items(result$items)))
  if (id == "mediation") return(list(qc_plot_mediation(result, s), qc_plot_boot(result)))
  list()
}

# Path diagram of a simple mediation model with the estimated paths
qc_plot_mediation <- function(r, s) {
  pv <- function(p) if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
  pt <- r$paths; lab <- function(i, name) sprintf("%s = %s%s", name, qc_fmt(pt$b[i]), pv(pt$p[i]))
  wrap <- function(x) paste(strwrap(qc_cap1(x), 22), collapse = "\n")
  nodes <- data.frame(x = c(0, 1, 2), y = c(0, 1, 0), lab = c(wrap(qc_lab(s$var1)), wrap(qc_lab(s$mediator)), wrap(qc_lab(s$outcome))))
  arr <- ggplot2::arrow(length = ggplot2::unit(.25, "cm"), type = "closed")
  g <- ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes(x = .18, y = .12, xend = .82, yend = .88), arrow = arr, colour = qc_col$accent, linewidth = .8) +
    ggplot2::geom_segment(ggplot2::aes(x = 1.18, y = .88, xend = 1.82, yend = .12), arrow = arr, colour = qc_col$accent, linewidth = .8) +
    ggplot2::geom_segment(ggplot2::aes(x = .3, y = 0, xend = 1.7, yend = 0), arrow = arr, colour = qc_col$accent2, linewidth = .8) +
    ggplot2::geom_label(data = nodes, ggplot2::aes(x, y, label = lab), size = 3.4, label.padding = ggplot2::unit(.45, "lines"), fill = "#f2f7ff", colour = qc_col$ink) +
    ggplot2::annotate("text", x = .38, y = .58, label = lab(1, "a"), size = 3.4, hjust = 1, colour = qc_col$ink) +
    ggplot2::annotate("text", x = 1.62, y = .58, label = lab(2, "b"), size = 3.4, hjust = 0, colour = qc_col$ink) +
    ggplot2::annotate("text", x = 1, y = -.14, label = sprintf("%s (total c = %s%s)", lab(3, "c'"), qc_fmt(pt$b[4]), pv(pt$p[4])), size = 3.4, colour = qc_col$ink) +
    ggplot2::annotate("text", x = 1, y = 1.3, label = sprintf("Indirect effect ab = %s, 95%% CI [%s, %s]", qc_fmt(r$ab, 3), qc_fmt(r$ab_ci[1], 3), qc_fmt(r$ab_ci[2], 3)),
                      size = 3.6, fontface = "bold", colour = if (r$excludes_zero) qc_col$accent else qc_col$muted) +
    ggplot2::coord_cartesian(xlim = c(-.35, 2.35), ylim = c(-.3, 1.4)) + ggplot2::theme_void() +
    ggplot2::labs(title = "Mediation model")
  qc_fig(g, "Mediation model with unstandardised path coefficients (* p < .05, ** p < .01, *** p < .001). The indirect effect is a x b; c' is the direct effect that remains when the mediator is included.", 6.5, 3.6)
}

qc_plot_boot <- function(r) {
  set.seed(2026)
  g <- ggplot2::ggplot(data.frame(ab = r$ab), ggplot2::aes(x = ab)) +
    ggplot2::geom_vline(xintercept = 0, colour = qc_col$neg, linetype = "dashed") +
    ggplot2::annotate("rect", xmin = r$ab_ci[1], xmax = r$ab_ci[2], ymin = -Inf, ymax = Inf, fill = qc_col$accent, alpha = .12) +
    ggplot2::geom_point(ggplot2::aes(y = 0), colour = qc_col$accent, size = 3) +
    ggplot2::geom_errorbarh(ggplot2::aes(y = 0, xmin = r$ab_ci[1], xmax = r$ab_ci[2]), height = .2, colour = qc_col$accent) +
    ggplot2::scale_y_continuous(NULL, breaks = NULL, limits = c(-1, 1)) +
    ggplot2::labs(title = "Indirect effect with 95% bootstrap interval", x = "Indirect effect (a x b)") + qc_theme()
  qc_fig(g, "Estimated indirect effect (dot) and its 95% bootstrap confidence interval (bar and shaded area). If the interval does not cross the dashed zero line, the indirect effect is statistically significant.", 6.5, 2.4)
}

qc_save_figures <- function(figs, dir, prefix, start = 1) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(figs)) {
    f <- file.path(dir, sprintf("%s_figure%02d.png", prefix, start + i - 1))
    suppressWarnings(ggplot2::ggsave(f, figs[[i]]$plot, width = figs[[i]]$width, height = figs[[i]]$height, dpi = 300, bg = "white"))
    figs[[i]]$file <- f
  }
  figs
}
