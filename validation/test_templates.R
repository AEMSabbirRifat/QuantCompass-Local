# Validation suite: every template must reproduce known reference results.
# Reference values come from the R documentation examples (?t.test, ?lm, ?chisq.test, ...)
# and from direct calls to R's own reference implementations.
# Run:  Rscript validation/run_validation.R

library(testthat)

vdir <- file.path(tempdir(), "qc_validation"); dir.create(vdir, showWarnings = FALSE)
run_tpl <- function(test_id, params, df, file = paste0(test_id, ".csv")) {
  path <- file.path(vdir, file)
  utils::write.csv(df, path, row.names = FALSE)
  out <- qc_analyse(path, NULL, list(test_id = test_id, params = params))
  invisible(out$result)
}
sleep_wide <- data.frame(id = 1:10, drug1 = sleep$extra[sleep$group == 1], drug2 = sleep$extra[sleep$group == 2])

test_that("Welch t-test matches ?t.test reference (sleep data)", {
  r <- run_tpl("ttest_independent", list(OUTCOME = "extra", GROUP = "group", ALTERNATIVE = "two.sided"), sleep)
  expect_equal(r$t, -1.8608, tolerance = 1e-4)
  expect_equal(r$df, 17.776, tolerance = 1e-4)
  expect_equal(r$p, 0.07939, tolerance = 1e-3)
  y1 <- sleep$extra[sleep$group == 1]; y2 <- sleep$extra[sleep$group == 2]
  sp <- sqrt((9 * var(y1) + 9 * var(y2)) / 18)
  expect_equal(r$cohen_d, (mean(y1) - mean(y2)) / sp)
  expect_match(r$apa, "t\\(17.78\\) = -1.86, p = .079")
})

test_that("Paired t-test matches ?t.test reference (sleep data)", {
  r <- run_tpl("ttest_paired", list(VAR1 = "drug1", VAR2 = "drug2", ALTERNATIVE = "two.sided"), sleep_wide)
  expect_equal(r$t, -4.0621, tolerance = 1e-4)
  expect_equal(r$df, 9)
  expect_equal(r$p, 0.002833, tolerance = 1e-3)
})

test_that("Mann-Whitney U equals stats::wilcox.test", {
  r <- run_tpl("mann_whitney", list(OUTCOME = "extra", GROUP = "group", ALTERNATIVE = "two.sided"), sleep)
  ref <- suppressWarnings(wilcox.test(extra ~ group, data = sleep, exact = FALSE))
  expect_equal(r$U, unname(ref$statistic)); expect_equal(r$p, ref$p.value)
  expect_true(r$rank_biserial < 0)   # group 1 sleeps less extra than group 2
})

test_that("Wilcoxon signed-rank equals stats::wilcox.test", {
  r <- run_tpl("wilcoxon_signed", list(VAR1 = "drug1", VAR2 = "drug2", ALTERNATIVE = "two.sided"), sleep_wide)
  ref <- suppressWarnings(wilcox.test(sleep_wide$drug1, sleep_wide$drug2, paired = TRUE, exact = FALSE))
  expect_equal(r$V, unname(ref$statistic)); expect_equal(r$p, ref$p.value)
  expect_equal(r$rank_biserial, -1)  # every non-zero difference is negative
})

test_that("One-way ANOVA matches ?chickwts / ?lm reference", {
  r <- run_tpl("anova_oneway", list(OUTCOME = "weight", GROUP = "feed"), chickwts)
  expect_equal(r$classic$F, 15.365, tolerance = 1e-3)
  expect_equal(r$classic$df1, 5); expect_equal(r$classic$df2, 65)
  expect_equal(r$classic$p, 5.936e-10, tolerance = 1e-2)
  ref <- oneway.test(weight ~ feed, chickwts)
  expect_equal(r$welch$F, unname(ref$statistic)); expect_equal(r$welch$p, ref$p.value)
  expect_true(all(r$posthoc$p_adj >= 0 & r$posthoc$p_adj <= 1))
})

test_that("Games-Howell with 2 groups equals the Welch t-test p-value (identity check)", {
  two <- subset(chickwts, feed %in% c("casein", "horsebean")); two$feed <- droplevels(two$feed)
  three <- subset(chickwts, feed %in% c("casein", "horsebean", "linseed"))
  r <- run_tpl("anova_oneway", list(OUTCOME = "weight", GROUP = "feed"), droplevels(three), "gh.csv")
  row <- r$posthoc[r$posthoc$comparison == "horsebean - casein", ]
  ref <- t.test(weight ~ feed, two)
  expect_equal(abs(row$t), abs(unname(ref$statistic)), tolerance = 1e-8)
  expect_equal(row$df, unname(ref$parameter), tolerance = 1e-8)
})

test_that("Kruskal-Wallis matches ?kruskal.test reference (airquality)", {
  r <- run_tpl("kruskal_wallis", list(OUTCOME = "Ozone", GROUP = "Month"), airquality)
  expect_equal(r$H, 29.267, tolerance = 1e-4)
  expect_equal(r$df, 4)
  expect_equal(r$p, 6.901e-06, tolerance = 1e-3)
  expect_equal(r$n_excluded, 37)   # rows with missing Ozone are reported, not silently dropped
})

test_that("Pearson correlation matches reference (mtcars mpg ~ wt)", {
  r <- run_tpl("correlation_pearson", list(VAR1 = "mpg", VAR2 = "wt"), mtcars)
  expect_equal(r$r, -0.8676594, tolerance = 1e-6)
  expect_equal(r$t, -9.559, tolerance = 1e-3)
  expect_equal(r$df, 30)
})

test_that("Spearman correlation equals stats::cor.test", {
  r <- run_tpl("correlation_spearman", list(VAR1 = "mpg", VAR2 = "wt"), mtcars)
  ref <- suppressWarnings(cor.test(mtcars$mpg, mtcars$wt, method = "spearman", exact = FALSE))
  expect_equal(r$rho, unname(ref$estimate)); expect_equal(r$p, ref$p.value)
  expect_true(r$rho_ci[1] < r$rho && r$rho < r$rho_ci[2])
})

test_that("Chi-square matches ?chisq.test reference (party x gender table)", {
  M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
  dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat", "Independent", "Republican"))
  long <- as.data.frame(M); long <- long[rep(seq_len(nrow(long)), long$Freq), c("gender", "party")]
  r <- run_tpl("chisq_independence", list(VAR1 = "gender", VAR2 = "party"), long)
  expect_equal(r$chisq, 30.07, tolerance = 1e-3)
  expect_equal(r$df, 2); expect_equal(r$p, 2.954e-07, tolerance = 1e-2)
  expect_false(r$used_fisher)
})

test_that("Chi-square switches to Fisher's exact test when expected counts are small", {
  small <- data.frame(a = c(rep("x", 6), rep("y", 6)), b = c("p", "p", "p", "p", "q", "q", "p", "q", "q", "q", "q", "q"))
  r <- run_tpl("chisq_independence", list(VAR1 = "a", VAR2 = "b"), small, "small.csv")
  expect_true(r$used_fisher)
  expect_equal(r$p, fisher.test(table(small$a, small$b))$p.value)
})

test_that("Linear regression matches ?lm reference (mtcars mpg ~ wt)", {
  r <- run_tpl("regression_linear", list(OUTCOME = "mpg", PREDICTORS_VEC = "wt"), mtcars)
  expect_equal(r$coefficients$b, c(37.2851, -5.3445), tolerance = 1e-4)
  expect_equal(r$r2, 0.7528, tolerance = 1e-3)
  expect_equal(r$coefficients$beta[2], -0.8676594, tolerance = 1e-6)   # simple regression: beta = r
})

test_that("Multiple regression equals stats::lm with two predictors", {
  r <- run_tpl("regression_linear", list(OUTCOME = "mpg", PREDICTORS_VEC = c("wt", "hp")), mtcars, "mlr.csv")
  ref <- lm(mpg ~ wt + hp, mtcars)
  expect_equal(r$coefficients$b, unname(coef(ref)))
  expect_equal(r$adj_r2, summary(ref)$adj.r.squared)
  expect_equal(length(r$assumptions$vif), 2)
})

test_that("Logistic regression equals stats::glm (mtcars am ~ wt)", {
  r <- run_tpl("regression_logistic", list(OUTCOME = "am", POSITIVE = "1", PREDICTORS_VEC = "wt"), mtcars)
  ref <- glm(am ~ wt, binomial, mtcars)
  expect_equal(r$coefficients$b, unname(coef(ref)))
  expect_equal(r$coefficients$b, c(12.040, -4.024), tolerance = 1e-3)
  expect_equal(r$lr_chisq, ref$null.deviance - ref$deviance, tolerance = 1e-8)
})

test_that("Cronbach's alpha equals the covariance-matrix formula and handles reverse items", {
  set.seed(1); n <- 200; f <- rnorm(n)
  items <- sapply(1:4, function(i) pmin(5, pmax(1, round(3 + f + rnorm(n, sd = .8)))))
  df <- as.data.frame(items); names(df) <- paste0("js_", 1:4)
  C <- cov(df); ref_alpha <- 4 / 3 * (1 - sum(diag(C)) / sum(C))
  r <- run_tpl("reliability_alpha", list(ITEMS_VEC = names(df), REVERSE_VEC = character(),
                                         SCALE_MIN = 1, SCALE_MAX = 5), df)
  expect_equal(r$alpha, ref_alpha)
  df_rev <- df; df_rev$js_3 <- 6 - df_rev$js_3   # simulate a negatively worded item
  r2 <- run_tpl("reliability_alpha", list(ITEMS_VEC = names(df), REVERSE_VEC = "js_3",
                                          SCALE_MIN = 1, SCALE_MAX = 5), df_rev, "rev.csv")
  expect_equal(r2$alpha, ref_alpha)
})

test_that("Descriptives match base R", {
  r <- run_tpl("descriptives", list(VARIABLES_VEC = c("mpg", "cyl")), mtcars)
  expect_equal(r$numeric$mean[1], 20.09062, tolerance = 1e-6)
  expect_equal(r$numeric$sd[1], 6.026948, tolerance = 1e-6)
})

test_that("The returned script is the exact script that ran and reproduces the result", {
  path <- file.path(vdir, "sleep.csv"); utils::write.csv(sleep, path, row.names = FALSE)
  opt <- list(test_id = "ttest_independent", params = list(OUTCOME = "extra", GROUP = "group", ALTERNATIVE = "two.sided"))
  out <- qc_analyse(path, NULL, opt)
  rerun <- new.env(); old <- setwd(vdir)
  writeLines(out$script, "rerun.R"); source("rerun.R", local = rerun); setwd(old)
  expect_identical(out$script, readLines(out$script_path))
  expect_equal(rerun$result$t, out$result$t)
  expect_false(any(grepl("\\{\\{", out$script)))
})

# ---- Rules engine ----
test_that("Rules engine routes designs to valid tests", {
  s <- data.frame(score = c(rnorm(40, 3), rnorm(40, 3.4)), gender = rep(c("f", "m"), each = 40),
                  dept = rep(c("a", "b", "c", "d"), 20), yes = rep(c("y", "n"), 40))
  o <- qc_suggest(s, list(goal = "difference", outcome = "score", group = "gender"))
  expect_true(all(c("ttest_independent", "mann_whitney") %in% sapply(o, `[[`, "test_id")))
  o <- qc_suggest(s, list(goal = "difference", outcome = "score", group = "dept"))
  expect_true("anova_oneway" %in% sapply(o, `[[`, "test_id"))
  o <- qc_suggest(s, list(goal = "difference", outcome = "yes", group = "dept"))
  expect_equal(o[[1]]$test_id, "chisq_independence")          # categorical outcome is re-routed
  o <- qc_suggest(s, list(goal = "relationship", var1 = "score", var2 = "gender"))
  expect_equal(o[[1]]$params$GROUP, "gender")                 # numeric x binary -> group difference
  o <- qc_suggest(s, list(goal = "reliability", items = "score"))
  expect_equal(o[[1]]$status, "blocked")
})

test_that("Rules engine warns about small samples and too many predictors", {
  tiny <- data.frame(y = rnorm(12), g = rep(c("a", "b"), 6), x1 = rnorm(12), x2 = rnorm(12))
  o <- qc_suggest(tiny, list(goal = "difference", outcome = "y", group = "g"))
  expect_equal(o[[1]]$status, "warn")
  o <- qc_suggest(tiny, list(goal = "prediction", outcome = "y", predictors = c("x1", "x2")))
  expect_equal(o[[1]]$status, "warn")
  wide <- as.data.frame(matrix(rnorm(30 * 40), 30)); names(wide) <- c("y", paste0("x", 1:39))
  o <- qc_suggest(wide, list(goal = "prediction", outcome = "y", predictors = paste0("x", 1:39)))
  expect_equal(o[[1]]$status, "blocked")                      # 30 rows, 39 predictors
})

test_that("Rules engine recommends rank-based tests for ordinal items", {
  s <- data.frame(item = rep(1:5, 8), g = rep(c("a", "b"), each = 20))
  o <- qc_suggest(s, list(goal = "difference", outcome = "item", group = "g"))
  expect_equal(o[[1]]$test_id, "mann_whitney")
})

# ---- Profiler ----
test_that("Profiler flags decimal commas, missing-value codes and item batteries", {
  p <- qc_profile(data.frame(id = 1:8, wage = c("3,5", "4,0", "2,5", "3", "4,5", "5,0", "3,5", "2"),
    age = c(23, 25, 99, 31, 28, 99, 26, 30), js_1 = 1:8 %% 5 + 1, js_2 = 1:8 %% 5 + 1, js_3 = 1:8 %% 4 + 1,
    stringsAsFactors = FALSE))
  expect_equal(p$verdict, "needs_fixing")
  expect_true(any(grepl("decimal comma", p$issues$message)))
  expect_true(any(p$issues$column == "age" & grepl("missing-value code", p$issues$message)))
  expect_equal(p$batteries$js, c("js_1", "js_2", "js_3"))
  expect_equal(p$columns$type[p$columns$column == "id"], "id")
})
