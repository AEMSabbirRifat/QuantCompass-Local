# v0.4: general hypothesis understanding, moderation analysis, before/after comparisons, coded categories in models.
library(testthat)
source(file.path(getOption("qc.root", "."), "validation", "corpus", "hypotheses.R"))

vdir4 <- file.path(tempdir(), "qc_v04"); dir.create(vdir4, showWarnings = FALSE)
run4 <- function(test_id, params, df, file = paste0(test_id, ".csv")) {
  path <- file.path(vdir4, file); utils::write.csv(df, path, row.names = FALSE)
  qc_analyse(path, NULL, list(test_id = test_id, params = params))$result
}

test_that("hypothesis library: wording is understood (type, variables, direction)", {
  r <- qc_hyp_corpus_report()
  expect_gte(nrow(r), 90)
  expect_gte(mean(r$all_ok), .95)
  expect_gte(mean(r$family_ok), .95)
  expect_gte(mean(r$vars_ok), .95)
  expect_gte(mean(r$direction_ok, na.rm = TRUE), .95)
  expect_gte(mean(r$higher_ok, na.rm = TRUE), .95)
  for (f in c("correlation", "group_diff", "paired", "categorical", "regression", "mediation", "moderation"))
    expect_gte(mean(r$all_ok[grepl(f, r$expected)]), .9, label = f)
})

test_that("moderation (numeric W) matches lm() with centred terms, and the simple slopes are right", {
  set.seed(3); n <- 300; x <- rnorm(n); w <- rnorm(n, 5, 2); y <- 1 + .5 * x + .2 * w - .3 * x * (w - 5) + rnorm(n)
  df <- data.frame(X = x, W = w, Y = y)
  r <- run4("moderation", list(VAR1 = "X", MODERATOR = "W", OUTCOME = "Y", ROBUST = "FALSE"), df)
  xc <- x - mean(x); wc <- w - mean(w); m <- lm(y ~ xc * wc); cf <- summary(m)$coefficients
  expect_equal(r$b_int, unname(cf["xc:wc", 1]), tolerance = 1e-8)
  expect_equal(r$p, unname(cf["xc:wc", 4]), tolerance = 1e-8)        # one-df F-change p = t-test p
  expect_equal(r$delta_r2, summary(m)$r.squared - summary(lm(y ~ xc + wc))$r.squared, tolerance = 1e-10)
  hi <- r$slopes[r$slopes$level == "high", ]; w_hi <- sd(w)
  expect_equal(hi$slope, unname(coef(m)["xc"] + coef(m)["xc:wc"] * w_hi), tolerance = 1e-8)
  # the simple slope at +1 SD equals the slope of xc after re-centring W at +1 SD (standard check)
  m2 <- lm(y ~ xc * I(wc - w_hi)); expect_equal(hi$se, unname(summary(m2)$coefficients["xc", 2]), tolerance = 1e-8)
  expect_lt(r$p, .001); expect_true(abs(r$slopes$slope[r$slopes$level == "low"]) > abs(hi$slope))
})

test_that("moderation with a robust test and with a categorical moderator", {
  set.seed(4); n <- 240; x <- rnorm(n); g <- sample(1:3, n, TRUE); y <- x * c(.1, .5, .9)[g] + rnorm(n, sd = ifelse(g == 3, 2, 1))
  df <- data.frame(X = x, G = c("a", "b", "c")[g], Y = y)   # text categories (labelled codes are turned into factors by the run)
  r <- run4("moderation", list(VAR1 = "X", MODERATOR = "G", OUTCOME = "Y", ROBUST = "TRUE"), df, "mod_cat.csv")
  expect_true(r$w_categorical); expect_equal(nrow(r$slopes), 3); expect_equal(r$df1, 2)
  xc <- x - mean(x); m <- lm(y ~ xc * factor(g))
  expect_equal(r$slopes$slope, unname(c(coef(m)["xc"], coef(m)["xc"] + coef(m)["xc:factor(g)2"], coef(m)["xc"] + coef(m)["xc:factor(g)3"])), tolerance = 1e-8)
  # robust Wald test of the two interaction terms, computed independently
  X <- model.matrix(m); e <- resid(m); h <- hatvalues(m); XtXi <- solve(crossprod(X)); V <- XtXi %*% crossprod(X * (e / (1 - h))) %*% XtXi
  k <- grep(":", names(coef(m))); Fw <- as.numeric(t(coef(m)[k]) %*% solve(V[k, k]) %*% coef(m)[k]) / 2
  expect_equal(r$F_int, Fw, tolerance = 1e-8)
})

test_that("moderation decision checks the expected pattern", {
  pair <- list(lab1 = "H1", lab0 = "H01", h1 = "x", expect_text = "stronger at low W", stronger = list(at = "low"))
  sl <- data.frame(at = c("Low (-1 SD)", "Mean", "High (+1 SD)"), level = c("low", "mean", "high"), slope = c(-.2, -.5, -.9), w_value = 1:3)
  d <- qc_decision_core("moderation", list(p = .01, slopes = sl, w_categorical = FALSE), pair)
  expect_false(d$rejected); expect_match(d$short, "opposite pattern")
  sl$slope <- rev(sl$slope); d <- qc_decision_core("moderation", list(p = .01, slopes = sl, w_categorical = FALSE), pair)
  expect_true(d$rejected)
  expect_false(qc_decision_core("moderation", list(p = .2, slopes = sl, w_categorical = FALSE), pair)$rejected)
})

test_that("before/after: pairs found from the labels, direction checked in the decision", {
  s <- qc_hyp_study("students"); cat0 <- qc_catalog(s$data, s$labels, s$levels)
  tp <- qc_time_pairs(cat0); expect_equal(length(tp), 1); expect_equal(c(tp[[1]]$early, tp[[1]]$late), c("ANX_PRE", "ANX_POST"))
  b <- qc_dissect("H1", "Anxiety is lower after the workshop than before it.", s$data, cat0)
  sp <- b$parts[[1]]$spec; expect_equal(sp$goal, "paired_difference"); expect_equal(sp$expect_higher, "ANX_PRE")
  assign("qc_labels", s$labels, envir = globalenv())
  pair <- qc_hypothesis_pair(sp, s$data, "H1"); expect_equal(pair$expect_sign, 1)
  d <- qc_decision_core("ttest_paired", list(p = .001, mean_diff = -.4), pair); expect_false(d$rejected); expect_match(d$short, "opposite")
  expect_true(qc_decision_core("ttest_paired", list(p = .001, mean_diff = .4), pair)$rejected)
  assign("qc_labels", list(), envir = globalenv())
})

test_that("two category variables become an association; unclear sentences get a low-confidence fallback", {
  s <- qc_hyp_study("work"); cat0 <- qc_catalog(s$data, s$labels, s$levels)
  b <- qc_dissect("H1", "Department and contract type are related.", s$data, cat0)
  expect_equal(b$parts[[1]]$spec$goal, "association")
  b2 <- qc_dissect("H2", "Workload ~ turnover intention, check this.", s$data, cat0)
  expect_true(length(b2$parts) == 1 && setequal(qc_spec_vars(b2$parts[[1]]$spec), c("WL", "TI")))
  expect_true(b2$parts[[1]]$confidence_label %in% c("medium", "low"))
  expect_equal(qc_dissect("H3", "Workload is negatively related to job satisfaction.", s$data, cat0)$parts[[1]]$confidence_label, "high")
})

test_that("free-text 'please specify' fields stay text, and nominal codes are categories", {
  d <- data.frame(G = sample(1:3, 50, TRUE), L = sample(1:5, 50, TRUE))
  cat0 <- qc_catalog(d, list(), list(G = c(`1` = "Male", `2` = "Female", `3` = "Diverse"), L = c(`1` = "Never", `2` = "Rarely", `3` = "Sometimes", `4` = "Often", `5` = "Always")))
  expect_equal(cat0$kind[cat0$variable == "G"], "categorical"); expect_equal(cat0$kind[cat0$variable == "L"], "ordinal")
})
