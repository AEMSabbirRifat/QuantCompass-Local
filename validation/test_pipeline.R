# Validation of the assumption-first pipeline: decision rules, reports, charts, hypothesis sheet.
library(testthat)

pdir <- file.path(tempdir(), "qc_pipeline"); dir.create(pdir, showWarnings = FALSE)
write_data <- function(df, name) { p <- file.path(pdir, name); utils::write.csv(df, p, row.names = FALSE); p }
ctx_for <- function(path, ...) utils::modifyList(list(path = path, sheet = NULL, fixes = character(), labels = list(),
                                      level_labels = list(), out_dir = file.path(pdir, "out")), list(...))
set.seed(2026)

test_that("Normality rule follows Kim (2013) by sample size", {
  expect_match(qc_normality_check(rnorm(30), "x")$method, "Shapiro")
  expect_match(qc_normality_check(rnorm(150), "x")$method, "z-scores")
  expect_match(qc_normality_check(rnorm(400), "x")$method, "Absolute")
  expect_false(qc_normality_check(rexp(40)^2, "x")$met)
  expect_true(qc_normality_check(rnorm(400), "x")$met)
})

test_that("Group comparisons: parametric when assumptions hold, rank-based when they fail", {
  normal <- data.frame(y = c(rnorm(25, 10), rnorm(25, 11)), g = rep(c("a", "b"), each = 25))
  expect_equal(qc_screen(normal, list(goal = "difference", outcome = "y", group = "g"))$decision$test_id, "ttest_independent")
  skewed <- data.frame(y = c(rexp(20)^3, rexp(20)^3 + 1), g = rep(c("a", "b"), each = 20))
  expect_equal(qc_screen(skewed, list(goal = "difference", outcome = "y", group = "g"))$decision$test_id, "mann_whitney")
  likert <- data.frame(y = rep(1:5, 20), g = rep(c("a", "b"), each = 50))
  s <- qc_screen(likert, list(goal = "difference", outcome = "y", group = "g"))
  expect_equal(s$decision$test_id, "mann_whitney"); expect_match(s$decision$reason, "ordinal")
  three <- data.frame(y = c(rexp(15)^3, rexp(15)^3, rexp(15)^3), g = rep(c("a", "b", "c"), each = 15))
  expect_equal(qc_screen(three, list(goal = "difference", outcome = "y", group = "g"))$decision$test_id, "kruskal_wallis")
  big <- data.frame(y = c(runif(60), runif(60) + .2), g = rep(c("a", "b"), each = 60))   # non-normal but n >= 30, no outliers
  s <- qc_screen(big, list(goal = "difference", outcome = "y", group = "g"))
  expect_equal(s$decision$test_id, "ttest_independent")
})

test_that("Correlation: curved relationships go to Spearman, linear normal ones to Pearson", {
  x <- rnorm(80); lin <- data.frame(x = x, y = 2 * x + rnorm(80))
  expect_equal(qc_screen(lin, list(goal = "relationship", var1 = "x", var2 = "y"))$decision$test_id, "correlation_pearson")
  x <- runif(80, 0, 3); curved <- data.frame(x = x, y = exp(2 * x) + rnorm(80))
  expect_equal(qc_screen(curved, list(goal = "relationship", var1 = "x", var2 = "y"))$decision$test_id, "correlation_spearman")
})

test_that("Chi-square decision flags small expected counts", {
  small <- data.frame(a = rep(c("x", "y"), each = 6), b = c("p", "p", "p", "p", "q", "q", "p", "q", "q", "q", "q", "q"))
  expect_false(qc_screen(small, list(goal = "association", var1 = "a", var2 = "b"))$checks$met)
})

test_that("Regression switches to HC3 robust standard errors under heteroscedasticity", {
  x <- runif(200, 1, 10); het <- data.frame(x = x, z = rnorm(200), y = 2 * x + rnorm(200, sd = x^1.5))
  s <- qc_screen(het, list(goal = "prediction", outcome = "y", predictors = c("x", "z")))
  expect_true(s$robust)
  m <- lm(y ~ x + z, het); X <- model.matrix(m); e <- resid(m); h <- hatvalues(m)
  manual <- sqrt(diag(solve(t(X) %*% X) %*% t(X) %*% diag(e^2 / (1 - h)^2) %*% X %*% solve(t(X) %*% X)))
  expect_equal(unname(qc_hc3(m)), unname(manual))
  p <- write_data(het, "het.csv")
  r <- qc_run_hypothesis(list(id = "R1", text = "x and z predict y", spec = list(goal = "prediction", outcome = "y", predictors = c("x", "z"))), ctx_for(p))
  expect_match(r$paragraph, "HC3")
})

test_that("Full pipeline writes report (Word + HTML), script and charts for every test family", {
  df <- data.frame(id = 1:90, g2 = rep(c("a", "b"), 45), g3 = rep(c("a", "b", "c"), 30), y = rnorm(90, 50, 10),
                   x = rnorm(90), pre = rnorm(90, 10), item1 = sample(1:5, 90, TRUE), cat = sample(c("u", "v"), 90, TRUE))
  df$post <- df$pre + rnorm(90, .5); df$item2 <- pmin(5, pmax(1, df$item1 + sample(-1:1, 90, TRUE)))
  df$item3 <- pmin(5, pmax(1, df$item1 + sample(-1:1, 90, TRUE))); df$bin <- as.integer(df$x + rnorm(90) > 0)
  p <- write_data(df, "all.csv")
  specs <- list(list(goal = "difference", outcome = "y", group = "g2"), list(goal = "difference", outcome = "y", group = "g3"),
    list(goal = "paired_difference", var1 = "pre", var2 = "post"), list(goal = "relationship", var1 = "x", var2 = "y"),
    list(goal = "association", var1 = "g2", var2 = "cat"), list(goal = "prediction", outcome = "y", predictors = c("x", "g3")),
    list(goal = "prediction", outcome = "bin", predictors = c("x", "y")), list(goal = "reliability", items = c("item1", "item2", "item3")))
  for (i in seq_along(specs)) {
    r <- qc_run_hypothesis(list(id = paste0("T", i), text = "test", spec = specs[[i]]), ctx_for(p))
    expect_false(isTRUE(r$blocked), info = specs[[i]]$goal)
    expect_true(all(file.exists(r$files)), info = specs[[i]]$goal)
    expect_gt(length(r$figures), 0)
    expect_true(all(file.exists(sapply(r$figures, `[[`, "file"))))
    expect_match(r$paragraph, "assumptions were screened")
    expect_true(any(grepl("ASSUMPTION SCREENING", readLines(r$files[["script"]]))))
  }
})

test_that("User override of the automatic test choice is applied and recorded", {
  df <- data.frame(y = c(rnorm(25, 10), rnorm(25, 11)), g = rep(c("a", "b"), each = 25)); p <- write_data(df, "ov.csv")
  r <- qc_run_hypothesis(list(id = "O1", text = "y differs", spec = list(goal = "difference", outcome = "y", group = "g"), choice = "non-parametric"), ctx_for(p))
  expect_equal(r$test, "Mann-Whitney U test"); expect_match(r$reason, "the user chose")
})

test_that("Friendly names appear in sentences and APA text; data fixes are written into the script", {
  df <- data.frame(value = c(rnorm(30, 20), rnorm(30, 25)), river = rep(0:1, 30), age = c(rep(99, 3), sample(20:60, 57, TRUE)))
  p <- write_data(df, "lab.csv")
  ctx <- ctx_for(p, labels = list(value = "house value", river = "river location"),
                 level_labels = list(river = c("0" = "homes away from the river", "1" = "homes near the river")))
  ctx$fixes <- qc_profile(qc_read(p))$issues$fix[!is.na(qc_profile(qc_read(p))$issues$fix)]
  r <- qc_run_hypothesis(list(id = "L1", text = "x", spec = list(goal = "difference", outcome = "value", group = "river")), ctx)
  expect_match(r$sentence, "house value differs between homes away from the river and homes near the river")
  expect_match(r$paragraph, "homes near the river \\(M = ")
  expect_true(any(grepl('data[["age"]][data[["age"]] %in% 99] <- NA', readLines(r$files[["script"]]), fixed = TRUE)))
})

test_that("Whole-dataset description and summary report are produced", {
  p <- write_data(data.frame(a = rnorm(50), b = rnorm(50), grp = rep(c("x", "y"), 25)), "desc.csv")
  d <- qc_describe_dataset(ctx_for(p), by = "grp")
  expect_true(all(file.exists(d$files))); expect_match(d$paragraph, "50 cases")
  expect_equal(length(d$tables), 3)
  s <- qc_summary_report(list(), ctx_for(p), d); expect_true(all(file.exists(s)))
})

test_that("Hypothesis sheet is parsed and column typos are caught with suggestions", {
  data <- data.frame(medv = rnorm(10), chas = rep(0:1, 5), rm = rnorm(10))
  hs <- data.frame(ID = c("H1", "H2"), `Hypothesis (your words)` = c("a", "b"), `Question type` = c("Compare groups", "Predict an outcome"),
    `Outcome or first variable` = c("medv", "medv"), `Group or second variable` = c("chas", NA), `Predictors or items (comma-separated)` = c(NA, "rn, chas"),
    check.names = FALSE)
  p <- file.path(pdir, "hyp.csv"); utils::write.csv(hs, p, row.names = FALSE)
  out <- qc_read_hypothesis_sheet(p, data)
  expect_equal(length(out$hypotheses), 1)
  expect_match(out$problems, "did you mean 'rm'")
})

test_that("Every report states the research and null hypotheses and the decision", {
  df <- data.frame(y = c(rnorm(30, 10), rnorm(30, 14)), g = rep(c("a", "b"), each = 30), z = rnorm(60)); p <- write_data(df, "hyp_dec.csv")
  r <- qc_run_hypothesis(list(id = "H2", text = "y differs by g", spec = list(goal = "difference", outcome = "y", group = "g")), ctx_for(p))
  expect_equal(c(r$lab1, r$lab0), c("H2", "H02"))
  expect_match(r$h0, "does not differ"); expect_true(r$decision$rejected); expect_match(r$decision$short, "H02 rejected")
  expect_match(r$paragraph, "H02 is rejected")
  txt <- officer::docx_summary(officer::read_docx(r$files[["docx"]]))$text
  expect_true(any(grepl("Null hypothesis (H02)", txt, fixed = TRUE))); expect_true(any(grepl("^Decision: H02 is rejected", txt)))
  r2 <- qc_run_hypothesis(list(id = "H3", text = "z related to y", spec = list(goal = "relationship", var1 = "z", var2 = "y")), ctx_for(p))
  expect_true(is.logical(r2$decision$rejected))
})
