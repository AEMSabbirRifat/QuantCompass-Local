# Validation of v0.3: import, codebook, preparation, rule checks, one-sided tests, direction checks,
# mediation and hypothesis breakdown. Synthetic data only.
library(testthat)
vdir <- file.path(tempdir(), "qc_v03"); dir.create(vdir, showWarnings = FALSE)
set.seed(2026)

# A small SoSci-style survey: UTF-16LE, tab-separated, second row = question texts, -9 = not answered
make_survey <- function(n = 60) {
  d <- data.frame(CASE = 100 + seq_len(n), FINISHED = c(rep(1, n - 5), rep(0, 5)), SR01 = c(rep(1, n - 8), rep(2, 8)),
                  JB01 = sample(1:3, n, TRUE), JB02 = sample(1:2, n, TRUE), AT01_01 = sample(c(4, 4, 4, 2), n, TRUE),
                  WB01_01 = sample(1:5, n, TRUE), WB01_02 = sample(1:5, n, TRUE), WB01_03 = sample(1:5, n, TRUE), GR01 = sample(c(1:5, 6), n, TRUE))
  d$JB02[d$JB01 == 3] <- NA; d$JB02[1] <- 1; d$JB01[1] <- 3        # case 101: skip-logic break
  d$WB01_02[2] <- -9; d$TIME_RSI <- round(runif(n, .5, 2.5), 2)
  lab <- c("Case", "Finished", "Student?", "Job status", "Job type", "Attention", "Mood 1", "Mood 2", "Mood 3", "Grade", "Speed")
  txt <- paste(c(paste(names(d), collapse = "\t"), paste(lab, collapse = "\t"),
                 apply(d, 1, function(r) paste(ifelse(is.na(r), "", trimws(r)), collapse = "\t"))), collapse = "\r\n")
  p <- file.path(vdir, "survey.csv"); con <- file(p, "wb")
  writeBin(as.raw(c(0xff, 0xfe)), con); writeBin(iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]], con); close(con)
  cbtxt <- c("Page 01", "", "      1. Are you a student?", "", "   SR01 Student check", "     1 = Yes", "     2 = No", "     -9 = Not answered",
    "Page 02", "", "  PHP code", "  if (value('SR01') == 2) {", "    goToPage('end');", "  }", "",
    "   JB01 Job status", "     1 = Part-time", "     2 = Full-time", "     3 = Not working", "     -9 = Not answered",
    "Page 03", "", "  PHP code", "  if (value('JB01') == 3) {", "    goToPage('mood');", "  }", "",
    "   JB02 Job type", "     1 = Field-related", "     2 = Unskilled", "     -9 = Not answered",
    "Page 04", "mood", "", "   AT01_01 To show that you read carefully, please select 'Agree' here.",
    "   WB01_01 I felt cheerful.", "   WB01_02 I felt calm.", "   WB01_03 I felt active.",
    "     1 = Strongly disagree", "     2 = Disagree", "     3 = Neutral", "     4 = Agree", "     5 = Strongly agree", "     -9 = Not answered",
    "   GR01 Grade", "     1 = Excellent", "     2 = Good", "     3 = Satisfactory", "     4 = Pass", "     5 = Fail", "     6 = Not applicable / no grades yet", "     -9 = Not answered")
  cp <- file.path(vdir, "codebook.txt"); writeLines(cbtxt, cp)
  list(path = p, codebook = cp, data = d)
}
S <- make_survey()

test_that("UTF-16 SoSci exports are read with the label row removed, and the load code reproduces it", {
  f <- qc_text_format(S$path)
  expect_equal(f$encoding, "UTF-16LE"); expect_equal(f$sep, "\t"); expect_true(f$label_row)
  d <- qc_read(S$path); expect_equal(dim(d), dim(S$data)); expect_equal(d$CASE, S$data$CASE); expect_true(is.numeric(d$WB01_01))
  code <- qc_load_code(S$path); expect_true(any(grepl("qc_text_file", code)))
})

test_that("Text codebooks: variables, shared value blocks, missing codes, pages and skip rules", {
  d <- qc_read(S$path); cb <- qc_read_any_codebook(S$codebook, d)
  expect_true(all(c("SR01", "JB01", "JB02", "AT01_01", "WB01_01", "WB01_03", "GR01") %in% names(cb$vars)))
  expect_equal(unname(cb$vars$WB01_02$values[["4"]]), "Agree"); expect_equal(cb$vars$WB01_02$missing, "-9")
  expect_equal(length(cb$rules), 2); expect_true(cb$rules[[1]]$screen_out)
  expect_equal(cb$rules[[2]]$vars_skipped, "JB02")
  ch <- qc_codebook_check(cb, d); expect_equal(ch$status[ch$variable == "GR01"], "warn"); expect_match(ch$message[ch$variable == "GR01"], "Not applicable")
  expect_equal(ch$status[ch$variable == "CASE"], "ok")
  ap <- qc_codebook_apply(cb, d, accept_na = "GR01"); expect_true(any(grepl("c\\(-9, 6\\)", ap$code)))
})

test_that("Exclusions follow the survey's own rules, in order, and the flow is reported", {
  d <- qc_read(S$path); cb <- qc_read_any_codebook(S$codebook, d)
  ex <- qc_exclusion_suggestions(d, cb); ids <- vapply(ex, `[[`, "", "id")
  expect_equal(ids, c("unfinished", "screen_SR01", "attention_AT01_01", "speeders")); expect_false(ex[[4]]$on)
  prep <- list(codebook_code = qc_codebook_apply(cb, d)$code, exclusions = ex, id_var = qc_id_column(d))
  r <- qc_apply_prep(d, prep); k <- d$FINISHED == 1; k2 <- k & d$SR01 != 2; k3 <- k2 & d$AT01_01 %in% 4
  expect_equal(r$counts$n, c(nrow(d), sum(k), sum(k2), sum(k3)))
  expect_match(r$flow[2], "unfinished")
})

test_that("Rule-break check flags only answers on pages the survey skipped, and invalid codes", {
  d <- qc_read(S$path); cb <- qc_read_any_codebook(S$codebook, d)
  d$GR01[5] <- 9
  rb <- qc_rule_breaks(d, cb, "CASE")
  expect_true(101 %in% rb$id[rb$type == "skip logic"]); expect_true(105 %in% rb$id[rb$type == "invalid code"])
  expect_equal(sum(rb$type == "skip logic"), 1)
})

test_that("Scales, reverse coding, WHO-5 index and groups produce the right values and code", {
  d <- data.frame(a = c(1, 2, 3, 6), b = c(2, 2, 4, 6), c = c(6, 5, 3, 1), x = c(1, 1, 2, 3), s = c(1, 9, 3, 2))
  env <- new.env(); assign("data", d, env)
  eval(parse(text = qc_scale_code(list(name = "m", items = c("a", "b", "c"), reverse = "c", method = "mean", min = 1, max = 6))), env)
  expect_equal(get("data", env)$m, rowMeans(cbind(d$a, d$b, 7 - d$c)))
  eval(parse(text = qc_scale_code(list(name = "w", items = c("a", "b", "c"), reverse = character(), method = "who5", min = 1, max = 6))), env)
  expect_equal(get("data", env)$w, (rowSums(d[, 1:3]) - 3) * 4)
  cond <- qc_parse_condition_text("x is 1 and s < 5", d); expect_null(cond$error)
  g <- list(name = "grp", groups = list(list(label = "A", cond = cond), list(label = "B", cond = qc_parse_condition_text("x is 2,3", d))))
  eval(parse(text = qc_group_code(g)), env); expect_equal(as.character(get("data", env)$grp), c("A", NA, "B", "B"))
  expect_match(qc_parse_condition_text("zz is 1", d)$error, "not a column")
})

test_that("One-sided tests give half the two-sided p in the stated direction; CIs stay two-sided", {
  df <- data.frame(y = c(rnorm(40, 11), rnorm(40, 10)), g = rep(c("a", "b"), each = 40)); p <- file.path(vdir, "os.csv"); utils::write.csv(df, p, row.names = FALSE)
  ctx <- list(path = p, sheet = NULL, fixes = character(), labels = list(), level_labels = list(), out_dir = file.path(vdir, "out"))
  two <- qc_analyse(p, NULL, list(test_id = "ttest_independent", params = list(OUTCOME = "y", GROUP = "g")))$result
  one <- qc_analyse(p, NULL, list(test_id = "ttest_independent", params = list(OUTCOME = "y", GROUP = "g", ALTERNATIVE = "greater")))$result
  expect_equal(one$p, two$p / 2, tolerance = 1e-10); expect_equal(one$mean_diff_ci, two$mean_diff_ci)
  expect_match(one$apa, "one-sided")
  x <- rnorm(50); df2 <- data.frame(x = x, y = x + rnorm(50)); p2 <- file.path(vdir, "os2.csv"); utils::write.csv(df2, p2, row.names = FALSE)
  c2 <- qc_analyse(p2, NULL, list(test_id = "correlation_pearson", params = list(VAR1 = "x", VAR2 = "y", ALTERNATIVE = "greater")))$result
  expect_equal(c2$p, stats::cor.test(df2$x, df2$y, alternative = "greater")$p.value)
})

test_that("A significant result in the opposite direction does not support a directional hypothesis", {
  x <- rnorm(80); df <- data.frame(x = x, y = 2 * x + rnorm(80)); p <- file.path(vdir, "dir.csv"); utils::write.csv(df, p, row.names = FALSE)
  ctx <- list(path = p, sheet = NULL, fixes = character(), labels = list(), level_labels = list(), out_dir = file.path(vdir, "out"))
  r <- qc_run_hypothesis(list(id = "H9", text = "x is negatively related to y", spec = list(goal = "relationship", var1 = "x", var2 = "y", direction = "negative")), ctx)
  expect_false(r$decision$rejected); expect_match(r$decision$short, "opposite direction")
  r2 <- qc_run_hypothesis(list(id = "H8", text = "x is positively related to y", spec = list(goal = "relationship", var1 = "x", var2 = "y", direction = "positive")), ctx)
  expect_true(r2$decision$rejected)
})

test_that("Mediation: paths match lm(), the indirect effect is a*b, the bootstrap CI is reproducible", {
  n <- 200; x <- rnorm(n); m <- .5 * x + rnorm(n); y <- .4 * m + .1 * x + rnorm(n)
  p <- file.path(vdir, "med.csv"); utils::write.csv(data.frame(x, m, y), p, row.names = FALSE)
  r <- qc_analyse(p, NULL, list(test_id = "mediation", params = list(VAR1 = "x", MEDIATOR = "m", OUTCOME = "y")))$result
  a <- coef(lm(m ~ x))[["x"]]; b <- coef(lm(y ~ x + m))[["m"]]
  expect_equal(r$a, a); expect_equal(r$b, b); expect_equal(r$ab, a * b); expect_equal(r$c, coef(lm(y ~ x))[["x"]])
  expect_true(r$excludes_zero)
  r2 <- qc_analyse(p, NULL, list(test_id = "mediation", params = list(VAR1 = "x", MEDIATOR = "m", OUTCOME = "y")))$result
  expect_equal(r$ab_ci, r2$ab_ci)
  ctx <- list(path = p, sheet = NULL, fixes = character(), labels = list(), level_labels = list(), out_dir = file.path(vdir, "out"))
  h <- qc_run_hypothesis(list(id = "H7", text = "m mediates x and y", spec = list(goal = "mediation", var1 = "x", mediator = "m", outcome = "y")), ctx)
  expect_true(h$decision$rejected); expect_true(all(file.exists(h$files)))
})

test_that("Hypothesis breakdown: type, direction, compound split, groups and variables", {
  d <- data.frame(stress = rnorm(90), wellbeing = rnorm(90), debt = sample(1:2, 90, TRUE), concern = sample(1:5, 90, TRUE),
                  job = factor(sample(c("Field-related job", "Unskilled job", "Not working"), 90, TRUE), levels = c("Field-related job", "Unskilled job", "Not working")))
  labels <- list(stress = "financial stress", wellbeing = "mental well-being", debt = "student debt", concern = "concern about debt", job = "employment")
  cat0 <- qc_catalog(d, labels, list(debt = c("1" = "Yes", "2" = "No")))
  b <- qc_dissect("H1", "There is a negative association between financial constraint and well-being.", d, cat0)
  expect_equal(b$type, "relationship"); expect_equal(b$parts[[1]]$spec[c("var1", "var2", "direction")], list(var1 = "stress", var2 = "wellbeing", direction = "negative"))
  b <- qc_dissect("H2", "Students in field-related jobs have higher well-being than students in unskilled jobs.", d, cat0)
  expect_equal(b$parts[[1]]$spec$group, "job"); expect_equal(b$parts[[1]]$spec$compare, c("Field-related job", "Unskilled job"))
  expect_equal(b$parts[[1]]$spec$expect_higher, "Field-related job")
  b <- qc_dissect("H3", "Holding student debt and the concern about it are negatively correlated with well-being.", d, cat0)
  expect_equal(length(b$parts), 2); expect_equal(b$parts[[1]]$spec$goal, "difference"); expect_equal(b$parts[[1]]$spec$expect_higher, "2")
  expect_equal(b$parts[[2]]$spec$var1, "concern")
  b <- qc_dissect("H4", "Concern about debt mediates the relationship between stress and well-being.", d, cat0)
  expect_equal(b$parts[[1]]$spec[c("var1", "mediator", "outcome")], list(var1 = "stress", mediator = "concern", outcome = "wellbeing"))
  b <- qc_dissect("H5", "Something vague about life.", d, cat0); expect_equal(b$type, "unknown"); expect_true(length(b$questions) > 0)
})

test_that("Regression planner: suitable outcomes, predictor checks, sample-size rule", {
  n <- 120; d <- data.frame(score = rnorm(n), x1 = rnorm(n), x2 = rnorm(n), item_1 = sample(1:5, n, TRUE), item_2 = sample(1:5, n, TRUE),
                           country = sample(1:4, n, TRUE), filtered = c(rnorm(40), rep(NA, 80)), const = 1, bin = sample(0:1, n, TRUE))
  d$x3 <- d$x1 + rnorm(n, sd = .05)
  sc <- list(list(name = "score", items = c("item_1", "item_2")))
  cat0 <- qc_catalog(d, list(), list(country = c("1" = "Germany", "2" = "France", "3" = "India", "4" = "Brazil")), NULL, sc)
  oc <- qc_mlr_outcomes(d, cat0, sc)
  expect_equal(oc$status[oc$variable == "item_1"], "no"); expect_equal(oc$status[oc$variable == "x1"], "ok"); expect_equal(oc$status[oc$variable == "bin"], "warn")
  pr <- qc_mlr_predictors(d, cat0, "score", sc)
  expect_equal(pr$status[pr$variable == "item_1"], "no"); expect_equal(pr$status[pr$variable == "filtered"], "warn")
  expect_equal(pr$status[pr$variable == "country"], "warn"); expect_match(pr$reason[pr$variable == "country"], "without an order")
  expect_false("const" %in% pr$variable[pr$status == "ok"])
  ck <- qc_mlr_check(d, "score", c("x1", "x2")); expect_equal(ck$need, 66); expect_equal(ck$status, "ok"); expect_equal(ck$max_terms, floor((n - 50) / 8))
  ck2 <- qc_mlr_check(d, "score", c("x1", "x3")); expect_equal(ck2$status, "warn"); expect_match(ck2$verdict, "highly correlated")
})

test_that("Breakdown recognises prediction and reliability hypotheses", {
  d <- data.frame(stress = rnorm(90), wellbeing = rnorm(90), grade = rnorm(90), wb_1 = sample(1:5, 90, TRUE), wb_2 = sample(1:5, 90, TRUE), wb_3 = sample(1:5, 90, TRUE))
  labels <- list(stress = "financial stress", wellbeing = "mental well-being", grade = "academic performance")
  sc <- list(list(name = "wellbeing", label = "mental well-being", items = c("wb_1", "wb_2", "wb_3"), reverse = "wb_3", min = 1, max = 5))
  cat0 <- qc_catalog(d, labels, list(), NULL, sc); qc_dissect_scales$scales <- sc
  b <- qc_dissect("H5", "Financial stress and academic performance together predict well-being.", d, cat0)
  expect_equal(b$type, "prediction"); expect_equal(b$parts[[1]]$spec$outcome, "wellbeing"); expect_setequal(b$parts[[1]]$spec$predictors, c("stress", "grade"))
  b <- qc_dissect("H6", "Well-being is predicted by financial stress and academic performance.", d, cat0); expect_equal(b$parts[[1]]$spec$outcome, "wellbeing")
  b <- qc_dissect("H7", "The items of mental well-being form a reliable scale.", d, cat0)
  expect_equal(b$type, "reliability"); expect_equal(b$parts[[1]]$spec$items, c("wb_1", "wb_2", "wb_3")); expect_equal(b$parts[[1]]$spec$reverse, "wb_3")
  qc_dissect_scales$scales <- NULL
})
