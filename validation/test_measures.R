# v0.5: variables recommended from the hypothesis - multi-item scales found in the codebook and data,
# reversed items found from the answers, roles (dependent / independent ...) with alternatives, AI suggestions checked.
library(testthat)
cfiles <- file.path(getOption("qc.root", "."), "validation", "corpus", "files")

breakdown <- function(text, d, cb, labels = list()) {          # the same chain the app uses in step 2
  cs <- qc_measure_candidates(d, cb, labels)
  wd <- qc_measure_data(d, cs)
  lab <- labels; for (s in cs) if (is.null(lab[[s$name]])) lab[[s$name]] <- s$label
  lv <- lapply(Filter(function(e) identical(e$kind, "codes"), cb$vars), function(e) e$values)
  cat0 <- qc_measure_catalog(wd, lab, lv, cb, cs, list())
  qc_dissect_scales$scales <- cs
  b <- qc_dissect("H1", text, wd, cat0)
  sp <- qc_prefer_scales(b$parts[[1]]$spec, cs)
  list(cands = cs, data = wd, cat = cat0, spec = sp, part = b$parts[[1]])
}

test_that("multi-item scales are found from codebook headings, with the reversed item found from the answers", {
  d <- qc_read(file.path(cfiles, "D1_plain.csv")); cb <- qc_read_any_codebook(file.path(cfiles, "C7_dictionary.csv"), d)
  cs <- qc_measure_candidates(d, cb)
  nm <- vapply(cs, `[[`, "", "name"); expect_setequal(nm, c("financial_stress", "life_satisfaction"))
  fs <- cs[[which(nm == "financial_stress")]]
  expect_setequal(fs$items, paste0("STRESS_", 1:4)); expect_equal(fs$reverse, "STRESS_3")      # "I feel in control of my finances"
  expect_equal(c(fs$min, fs$max), c(1, 5)); expect_match(fs$why, "reversed")
  expect_length(cs[[which(nm == "life_satisfaction")]]$reverse, 0)
  # never scales: categories, the attention check, single questions
  expect_false(any(c("JOB", "HOURS", "REGION", "GENDER", "ATTN", "GRADE") %in% unlist(lapply(cs, `[[`, "items"))))
})

test_that("the same scales are found in every data format of the test library", {
  for (f in c("D1_plain.csv", "D2_semicolon_decimal_comma_latin1.csv", "D3_sosci_utf16.csv", "D7_spss.sav", "D8_stata.dta")) {
    d <- qc_read(file.path(cfiles, f)); cb <- qc_read_any_codebook(file.path(cfiles, "C7_dictionary.csv"), d)
    cs <- qc_measure_candidates(d, cb); fs <- Filter(function(s) "STRESS_1" %in% s$items, cs)
    expect_length(fs, 1); expect_setequal(fs[[1]]$items, paste0("STRESS_", 1:4)); expect_equal(fs[[1]]$reverse, "STRESS_3", label = f)
  }
})

test_that("scale scores use the reversed item and ignore missing codes", {
  d <- qc_read(file.path(cfiles, "D1_plain.csv")); cb <- qc_read_any_codebook(file.path(cfiles, "C7_dictionary.csv"), d)
  d[] <- lapply(d, function(x) if (is.numeric(x)) replace(x, x == -9, NA) else x)    # step 1 sets the codebook's missing codes to missing first
  cs <- qc_measure_candidates(d, cb); wd <- qc_measure_data(d, cs)
  x <- d[, paste0("STRESS_", 1:4)]; x$STRESS_3 <- 6 - x$STRESS_3
  expect_equal(wd$financial_stress, unname(rowMeans(x)), tolerance = 1e-8)        # mean of the four items; missing if one is unanswered
  expect_true(any(is.na(wd$financial_stress))); expect_true(all(wd$financial_stress >= 1 & wd$financial_stress <= 5, na.rm = TRUE))
  expect_false("STRESS_3_r" %in% names(wd))                          # helper column not left in the data
})

test_that("the hypothesis is measured by whole scales, with the roles a beginner understands", {
  d <- qc_read(file.path(cfiles, "D1_plain.csv")); cb <- qc_read_any_codebook(file.path(cfiles, "C7_dictionary.csv"), d)
  r <- breakdown("Financial stress is negatively related to life satisfaction.", d, cb)
  expect_equal(r$spec$goal, "relationship")
  expect_setequal(c(r$spec$var1, r$spec$var2), c("financial_stress", "life_satisfaction"))
  roles <- qc_recommend_roles(r$spec, r$part$phrases, r$cat)
  expect_equal(vapply(roles, `[[`, "", "label"), c("Independent variable (X)", "Dependent variable (Y)"))
  expect_true(all(lengths(lapply(roles, `[[`, "chosen")) == 1))
  for (ro in roles) expect_false(ro$chosen %in% ro$alts)              # alternatives never repeat the recommendation
  # a group difference names the groups as the independent and the scale as the dependent variable
  g <- breakdown("Students with student debt report higher financial stress than students without debt.", d, cb)
  expect_equal(g$spec$goal, "difference"); expect_equal(g$spec$group, "DEBT"); expect_equal(g$spec$outcome, "financial_stress")
  rr <- qc_recommend_roles(g$spec, g$part$phrases, g$cat)
  expect_match(rr[[1]]$label, "^Independent variable"); expect_match(rr[[2]]$label, "^Dependent variable")
})

test_that("an item picked for a construct is replaced by its whole scale", {
  sc <- list(list(name = "financial_stress", items = paste0("STRESS_", 1:4)))
  sp <- qc_prefer_scales(list(goal = "relationship", var1 = "STRESS_2", var2 = "SLEEP"), sc)
  expect_equal(sp$var1, "financial_stress"); expect_equal(sp$var2, "SLEEP")
  expect_equal(qc_prefer_scales(list(goal = "prediction", outcome = "SLEEP", predictors = c("STRESS_1", "STRESS_4", "AGE")), sc)$predictors, c("financial_stress", "AGE"))
  expect_equal(qc_prefer_scales(list(goal = "reliability", items = paste0("STRESS_", 1:4)), sc)$items, paste0("STRESS_", 1:4))   # reliability keeps the items
})

test_that("roles of every analysis type are defined", {
  for (g in c("relationship", "difference", "paired_difference", "association", "prediction", "mediation", "moderation", "reliability"))
    expect_gt(length(qc_roles(g)), 0, label = g)
  expect_equal(vapply(qc_roles("mediation"), `[[`, "", "field"), c("var1", "mediator", "outcome"))
  expect_equal(vapply(qc_roles("moderation"), `[[`, "", "field"), c("var1", "moderator", "outcome"))
})

test_that("the AI sees the scales and its suggestion is checked against the data", {
  d <- qc_read(file.path(cfiles, "D1_plain.csv")); cb <- qc_read_any_codebook(file.path(cfiles, "C7_dictionary.csv"), d)
  r <- breakdown("Money worries go along with lower satisfaction.", d, cb)
  seen <- NULL
  fake <- function(answer) function(url, headers, body) { seen <<- as.character(body)
    jsonlite::toJSON(list(choices = list(list(message = list(role = "assistant", content = jsonlite::toJSON(answer, auto_unbox = TRUE))))), auto_unbox = TRUE) }
  cfg <- qc_ai_config("mistral", key = "k", env = list())
  old <- options(qc.ai.transport = fake(list(goal = "relationship", var1 = "financial_stress", var2 = "life_satisfaction", direction = "negative", confidence = .9, reason = "scales")))
  on.exit(options(old))
  a <- qc_ai_dissect("H1", "Money worries go along with lower satisfaction.", r$data, r$cat, cfg)
  expect_true(a$ok); expect_setequal(c(a$spec$var1, a$spec$var2), c("financial_stress", "life_satisfaction"))
  expect_match(seen, "financial_stress"); expect_match(seen, "STRESS_1")          # the scale and its items are described to the AI
  expect_false(grepl("\"CASE\":|\\b(1|2|3|4|5),(1|2|3|4|5),(1|2|3|4|5),", seen))   # no data values are sent
  options(qc.ai.transport = fake(list(goal = "relationship", var1 = "not_a_column", var2 = "life_satisfaction", direction = "negative", confidence = .9, reason = "x")))
  expect_false(qc_ai_dissect("H1", "Money worries go along with lower satisfaction.", r$data, r$cat, cfg)$ok)
})
