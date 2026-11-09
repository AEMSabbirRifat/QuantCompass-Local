# v0.6: codebook definitions (scales, screening table), wrapped code lines, the user's own hypothesis count,
# and the AI assistant (context, lookups, checked actions, conversation turn) - all with synthetic data.
library(testthat)
source(file.path(getOption("qc.root", "."), "validation", "corpus", "hypotheses.R"))

# a synthetic study in the layout of a typical PDF codebook (variable list with wrapped cells, a screening table, a scale table)
v6_study <- function(n = 150, seed = 6) {
  set.seed(seed)
  f <- rnorm(n); ms <- sapply(1:4, function(i) pmin(5, pmax(1, round(3 + f + rnorm(n, sd = .7)))))
  ms[, 3] <- 6 - ms[, 3]                                                     # MS03 is reverse-keyed
  sl <- sapply(1:5, function(i) pmin(6, pmax(1, round(3.5 - .6 * f + rnorm(n, sd = .8)))))
  d <- data.frame(CASE = seq_len(n), FINISHED = c(rep(0, 6), rep(1, n - 6)), CO01 = c(rep(1, 10), 2, 2, 2, rep(1, n - 13)),
    AGE = sample(19:38, n, TRUE), HOUSE = sample(1:3, n, TRUE), JOB = sample(1:2, n, TRUE),
    MS01 = ms[, 1], MS02 = ms[, 2], MS03 = ms[, 3], MS04 = ms[, 4], SL01 = sl[, 1], SL02 = sl[, 2], SL03 = sl[, 3], SL04 = sl[, 4], SL05 = sl[, 5],
    ATT = c(rep(5, 20), 2, 2, 3, rep(5, n - 23)), RSI = round(runif(n, .5, 1.8), 2))
  d$RSI[30:33] <- 2.4
  d
}
v6_codebook_lines <- c(
  " Codebook: Money and Sleep Survey (synthetic)",
  " 2. Screening (filtration) process: 150 to 133",
  " Step                         Rule (variable)                         Removed        Remaining",
  " Raw export                   All cases downloaded                    -              150",
  " S1 Incomplete                FINISHED = 1                            6              144",
  " S2 Consent                   CO01 = 1                                3              141",
  " S3 Attention check           ATT = 5                                 3              138",
  " S4 Speeding                  RSI < 2.0                               4              134",
  " Final                        Analysis sample                         16             134",
  "",
  " 3. Scales",
  " Scale                  Items          Response format               Scoring",
  " Money stress           MS01-MS04      1 = disagree ... 5 =          Recode MS03 (6 - x); mean of 4",
  " (MS_MEAN)                             agree                         items",
  " Sleep quality          SL01-SL05      1 = never ... 6 = always      Sum of SL01-SL05",
  " (SL_SUM)",
  "",
  " 4. Variable list",
  " Variable     Label / item wording                      Level        Values                               In file",
  " CASE         Case number                               ID           1-150                                raw",
  " FINISHED     Questionnaire completed                   binary       0 = no, 1 = yes                      raw",
  " CO01         I consent to take part.                   binary       1 = yes, 2 = no                      raw",
  " AGE          Age in years                              ratio        whole years                          raw",
  " HOUSE        Type of housing                           nominal      1 = dormitory, 2 = shared flat,      raw",
  "                                                                     3 = own flat",
  " JOB          Paid work                                 binary       1 = no, 2 = yes                      raw",
  " MS01         I worry about money.                      ordinal      1 = disagree, 2 = rather disagree,   raw",
  "                                                                     3 = neutral, 4 = rather agree,",
  "                                                                     5 = agree",
  " MS02         Money worries keep me awake.              ordinal      as MS01                              raw",
  " MS03 (R)     I can cover my costs easily.              ordinal      as MS01; reverse-keyed               raw",
  " MS04         I avoid spending because of money.        ordinal      as MS01                              raw",
  " SL01         I sleep well.                             ordinal      1 = never, 2 = rarely, 3 = sometimes,  raw",
  "                                                                     4 = often, 5 = mostly, 6 = always",
  " SL02         I wake up rested.                         ordinal      as SL01                              raw",
  " SL03         I fall asleep easily.                     ordinal      as SL01                              raw",
  " SL04         I sleep through the night.                ordinal      as SL01                              raw",
  " SL05         I feel fresh in the morning.              ordinal      as SL01                              raw",
  " ATT          Attention check: please select 'agree'.   ordinal      as MS01; correct answer = 5          raw",
  " RSI          Relative speed index                      ratio        approx. 0.5-4                        raw")

test_that("codes that describe none of the values lose against a reading without codes (wrapped '3 = own flat')", {
  e_codes <- list(label = "Age in years", values = c(`3` = "own flat"), kind = "codes", missing = character(), question = NA)
  e_none <- list(label = "Age in years", values = character(), kind = "none", missing = character(), question = NA)
  x <- sample(19:38, 100, TRUE)
  expect_gt(qc_cb_fit(e_none, x, character()), qc_cb_fit(e_codes, x, character()))
  expect_equal(qc_label_complete("I worry about having enough my rent and bills.", c("I worry about having enough money to pay my rent and bills.", "I worry about having enough money to pay ordinal 5-pt my rent")),
               "I worry about having enough money to pay my rent and bills.")
})

test_that("a text codebook in table layout: codes, scales and the screening table are read", {
  d <- v6_study(); p <- file.path(tempdir(), "v6_codebook.txt"); writeLines(v6_codebook_lines, p)
  cb <- qc_read_any_codebook(p, d)
  expect_length(cb$vars$AGE$values, 0)                                         # not "3 = own flat"
  expect_equal(unname(cb$vars$HOUSE$values), c("dormitory", "shared flat", "own flat"))
  ch <- qc_codebook_check(cb, d); expect_false(any(ch$status[ch$variable %in% c("AGE", "RSI", "CASE")] == "warn"))
  sd <- cb$scale_defs; nm <- vapply(sd, function(s) s$code %||% NA_character_, "")
  ms <- sd[[which(nm == "MS_MEAN")]]; expect_equal(ms$items, paste0("MS0", 1:4)); expect_equal(ms$reverse, "MS03"); expect_equal(ms$method, "mean"); expect_equal(ms$label, "Money stress")
  sl <- sd[[which(nm == "SL_SUM")]]; expect_equal(sl$items, paste0("SL0", 1:5)); expect_equal(sl$method, "sum"); expect_length(sl$reverse, 0)
  st <- cb$screening$steps; expect_equal(vapply(st, `[[`, "", "label"), c("S1 Incomplete", "S2 Consent", "S3 Attention check", "S4 Speeding"))
  expect_equal(cb$screening$raw_n, 150L)
  # applied in order, each step removes what the codebook documents
  ex <- qc_exclusion_suggestions(d, cb); x <- d; removed <- integer()
  for (s in ex) { hit <- eval(parse(text = s$cond), list(data = x)) %in% TRUE; removed <- c(removed, sum(hit)); x <- x[!hit, ] }
  expect_equal(removed, c(6L, 3L, 3L, 4L)); expect_equal(nrow(x), 134L)
  expect_equal(vapply(ex, function(s) s$documented %||% NA_integer_, 0L), c(6L, 3L, 3L, 4L))
  expect_false(any(vapply(ex, `[[`, "", "id") %in% c("unfinished", "speeders")))   # built-in steps on the same variables are not doubled
  # the scale finder uses the codebook's definitions (not guesses)
  cs <- qc_measure_candidates(d, cb); nm2 <- vapply(cs, `[[`, "", "name")
  expect_true(all(c("MS_MEAN", "SL_SUM") %in% nm2)); m <- cs[[which(nm2 == "MS_MEAN")]]
  expect_equal(m$source, "codebook"); expect_equal(m$reverse, "MS03"); expect_match(m$why, "Defined in your codebook")
  wd <- qc_measure_data(d, cs); expect_equal(wd$MS_MEAN, unname(rowMeans(cbind(d$MS01, d$MS02, 6 - d$MS03, d$MS04))))
  expect_equal(wd$SL_SUM, unname(rowSums(d[, paste0("SL0", 1:5)])))
})

test_that("variables named in text: ranges, families, reverse-coded copies", {
  k <- c("FC01", "FC02", "FC03", "FC04", "FC05", "WL01", "WL02", "WB01", "WB02", "TIME001", "TIME002", "TIME003", "DE01")
  expect_equal(qc_expand_vars("FC01-FC03", k)$vars, c("FC01", "FC02", "FC03"))
  expect_equal(qc_expand_vars("mean of FC01, FC02, FC03R, FC04", k)$reverse, "FC03")
  expect_equal(qc_expand_vars("all 9 scale items (FC, WL, WB)", k)$vars, c(paste0("FC0", 1:5), "WL01", "WL02", "WB01", "WB02"))
  expect_equal(qc_expand_vars("TIME001 to TIME003", k)$vars, c("TIME001", "TIME002", "TIME003"))
})

test_that("the user's hypotheses are counted as the user wrote them", {
  r <- qc_read_hyp_text("H1₁: A is related to B.\nH1₀: A is not related to B.\nH2₁: C predicts D\nwhen E is high.\nH2₀: C does not predict D.\nH3: F differs between groups.\nH03: no difference.")
  expect_equal(names(r$hyps), c("H1", "H2", "H3")); expect_equal(unname(r$hyps[["H2"]]), "C predicts D when E is high.")
  expect_equal(names(r$nulls), c("H1", "H2", "H3")); expect_match(r$notes, "not as extra hypotheses")
  expect_equal(names(qc_read_hyp_text("Hypotheses:\n1. Stress lowers sleep.\n2) Debt raises stress.")$hyps), c("H1", "H2"))
  expect_equal(length(qc_read_hyp_text("Stress lowers sleep.\nDebt raises stress.\nAge relates to income.")$hyps), 3)
  expect_equal(names(qc_read_hyp_text("H1a: x relates to y\nH1b: x relates to z\nNull hypothesis: none")$hyps), c("H1a", "H1b"))
  expect_equal(names(qc_read_hyp_text("H₁1: X relates to Y\nH₀1: X does not relate to Y")$hyps), "H1")
})

test_that("a later claim keeps its own direction", {
  s <- qc_hyp_study("students"); cat0 <- qc_catalog(s$data, s$labels, s$levels)
  b <- qc_dissect("H4", "Financial stress is positively related to anxiety and negatively related to sleep.", s$data, cat0)
  dirs <- vapply(b$parts, function(p) p$spec[["direction"]] %||% NA_character_, "")
  expect_equal(length(b$parts), 2); expect_equal(unname(dirs), c("positive", "negative"))
})

# ---------------------------------------------------------------- the assistant
v6_state <- function() {
  d <- v6_study(); p <- file.path(tempdir(), "v6_codebook.txt"); writeLines(v6_codebook_lines, p); cb <- qc_read_any_codebook(p, d)
  cs <- qc_measure_candidates(d, cb); wd <- qc_measure_data(d, cs)
  lab <- lapply(cb$vars, `[[`, "label"); for (s in cs) lab[[s$name]] <- s$label
  lv <- lapply(Filter(function(e) identical(e$kind, "codes"), cb$vars), function(e) e$values)
  cat0 <- qc_measure_catalog(wd, lab, lv, cb, cs, list())
  list(step = "Hypotheses & variables", file = "survey.csv", n_raw = nrow(d), n_cols = ncol(d), codebook = "v6_codebook.txt", data = wd, cat = cat0,
    check = qc_codebook_check(cb, d), level_labels = lv, exclusions = qc_exclusion_suggestions(d, cb), scales = cs, id_var = "CASE", raw_names = stats::setNames(names(d), names(d)),
    hyps = list(list(id = "H1", text = "Money stress is negatively related to sleep quality.", goal = "relationship", roles = "var1 = MS_MEAN, var2 = SL_SUM", confirmed = FALSE)),
    part_ids = "H1", codebook_lines = v6_codebook_lines, n_hyp = 1)
}

test_that("the assistant sees the whole project: summaries always, single answers only with full access", {
  st <- v6_state(); ctx <- paste(qc_assist_context(st, "summaries"), collapse = "\n")
  expect_match(ctx, "MS_MEAN \\(Money stress\\): mean of MS01, MS02, MS03, MS04; reversed: MS03")
  expect_match(ctx, "Hypotheses \\(1, as the user wrote them\\)"); expect_match(ctx, "S1 Incomplete"); expect_match(ctx, "AGE \\| Age in years \\| numeric")
  expect_match(ctx, "mean=")                                                  # aggregates
  expect_match(qc_assist_lookup(list(type = "rows", ids = list(5, 6)), st, "summaries"), "Not allowed")
  r <- qc_assist_lookup(list(type = "rows", ids = list(5, 6), variables = list("AGE", "FINISHED")), st, "full")
  expect_match(r, "2 case\\(s\\) match"); expect_match(r, "AGE")
  expect_match(qc_assist_lookup(list(type = "values", variable = "HOUSE"), st), "Answers of HOUSE")
  expect_match(qc_assist_lookup(list(type = "codebook", term = "Speeding"), st), "RSI < 2.0")
  expect_match(qc_assist_lookup(list(type = "describe", variables = list("nope")), st), "Lookup failed")
})

test_that("every proposed change is checked against the data before it can be applied", {
  st <- v6_state(); chk <- function(a) qc_assist_check_action(a, st)
  expect_true(chk(list(type = "clear_codes", variable = "AGE"))$ok)
  expect_false(chk(list(type = "clear_codes", variable = "NOPE"))$ok)
  expect_false(chk(list(type = "launch_rockets"))$ok)
  a <- chk(list(type = "add_exclusion", label = "speeders", condition = "RSI >= 2")); expect_true(a$ok); expect_match(a$text, "4 case")
  expect_false(chk(list(type = "add_exclusion", condition = "RSI greater 2"))$ok)
  expect_true(chk(list(type = "create_scale", name = "MS_ALT", items = list("MS01", "MS02", "MS04"), method = "mean"))$ok)
  expect_false(chk(list(type = "create_scale", name = "MS_ALT", items = list("MS01"), method = "mean"))$ok)
  expect_false(chk(list(type = "create_scale", name = "W5", items = list("SL01", "SL02", "SL03"), method = "who5"))$ok)
  s <- chk(list(type = "set_hypothesis", id = "H1", goal = "relationship", var1 = "MS_MEAN", var2 = "SL_SUM", direction = "negative"))
  expect_true(s$ok); expect_equal(s$spec$var1, "MS_MEAN"); expect_equal(s$spec[["direction"]], "negative")
  expect_false(chk(list(type = "set_hypothesis", id = "H9", goal = "relationship", var1 = "MS_MEAN", var2 = "SL_SUM"))$ok)
  expect_false(chk(list(type = "exclude_cases", ids = list(9999), reason = "x"))$ok)
  expect_false(chk(list(type = "exclude_cases", ids = list(3)))$ok)        # a reason is required
  expect_true(chk(list(type = "exclusion_off", id = st$exclusions[[4]]$id))$ok)
})

test_that("a conversation turn: lookups first, then an answer with checked actions; the rules are in the prompt", {
  st <- v6_state(); calls <- list()
  answers <- list(list(reply = "", lookups = list(list(type = "values", variable = "HOUSE")), actions = list()),
                  list(reply = "AGE holds plain numbers, so I removed the wrong codes.", lookups = list(),
                       actions = list(list(type = "clear_codes", variable = "AGE"), list(type = "clear_codes", variable = "NOPE"))))
  i <- 0
  tr <- function(url, headers, body) { i <<- i + 1; calls[[i]] <<- as.character(body)
    jsonlite::toJSON(list(choices = list(list(message = list(role = "assistant", content = as.character(jsonlite::toJSON(answers[[i]], auto_unbox = TRUE)))))), auto_unbox = TRUE) }
  old <- options(qc.ai.transport = tr); on.exit(options(old))
  r <- qc_assist_turn(qc_ai_config("mistral", key = "k", env = list()), st, list(list(role = "user", text = "hi"), list(role = "assistant", text = "hello")), "Why does AGE have codes? Fix it.")
  expect_equal(i, 2); expect_match(calls[[2]], "Answers of HOUSE")                 # the lookup result was sent back
  expect_match(calls[[1]], "Never add, split, merge or remove hypotheses")
  expect_match(r$reply, "plain numbers"); expect_length(r$actions, 2)
  expect_true(r$actions[[1]]$ok); expect_false(r$actions[[2]]$ok)
})
