# Optional AI layer: tested offline with a fake transport. Checks the request format of every provider, that no data
# values are ever sent, and that AI answers are validated against the real variables before they are used.
library(testthat)
source(file.path(getOption("qc.root", "."), "validation", "corpus", "hypotheses.R"))

fake <- function(answer, log = new.env()) function(url, headers, body) {
  log$url <- url; log$headers <- headers; log$body <- as.character(body)
  js <- jsonlite::toJSON(answer, auto_unbox = TRUE)
  if (grepl("generateContent", url)) jsonlite::toJSON(list(candidates = list(list(content = list(parts = list(list(text = js)))))), auto_unbox = TRUE)
  else if (grepl("/messages$", url)) jsonlite::toJSON(list(content = list(list(type = "text", text = js))), auto_unbox = TRUE)
  else jsonlite::toJSON(list(choices = list(list(message = list(role = "assistant", content = js)))), auto_unbox = TRUE)
}

test_that("keys come from the session or .env, and every provider is called in its own format", {
  f <- tempfile(); writeLines(c("# comment", "QC_AI_PROVIDER=mistral", 'MISTRAL_API_KEY="abc123"  # mine'), f)
  cfg <- qc_ai_config(env = qc_ai_read_env(f)); expect_equal(cfg$provider, "mistral"); expect_equal(cfg$key, "abc123"); expect_true(cfg$ready)
  expect_equal(qc_ai_config("gemini", key = "typed", env = qc_ai_read_env(f))$key, "typed")
  expect_false(qc_ai_config("anthropic", env = list())$ready || nzchar(Sys.getenv("ANTHROPIC_API_KEY")))
  expect_true(qc_ai_config("ollama", env = list())$ready)
  for (p in names(qc_ai_providers)) {
    log <- new.env(); old <- options(qc.ai.transport = fake(list(ok = TRUE), log))
    a <- qc_ai_chat(qc_ai_config(p, key = "k", env = list()), "sys", "user"); options(old)
    expect_true(isTRUE(a$ok), info = p)
    if (p == "gemini") { expect_match(log$url, ":generateContent$"); expect_equal(log$headers$`x-goog-api-key`, "k"); expect_false(grepl("key=", log$url)) }
    if (p == "anthropic") { expect_match(log$url, "/messages$"); expect_equal(log$headers$`x-api-key`, "k") }
    if (p %in% c("mistral", "openai_compatible")) expect_equal(log$headers$Authorization, "Bearer k")
    if (p == "ollama") expect_match(log$url, "localhost:11434/v1/chat/completions")
  }
  expect_error(qc_ai_chat(qc_ai_config("gemini", env = list(GEMINI_API_KEY = "")), "s", "u"), "No API key|error")
})

test_that("only names, labels and codebook categories are sent - never data values", {
  s <- qc_hyp_study("students"); d <- s$data; d$STRESS[1] <- 3.14159; d$OPEN <- c("my secret answer", rep("x", nrow(d) - 1))
  cat0 <- qc_catalog(d, s$labels, s$levels); log <- new.env()
  old <- options(qc.ai.transport = fake(list(goal = "relationship", var1 = "STRESS", var2 = "SLEEP_H", direction = "negative", confidence = .9, reason = "r"), log))
  r <- qc_ai_dissect("H1", "Stress goes with less sleep.", d, cat0, qc_ai_config("gemini", key = "k", env = list()), s$levels); options(old)
  expect_true(r$ok); expect_equal(r$spec$goal, "relationship"); expect_equal(r$spec[["direction"]], "negative")
  expect_false(grepl("3.14159", log$body, fixed = TRUE)); expect_false(grepl("secret", log$body))
  expect_true(grepl("Perceived stress", log$body)); expect_true(grepl("2 = Female", log$body))
})

test_that("AI answers are checked against the variables and the data", {
  s <- qc_hyp_study("work"); cat0 <- qc_catalog(s$data, s$labels, s$levels)
  bad <- qc_ai_validate_spec(list(goal = "relationship", var1 = "JS", var2 = "SALARY"), s$data, cat0)
  expect_false(bad$ok); expect_match(bad$problems, "not in your data")
  expect_false(qc_ai_validate_spec(list(goal = "teleport", var1 = "JS"), s$data, cat0)$ok)
  expect_false(qc_ai_validate_spec(list(goal = "mediation", var1 = "DEPT", mediator = "ENG", outcome = "TI"), s$data, cat0)$ok)
  ok <- qc_ai_validate_spec(list(goal = "difference", outcome = "JS", group = "ARR", higher_group = "remote workers", other_group = "office workers"), s$data, cat0)
  expect_true(ok$ok); expect_equal(ok$spec$expect_higher, "1")
  rr <- qc_ai_validate_spec(list(goal = "relationship", var1 = "SEX", var2 = "WL"), s$data, cat0)
  expect_equal(rr$spec$goal, "difference"); expect_equal(rr$spec$group, "SEX")
  mo <- qc_ai_validate_spec(list(goal = "moderation", var1 = "WL", moderator = "CONTR", outcome = "TI", stronger = "temporary"), s$data, cat0)
  expect_true(mo$ok); expect_equal(mo$spec$stronger$level, "2")
})

test_that("codebook gaps: AI codes are kept only when they fit the data", {
  d <- data.frame(Q1 = sample(1:3, 50, TRUE), Q2 = sample(1:5, 50, TRUE), AGE = sample(18:30, 50, TRUE))
  cb <- list(vars = list(AGE = qc_cb_entry("AGE", "Age in years")), rules = list())
  lines <- c("Q1 Satisfaction with housing", "1 = low 2 = medium 3 = high", "Q2 How often do you exercise?", "1 = never 2 = rarely")
  ans <- list(variables = list(list(name = "Q1", label = "Satisfaction with housing", codes = list(`1` = "low", `2` = "medium", `3` = "high")),
                               list(name = "Q2", label = "Exercise frequency", codes = list(`1` = "never", `2` = "rarely")),
                               list(name = "NOPE", label = "x")))
  old <- options(qc.ai.transport = fake(ans)); g <- qc_ai_codebook_gaps(cb, d, lines, qc_ai_config("gemini", key = "k", env = list())); options(old)
  expect_setequal(names(g$vars), c("Q1", "Q2")); expect_equal(length(g$vars$Q1$values), 3)
  expect_length(g$vars$Q2$values, 0); expect_match(g$problems, "Q2")
})
