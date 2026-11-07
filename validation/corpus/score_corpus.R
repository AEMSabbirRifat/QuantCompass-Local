# Scores QuantCompass's readers against the test library (validation/corpus/files, built by make_corpus.R).
# qc_corpus_report() returns one row per file with the share of variables, labels, codes, missing codes and rules
# that were read correctly. Used by validation/test_corpus.R and printed by run_validation.R.
qc_corpus_dir <- function() file.path(getOption("qc.root", "."), "validation", "corpus", "files")

qc_corpus_names <- function(kind) {
  qn <- c(CASE = "ResponseId", FINISHED = "Finished", ENROLLED = "Q1", AGE = "Q2", GENDER = "Q3", REGION = "Q4", JOB = "Q5", DEBT = "Q7", HOURS = "Q6",
          STRESS_1 = "Q8_1", STRESS_2 = "Q8_2", STRESS_3 = "Q8_3", STRESS_4 = "Q8_4", ATTN = "Q8_5", SAT_1 = "Q9_1", SAT_2 = "Q9_2", SAT_3 = "Q9_3",
          GRADE = "Q10", SLEEP = "Q11", PRE_SCORE = "Q12", POST_SCORE = "Q13", COMMENT = "Q14")
  if (kind == "qualtrics") return(qn)
  if (kind == "limesurvey") { v <- names(qn); m <- stats::setNames(sub("^(STRESS|SAT)_([0-9])$", "\\1[SQ00\\2]", v), v); m["CASE"] <- "id"; return(m[names(m) != "FINISHED"]) }
  stats::setNames(names(qn), names(qn))
}

qc_corpus_data_report <- function() {
  dir <- qc_corpus_dir(); truth <- utils::read.csv(file.path(dir, "D1_plain.csv"), check.names = FALSE)
  files <- c(D1_plain.csv = "plain", D2_semicolon_decimal_comma_latin1.csv = "plain", D3_sosci_utf16.csv = "plain", D4_qualtrics.csv = "qualtrics",
             D5_limesurvey.csv = "limesurvey", D6_excel_notes_rows.xlsx = "plain", D7_spss.sav = "plain", D8_stata.dta = "plain", D9_missing99_quoted.csv = "plain")
  do.call(rbind, lapply(names(files), function(f) {
    d <- tryCatch(qc_read(file.path(dir, f)), error = function(e) NULL)
    if (is.null(d)) return(data.frame(file = f, rows_ok = FALSE, columns_found = 0, numbers_ok = 0, note = "could not read"))
    nm <- qc_corpus_names(files[[f]]); found <- nm[nm %in% names(d)]
    chk <- c("AGE", "SLEEP", "STRESS_1", "SAT_2", "PRE_SCORE"); ok <- 0
    for (v in chk) { if (!nm[[v]] %in% names(d)) next
      a <- suppressWarnings(as.numeric(d[[nm[[v]]]])); b <- truth[[v]]
      if (f == "D9_missing99_quoted.csv") b[b == -9] <- 99
      ok <- ok + isTRUE(all.equal(a, b, check.attributes = FALSE)) }
    data.frame(file = f, rows_ok = nrow(d) == nrow(truth), columns_found = length(found) / length(nm), numbers_ok = ok / length(chk), note = "")
  }))
}

qc_corpus_codebook_report <- function() {
  dir <- qc_corpus_dir(); truth <- jsonlite::fromJSON(file.path(dir, "truth.json"), simplifyVector = FALSE)
  pairs <- list(C1_sosci_variable_view.txt = c("D1_plain.csv", "plain"), C1_sosci_variable_view.pdf = c("D1_plain.csv", "plain"),
    C2_word_table.docx = c("D1_plain.csv", "plain"), C3_table.pdf = c("D1_plain.csv", "plain"), C4_word_paragraphs.docx = c("D1_plain.csv", "plain"),
    C5_excel_wide.xlsx = c("D1_plain.csv", "plain"), C6_excel_long.xlsx = c("D1_plain.csv", "plain"), C7_dictionary.csv = c("D1_plain.csv", "plain"),
    C8_plain.txt = c("D1_plain.csv", "plain"), C9_codebook.html = c("D1_plain.csv", "plain"), C10_codebook.md = c("D1_plain.csv", "plain"),
    C11_spss_syntax.sps = c("D1_plain.csv", "plain"), C12_qualtrics.qsf = c("D4_qualtrics.csv", "qualtrics"), C13_limesurvey.lss = c("D5_limesurvey.csv", "limesurvey"),
    C14_unusual_codes.txt = c("D1_plain.csv", "plain"), D6_excel_notes_rows.xlsx = c("D6_excel_notes_rows.xlsx", "plain"), D7_spss.sav = c("D7_spss.sav", "plain"))
  words <- function(x) { w <- strsplit(tolower(gsub("[^A-Za-z0-9 ]", " ", x)), "\\s+")[[1]]; w[nchar(w) > 2] }
  do.call(rbind, lapply(names(pairs), function(f) {
    d <- qc_read(file.path(dir, pairs[[f]][1])); nm <- qc_corpus_names(pairs[[f]][2])
    cb <- tryCatch(qc_read_any_codebook(file.path(dir, f), d), error = function(e) list(vars = list(), rules = list(), problems = conditionMessage(e)))
    n <- 0; lab <- 0; cod <- 0; ncod <- 0; mis <- 0
    for (tv in truth$variables) { v <- nm[[tv$variable]]; e <- cb$vars[[v]]; if (is.null(e)) next
      n <- n + 1; tw <- words(tv$label); ew <- words(paste(e$label, e$question))
      lab <- lab + (length(tw) && mean(tw %in% ew) >= .6)
      if (length(tv$codes)) { ncod <- ncod + 1; tc <- unlist(tv$codes)
        okc <- setequal(names(e$values), names(tc)) && all(vapply(names(tc), function(k) all(words(tc[[k]]) %in% words(e$values[[k]])), logical(1)))
        cod <- cod + okc; mis <- mis + ("-9" %in% e$missing) } }
    nv <- length(truth$variables); nc <- sum(vapply(truth$variables, function(x) length(x$codes) > 0, logical(1)))
    rr <- cb$rules; scr <- any(vapply(rr, function(r) isTRUE(r$screen_out) && grepl(nm[["ENROLLED"]], r$condition, fixed = TRUE), logical(1)))
    skp <- any(vapply(rr, function(r) !isTRUE(r$screen_out) && isTRUE(r$readable) && nm[["HOURS"]] %in% r$vars_skipped && grepl(nm[["JOB"]], r$condition, fixed = TRUE), logical(1)))
    att <- tryCatch({ a <- qc_attention_items(cb); length(a) > 0 && any(vapply(a, function(x) identical(x$variable, nm[["ATTN"]]) && isTRUE(x$code == 4), logical(1))) }, error = function(e) FALSE)
    data.frame(file = f, variables = n / nv, labels = lab / nv, codes = cod / nc, missing = mis / nc, screen_rule = scr, skip_rule = skp, attention = att)
  }))
}
