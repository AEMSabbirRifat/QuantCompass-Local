# Builds the QuantCompass test library: one synthetic survey exported in many data formats, its codebook in many
# formats, and the "truth" every reader is scored against. Run from the repository root:
#   Rscript validation/corpus/make_corpus.R
# Needs officer, writexl, haven, jsonlite, xml2 (and LibreOffice for the PDF codebooks).
set.seed(2026)
out <- file.path("validation", "corpus", "files"); dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------- the truth
V <- list(
  list(v = "ENROLLED", label = "Are you currently enrolled at a university?", codes = c("1" = "Yes", "2" = "No")),
  list(v = "AGE", label = "Age in years", codes = NULL),
  list(v = "GENDER", label = "Gender", codes = c("1" = "Female", "2" = "Male", "3" = "Diverse", "4" = "Prefer not to say")),
  list(v = "REGION", label = "Region of citizenship", codes = c("1" = "European Union", "2" = "Europe outside the EU", "3" = "Asia", "4" = "Africa", "5" = "Americas")),
  list(v = "JOB", label = "Current employment situation", codes = c("1" = "Part-time job", "2" = "Full-time job", "3" = "Not working")),
  list(v = "HOURS", label = "Weekly working hours", codes = c("1" = "1-10 hours", "2" = "11-20 hours", "3" = "More than 20 hours")),
  list(v = "DEBT", label = "Do you have student loans or other educational debt?", codes = c("1" = "Yes", "2" = "No")),
  list(v = "STRESS_1", label = "Financial stress: I worry about paying my monthly bills.", codes = c("1" = "Strongly disagree", "2" = "Disagree", "3" = "Neutral", "4" = "Agree", "5" = "Strongly agree")),
  list(v = "STRESS_2", label = "Financial stress: I lose sleep over money.", codes = "STRESS_1"),
  list(v = "STRESS_3", label = "Financial stress: I feel in control of my finances.", codes = "STRESS_1"),
  list(v = "STRESS_4", label = "Financial stress: Money problems affect my studies.", codes = "STRESS_1"),
  list(v = "ATTN", label = "Attention check: please select 'Agree' for this statement.", codes = "STRESS_1"),
  list(v = "SAT_1", label = "Life satisfaction: In most ways my life is close to my ideal.", codes = c("1" = "Completely disagree", "2" = "Disagree", "3" = "Slightly disagree", "4" = "Neither", "5" = "Slightly agree", "6" = "Agree", "7" = "Completely agree")),
  list(v = "SAT_2", label = "Life satisfaction: The conditions of my life are excellent.", codes = "SAT_1"),
  list(v = "SAT_3", label = "Life satisfaction: I am satisfied with my life.", codes = "SAT_1"),
  list(v = "GRADE", label = "Average grade so far", codes = c("1" = "Very good", "2" = "Good", "3" = "Satisfactory", "4" = "Sufficient", "5" = "Fail", "6" = "Not applicable / no grades yet")),
  list(v = "SLEEP", label = "Average hours of sleep per night", codes = NULL),
  list(v = "PRE_SCORE", label = "Test score at the start of the semester", codes = NULL),
  list(v = "POST_SCORE", label = "Test score at the end of the semester", codes = NULL),
  list(v = "COMMENT", label = "Any other comments?", codes = NULL, text = TRUE))
for (i in seq_along(V)) if (is.character(V[[i]]$codes) && length(V[[i]]$codes) == 1) V[[i]]$codes <- V[[which(vapply(V, `[[`, "", "v") == V[[i]]$codes)]]$codes
MISS <- c("-9" = "Not answered")
RULES <- list(list(cond_var = "ENROLLED", codes = 2, screen_out = TRUE, skipped = character()),
              list(cond_var = "JOB", codes = 3, screen_out = FALSE, skipped = "HOURS"))

# ---------------------------------------------------------------- the data
n <- 150
d <- data.frame(ENROLLED = c(rep(1, 140), rep(2, 10)), AGE = sample(19:34, n, TRUE), GENDER = sample(1:4, n, TRUE, c(.48, .46, .03, .03)),
  REGION = sample(1:5, n, TRUE, c(.2, .1, .4, .15, .15)), JOB = sample(1:3, n, TRUE, c(.45, .15, .4)), DEBT = sample(1:2, n, TRUE, c(.35, .65)))
d$HOURS <- ifelse(d$JOB == 3, NA, sample(1:3, n, TRUE)); stress <- rnorm(n) + .6 * (d$DEBT == 1)
lik <- function(z, k = 5) pmin(k, pmax(1, round((k + 1) / 2 + z + rnorm(length(z), 0, .7))))
d$STRESS_1 <- lik(stress); d$STRESS_2 <- lik(stress); d$STRESS_3 <- lik(-stress); d$STRESS_4 <- lik(stress)
d$ATTN <- sample(c(4, 4, 4, 4, 4, 4, 2, 5), n, TRUE)
sat <- -.5 * stress + rnorm(n); d$SAT_1 <- lik(sat, 7); d$SAT_2 <- lik(sat, 7); d$SAT_3 <- lik(sat, 7)
d$GRADE <- sample(c(1:5, 6), n, TRUE, c(.15, .35, .3, .1, .03, .07)); d$SLEEP <- round(rnorm(n, 7 - .3 * stress, .8), 1)
d$PRE_SCORE <- round(rnorm(n, 60, 10)); d$POST_SCORE <- d$PRE_SCORE + round(rnorm(n, 4, 6))
d$COMMENT <- ifelse(runif(n) < .15, sample(c("Good survey", "Too long, sorry", "Thanks, Müller", "No comment"), n, TRUE), "")
for (v in c("STRESS_2", "SAT_3", "GRADE", "GENDER")) d[[v]][sample(n, 4)] <- -9
d$HOURS[is.na(d$HOURS) & runif(n) < 0] <- 1
scr <- d$ENROLLED == 2; d[scr, setdiff(names(d), c("ENROLLED"))] <- NA
d$FINISHED <- ifelse(runif(n) < .08, 0, 1); d$FINISHED[scr] <- 1
d$CASE <- 1000 + seq_len(n)
d <- d[, c("CASE", "FINISHED", setdiff(names(d), c("CASE", "FINISHED")))]
truth <- list(variables = lapply(V, function(x) list(variable = x$v, label = x$label, codes = as.list(x$codes), text = isTRUE(x$text))),
              missing = names(MISS), rules = RULES, n = n, attention = list(variable = "ATTN", code = 4),
              scales = list(STRESS = c("STRESS_1", "STRESS_2", "STRESS_3", "STRESS_4"), SAT = c("SAT_1", "SAT_2", "SAT_3")), reverse = "STRESS_3")
jsonlite::write_json(truth, file.path(out, "truth.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
labs <- vapply(V, `[[`, "", "label"); names(labs) <- vapply(V, `[[`, "", "v")
lab_of <- function(v) if (v %in% names(labs)) labs[[v]] else v
num_str <- function(x, dec = ".") { s <- ifelse(is.na(x), "", format(x, trim = TRUE)); if (dec != ".") s <- gsub(".", dec, s, fixed = TRUE); s }

# ---------------------------------------------------------------- data exports
wcsv <- function(df, file, sep = ",", dec = ".", enc = "UTF-8", bom = FALSE, label_row = NULL, extra_rows = NULL) {
  q <- function(s) ifelse(grepl(paste0("[", sep, "\"\n]"), s), paste0("\"", gsub("\"", "\"\"", s), "\""), s)
  body <- apply(df, 1, function(r) paste(q(vapply(r, function(z) if (is.na(z)) "" else z, "")), collapse = sep))
  lines <- c(paste(q(names(df)), collapse = sep), if (!is.null(label_row)) paste(q(label_row), collapse = sep), extra_rows, body)
  txt <- paste(lines, collapse = "\r\n")
  if (enc == "UTF-16LE") { con <- file(file, "wb"); writeBin(as.raw(c(0xff, 0xfe)), con); writeBin(iconv(txt, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]], con); close(con) }
  else { con <- file(file, "wb"); if (bom) writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con); writeBin(iconv(txt, "UTF-8", enc, toRaw = TRUE)[[1]], con); close(con) }
}
ch <- d; for (v in names(ch)) ch[[v]] <- if (is.numeric(ch[[v]])) num_str(ch[[v]]) else ifelse(is.na(ch[[v]]), "", ch[[v]])
wcsv(ch, file.path(out, "D1_plain.csv"))
ch2 <- d; for (v in names(ch2)) ch2[[v]] <- if (is.numeric(ch2[[v]])) num_str(ch2[[v]], ",") else ifelse(is.na(ch2[[v]]), "", ch2[[v]])
wcsv(ch2, file.path(out, "D2_semicolon_decimal_comma_latin1.csv"), sep = ";", enc = "latin1")
so <- ch; so$SERIAL <- ""; so$QUESTNNR <- "base"; so$STARTED <- "2026-05-01 10:00:00"; so$TIME_RSI <- format(round(runif(n, .4, 2.6), 2)); so$LASTPAGE <- "9"
wcsv(so, file.path(out, "D3_sosci_utf16.csv"), sep = "\t", enc = "UTF-16LE", label_row = vapply(names(so), function(v) if (v %in% names(labs)) labs[[v]] else switch(v, CASE = "Interview number", FINISHED = "Completed", v), ""))
qn <- c(CASE = "ResponseId", FINISHED = "Finished", ENROLLED = "Q1", AGE = "Q2", GENDER = "Q3", REGION = "Q4", JOB = "Q5", DEBT = "Q7", HOURS = "Q6",
        STRESS_1 = "Q8_1", STRESS_2 = "Q8_2", STRESS_3 = "Q8_3", STRESS_4 = "Q8_4", ATTN = "Q8_5", SAT_1 = "Q9_1", SAT_2 = "Q9_2", SAT_3 = "Q9_3",
        GRADE = "Q10", SLEEP = "Q11", PRE_SCORE = "Q12", POST_SCORE = "Q13", COMMENT = "Q14")
qd <- ch; qd$CASE <- sprintf("R_%04d", d$CASE); names(qd) <- qn[names(qd)]; qd$Progress <- ifelse(d$FINISHED == 1, "100", "60")
qd <- qd[, c("ResponseId", "Progress", setdiff(names(qd), c("ResponseId", "Progress")))]
qlab <- vapply(names(qd), function(q) { v <- names(qn)[qn == q]; if (length(v)) lab_of(v) else q }, "")
qimp <- vapply(names(qd), function(q) sprintf('{"ImportId":"QID%s"}', gsub("\\D", "", q)), "")
wcsv(qd, file.path(out, "D4_qualtrics.csv"), label_row = qlab, extra_rows = paste0("\"", gsub("\"", "\"\"", qimp), "\"", collapse = ","), bom = TRUE)
ld <- ch; ld$CASE <- as.character(d$CASE); names(ld)[names(ld) == "CASE"] <- "id"; ld$submitdate <- ifelse(d$FINISHED == 1, "2026-05-01 10:20:00", ""); ld$FINISHED <- NULL
names(ld) <- sub("^(STRESS|SAT)_([0-9])$", "\\1[SQ00\\2]", names(ld))
wcsv(ld, file.path(out, "D5_limesurvey.csv"))
xl <- d; xl[] <- lapply(xl, function(x) if (is.numeric(x)) x else ifelse(is.na(x), NA, x))
notes <- data.frame(matrix("", 2, ncol(xl)), stringsAsFactors = FALSE); names(notes) <- names(xl); notes[1, 1] <- "Student life survey 2026 - raw export"; notes[2, 1] <- "Exported 01.06.2026"
xlsx_with_notes <- rbind(setNames(as.data.frame(as.list(names(xl))), names(xl)), as.data.frame(lapply(xl, as.character)))
xlsx_with_notes <- rbind(notes, xlsx_with_notes); names(xlsx_with_notes) <- paste0("...", seq_along(xl))
cbsheet <- data.frame(variable = names(labs), label = unname(labs), values = vapply(V, function(x) if (length(x$codes)) paste(sprintf("%s = %s", names(x$codes), x$codes), collapse = "; ") else "", ""))
writexl::write_xlsx(list(Responses = xlsx_with_notes, Codebook = cbsheet), file.path(out, "D6_excel_notes_rows.xlsx"), col_names = FALSE)
sv <- d; sv$COMMENT <- ifelse(is.na(sv$COMMENT), "", sv$COMMENT)
for (x in V) { if (length(x$codes)) sv[[x$v]] <- haven::labelled(sv[[x$v]], c(stats::setNames(as.numeric(names(x$codes)), x$codes), "Not answered" = -9), label = x$label)
  else sv[[x$v]] <- structure(sv[[x$v]], label = x$label) }
haven::write_sav(sv, file.path(out, "D7_spss.sav")); haven::write_dta(sv, file.path(out, "D8_stata.dta"))
d9 <- ch; for (v in names(d9)) if (v != "COMMENT") d9[[v]] <- ifelse(d9[[v]] == "-9", "99", d9[[v]]); d9$COMMENT <- ifelse(d9$COMMENT == "", "NA", d9$COMMENT)
wcsv(d9, file.path(out, "D9_missing99_quoted.csv"))

# ---------------------------------------------------------------- codebooks
codes_txt <- function(x, fmt = "%s = %s", sep = "; ") if (length(x$codes)) paste(sprintf(fmt, c(names(x$codes), names(MISS)), c(x$codes, MISS)), collapse = sep) else ""
filt <- function(v) switch(v, HOURS = "Shown only if JOB = 1 or 2 (skipped if JOB = 3)", ENROLLED = "If ENROLLED = 2, the survey ends (screen-out)", "")
# C1 SoSci-style variable view (text -> PDF)
c1 <- c("Codebook - Student life survey 2026", "")
page_of <- c(ENROLLED = 1, AGE = 2, GENDER = 2, REGION = 2, JOB = 2, HOURS = 3)
php <- list(`2` = c("  PHP code", "  if (value('ENROLLED') == 2) {", "    goToPage('end');", "  }", ""),
            `3` = c("  PHP code", "  if (value('JOB') == 3) {", "    goToPage('next');", "  }", ""))
cur <- 0
for (x in V) {
  pg <- if (x$v %in% names(page_of)) page_of[[x$v]] else 4
  if (pg != cur) { cur <- pg; c1 <- c(c1, sprintf("Page %02d", pg), "", php[[as.character(pg)]]) }
  c1 <- c(c1, sprintf("      %s", x$label), "", sprintf("                         %s %s", x$v, sub("^[^:]*: ", "", x$label)))
  if (length(x$codes)) c1 <- c(c1, sprintf("                            %s = %s", c(names(x$codes), names(MISS)), c(x$codes, MISS)))
  else c1 <- c(c1, if (isTRUE(x$text)) "                            Free text" else "                            Numeric (integer or decimal)")
  c1 <- c(c1, "")
}
writeLines(c1, file.path(out, "C1_sosci_variable_view.txt"))
# C2 Word table, C3 the same as PDF
tab <- data.frame(Variable = names(labs), `Question / label` = unname(labs), Values = vapply(V, codes_txt, ""), Filter = vapply(names(labs), filt, ""), check.names = FALSE)
doc <- officer::read_docx(); doc <- officer::body_add_par(doc, "Codebook - Student life survey 2026", style = "heading 1")
doc <- officer::body_add_par(doc, "Missing values: -9 = not answered (all closed questions).")
doc <- officer::body_add_table(doc, tab, style = "table_template"); print(doc, target = file.path(out, "C2_word_table.docx"))
# C4 Word paragraphs
doc <- officer::read_docx(); doc <- officer::body_add_par(doc, "Student life survey - variable descriptions", style = "heading 1")
for (x in V) { doc <- officer::body_add_par(doc, sprintf("%s - %s", x$v, x$label), style = "heading 2")
  doc <- officer::body_add_par(doc, if (length(x$codes)) paste("Answer codes:", codes_txt(x, "%s: %s", ", ")) else if (isTRUE(x$text)) "Open text answer." else "Numeric answer.")
  if (nzchar(filt(x$v))) doc <- officer::body_add_par(doc, filt(x$v)) }
print(doc, target = file.path(out, "C4_word_paragraphs.docx"))
# C5 Excel wide, C6 Excel long, C7 CSV dictionary
writexl::write_xlsx(list(Variables = data.frame(Name = names(labs), Description = unname(labs), `Value labels` = vapply(V, function(x) if (length(x$codes)) paste(sprintf("%s=%s", names(x$codes), x$codes), collapse = " | ") else "", ""),
  `Missing values` = vapply(V, function(x) if (length(x$codes)) "-9" else "", ""), check.names = FALSE)), file.path(out, "C5_excel_wide.xlsx"))
long <- do.call(rbind, lapply(V, function(x) if (length(x$codes)) data.frame(variable = x$v, variable_label = x$label, value = c(names(x$codes), "-9"), value_label = c(unname(x$codes), "Not answered"))
  else data.frame(variable = x$v, variable_label = x$label, value = NA, value_label = NA)))
writexl::write_xlsx(list(Dictionary = long), file.path(out, "C6_excel_long.xlsx"))
wcsv(data.frame(var = names(labs), label = unname(labs), codes = vapply(V, codes_txt, "")), file.path(out, "C7_dictionary.csv"), sep = ";")
# C8 plain text key-value
c8 <- unlist(lapply(V, function(x) c(sprintf("Variable: %s", x$v), sprintf("Label: %s", x$label), if (length(x$codes)) sprintf("Codes: %s", codes_txt(x, "%s: %s", " | ")), if (nzchar(filt(x$v))) sprintf("Note: %s", filt(x$v)), "")))
writeLines(c8, file.path(out, "C8_plain.txt"))
# C9 HTML table, C10 Markdown table
writeLines(c("<html><body><h1>Codebook</h1><table>", "<tr><th>Variable</th><th>Label</th><th>Codes</th></tr>",
  sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>", names(labs), unname(labs), vapply(V, function(x) codes_txt(x, "%s = %s", "<br>"), "")), "</table></body></html>"), file.path(out, "C9_codebook.html"))
writeLines(c("# Codebook", "", "| Variable | Label | Codes |", "|---|---|---|", sprintf("| %s | %s | %s |", names(labs), unname(labs), vapply(V, codes_txt, ""))), file.path(out, "C10_codebook.md"))
# C11 SPSS syntax
c11 <- c("VARIABLE LABELS", paste0("  ", names(labs), " '", gsub("'", "''", labs), "'", c(rep("", length(labs) - 1), ".")), "",
  unlist(lapply(V, function(x) if (length(x$codes)) c(sprintf("VALUE LABELS %s", x$v), paste0("  ", c(names(x$codes), "-9"), " '", c(x$codes, "Not answered"), "'"), "  ."))),
  "MISSING VALUES ALL (-9).")
writeLines(c11, file.path(out, "C11_spss_syntax.sps"))
# C12 Qualtrics .qsf
els <- lapply(V, function(x) { q <- qn[[x$v]]; base <- sub("_.*$", "", q)
  list(Element = "SQ", PrimaryAttribute = paste0("QID", gsub("\\D", "", base)), Payload = list(DataExportTag = q, QuestionText = sprintf("<p>%s</p>", x$label),
    QuestionType = "MC", Choices = if (length(x$codes)) stats::setNames(lapply(x$codes, function(l) list(Display = l)), names(x$codes)) else NULL,
    DisplayLogic = if (x$v == "HOURS") list(`0` = list(`0` = list(LogicType = "Question", QuestionID = "QID5", ChoiceLocator = "q://QID5/SelectableChoice/3", Operator = "NotSelected", Description = "If Q5 Not working Is Not Selected"))) else NULL)) })
jsonlite::write_json(list(SurveyEntry = list(SurveyName = "Student life survey"), SurveyElements = els), file.path(out, "C12_qualtrics.qsf"), auto_unbox = TRUE, null = "null")
# C13 LimeSurvey .lss (subset of the real structure)
lv <- names(ld)[!names(ld) %in% c("id", "submitdate")]
x <- xml2::xml_new_root("document"); xml2::xml_add_child(x, "LimeSurveyDocType", "Survey")
qs <- xml2::xml_add_child(x, "questions"); xml2::xml_add_child(qs, "fields"); rows <- xml2::xml_add_child(qs, "rows")
as <- xml2::xml_add_child(x, "answers"); xml2::xml_add_child(as, "fields"); arows <- xml2::xml_add_child(as, "rows")
sq <- xml2::xml_add_child(x, "subquestions"); xml2::xml_add_child(sq, "fields"); srows <- xml2::xml_add_child(sq, "rows")
qid <- 0; for (b in unique(sub("\\[.*$", "", lv))) { qid <- qid + 1; vv <- V[[which(vapply(V, `[[`, "", "v") == ifelse(b %in% c("STRESS", "SAT"), paste0(b, "_1"), b))]]
  r <- xml2::xml_add_child(rows, "row"); xml2::xml_add_child(r, "qid", qid); xml2::xml_add_child(r, "parent_qid", 0); xml2::xml_add_child(r, "title", b)
  xml2::xml_add_child(r, "question", if (b %in% c("STRESS", "SAT")) sub(":.*$", "", vv$label) else vv$label)
  xml2::xml_add_child(r, "relevance", if (b == "HOURS") "((JOB.NAOK == \"1\" or JOB.NAOK == \"2\"))" else "1")
  if (length(vv$codes)) for (k in names(vv$codes)) { a <- xml2::xml_add_child(arows, "row"); xml2::xml_add_child(a, "qid", qid); xml2::xml_add_child(a, "code", k); xml2::xml_add_child(a, "answer", vv$codes[[k]]) }
  if (b %in% c("STRESS", "SAT")) for (k in grep(paste0("^", b, "\\["), lv, value = TRUE)) { s <- xml2::xml_add_child(srows, "row"); kk <- sub("^.*\\[(.*)\\]$", "\\1", k)
    xml2::xml_add_child(s, "qid", 100 + qid * 10 + as.integer(sub("SQ00", "", kk))); xml2::xml_add_child(s, "parent_qid", qid); xml2::xml_add_child(s, "title", kk)
    xml2::xml_add_child(s, "question", sub("^[^:]*: ", "", labs[[sub("\\[SQ00", "_", sub("\\]$", "", k))]])) } }
xml2::write_xml(x, file.path(out, "C13_limesurvey.lss"))
# C14 unusual code styles: "Yes (1)", "[2] No"
c14 <- unlist(lapply(V, function(x) c(sprintf("%s  %s", x$v, x$label), if (length(x$codes)) paste0("    ", if (x$v %in% c("GENDER", "DEBT", "ENROLLED")) sprintf("%s (%s)", x$codes, names(x$codes)) else sprintf("[%s] %s", names(x$codes), x$codes)), "")))
writeLines(c("Student life survey - codebook", "Missing: -9 = not answered", "", c14), file.path(out, "C14_unusual_codes.txt"))
# PDFs via LibreOffice (needs a writable HOME)
if (nzchar(Sys.which("soffice"))) {
  home <- file.path(tempdir(), "soffice_home"); dir.create(home, showWarnings = FALSE); pdo <- file.path(tempdir(), "pdf_out"); dir.create(pdo, showWarnings = FALSE)
  so <- function(args) system(paste(c(sprintf("env -u LD_LIBRARY_PATH HOME=%s soffice --headless", shQuote(home)), args), collapse = " "), ignore.stdout = TRUE, ignore.stderr = TRUE)
  so(c("--convert-to pdf --outdir", shQuote(pdo), shQuote(file.path(out, "C2_word_table.docx"))))
  so(c(shQuote("--infilter=Text (encoded):UTF8,LF,Liberation Mono,en-US"), "--convert-to pdf --outdir", shQuote(pdo), shQuote(file.path(out, "C1_sosci_variable_view.txt"))))
  file.copy(file.path(pdo, "C2_word_table.pdf"), file.path(out, "C3_table.pdf"), overwrite = TRUE)
  file.copy(file.path(pdo, "C1_sosci_variable_view.pdf"), file.path(out, "C1_sosci_variable_view.pdf"), overwrite = TRUE)
}
cat("Corpus written to", out, ":", length(list.files(out)), "files\n")
