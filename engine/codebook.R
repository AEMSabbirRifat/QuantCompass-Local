# Codebook import, check and application.
# Principle: only what the codebook states is used. Anything uncertain is shown as a warning
# and is applied only if the user ticks it. Nothing is guessed.
#
# A codebook object (cb) is a list:
#   vars   : named list, one entry per variable:
#            variable, label, values (named character: names = codes, values = labels),
#            missing (codes meaning "no answer"), kind ("codes" | "range" | "text"), page, question
#   rules  : list of survey rules (skip/filter logic), each: page, condition (R code), target,
#            vars_skipped, text (original), screen_out (TRUE when target is the end)
#   pages  : data.frame(page, page_id)
#   source : where it came from; problems : messages for the user

qc_missing_label_re <- "(?i)^\\s*(not answered|no answer|missing|keine angabe|nicht beantwortet|refused|n/?a - not answered|system missing)\\s*$"
qc_na_like_re <- "(?i)(not applicable|does not apply|no grades yet|don.?t know|cannot say|prefer not to say|rather not say|k\\.?\\s?a\\.?|trifft nicht zu)"

# Built-in descriptions of the system columns that survey tools add (not in user codebooks)
qc_system_vars <- c(
  CASE = "Case number (SoSci Survey)", SERIAL = "Serial number (SoSci Survey)", REF = "Reference (SoSci Survey)",
  QUESTNNR = "Questionnaire used (SoSci Survey)", MODE = "Interview mode (SoSci Survey)", STARTED = "Time the interview started",
  MAILSENT = "Time an invitation was sent", LASTDATA = "Time of the last data received", STATUS = "Participant status",
  FINISHED = "Completed the questionnaire (1 = yes, 0 = no)", Q_VIEWER = "Respondent only viewed the questionnaire",
  LASTPAGE = "Last page reached", MAXPAGE = "Furthest page reached", MISSING = "Share of missing answers (%)",
  MISSREL = "Weighted share of missing answers (%)", TIME_RSI = "Relative speed index (values above 2 = suspiciously fast)",
  TIME_SUM = "Total time spent (seconds)", DEG_TIME = "Degradation points for fast completion",
  ResponseId = "Response ID (Qualtrics)", StartDate = "Start time (Qualtrics)", EndDate = "End time (Qualtrics)",
  Status = "Response type (Qualtrics)", IPAddress = "IP address (Qualtrics)", Progress = "Progress in % (Qualtrics)",
  `Duration (in seconds)` = "Duration in seconds (Qualtrics)", Finished = "Completed (Qualtrics, 1 = yes)",
  RecordedDate = "Recorded time (Qualtrics)", DistributionChannel = "Distribution channel (Qualtrics)", UserLanguage = "Language (Qualtrics)",
  id = "Response ID (LimeSurvey)", submitdate = "Submission time (LimeSurvey; empty = not completed)", lastpage = "Last page (LimeSurvey)",
  startlanguage = "Start language (LimeSurvey)", seed = "Random seed (LimeSurvey)", startdate = "Start time (LimeSurvey)", datestamp = "Last action time (LimeSurvey)")
qc_is_system_var <- function(v) v %in% names(qc_system_vars) | grepl("^TIME[0-9]{3}$", v)
qc_system_label <- function(v) if (v %in% names(qc_system_vars)) qc_system_vars[[v]] else if (grepl("^TIME[0-9]{3}$", v)) paste("Time spent on page", as.integer(sub("TIME", "", v))) else NA_character_

qc_cb_entry <- function(variable, label = NA_character_, values = NULL, page = NA_integer_, question = NA_character_, kind = NULL) {
  values <- if (is.null(values)) character() else values
  values <- values[!is.na(names(values)) & nzchar(names(values)) & names(values) != "NA" & !is.na(values)]
  codes <- suppressWarnings(as.numeric(names(values)))
  miss <- names(values)[grepl(qc_missing_label_re, values, perl = TRUE)]
  valid <- values[!names(values) %in% miss]
  if (is.null(kind)) kind <- if (length(valid) == 1 && grepl("(?i)free text|open text|text entry", valid, perl = TRUE)) "text"
    else if (length(valid) == 2 && all(!is.na(suppressWarnings(as.numeric(names(valid))))) && diff(range(as.numeric(names(valid)))) > 1) "range"
    else if (length(valid)) "codes" else "none"
  if (kind == "text") valid <- character()
  list(variable = variable, label = if (is.null(label) || is.na(label)) NA_character_ else trimws(gsub("\\s+", " ", label)),
       values = valid, missing = miss, kind = kind, page = page, question = question)
}

qc_codebook_from_list <- function(rows, source, rules = list(), pages = NULL, problems = character()) {
  vars <- list()
  for (r in rows) vars[[r$variable]] <- qc_cb_entry(r$variable, r$label, r$values, if (is.null(r$page)) NA_integer_ else r$page,
                                                   if (is.null(r$question)) NA_character_ else r$question, r$kind)
  list(vars = vars, rules = rules, pages = pages, source = source, problems = problems)
}

# ---------------------------------------------------------------- Word files
# Reads a .docx straight from its XML (word/document.xml), so the result does not depend on the officer version:
# paragraphs outside tables in document order, every paragraph (incl. table cells) for plain text, and each table as
# a data frame whose first row is the header. Several paragraphs in one cell are joined with "; ".
qc_docx_read <- function(path) {
  dir <- tempfile("qc_docx_"); on.exit(unlink(dir, recursive = TRUE))
  utils::unzip(path, files = "word/document.xml", exdir = dir)
  x <- xml2::read_xml(file.path(dir, "word", "document.xml")); ns <- xml2::xml_ns(x)
  ptext <- function(p) paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", ns)), collapse = "")
  body <- xml2::xml_find_first(x, "//w:body", ns)
  paras <- vapply(xml2::xml_find_all(body, "./w:p", ns), ptext, "")
  all_paras <- vapply(xml2::xml_find_all(body, ".//w:p", ns), ptext, "")
  tables <- lapply(xml2::xml_find_all(body, ".//w:tbl", ns), function(tb) {
    rows <- lapply(xml2::xml_find_all(tb, "./w:tr", ns), function(tr)
      vapply(xml2::xml_find_all(tr, "./w:tc", ns), function(tc) { ps <- vapply(xml2::xml_find_all(tc, "./w:p", ns), ptext, ""); paste(ps[nzchar(trimws(ps))], collapse = "; ") }, ""))
    rows <- rows[lengths(rows) > 0]; if (length(rows) < 2) return(NULL)
    k <- max(lengths(rows)); m <- t(vapply(rows, function(z) c(z, rep("", k - length(z)))[seq_len(k)], character(k)))
    df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE); names(df) <- make.unique(ifelse(nzchar(m[1, ]), m[1, ], paste0("col", seq_len(k)))); df })
  list(paragraphs = paras[nzchar(trimws(paras))], text = all_paras[nzchar(trimws(all_paras))], tables = Filter(Negate(is.null), tables))
}

# ---------------------------------------------------------------- text extraction
qc_codebook_lines <- function(path) {
  ext <- tolower(tools::file_ext(path))
  txt <- switch(ext,
    pdf = { t <- pdftools::pdf_text(path); paste(t, collapse = "\n") },
    docx = paste(qc_docx_read(path)$text, collapse = "\n"),
    html = , htm = { h <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      h <- gsub("(?i)<(br|/p|/div|/tr|/li|/h[1-6])[^>]*>", "\n", h, perl = TRUE); h <- gsub("<[^>]+>", " ", h)
      h <- gsub("&nbsp;", " ", h); h <- gsub("&amp;", "&", h); gsub("&lt;", "<", gsub("&gt;", ">", h)) },
    { enc <- qc_detect_encoding(path); qc_text_file(path, enc) })
  strsplit(gsub("\r", "", txt), "\n", fixed = TRUE)[[1]]
}

# ---------------------------------------------------------------- generic text parser
# Reads lines of the form "VARNAME Label" followed by "code = label" lines (SoSci Survey variable view,
# most exported codebooks, and the QuantCompass text template). Battery items listed together share
# the value block that follows them. With `known` (the data's column names), only those names count
# as variables, which prevents answer options being read as variables.
qc_parse_codebook_text <- function(lines, known = NULL, source = "codebook") {
  var_re <- "^(\\s*)([A-Za-z][A-Za-z0-9_.]*)\\s+(\\S.*)$"
  val_re <- "^\\s*(-?[0-9]+(?:\\.[0-9]+)?)\\s*=\\s*(.*?)\\s*$"
  page_re <- "^\\s*(Page\\s+([0-9]+)|Last Page)\\s*$"
  rows <- list(); pending <- character(); cur <- NULL; cur_indent <- 0; in_values <- FALSE
  page <- NA_integer_; pages <- data.frame(page = integer(), page_id = character()); expect_page_id <- FALSE
  php <- character(); in_php <- FALSE; php_blocks <- list()
  flush_values <- function(vals) {
    for (v in pending) rows[[v]]$values <<- vals
    pending <<- character()
  }
  vals <- character(); q_text <- NA_character_; in_q <- FALSE; q_indent <- 0
  q_re <- "^(\\s{0,12})([0-9]{1,3})\\.\\s+(\\S.*)$"
  close_block <- function() { if (length(pending) && length(vals)) flush_values(vals); vals <<- character(); in_values <<- FALSE }
  for (i in seq_along(lines)) {
    ln <- lines[i]
    if (grepl("^\\s*$", ln)) { in_q <- FALSE; next }
    if (grepl("^https?://|^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}, ", trimws(ln))) next
    if (!in_php && grepl(q_re, ln) && !grepl(val_re, ln)) {
      close_block(); cur <- NULL
      q_text <- sub("\\s{3,}[A-Z]{1,6}[0-9]{1,3}\\s*$", "", sub(q_re, "\\3", ln)); q_indent <- nchar(sub(q_re, "\\1", ln)); in_q <- TRUE; next
    }
    if (in_q) { ind <- nchar(sub("^(\\s*).*$", "\\1", ln))
      if (ind == q_indent) { q_text <- paste(q_text, sub("\\s{3,}[A-Z]{1,6}[0-9]{1,3}\\s*$", "", trimws(ln))); next }
      in_q <- FALSE }
    if (grepl(page_re, ln)) {
      close_block(); cur <- NULL
      page <- if (grepl("Last Page", ln)) NA_integer_ else as.integer(sub(page_re, "\\2", ln))
      if (!is.na(page)) pages <- rbind(pages, data.frame(page = page, page_id = NA_character_))
      expect_page_id <- TRUE; next
    }
    if (expect_page_id) {
      expect_page_id <- FALSE
      tok <- trimws(ln)
      if (grepl("^[a-z][a-z0-9_]*$", tok) && !is.na(page)) { pages$page_id[nrow(pages)] <- tok; next }
    }
    if (grepl("^\\s*PHP code\\s*$", ln)) { close_block(); in_php <- TRUE; php <- character(); next }
    if (in_php) {
      php <- c(php, ln)
      opens <- sum(lengths(regmatches(php, gregexpr("\\{", php)))); closes <- sum(lengths(regmatches(php, gregexpr("\\}", php))))
      if (opens > 0 && opens == closes) { php_blocks[[length(php_blocks) + 1]] <- list(page = page, code = php); in_php <- FALSE }
      next
    }
    if (grepl(val_re, ln)) {
      if (!length(pending)) next
      in_values <- TRUE
      code <- sub(val_re, "\\1", ln); lab <- sub(val_re, "\\2", ln)
      vals[code] <- lab; next
    }
    if (grepl(var_re, ln)) {
      v <- sub(var_re, "\\2", ln); lab <- sub(var_re, "\\3", ln); ind <- nchar(sub(var_re, "\\1", ln))
      is_var <- if (!is.null(known)) v %in% known else grepl("[0-9]", v) && ind >= 0
      if (is_var) {
        if (in_values) close_block()
        rows[[v]] <- list(variable = v, label = lab, values = NULL, page = page, question = q_text)
        pending <- c(pending, v); cur <- v; cur_indent <- ind; next
      }
    }
    # free-text marker or continuation of a wrapped label
    if (!is.null(cur) && !in_values) {
      if (grepl("(?i)^\\s*(free text|open text)\\s*$", ln, perl = TRUE)) { rows[[cur]]$kind <- "text"; pending <- setdiff(pending, cur); next }
      ind <- nchar(sub("^(\\s*).*$", "\\1", ln))
      if (ind > cur_indent && ind <= cur_indent + 15 && !grepl("^[A-Z]{1,6}[0-9]{1,3}$", trimws(ln))) { rows[[cur]]$label <- paste(rows[[cur]]$label, trimws(ln)); next }
    }
    if (in_values) close_block()
    cur <- NULL
  }
  close_block()
  cb <- qc_codebook_from_list(rows, source = source, pages = pages)
  cb$rules <- qc_parse_php_rules(php_blocks, cb, pages)
  cb
}

# SoSci Survey PHP filters:  if (value('X') == 2) { goToPage('end'); }
# Only simple conditions (value comparisons joined by || and &&) are translated. Anything else is
# reported to the user instead of being guessed.
qc_parse_php_rules <- function(blocks, cb, pages) {
  out <- list()
  page_of <- vapply(cb$vars, function(e) as.numeric(e$page), numeric(1))
  for (b in blocks) {
    code <- paste(b$code[!grepl("^\\s*//", b$code)], collapse = " ")
    m <- regmatches(code, regexec("if\\s*\\((.*)\\)\\s*\\{.*goToPage\\('([^']+)'\\)", code))[[1]]
    if (length(m) < 3) { out[[length(out) + 1]] <- list(page = b$page, condition = NA, target = NA, vars_skipped = character(),
      text = trimws(code), screen_out = FALSE, readable = FALSE); next }
    cond_php <- m[2]; target <- m[3]
    cond <- gsub("value\\('([^']+)'\\)", 'data[["\\1"]]', cond_php)
    cond <- gsub("\\|\\|", "|", cond); cond <- gsub("&&", "&", cond)
    readable <- grepl('^[][A-Za-z0-9_"() .=!<>|&-]+$', cond) && !grepl("\\b(function|system|eval)\\b", cond)
    if (readable) { ok <- tryCatch({ parse(text = cond); TRUE }, error = function(e) FALSE); readable <- ok }
    # target page number
    tgt <- if (target == "end") Inf else if (target == "next") b$page + 1 else {
      p <- pages$page[!is.na(pages$page_id) & pages$page_id == target]; if (length(p)) p[1] else NA }
    skipped <- if (is.na(tgt) || is.na(b$page)) character() else names(page_of)[!is.na(page_of) & page_of >= b$page & page_of < tgt]
    skipped <- skipped[vapply(skipped, function(v) cb$vars[[v]]$kind != "none", logical(1))]
    out[[length(out) + 1]] <- list(page = b$page, condition = if (readable) cond else NA, condition_php = cond_php, target = target,
      vars_skipped = skipped, text = trimws(code), screen_out = identical(target, "end"), readable = readable && !is.na(tgt))
  }
  out
}

# ---------------------------------------------------------------- tables (Excel, CSV, Word tables)
qc_value_string <- function(s) {
  if (is.na(s) || !nzchar(trimws(s))) return(NULL)
  # codes may be numbers ("1 = Yes") or short words ("yes = remote workers"), separated by ; | newline, comma,
  # an ellipsis ("1 = Never ... 10 = Always") or a sentence break ("5 = Strongly agree. Correct = 4")
  parts <- trimws(unlist(strsplit(s, "[;|\n]|,(?=\\s*[^,=;]{1,30}\\s*=)|\\s*(?:\u2026|\\.\\.\\.)\\s*(?=-?[0-9]+\\s*=)|\\.\\s+(?=[A-Za-z][^=.;]{0,30}=)", perl = TRUE)))
  parts <- parts[grepl("^[^=]{1,30}=", parts) & !grepl("^\\s*=", parts)]
  if (!length(parts)) return(NULL)
  codes <- gsub("^[\"'\u201c\u201d]|[\"'\u201c\u201d]$", "", trimws(sub("\\s*=.*$", "", parts)))
  labs <- trimws(sub("^[^=]*=\\s*", "", parts))
  num <- grepl("^-?[0-9]+(\\.[0-9]+)?$", codes)
  if (any(num)) { codes <- codes[num]; labs <- labs[num] }     # numeric codes present: ignore wording such as "Correct = 4"
  stats::setNames(labs, codes)
}

# Codebooks laid out as a table (columns such as Variable | Label | Type | Value codes | Page), e.g. a PDF
# made in Word. Column positions are taken from the header row, so wrapped cells are joined correctly.
qc_parse_codebook_table_lines <- function(lines, known, source) {
  hdr <- grep("^\\s*Variable\\s{2,}.*(Label|Value)", lines)
  if (!length(hdr)) return(NULL)
  rows <- list(); cur <- NULL; cols <- NULL; refs <- list()
  colpos <- function(h) { m <- gregexpr("\\S+(?: \\S+)*", h)[[1]]; data.frame(start = as.integer(m), name = tolower(regmatches(h, list(m))[[1]])) }
  slice <- function(ln, a, b) if (nchar(ln) < a) "" else trimws(substr(ln, a, if (is.na(b)) nchar(ln) else b - 1))
  flush <- function() if (!is.null(cur)) rows[[cur$v]] <<- cur
  for (i in seq_along(lines)) {
    ln <- lines[i]
    if (i %in% hdr) { flush(); cur <- NULL; cols <- colpos(ln); next }
    if (is.null(cols) || !nzchar(trimws(ln))) { if (!nzchar(trimws(ln))) NULL; next }
    if (grepl("^\\s*Codebook\\b|^\\s*[0-9]+(\\.[0-9]+)*\\.?\\s+[A-Z][a-z]", ln)) { flush(); cur <- NULL; if (grepl("^\\s*[0-9]+(\\.[0-9]+)*\\.?\\s+[A-Z][a-z]", ln)) cols <- NULL; next }   # "4. Variable list", "4.4 Computed variables"
    ic <- function(pat) { k <- which(grepl(pat, cols$name)); if (length(k)) k[1] else NA }
    il <- ic("label|wording|question|description"); iv <- ic("value|code"); ivar <- 1
    nxt <- function(k) if (k < nrow(cols)) cols$start[k + 1] else NA
    first <- slice(ln, 1, if (!is.na(il)) cols$start[il] else NA)
    v <- sub("\\s.*$", "", first)
    starts_var <- nzchar(v) && (is.null(known) || v %in% known) && grepl("^[A-Za-z][A-Za-z0-9_.]*$", v)
    if (starts_var) { flush(); cur <- list(variable = v, v = v, label = "", vtxt = "", page = NA) }
    if (is.null(cur)) next
    if (!is.na(il)) cur$label <- trimws(paste(cur$label, slice(ln, cols$start[il], nxt(il))))
    if (!is.na(iv)) cur$vtxt <- trimws(paste(cur$vtxt, slice(ln, cols$start[iv], nxt(iv))))
  }
  flush()
  if (!length(rows)) return(NULL)
  out <- lapply(rows, function(r) {
    vals <- qc_value_string(gsub("\\s+", " ", r$vtxt))
    ref <- regmatches(r$vtxt, regexec("^as\\s+([A-Za-z][A-Za-z0-9_]*)", r$vtxt))[[1]]
    list(variable = r$variable, label = r$label, values = vals, ref = if (length(ref) == 2) ref[2] else NA,
         kind = if (grepl("(?i)^(text|string)|free text", r$vtxt, perl = TRUE)) "text" else NULL)
  })
  for (k in names(out)) if (!is.na(out[[k]]$ref) && !is.null(out[[out[[k]]$ref]])) out[[k]]$values <- out[[out[[k]]$ref]]$values
  qc_codebook_from_list(out, source = source)
}

# Filter rules written out in a codebook table, e.g.
#   "FC07 (page 5)   FC07 = 2 (no educational debt)   Page 6 skipped: FC08, FC09 and TIME006 are blank."
#   "SR01 (page 2)   SR01 = 2 (not an international student)   Sent to the end page."
qc_parse_documented_rules <- function(lines, known) {
  start <- grep("^\\s*([A-Z][A-Za-z0-9_]*)\\s+\\(page\\s*[0-9]+\\)\\s+\\S", lines)
  out <- list()
  for (k in start) {
    txt <- lines[k]; j <- k + 1
    while (j <= length(lines) && nzchar(trimws(lines[j])) && !j %in% start && !grepl("^\\s*[A-Z][A-Za-z0-9_]*\\s+\\(page", lines[j])) { txt <- paste(txt, trimws(lines[j])); j <- j + 1 }
    txt <- gsub("\\s+", " ", txt)
    m <- regmatches(txt, regexec("^\\s*([A-Z][A-Za-z0-9_]*)\\s+\\(page\\s*([0-9]+)\\)\\s+(.*)$", txt))[[1]]
    v <- m[2]; page <- as.integer(m[3]); rest <- m[4]
    cm <- regmatches(rest, regexec(sprintf("%s\\s*=\\s*(-?[0-9]+(?:\\s*(?:,|or)\\s*-?[0-9]+)*)", v), rest))[[1]]
    if (length(cm) < 2 || (!is.null(known) && !v %in% known)) next
    codes <- as.numeric(regmatches(cm[2], gregexpr("-?[0-9]+", cm[2]))[[1]])
    screen <- grepl("(?i)end page|sent to the end|screen-?out|goToPage\\W+end", rest, perl = TRUE)
    skipped <- character()
    if (!screen && grepl("(?i)skipped", rest, perl = TRUE)) {
      lst <- sub("(?i)^.*?skipped[^:]*:\\s*", "", rest, perl = TRUE); lst <- sub("(?i)\\s+(are|is|will be)\\s+(blank|empty|missing).*$", "", lst, perl = TRUE)
      items <- trimws(unlist(strsplit(lst, "\\s*,\\s*|\\s+and\\s+")))
      for (it in items) {
        rg <- regmatches(it, regexec("^([A-Za-z][A-Za-z0-9_]*)\\s*(?:\u2026|\\.\\.\\.|\u2013|-|to)\\s*([A-Za-z][A-Za-z0-9_]*)$", it))[[1]]
        if (length(rg) == 3 && !is.null(known) && all(rg[2:3] %in% known)) skipped <- c(skipped, known[match(rg[2], known):match(rg[3], known)])
        else if (it %in% known) skipped <- c(skipped, it)
      }
      skipped <- skipped[!qc_is_system_var(skipped)]
    }
    cond <- sprintf('data[["%s"]] %%in%% %s', v, if (length(codes) == 1) codes else paste0("c(", paste(codes, collapse = ", "), ")"))
    out[[length(out) + 1]] <- list(page = page, condition = cond, condition_php = sprintf("%s = %s", v, paste(codes, collapse = " or ")),
      target = if (screen) "end" else "later page", vars_skipped = unique(skipped), text = txt, screen_out = screen, readable = TRUE)
  }
  out
}

qc_codebook_table <- function(df, source) {
  nm <- tolower(trimws(names(df)))
  pick <- function(pats) { i <- which(Reduce(`|`, lapply(pats, function(p) grepl(p, nm)))); if (length(i)) i[1] else NA }
  iv <- pick(c("^var", "^name$", "^column", "^item$", "^code$", "variable"))
  il <- pick(c("^label", "meaning", "description", "question", "text"))
  ival <- pick(c("^values?$", "value label", "codes", "categories", "answer", "coding"))
  icode <- pick(c("^value$", "^code$", "^level$")); ivl <- pick(c("value.?label", "^category", "^label$"))
  imiss <- pick(c("missing"))
  if (is.na(iv)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # long format: one row per code (variable repeated, a value column and a value-label column)
  long <- !is.na(icode) && icode != iv && !is.na(ivl) && ivl != icode && any(duplicated(df[[iv]][!is.na(df[[iv]])]))
  rows <- list()
  if (long) {
    vv <- as.character(df[[iv]]); for (k in seq_along(vv)) if (is.na(vv[k]) || !nzchar(vv[k])) vv[k] <- if (k > 1) vv[k - 1] else NA
    for (v in unique(stats::na.omit(vv))) { sub <- df[vv %in% v, ]
      lab <- if (!is.na(il) && il != ivl) stats::na.omit(as.character(sub[[il]]))[1] else NA
      vals <- stats::setNames(as.character(sub[[ivl]]), as.character(sub[[icode]])); vals <- vals[!is.na(names(vals)) & nzchar(names(vals))]
      rows[[v]] <- list(variable = v, label = lab, values = vals) }
  } else for (k in seq_len(nrow(df))) {
    v <- trimws(as.character(df[[iv]][k])); if (is.na(v) || !nzchar(v)) next
    vals <- if (!is.na(ival)) qc_value_string(as.character(df[[ival]][k])) else NULL
    if (!is.na(imiss) && !is.na(df[[imiss]][k])) { mc <- trimws(unlist(strsplit(as.character(df[[imiss]][k]), "[,;]")))
      mc <- mc[nzchar(mc)]; if (length(mc)) vals <- c(vals, stats::setNames(rep("Not answered", length(mc)), mc)) }
    rows[[v]] <- list(variable = v, label = if (is.na(il)) NA else as.character(df[[il]][k]), values = vals)
  }
  qc_codebook_from_list(rows, source = source)
}

# ---------------------------------------------------------------- Qualtrics (.qsf) and LimeSurvey (.lss)
qc_codebook_qsf <- function(path) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  rows <- list(); problems <- character()
  strip <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", x)))
  for (el in j$SurveyElements) {
    if (!identical(el$Element, "SQ")) next
    p <- el$Payload; tag <- p$DataExportTag; qtext <- strip(p$QuestionText)
    ch <- p$Choices; rec <- p$RecodeValues
    codes <- function(ids) vapply(ids, function(id) if (!is.null(rec[[id]])) as.character(rec[[id]]) else id, character(1))
    if (!is.null(p$DisplayLogic) || !is.null(p$SkipLogic)) problems <- c(problems, sprintf("%s has display/skip logic in Qualtrics. It is listed for you, but not checked automatically.", tag))
    if (identical(p$QuestionType, "Matrix") && !is.null(p$Answers)) {
      vals <- stats::setNames(vapply(p$Answers, function(a) strip(a$Display), character(1)), codes(names(p$Answers)))
      for (id in names(ch)) rows[[paste0(tag, "_", id)]] <- list(variable = paste0(tag, "_", id), label = paste(qtext, "-", strip(ch[[id]]$Display)), values = vals)
    } else {
      vals <- if (length(ch)) stats::setNames(vapply(ch, function(a) strip(a$Display), character(1)), codes(names(ch))) else c("0" = "Free text")
      rows[[tag]] <- list(variable = tag, label = qtext, values = vals)
    }
  }
  qc_codebook_from_list(rows, source = paste("Qualtrics survey file", basename(path)), problems = unique(problems))
}

qc_codebook_lss <- function(path) {
  x <- xml2::read_xml(path)
  rd <- function(node, tbl) { rs <- xml2::xml_find_all(node, sprintf("//%s/rows/row", tbl))
    if (!length(rs)) return(data.frame())
    do.call(rbind, lapply(rs, function(r) { k <- xml2::xml_children(r); stats::setNames(as.data.frame(as.list(xml2::xml_text(k)), stringsAsFactors = FALSE), xml2::xml_name(k)) })) }
  q <- rd(x, "questions"); a <- rd(x, "answers"); sq <- rd(x, "subquestions"); l10 <- rd(x, "question_l10ns"); al10 <- rd(x, "answer_l10ns")
  if (!nrow(q)) return(NULL)
  qtext <- function(qid) { if ("question" %in% names(q)) return(q$question[q$qid == qid][1])
    if (nrow(l10)) l10$question[l10$qid == qid][1] else NA }
  ans <- function(qid) { s <- a[a$qid == qid, , drop = FALSE]; if (!nrow(s)) return(NULL)
    lab <- if ("answer" %in% names(s)) s$answer else al10$answer[match(s$aid, al10$aid)]
    stats::setNames(gsub("<[^>]+>", "", lab), s$code) }
  rows <- list(); problems <- character()
  top <- if ("parent_qid" %in% names(q)) q[q$parent_qid %in% c("0", ""), ] else q
  for (k in seq_len(nrow(top))) {
    qq <- top[k, ]; tag <- qq$title; vals <- ans(qq$qid)
    if ("relevance" %in% names(qq) && !is.na(qq$relevance) && !qq$relevance %in% c("", "1"))
      problems <- c(problems, sprintf("%s is shown only if: %s (LimeSurvey condition, listed for you to check).", tag, qq$relevance))
    subs <- if (nrow(sq)) sq[sq$parent_qid == qq$qid, ] else if ("parent_qid" %in% names(q)) q[q$parent_qid == qq$qid, ] else data.frame()
    lab <- gsub("\\s+", " ", gsub("<[^>]+>", " ", qtext(qq$qid)))
    sqtext <- function(r) { if ("question" %in% names(r) && !is.na(r$question) && nzchar(r$question)) return(r$question)
      if (nrow(l10)) { z <- l10$question[l10$qid == r$qid]; if (length(z)) return(z[1]) }; NA }
    if (nrow(subs)) for (s in seq_len(nrow(subs))) { v <- paste0(tag, "_", subs$title[s]); rows[[v]] <- list(variable = v, label = paste(lab, "-", gsub("<[^>]+>", "", sqtext(subs[s, , drop = FALSE]))), values = vals) }
    else rows[[tag]] <- list(variable = tag, label = lab, values = vals)
  }
  qc_codebook_from_list(rows, source = paste("LimeSurvey structure file", basename(path)), problems = unique(problems))
}

# ---------------------------------------------------------------- dispatcher
qc_read_any_codebook_legacy <- function(path, data = NULL) {
  ext <- tolower(tools::file_ext(path)); known <- if (!is.null(data)) names(data) else NULL
  src <- basename(path)
  cb <- tryCatch(switch(ext,
    qsf = qc_codebook_qsf(path), lss = qc_codebook_lss(path),
    sav = , zsav = , por = , dta = , sas7bdat = qc_embedded_codebook(path),
    xlsx = , xls = , xlsm = { sh <- readxl::excel_sheets(path)
      pref <- sh[grepl("(?i)codebook|variables|labels|dictionary", sh, perl = TRUE)]; sh <- c(pref, setdiff(sh, pref))
      res <- NULL; for (s in sh) { res <- qc_codebook_table(readxl::read_excel(path, sheet = s), paste0(src, " (sheet ", s, ")")); if (!is.null(res) && length(res$vars)) break }
      res },
    csv = , tsv = { f <- qc_text_format(path)
      df <- utils::read.delim(text = qc_text_file(path, f$encoding), sep = f$sep, colClasses = "character", check.names = FALSE)
      qc_codebook_table(df, src) },
    docx = { res <- NULL
      for (df in qc_docx_read(path)$tables) { res <- qc_codebook_table(df, paste(src, "(table)")); if (!is.null(res) && length(res$vars)) break }
      if (is.null(res) || !length(res$vars)) res <- qc_parse_codebook_text(qc_codebook_lines(path), known, src)
      res },
    { lines <- qc_codebook_lines(path)
      if (ext == "pdf" && sum(nchar(trimws(lines))) < 50)
        return(list(vars = list(), rules = list(), source = src, problems = "This PDF has no readable text (it is probably a scan). QuantCompass does not guess from images: please upload the codebook as Word, Excel, CSV or the survey tool's export."))
      cb <- qc_parse_codebook_table_lines(lines, known, src)
      if (is.null(cb) || !length(cb$vars)) cb <- qc_parse_codebook_text(lines, known, src)
      if (!length(cb$rules)) cb$rules <- qc_parse_documented_rules(lines, known)
      cb }),
    error = function(e) list(vars = list(), rules = list(), source = src, problems = paste("The codebook could not be read:", conditionMessage(e))))
  if (is.null(cb) || !length(cb$vars)) {
    cb <- if (is.null(cb)) list(vars = list(), rules = list(), source = src, problems = character()) else cb
    cb$problems <- unique(c(cb$problems, "No variables were found in this codebook. Check that it lists variable names (as in the data) with their labels and codes."))
  }
  cb
}

# ---------------------------------------------------------------- check against the data
qc_value_summary <- function(e) {
  if (is.null(e)) return("")
  if (e$kind == "range") { k <- names(e$values); return(sprintf("%s = %s ... %s = %s", k[1], e$values[1], k[2], e$values[2])) }
  if (e$kind == "text") return("free text")
  if (!length(e$values)) return("")
  s <- paste(sprintf("%s = %s", names(e$values), e$values), collapse = "; ")
  if (nchar(s) > 140) paste0(substr(s, 1, 137), "...") else s
}

# Returns a table with one row per data column (plus codebook variables missing from the data).
# status: "ok" (✓), "warn" (⚠, needs a decision) or "missing" (✗, not in the codebook).
# Suggested actions (na_codes) are listed but only applied when the user accepts them.
qc_codebook_check <- function(cb, data) {
  rows <- list()
  for (v in names(data)) {
    e <- cb$vars[[v]]; x <- data[[v]]; obs <- sort(unique(x[!is.na(x)]))
    if (is.null(e)) {
      if (qc_is_system_var(v)) rows[[v]] <- data.frame(variable = v, status = "ok", label = qc_system_label(v), values = "", missing_codes = "",
        message = "Recognised as a standard column (case ID, time or completion flag).", na_codes = "", na_labels = "")
      else rows[[v]] <- data.frame(variable = v, status = "missing", label = NA, values = "", missing_codes = "",
        message = "Not described in the codebook. Name it yourself or leave it unused.", na_codes = "", na_labels = "")
      next
    }
    msg <- character(); status <- "ok"
    if ((is.na(e$label) || !nzchar(e$label)) && qc_is_system_var(v)) e$label <- qc_system_label(v)
    if (is.na(e$label) || !nzchar(e$label)) { msg <- c(msg, "No label in the codebook."); status <- "warn" }
    na_codes <- character(); na_labels <- character()
    if (e$kind %in% c("codes", "range") && is.numeric(x)) {
      codes <- suppressWarnings(as.numeric(names(e$values))); mc <- suppressWarnings(as.numeric(e$missing))
      allowed <- if (e$kind == "range") c(seq(min(codes), max(codes)), mc) else c(codes, mc)
      bad <- setdiff(obs, allowed)
      if (length(bad)) { status <- "warn"; msg <- c(msg, sprintf("Value(s) %s occur in the data but are not codes in the codebook.", paste(bad, collapse = ", "))) }
      nl <- e$values[grepl(qc_na_like_re, e$values, perl = TRUE)]
      if (length(nl)) { status <- "warn"; na_codes <- names(nl); na_labels <- unname(nl)
        msg <- c(msg, sprintf("Code %s means '%s'. It is not a score - treat it as missing for analysis?", paste(names(nl), collapse = ", "), paste(nl, collapse = "', '"))) }
    } else if (e$kind == "codes" && !is.numeric(x)) {
      bad <- setdiff(as.character(obs), c(names(e$values), e$missing))
      if (length(bad) && length(bad) < 20) { status <- "warn"; msg <- c(msg, sprintf("Value(s) %s are not listed in the codebook.", paste(utils::head(bad, 5), collapse = ", "))) }
    }
    if (!length(msg)) msg <- if (length(e$missing)) sprintf("Matches. Code %s = no answer, set to missing.", paste(e$missing, collapse = ", ")) else "Matches."
    rows[[v]] <- data.frame(variable = v, status = status, label = e$label, values = qc_value_summary(e), missing_codes = paste(e$missing, collapse = ", "),
                            message = paste(msg, collapse = " "), na_codes = paste(na_codes, collapse = ", "), na_labels = paste(na_labels, collapse = "; "))
  }
  extra <- setdiff(names(cb$vars), names(data)); extra <- extra[vapply(extra, function(v) cb$vars[[v]]$kind != "none", logical(1))]
  for (v in extra) rows[[paste0("cb_", v)]] <- data.frame(variable = v, status = "warn", label = cb$vars[[v]]$label, values = qc_value_summary(cb$vars[[v]]),
    missing_codes = "", message = "In the codebook but not in the data file (check the export).", na_codes = "", na_labels = "")
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out$symbol <- c(ok = "✓", warn = "⚠", missing = "✗")[out$status]
  out
}

# Code that applies the codebook: missing codes to NA, accepted "does not apply" codes to NA.
# Also returns friendly names and category labels used in sentences and tables.
qc_codebook_apply <- function(cb, data, accept_na = character()) {
  code <- character(); labels <- list(); level_labels <- list()
  by_codes <- list()
  for (v in intersect(names(data), names(cb$vars))) {
    e <- cb$vars[[v]]
    if (!is.na(e$label) && nzchar(e$label)) labels[[v]] <- e$label
    if (e$kind == "codes" && length(e$values)) {
      lv <- e$values; if (v %in% accept_na) lv <- lv[!grepl(qc_na_like_re, lv, perl = TRUE)]
      level_labels[[v]] <- lv
    }
    mc <- e$missing
    if (v %in% accept_na) mc <- c(mc, names(e$values)[grepl(qc_na_like_re, e$values, perl = TRUE)])
    if (length(mc) && is.numeric(data[[v]])) { key <- paste(mc, collapse = ", "); by_codes[[key]] <- c(by_codes[[key]], v) }
  }
  for (key in names(by_codes)) {
    vs <- by_codes[[key]]
    code <- c(code, sprintf("for (v in c(%s)) data[[v]][data[[v]] %%in%% c(%s)] <- NA   # codebook: %s",
                            paste0('"', vs, '"', collapse = ", "), key, if (grepl(",", key) || length(vs) < 3) "no answer / does not apply" else "not answered"))
  }
  list(code = code, labels = labels, level_labels = level_labels)
}
