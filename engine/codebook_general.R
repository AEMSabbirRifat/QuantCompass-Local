# General codebook reading: works for any layout by
#   1. turning every file into plain text lines and real tables (PDF tables are rebuilt from word positions),
#   2. running several readers (tables, variable mentions in text, SPSS syntax, SoSci view, survey-tool files),
#   3. keeping, for every variable, the version that fits the actual data best, and
#   4. collecting missing-value codes and survey rules wherever they are written.
# The data's own column names anchor everything, so answer options are never mistaken for variables.

# ---------------------------------------------------------------- answer codes in any notation
qc_code_part <- function(p) {
  p <- trimws(gsub("\\s+", " ", p)); if (!nzchar(p) || nchar(p) > 160) return(NULL)
  m <- regmatches(p, regexec("^\\[?(-?[0-9]+(?:\\.[0-9]+)?)\\]?\\s*(?:=|:|\\)|\\.(?=\\s)|\u2013|\u2014|-(?=\\s*[A-Za-z\"']))\\s*(.+)$", p, perl = TRUE))[[1]]
  if (length(m) == 3) return(c(m[2], m[3]))
  m <- regmatches(p, regexec("^\\[(-?[0-9]+)\\]\\s*(.+)$", p, perl = TRUE))[[1]]; if (length(m) == 3) return(c(m[2], m[3]))
  m <- regmatches(p, regexec("^(.*?[A-Za-z].*?)\\s*[\\(\\[](-?[0-9]+)[\\)\\]]$", p, perl = TRUE))[[1]]; if (length(m) == 3 && nchar(m[2]) <= 60) return(c(m[3], m[2]))
  NULL
}
qc_codes_norm <- function(s) {
  s <- gsub("<br\\s*/?>", "; ", s, ignore.case = TRUE)
  s <- gsub("(^|[;|,\\s])-\\s+([0-9]+\\s*=)", "\\1-\\2", s, perl = TRUE)          # "-\n9 = Not answered" split by a line break
  s <- gsub("([0-9]+)-\\s+([0-9]+)", "\\1-\\2", s, perl = TRUE)                    # "11-\n20 hours"
  gsub("\\s+", " ", s)
}
# All codes in a piece of text, in any common notation; numeric codes win over word codes
qc_parse_codes <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(trimws(s))) return(NULL)
  s <- qc_codes_norm(s)
  # a filter condition ("blank unless DM02 = 5", "only if JOB = 1") is not a list of answer codes
  if (grepl("(?i)^\\s*(?:blank|empty|missing|shown|asked|answered|only|filled)?\\s*(?:unless|only if|if|when)\\s+[A-Za-z][A-Za-z0-9_\\[\\]]*\\s*(?:==?|!=|is|>|<)", s, perl = TRUE)) return(NULL)
  chunks <- unlist(strsplit(s, "\\s*[;|\n]\\s*|\\s+/\\s+(?=\\[?-?[0-9]+\\]?\\s*[=:])", perl = TRUE))
  parts <- unlist(lapply(chunks, function(ch) {
    if (!nzchar(trimws(ch))) return(character())
    # split a chunk that holds several codes: before "k =", "k:", "[k]", after "label (k)", at an ellipsis or a sentence break
    trimws(unlist(strsplit(ch, paste0(",\\s*(?=\\[?-?[0-9]+\\]?\\s*(?:=|:|\\)))|(?<=\\))\\s*,\\s*|(?<=[0-9]\\])\\s*,\\s*|\\s*(?:\u2026|\\.\\.\\.)\\s*(?=-?[0-9]+\\s*=)|",
      "\\.\\s+(?=[A-Za-z][^=.;]{0,30}=)|\\s+(?=-?[0-9]+\\s*=\\s*[A-Za-z\"'])"), perl = TRUE)))
  }))
  hits <- lapply(parts, qc_code_part); hits <- hits[!vapply(hits, is.null, logical(1))]
  if (!length(hits)) return(qc_value_string(s))
  codes <- vapply(hits, `[`, "", 1); labs <- trimws(sub("[,;]\\s*$", "", vapply(hits, `[`, "", 2)))
  labs <- trimws(sub("\\s+(?:p\\.|page)\\s*[0-9]+\\b.*$|\\s+\u00b7\\s+.*$|\\s+(?:only|shown only|asked only) (?:if|when) .*$", "", labs, perl = TRUE))   # spill-over from page/filter columns
  ok <- nzchar(labs) & !is.na(codes) & nzchar(codes); codes <- codes[ok]; labs <- labs[ok]; if (!length(codes)) return(NULL)
  keep <- !duplicated(codes); if (sum(keep) < 2 && !any(grepl(qc_missing_label_re, labs, perl = TRUE))) return(NULL)
  stats::setNames(labs[keep], codes[keep])
}

# ---------------------------------------------------------------- turning files into lines and tables
qc_tables_from_lines <- function(lines) {          # Markdown / pipe tables written in text
  idx <- grep("^\\s*\\|.*\\|\\s*$", lines); if (length(idx) < 3) return(list())
  runs <- split(idx, cumsum(c(1, diff(idx) != 1))); out <- list()
  for (r in runs) { rows <- lines[r]; rows <- rows[!grepl("^\\s*\\|?\\s*:?-{2,}", rows)]
    cells <- lapply(rows, function(x) trimws(strsplit(sub("^\\s*\\|", "", sub("\\|\\s*$", "", x)), "\\|")[[1]]))
    k <- max(lengths(cells)); m <- t(vapply(cells, function(z) c(z, rep("", k - length(z)))[seq_len(k)], character(k)))
    df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE); names(df) <- m[1, ]; out[[length(out) + 1]] <- df }
  out
}

qc_tables_from_html <- function(path) {
  h <- tryCatch(xml2::read_html(path), error = function(e) NULL); if (is.null(h)) return(list())
  lapply(xml2::xml_find_all(h, "//table"), function(t) {
    rows <- xml2::xml_find_all(t, ".//tr")
    cells <- lapply(rows, function(r) { cs <- xml2::xml_find_all(r, "./th|./td")
      vapply(cs, function(c) { xml2::xml_find_all(c, ".//br") |> lapply(function(b) xml2::xml_add_sibling(b, "span", "; ")); trimws(xml2::xml_text(c)) }, "") })
    k <- max(lengths(cells)); m <- t(vapply(cells, function(z) c(z, rep("", k - length(z)))[seq_len(k)], character(k)))
    df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE); names(df) <- m[1, ]; df })
}

# PDF tables rebuilt from word positions: the header row names the columns, column borders are placed in the
# empty vertical gaps between cell contents (works for left-, right- or centre-aligned cells), and a known
# variable name in the first column starts a new row
qc_tables_from_pdf <- function(path, known) {
  pages <- tryCatch(pdftools::pdf_data(path), error = function(e) list()); if (!length(pages)) return(list())
  hdr_re <- "^(variable|variables|name|var|label|question|questions|values|value|codes|code|filter|type|description|wording|item|answer|answers|categories|missing|notes?|page|scale|level)$"
  rows <- list(); cols <- NULL; bounds <- NULL; cur <- NULL; tables <- list()
  flush <- function() if (!is.null(cur)) rows[[length(rows) + 1]] <<- cur
  save_table <- function() { if (length(rows)) { m <- do.call(rbind, rows); df <- as.data.frame(m, stringsAsFactors = FALSE); names(df) <- cols$name
    tables[[length(tables) + 1]] <<- df }; rows <<- list() }
  for (w in pages) {
    if (!nrow(w)) next
    w <- w[order(w$y, w$x), ]; w$line <- cumsum(c(1, diff(w$y) > 3))
    hdr_line <- NA
    for (ln in unique(w$line)) { L <- w[w$line == ln, ]; low <- tolower(gsub("[^A-Za-z]", "", L$text))
      gaps <- sum((L$x[-1] - (L$x + L$width)[-nrow(L)]) > 25)
      if (sum(grepl(hdr_re, low)) >= 2 && any(low %in% c("variable", "variables", "name", "var")) && mean(grepl(hdr_re, low[nzchar(low)])) >= .5 && gaps >= 1) { hdr_line <- ln; break } }
    if (!is.na(hdr_line)) { L <- w[w$line == hdr_line, ]
      g <- cumsum(c(1, (L$x[-1] - (L$x + L$width)[-nrow(L)]) > 25))
      newcols <- do.call(rbind, lapply(split(L, g), function(z) data.frame(name = paste(z$text, collapse = " "), x0 = min(z$x), x1 = max(z$x + z$width))))
      if (!is.null(cols) && !identical(newcols$name, cols$name)) { flush(); cur <- NULL; save_table() }       # a different table starts
      cols <- newcols
      body <- w[w$line > hdr_line, ] } else body <- w
    if (is.null(cols) || !nrow(body)) next
    cover <- integer(ceiling(max(body$x + body$width, cols$x1)) + 2)
    for (k in seq_len(nrow(body))) cover[(floor(body$x[k]) + 1):(ceiling(body$x[k] + body$width[k]) + 1)] <- 1L
    ctr <- (cols$x0 + cols$x1) / 2
    bounds <- c(-Inf, vapply(seq_len(nrow(cols) - 1), function(i) {
      a <- floor(ctr[i]) + 1; b <- min(length(cover), ceiling(ctr[i + 1]) + 1); z <- which(cover[a:b] == 0) + a - 1
      if (!length(z)) return((ctr[i] + ctr[i + 1]) / 2)
      runs <- split(z, cumsum(c(1, diff(z) != 1))); r <- runs[[which.max(lengths(runs))]]; mean(r) - 1 }, 0), Inf)
    for (ln in unique(body$line)) {      # a row may continue on the next page (below a repeated header)
      L <- body[body$line == ln, ]; col <- findInterval(L$x + L$width / 2, bounds, left.open = TRUE)
      txt <- tapply(L$text, col, function(z) paste(z, collapse = " "))
      first <- if ("1" %in% names(txt)) trimws(txt[["1"]]) else ""
      if (nzchar(first) && sub("\\s.*$", "", first) %in% known) { flush(); cur <- stats::setNames(rep("", nrow(cols)), cols$name) }
      if (is.null(cur)) next
      for (k in names(txt)) { i <- as.integer(k); sepr <- if (grepl("[A-Za-z0-9]-$|^-$|\\s-$", cur[i]) ) "" else " "
        cur[i] <- trimws(paste(cur[i], txt[[k]], sep = sepr)) }
    }
  }
  flush(); save_table()
  tables
}

qc_cb_extract <- function(path, known) {
  ext <- tolower(tools::file_ext(path)); lines <- character(); tables <- list()
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    for (s in readxl::excel_sheets(path)) { m <- as.matrix(suppressMessages(readxl::read_excel(path, sheet = s, col_names = FALSE, col_types = "text")))
      if (!length(m)) next
      h <- qc_header_row(m)
      if (any(trimws(m[h + 1, ]) %in% known)) {        # the first row already describes a variable: the sheet has no header row
        df <- as.data.frame(m[(h + 1):nrow(m), , drop = FALSE], stringsAsFactors = FALSE); names(df) <- paste0("col", seq_len(ncol(m))) }
      else { df <- as.data.frame(m[-seq_len(h + 1), , drop = FALSE], stringsAsFactors = FALSE)
        names(df) <- ifelse(is.na(m[h + 1, ]), paste0("col", seq_len(ncol(m))), m[h + 1, ]) }
      tables[[s]] <- df }
    return(list(lines = lines, tables = tables))
  }
  if (ext %in% c("csv", "tsv")) {
    f <- qc_text_format(path)
    df <- utils::read.delim(text = qc_text_file(path, f$encoding), sep = f$sep, skip = f$skip, colClasses = "character", check.names = FALSE, quote = "\"", fill = TRUE)
    return(list(lines = character(), tables = list(df)))
  }
  if (ext == "docx") {
    dx <- qc_docx_read(path)
    return(list(lines = unlist(strsplit(dx$paragraphs, "\n")), tables = dx$tables))
  }
  if (ext %in% c("html", "htm")) tables <- qc_tables_from_html(path)
  if (ext == "pdf") tables <- qc_tables_from_pdf(path, known)
  lines <- qc_codebook_lines(path)
  c(list(lines = lines), list(tables = c(tables, qc_tables_from_lines(lines))))
}

# ---------------------------------------------------------------- table reader (columns identified from the data)
qc_cb_from_table2 <- function(df, known, source) {
  if (!nrow(df) || !ncol(df)) return(NULL)
  df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) })
  kl <- tolower(known); clean <- function(x) tolower(gsub("^[`*\"']+|[`*\"':]+$", "", x))
  hit <- vapply(df, function(x) { x <- x[nzchar(x)]; if (!length(x)) 0 else mean(clean(sub("\\s.*$", "", x)) %in% kl) }, 0)
  if (max(hit) < .3) return(NULL)
  iv <- which.max(hit); hn <- tolower(names(df))
  vv <- df[[iv]]; for (k in seq_along(vv)) if (!nzchar(vv[k]) && k > 1) vv[k] <- vv[k - 1]          # merged / continued cells
  vv <- known[match(clean(sub("\\s.*$", "", vv)), kl)]
  other <- setdiff(seq_along(df), iv)
  numcodes <- function(z) { r <- qc_parse_codes(z); !is.null(r) && length(r) >= 2 && mean(grepl("^-?[0-9]", names(r))) > .5 }
  codeish <- vapply(other, function(j) { x <- df[[j]][nzchar(df[[j]])]; if (!length(x)) 0 else mean(vapply(x, numcodes, logical(1))) }, 0)
  intish <- vapply(other, function(j) { x <- df[[j]][nzchar(df[[j]])]; if (!length(x)) 0 else mean(grepl("^-?[0-9]+$", x)) }, 0)
  textlen <- vapply(other, function(j) mean(nchar(df[[j]])), 0)
  filt <- other[grepl("filter|condition|skip|logic|routing|shown|display", hn[other]) |
                vapply(other, function(j) mean(grepl("(?i)\\b(only if|shown if|skipped|if [A-Za-z0-9_]+ ?=|survey ends|screen-?out)\\b", df[[j]], perl = TRUE)) > .15, logical(1))]
  miss_col <- other[grepl("missing", hn[other])]
  long <- anyDuplicated(vv[!is.na(vv)]) > 0 && any(intish > .6)
  rows <- list(); rules <- list()
  if (long) {
    jc <- other[which.max(intish)]
    rest <- setdiff(other, c(jc, filt, miss_col))
    lab_by_var <- function(j) all(tapply(df[[j]], vv, function(z) length(unique(z[nzchar(z)])) <= 1), na.rm = TRUE)
    varlab <- rest[vapply(rest, lab_by_var, logical(1))]; varlab <- if (length(varlab)) varlab[which.max(textlen[match(varlab, other)])] else NA
    jl <- setdiff(rest, varlab); jl <- if (length(jl)) jl[which.max(textlen[match(jl, other)])] else NA
    for (v in unique(stats::na.omit(vv))) { r <- which(vv == v); codes <- df[[jc]][r]; labs <- if (!is.na(jl)) df[[jl]][r] else codes
      ok <- grepl("^-?[0-9]+(\\.[0-9]+)?$", codes)
      rows[[v]] <- list(variable = v, label = if (!is.na(varlab)) df[[varlab]][r][nzchar(df[[varlab]][r])][1] else NA,
                        values = if (any(ok)) stats::setNames(labs[ok], codes[ok]) else NULL) }
  } else {
    cv <- codeish; cv[other %in% c(filt, miss_col)] <- 0
    jv <- if (any(cv > .15)) other[which.max(cv)] else NA
    lab_pref <- other[grepl("label|question|description|wording|text|item|meaning", hn[other]) & !other %in% c(jv, filt, miss_col)]
    cand <- setdiff(other, c(jv, filt, miss_col))
    jl <- if (length(lab_pref)) lab_pref[1] else if (length(cand)) cand[which.max(textlen[match(cand, other)])] else NA
    for (k in which(!is.na(vv))) {
      v <- vv[k]; r <- rows[[v]]; if (is.null(r)) r <- list(variable = v, label = "", vtxt = "", ftxt = "", mtxt = "")
      if (!is.na(jl)) r$label <- trimws(paste(r$label, df[[jl]][k]))
      if (!is.na(jv)) r$vtxt <- paste(r$vtxt, df[[jv]][k], sep = "; ")
      for (j in filt) r$ftxt <- paste(r$ftxt, df[[j]][k])
      for (j in miss_col) r$mtxt <- paste(r$mtxt, df[[j]][k])
      first <- df[[iv]][k]; rest_txt <- trimws(sub("^\\S+\\s*", "", first)); if (nzchar(rest_txt) && is.na(jl)) r$label <- trimws(paste(r$label, rest_txt))
      rows[[v]] <- r }
    for (v in names(rows)) { r <- rows[[v]]
      vals <- qc_parse_codes(r$vtxt)
      mc <- regmatches(r$mtxt, gregexpr("-?[0-9]+", r$mtxt))[[1]]
      if (length(mc)) vals <- c(vals, stats::setNames(rep("Not answered", length(mc)), mc)[!mc %in% names(vals)])
      rows[[v]]$values <- vals
      if (nzchar(trimws(r$ftxt))) rules <- c(rules, qc_rules_from_text(r$ftxt, v, known)) }
  }
  cb <- qc_codebook_from_list(rows, source = source); cb$rules <- rules; cb
}

# ---------------------------------------------------------------- reader for text that mentions variables (any layout)
qc_cb_from_mentions <- function(lines, known, source) {
  kl <- tolower(known)
  lines <- gsub("\t", "    ", lines)
  key <- function(ln) { x <- sub("^\\s*(?:[-*\u2022#>]+\\s*|[0-9]+[.)]\\s+|(?i:variable|var|name|item)\\s*[:=]\\s*)*", "", ln, perl = TRUE)
    tok <- sub("^([A-Za-z][A-Za-z0-9_.\\[\\]]*).*$", "\\1", x, perl = TRUE); if (!grepl("^[A-Za-z]", x)) return(NA_character_)
    k <- match(tolower(sub("[:.,]$", "", tok)), kl); if (is.na(k)) NA_character_ else known[k] }
  starts <- vapply(lines, key, "", USE.NAMES = FALSE)
  idx <- which(!is.na(starts)); if (!length(idx)) return(NULL)
  rows <- list(); rules <- list(); pending <- character()
  for (b in seq_along(idx)) {
    i <- idx[b]; v <- starts[i]; end <- if (b < length(idx)) idx[b + 1] - 1 else length(lines)
    blk <- lines[i:end]; first <- blk[1]
    rest1 <- sub("^\\s*(?:[-*\u2022#>]+\\s*|[0-9]+[.)]\\s+|(?i:variable|var|name|item)\\s*[:=]\\s*)*[A-Za-z][A-Za-z0-9_.\\[\\]]*[:.,]?", "", first, perl = TRUE)
    rest1 <- trimws(sub("^\\s*(?:-|\u2013|\u2014|:|=|\\|)\\s*", "", rest1, perl = TRUE))
    body <- blk[-1]
    lab <- rest1; inline <- NULL
    if (nzchar(lab) && !is.null(qc_parse_codes(lab)) && grepl("=", lab)) {         # "VAR Label  1 = a; 2 = b" on one line
      cut <- regexpr("(\\s{2,}|\\s)-?[0-9]+\\s*=", lab, perl = TRUE); if (cut > 0) { inline <- substring(lab, cut); lab <- trimws(substr(lab, 1, cut - 1)) } }
    kv <- regmatches(body, regexec("^\\s*(?i:label|question|description|wording|text|item text)\\s*[:=]\\s*(.+)$", body, perl = TRUE))
    kv <- vapply(kv, function(z) if (length(z) == 2) z[2] else NA_character_, "")
    if (!nzchar(lab) && any(!is.na(kv))) lab <- kv[!is.na(kv)][1]
    ckv <- regmatches(body, regexec("^\\s*(?i:codes?|values?|value labels?|answer codes?|answers?|categories|response options?|coding|scale)\\s*[:=]\\s*(.+)$", body, perl = TRUE))
    ckv <- vapply(ckv, function(z) if (length(z) == 2) z[2] else NA_character_, "")
    single <- lapply(body, qc_code_part); is_code <- !vapply(single, is.null, logical(1)) & !grepl("(?i)^\\s*(label|question|note|filter)\\s*:", body, perl = TRUE)
    vals <- NULL
    if (any(is_code)) { sc <- do.call(rbind, single[is_code]); vals <- stats::setNames(sc[, 2], sc[, 1]); vals <- vals[!duplicated(names(vals))] }
    if (is.null(vals) && any(!is.na(ckv))) vals <- qc_parse_codes(paste(ckv[!is.na(ckv)], collapse = "; "))
    if (is.null(vals) && !is.null(inline)) vals <- qc_parse_codes(inline)
    if (nzchar(lab)) for (b2 in seq_along(body)) {            # a label wrapped onto the next lines
      if (!nzchar(trimws(body[b2])) || is_code[b2] || !is.na(ckv[b2]) || !is.na(kv[b2]) || grepl("(?i)^\\s*(note|filter|missing|free text|numeric)", body[b2], perl = TRUE)) break
      lab <- paste(lab, trimws(body[b2])) }
    if (!nzchar(lab)) { txt <- trimws(body[!is_code & nzchar(trimws(body)) & is.na(ckv)]); txt <- txt[!grepl("(?i)^(note|filter|missing)", txt, perl = TRUE)]; if (length(txt)) lab <- txt[1] }
    is_text <- any(grepl("(?i)\\b(free text|open text|open answer|text answer|string)\\b", blk, perl = TRUE))
    numeric_only <- any(grepl("(?i)\\b(numeric|integer|number|decimal|years|count)\\b", body, perl = TRUE)) && is.null(vals)
    rows[[v]] <- list(variable = v, label = lab, values = vals, kind = if (is_text) "text" else NULL)
    # battery items listed one after another share the codes printed after the last one
    only_header <- all(!nzchar(trimws(body)))
    if (is.null(vals) && only_header && !is_text) pending <- c(pending, v)
    else { if (!is.null(vals) && length(pending)) { stem <- sub("[_.]?[0-9]+$", "", v)
             for (p in pending) if (sub("[_.]?[0-9]+$", "", p) == stem) rows[[p]]$values <- vals }
           pending <- character() }
    note <- paste(body[grepl("(?i)\\b(only if|shown if|skipped|filter|asked only|survey ends|screen)", body, perl = TRUE)], collapse = " ")
    if (nzchar(note)) rules <- c(rules, qc_rules_from_text(note, v, known))
  }
  cb <- qc_codebook_from_list(rows, source = source); cb$rules <- rules; cb
}

# ---------------------------------------------------------------- SPSS syntax (VARIABLE LABELS / VALUE LABELS / MISSING VALUES)
qc_cb_from_spss_syntax <- function(lines, known, source) {
  txt <- paste(lines, collapse = "\n"); if (!grepl("(?i)value labels|variable labels", txt, perl = TRUE)) return(NULL)
  cmds <- regmatches(txt, gregexpr("(?is)(variable labels|value labels|add value labels|missing values)\\s.*?(?=\\.\\s*(\\n|$))", txt, perl = TRUE))[[1]]
  rows <- list(); miss <- list()
  q <- "'((?:[^']|'')*)'|\"((?:[^\"]|\"\")*)\""
  for (cm in cmds) {
    type <- tolower(sub("(?is)^(variable labels|value labels|add value labels|missing values).*$", "\\1", cm, perl = TRUE)); body <- sub("(?i)^(variable labels|add value labels|value labels|missing values)", "", cm, perl = TRUE)
    if (type == "variable labels") { m <- regmatches(body, gregexpr(paste0("([A-Za-z][A-Za-z0-9_.]*)\\s+(", q, ")"), body, perl = TRUE))[[1]]
      for (x in m) { v <- sub("\\s.*$", "", trimws(x)); l <- gsub("^['\"]|['\"]$", "", trimws(sub("^\\S+\\s+", "", trimws(x)))); if (v %in% known) rows[[v]]$label <- l } }
    else if (type %in% c("value labels", "add value labels")) for (blk in {
        qs <- regmatches(body, gregexpr(q, body, perl = TRUE))[[1]]; tmp <- body
        for (k in seq_along(qs)) tmp <- sub(qs[k], sprintf("@@Q%d@@", k), tmp, fixed = TRUE)      # protect "/" inside quoted labels
        parts <- strsplit(tmp, "/")[[1]]
        vapply(parts, function(pt) { for (k in seq_along(qs)) pt <- sub(sprintf("@@Q%d@@", k), qs[k], pt, fixed = TRUE); pt }, "") }) {
      vars <- regmatches(blk, regexpr("^\\s*([A-Za-z][A-Za-z0-9_.]*(\\s+(TO\\s+)?[A-Za-z][A-Za-z0-9_.]*)*)", blk)); vars <- strsplit(trimws(vars), "\\s+")[[1]]
      if (length(vars) == 3 && toupper(vars[2]) == "TO" && all(vars[c(1, 3)] %in% known)) vars <- known[match(vars[1], known):match(vars[3], known)]
      pr <- regmatches(blk, gregexpr(paste0("(-?[0-9]+(?:\\.[0-9]+)?)\\s+(", q, ")"), blk, perl = TRUE))[[1]]
      vals <- stats::setNames(gsub("^['\"]|['\"]$", "", trimws(sub("^\\S+\\s+", "", trimws(pr)))), sub("\\s.*$", "", trimws(pr)))
      for (v in intersect(vars, known)) rows[[v]]$values <- vals }
    else { m <- regmatches(body, gregexpr("([A-Za-z][A-Za-z0-9_. ]*?)\\s*\\(([^)]*)\\)", body, perl = TRUE))[[1]]
      for (x in m) { vs <- strsplit(trimws(sub("\\(.*$", "", x)), "\\s+")[[1]]; cs <- regmatches(x, gregexpr("-?[0-9]+", sub("^[^(]*", "", x)))[[1]]
        for (v in if (toupper(vs[1]) == "ALL") known else intersect(vs, known)) miss[[v]] <- unique(c(miss[[v]], cs)) } }
  }
  for (v in names(miss)) if (!is.null(rows[[v]])) { vals <- rows[[v]]$values; add <- setdiff(miss[[v]], names(vals))
    rows[[v]]$values <- c(vals, stats::setNames(rep("Not answered", length(add)), add)); rows[[v]]$values[miss[[v]]] <- "Not answered" }
  rows <- lapply(names(rows), function(v) c(list(variable = v), rows[[v]]))
  if (!length(rows)) return(NULL)
  cb <- qc_codebook_from_list(rows, source = source); cb$global_missing <- unique(unlist(miss)); cb
}

# ---------------------------------------------------------------- missing codes stated once for the whole codebook
qc_global_missing <- function(lines) {
  txt <- paste(lines, collapse = "\n"); out <- character()
  m <- regmatches(txt, gregexpr("(?i)(-?[0-9]+)\\s*(?:=|:)\\s*(?:not answered|no answer|missing|refused|keine angabe|nicht beantwortet)", txt, perl = TRUE))[[1]]
  out <- c(out, regmatches(m, regexpr("-?[0-9]+", m)))
  m <- regmatches(txt, gregexpr("(?i)missing(?: values?)?[^.\n]{0,40}?\\(?(-?[0-9]+(?:\\s*(?:,|and|or)\\s*-?[0-9]+)*)\\)?", txt, perl = TRUE))[[1]]
  for (x in m) out <- c(out, regmatches(x, gregexpr("-[0-9]+|\\b9{2,4}\\b", x))[[1]])
  out <- unique(out)
  out[grepl("^-[0-9]+$|^9{2,4}$", out)]          # conventional missing codes only (negative, 99, 999, 9999)
}

# ---------------------------------------------------------------- survey rules written as sentences
qc_rule <- function(cond_var, codes, negate, skipped, screen, text) {
  vec <- if (length(codes) == 1) codes else paste0("c(", paste(codes, collapse = ", "), ")")
  cond <- if (negate) sprintf('!is.na(data[["%s"]]) & !(data[["%s"]] %%in%% %s)', cond_var, cond_var, vec) else sprintf('data[["%s"]] %%in%% %s', cond_var, vec)
  list(page = NA, condition = cond, condition_php = sprintf("%s %s %s", cond_var, if (negate) "is not" else "=", paste(codes, collapse = " or ")),
       target = if (screen) "end" else "later page", vars_skipped = unique(skipped), text = text, screen_out = screen, readable = TRUE)
}
qc_rules_from_text <- function(txt, subject, known) {
  txt <- gsub("\\s+", " ", txt); out <- list(); kv <- paste0("(", paste(gsub("([.\\[\\]])", "\\\\\\1", known[order(-nchar(known))]), collapse = "|"), ")")
  codes_re <- "(-?[0-9]+(?:\\s*(?:,|or|/|and)\\s*-?[0-9]+)*)"; eq <- "\\s*(?:=|==|is|equals|in)\\s*\\(?"
  num <- function(x) as.numeric(regmatches(x, gregexpr("-?[0-9]+", x))[[1]])
  m <- regmatches(txt, regexec(paste0("(?i)\\bif\\s+", kv, eq, codes_re, "[^.]*?(survey ends|end page|screen-?out|ends the survey|to the end|terminat)"), txt, perl = TRUE))[[1]]
  if (length(m) >= 3) out[[length(out) + 1]] <- qc_rule(m[2], num(m[3]), FALSE, character(), TRUE, txt)
  m <- regmatches(txt, regexec(paste0("(?i)(?:shown|asked|displayed|presented|answered|applies)?\\s*only\\s+(?:if|when|for|to)\\s+(?:respondents\\s+|those\\s+|participants\\s+)?(?:with\\s+|who\\s+)?", kv, eq, codes_re), txt, perl = TRUE))[[1]]
  if (length(m) >= 3 && !is.null(subject)) out[[length(out) + 1]] <- qc_rule(m[2], num(m[3]), TRUE, subject, FALSE, txt)
  else { m <- regmatches(txt, regexec(paste0("(?i)(?:skipped|not shown|not asked|hidden|filtered out)\\s+(?:if|when|for)\\s+", kv, eq, codes_re), txt, perl = TRUE))[[1]]
    if (length(m) >= 3 && !is.null(subject)) out[[length(out) + 1]] <- qc_rule(m[2], num(m[3]), FALSE, subject, FALSE, txt) }
  m <- regmatches(txt, regexec(paste0("(?i)\\bif\\s+", kv, eq, codes_re, ",?\\s+(?:then\\s+)?(.+?)\\s+(?:is|are)\\s+(?:skipped|not shown|not asked|left blank|empty)"), txt, perl = TRUE))[[1]]
  if (length(m) >= 4) { sk <- intersect(trimws(strsplit(m[4], "\\s*,\\s*|\\s+and\\s+")[[1]]), known); if (length(sk)) out[[length(out) + 1]] <- qc_rule(m[2], num(m[3]), FALSE, sk, FALSE, txt) }
  out
}

# ---------------------------------------------------------------- Qualtrics display logic, LimeSurvey relevance
qc_qsf_rules <- function(path, known) {
  j <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL); if (is.null(j)) return(list())
  tag <- list(); for (el in j$SurveyElements) if (identical(el$Element, "SQ")) { qid <- el$PrimaryAttribute; if (is.null(qid)) qid <- el$Payload$QuestionID
    if (!is.null(qid)) tag[[qid]] <- unique(c(tag[[qid]], el$Payload$DataExportTag)) }
  out <- list()
  for (el in j$SurveyElements) { if (!identical(el$Element, "SQ") || is.null(el$Payload$DisplayLogic)) next
    me <- el$Payload$DataExportTag
    conds <- unlist(lapply(el$Payload$DisplayLogic, function(g) if (is.list(g)) Filter(function(z) is.list(z) && !is.null(z$QuestionID), g)), recursive = FALSE)
    for (cd in conds) { src <- tag[[cd$QuestionID]]; src <- intersect(src, known); if (!length(src) || !me %in% known) next
      code <- as.numeric(sub("^.*/", "", cd$ChoiceLocator)); sel <- identical(cd$Operator, "Selected")
      out[[length(out) + 1]] <- qc_rule(src[1], code, sel, me, FALSE, cd$Description %||% "Qualtrics display logic") } }
  out
}
`%||%` <- function(a, b) if (is.null(a)) b else a

qc_lss_relevance <- function(expr, subject, known) {
  m <- regmatches(expr, gregexpr("([A-Za-z][A-Za-z0-9_]*)(?:\\.NAOK)?\\s*==\\s*\"?(-?[0-9]+)\"?", expr, perl = TRUE))[[1]]
  if (!length(m)) return(list())
  v <- unique(sub("(\\.NAOK)?\\s*==.*$", "", m)); if (length(v) != 1 || !v %in% known) return(list())
  codes <- as.numeric(sub("^.*==\\s*\"?(-?[0-9]+)\"?$", "\\1", m))
  list(qc_rule(v, codes, TRUE, subject, FALSE, expr))
}

# ---------------------------------------------------------------- choosing the best reading per variable
qc_cb_fit <- function(e, x, miss) {
  if (is.null(e)) return(-1)
  s <- (!is.na(e$label) && nzchar(e$label)) * 1 + (!is.na(e$question) && nzchar(e$question)) * .3
  if (length(e$values) && is.numeric(x)) { obs <- unique(x[!is.na(x)]); obs <- obs[!obs %in% suppressWarnings(as.numeric(c(e$missing, miss)))]
    codes <- suppressWarnings(as.numeric(names(e$values)))
    allowed <- if (e$kind == "range") seq(min(codes), max(codes)) else codes
    s <- s + .5 + if (length(obs)) 3 * mean(obs %in% allowed) else 1 }
  else if (length(e$values)) s <- s + .5 + 2 * mean(as.character(unique(x[!is.na(x)])) %in% c(names(e$values), e$missing))
  if (identical(e$kind, "text")) s <- s + .5
  s
}

qc_cb_merge <- function(cands, data, source, extra_rules = list(), global_missing = character()) {
  cands <- Filter(function(c) !is.null(c) && length(c$vars), cands)
  vars <- list(); used <- character()
  for (v in names(data)) {
    es <- lapply(cands, function(c) c$vars[[v]]); ok <- !vapply(es, is.null, logical(1)); if (!any(ok)) next
    es <- es[ok]; fit <- vapply(es, qc_cb_fit, 0, x = data[[v]], miss = global_missing); best <- es[[which.max(fit)]]
    l <- unlist(lapply(es, `[[`, "label")); l <- l[!is.na(l) & nzchar(l)]
    if (is.na(best$label) || !nzchar(best$label)) { if (length(l)) best$label <- l[which.max(nchar(l))] }
    else { short <- l[nchar(l) < nchar(best$label) - 3 & startsWith(tolower(best$label), tolower(l)) & nchar(l) >= 4]
      spill <- vapply(short, function(z) grepl("(?i)^\\s*[(\u00b7]?\\s*(numeric|ordinal|nominal|categorical|text|string|integer|decimal|likert|metric|binary|dichotomous|p\\.\\s*[0-9]|page\\s*[0-9]|\u00b7)",
                                               substring(best$label, nchar(z) + 1), perl = TRUE), logical(1))
      short <- short[spill]; if (length(short)) best$label <- short[which.max(nchar(short))] }
    if (is.na(best$question)) { q <- unlist(lapply(es, `[[`, "question")); q <- q[!is.na(q)]; if (length(q)) best$question <- q[1] }
    if (is.na(best$page)) { pg <- unlist(lapply(es, `[[`, "page")); pg <- pg[!is.na(pg)]; if (length(pg)) best$page <- pg[1] }
    best$missing <- unique(c(best$missing, unlist(lapply(es, `[[`, "missing"))))
    # missing codes stated once for the whole codebook apply where they occur in the data or among the codes
    if (length(global_missing) && is.numeric(data[[v]])) { gm <- global_missing[suppressWarnings(as.numeric(global_missing)) %in% data[[v]] | (length(best$values) & !identical(best$kind, "text"))]
      best$missing <- unique(c(best$missing, gm)); best$values <- best$values[!names(best$values) %in% best$missing] }
    x <- data[[v]]
    if (is.numeric(x) && length(best$values) && mean(x[!is.na(x)] != round(x[!is.na(x)])) > .2) { best$values <- character(); best$kind <- "none" }   # measured values with decimals have no answer codes
    # free-text answers: the codebook says so, or the data holds words where the codebook lists number codes
    txt_mark <- grepl("(?i)\\b(free text|open text|open answer|text answer|please specify|string)\\b", paste(best$label, if (identical(best$kind, "text")) "free text"), perl = TRUE)
    words <- if (is.character(x)) { u <- unique(trimws(x[!is.na(x) & nzchar(trimws(x))])); length(u) > 0 && mean(is.na(suppressWarnings(as.numeric(u)))) > .5 } else FALSE
    numcodes <- length(best$values) && all(!is.na(suppressWarnings(as.numeric(names(best$values)))))
    if (words && (txt_mark || numcodes || !length(best$values))) { best$values <- character(); best$kind <- "text" }
    vars[[v]] <- best; used <- c(used, which(ok))
  }
  # undescribed missing codes: a conventional code (-9, -99, 99, 999 ...) that is no answer option yet appears in
  # several coded questions is the survey's missing code; it is used and reported so the user can check it
  inferred <- character()
  coded <- names(vars)[vapply(names(vars), function(v) identical(vars[[v]]$kind, "codes") && is.numeric(data[[v]]) && length(vars[[v]]$values) > 0, logical(1))]
  if (length(coded)) {
    stray <- unlist(lapply(coded, function(v) { e <- vars[[v]]; obs <- unique(data[[v]][!is.na(data[[v]])]); codes <- suppressWarnings(as.numeric(names(e$values)))
      cand <- obs[!obs %in% codes & !as.character(obs) %in% e$missing]
      cand <- cand[cand < 0 | (grepl("^9{2,4}$", as.character(cand)) & cand > max(codes, na.rm = TRUE))]; unique(as.character(cand)) }))
    tab <- table(stray); inferred <- names(tab)[tab >= 2]
    listed <- unique(unlist(lapply(vars, function(e) names(e$values))))
    inferred <- setdiff(inferred, listed)
    for (v in names(vars)) { x <- data[[v]]; if (!is.numeric(x) || identical(vars[[v]]$kind, "text")) next
      use <- if (v %in% coded) inferred else inferred[suppressWarnings(as.numeric(inferred)) < 0 & suppressWarnings(as.numeric(inferred)) %in% x]
      vars[[v]]$missing <- unique(c(vars[[v]]$missing, use)) }
  }
  rules <- c(unlist(lapply(cands, `[[`, "rules"), recursive = FALSE), extra_rules)
  key <- vapply(rules, function(r) paste(r$condition, paste(sort(r$vars_skipped), collapse = ","), r$screen_out), "")
  rules <- rules[!duplicated(key)]
  # a real skip rule is followed by (almost) every respondent: a rule that most cases break was misread, so it is not used
  dropped <- character()
  rules <- Filter(function(r) {
    if (!isTRUE(r$readable) || isTRUE(r$screen_out) || !length(r$vars_skipped)) return(TRUE)
    hit <- tryCatch(eval(parse(text = r$condition), list(data = data)) %in% TRUE, error = function(e) NULL)
    if (is.null(hit)) return(FALSE)
    if (!any(hit)) return(TRUE)
    sk <- intersect(r$vars_skipped, names(data)); if (!length(sk)) return(FALSE)
    ans <- rowSums(vapply(sk, function(v) { x <- data[[v]][hit]; !is.na(x) & !(x %in% suppressWarnings(as.numeric(global_missing))) & !(as.character(x) %in% c("", "-9")) }, logical(sum(hit))) |> as.matrix()) > 0
    if (mean(ans) > .25) { dropped <<- c(dropped, r$condition_php); FALSE } else TRUE }, rules)
  pages <- NULL; for (c in cands) if (!is.null(c$pages) && nrow(c$pages)) { pages <- c$pages; break }
  probs <- c(unique(unlist(lapply(cands, `[[`, "problems"))),
    if (length(inferred)) sprintf("The codebook does not describe the value(s) %s, but they appear in several coded questions without being an answer option, so they are treated as 'missing / not answered'. Please check.", paste(inferred, collapse = ", ")),
    if (length(dropped)) sprintf("A filter rule was found (%s) but most answers in the data do not follow it, so it is not used.", paste(unique(dropped), collapse = "; ")))
  list(vars = vars, rules = rules, pages = pages, source = source, problems = probs,
       readers = vapply(cands, function(c) if (is.null(c$reader)) "reader" else c$reader, ""))
}

# ---------------------------------------------------------------- the dispatcher used by the app
qc_read_any_codebook <- function(path, data = NULL) {
  ext <- tolower(tools::file_ext(path)); known <- if (!is.null(data)) names(data) else NULL; src <- basename(path)
  tag <- function(cb, r) { if (!is.null(cb)) cb$reader <- r; cb }
  res <- tryCatch({
    if (ext == "qsf") { cb <- tag(qc_codebook_qsf(path), "Qualtrics survey file"); cb$rules <- qc_qsf_rules(path, known)
      if (!is.null(data)) qc_cb_merge(list(cb), data, src, global_missing = qc_global_missing(unlist(lapply(cb$vars, function(e) paste(names(e$values), "=", e$values))))) else cb }
    else if (ext == "lss") { cb <- tag(qc_codebook_lss(path), "LimeSurvey structure file")
      if (!is.null(data)) { # LimeSurvey exports sub-questions as STRESS[SQ001]
        for (v in names(cb$vars)) { alt <- sub("_([^_]+)$", "[\\1]", v); if (!v %in% known && alt %in% known) { e <- cb$vars[[v]]; e$variable <- alt; cb$vars[[alt]] <- e; cb$vars[[v]] <- NULL } } }
      x <- xml2::read_xml(path); rel <- xml2::xml_find_all(x, "//questions/rows/row")
      for (r in rel) { t <- xml2::xml_text(xml2::xml_find_first(r, "title")); e <- xml2::xml_text(xml2::xml_find_first(r, "relevance"))
        if (!is.na(e) && !e %in% c("", "1")) { subj <- c(t, known[startsWith(known, paste0(t, "["))]); cb$rules <- c(cb$rules, qc_lss_relevance(e, intersect(subj, known), known)) } }
      cb$problems <- character()
      if (!is.null(data)) qc_cb_merge(list(cb), data, src) else cb }
    else if (ext %in% c("sav", "zsav", "por", "dta", "sas7bdat")) tag(qc_embedded_codebook(path), "labels stored in the data file")
    else {
      if (is.null(known)) return(qc_read_any_codebook_legacy(path, data))
      ex <- qc_cb_extract(path, known); lines <- ex$lines
      if (ext == "pdf" && !length(ex$tables) && sum(nchar(trimws(lines))) < 50)
        return(list(vars = list(), rules = list(), source = src, problems = "This PDF has no readable text (it is probably a scan). QuantCompass does not guess from images: please upload the codebook as Word, Excel, CSV or the survey tool's export."))
      cands <- list()
      for (t in ex$tables) cands[[length(cands) + 1]] <- tag(qc_cb_from_table2(t, known, src), "table")
      if (length(lines)) {
        cands[[length(cands) + 1]] <- tag(qc_cb_from_spss_syntax(lines, known, src), "SPSS syntax")
        sos <- tryCatch(qc_parse_codebook_text(lines, known, src), error = function(e) NULL); cands[[length(cands) + 1]] <- tag(sos, "survey variable view")
        cands[[length(cands) + 1]] <- tag(qc_cb_from_mentions(lines, known, src), "variable descriptions in text")
        tl <- tryCatch(qc_parse_codebook_table_lines(lines, known, src), error = function(e) NULL); cands[[length(cands) + 1]] <- tag(tl, "text table")
      }
      gm <- unique(c(qc_global_missing(qc_codes_norm(c(lines, unlist(lapply(ex$tables, function(t) unlist(t))))))))
      extra <- c(if (length(lines)) qc_parse_documented_rules(lines, known) else list(),
                 unlist(lapply(lines[grepl("(?i)\\bif\\b.*(skipped|not shown|not asked|survey ends|screen-?out)", lines, perl = TRUE)], qc_rules_from_text, subject = NULL, known = known), recursive = FALSE))
      qc_cb_merge(cands, data, src, extra, gm)
    }
  }, error = function(e) list(vars = list(), rules = list(), source = src, problems = paste("The codebook could not be read:", conditionMessage(e))))
  if (is.null(res) || !length(res$vars)) {
    res <- if (is.null(res)) list(vars = list(), rules = list(), source = src, problems = character()) else res
    res$problems <- unique(c(res$problems, "No variables were found in this codebook. Check that it lists variable names (as in the data) with their labels and codes."))
  }
  res
}
