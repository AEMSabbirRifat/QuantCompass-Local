# Universal data import. Returns the exact R code that reads the file, so every script reproduces it.
# Supported: CSV/TSV/TXT (any encoding, separator; SoSci Survey label row), Excel (.xlsx/.xls),
# OpenDocument (.ods), SPSS (.sav/.por), Stata (.dta), SAS (.sas7bdat).

qc_detect_encoding <- function(path) {
  b <- readBin(path, "raw", min(file.info(path)$size, 200000))
  if (length(b) >= 2 && b[1] == as.raw(0xff) && b[2] == as.raw(0xfe)) return("UTF-16LE")
  if (length(b) >= 2 && b[1] == as.raw(0xfe) && b[2] == as.raw(0xff)) return("UTF-16BE")
  if (length(b) >= 3 && all(b[1:3] == as.raw(c(0xef, 0xbb, 0xbf)))) return("UTF-8")
  # UTF-16 without BOM: many zero bytes
  if (mean(b == as.raw(0)) > .3) return(if (b[1] == as.raw(0)) "UTF-16BE" else "UTF-16LE")
  if (validUTF8(rawToChar(b[b != as.raw(0)]))) "UTF-8" else "latin1"
}

qc_detect_sep <- function(lines) {
  cand <- c("\t" = "\t", ";" = ";", "," = ",", "|" = "|")
  counts <- sapply(cand, function(s) { k <- lengths(regmatches(lines, gregexpr(s, lines, fixed = TRUE))); if (all(k == k[1]) && k[1] > 0) k[1] + 1000 else stats::median(k) })
  names(cand)[which.max(counts)]
}

# A second header row of question texts (SoSci Survey, Qualtrics) - detected when row 1 is text
# in columns whose remaining values are all numbers
qc_has_label_row <- function(d) {
  if (nrow(d) < 3) return(FALSE)
  first <- unlist(d[1, ]); rest <- d[-1, , drop = FALSE]
  numeric_cols <- vapply(rest, function(x) { x <- x[!is.na(x) & nzchar(x)]; length(x) > 0 && all(!is.na(suppressWarnings(as.numeric(x)))) }, logical(1))
  if (sum(numeric_cols) < 2) return(FALSE)
  txt <- first[numeric_cols]; txt <- txt[!is.na(txt) & nzchar(txt)]
  length(txt) > 0 && mean(is.na(suppressWarnings(as.numeric(txt)))) > .8
}

qc_file_kind <- function(path) {
  ext <- tolower(tools::file_ext(path))
  # the content decides when the extension is missing or wrong (e.g. an Excel file saved as .csv)
  b <- tryCatch(readBin(path, "raw", 8), error = function(e) raw())
  if (length(b) >= 4) {
    if (identical(b[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04))) && !ext %in% c("xlsx", "xlsm", "ods", "docx")) return("excel")
    if (identical(b[1:4], as.raw(c(0xd0, 0xcf, 0x11, 0xe0))) && ext != "xls") return("excel")
    if (rawToChar(b[1:4][b[1:4] != as.raw(0)]) %in% c("$FL2", "$FL3")) return("spss")
  }
  if (ext %in% c("xlsx", "xls", "xlsm")) "excel" else if (ext == "ods") "ods" else if (ext %in% c("sav", "zsav", "por")) "spss"
  else if (ext == "dta") "stata" else if (ext %in% c("sas7bdat", "xpt")) "sas" else "text"
}

# Rows to skip before the header (titles or notes above the table): the header is the first row that is
# (almost) as wide as the table and holds unique, mostly non-numeric names
qc_header_row <- function(m) {
  if (!nrow(m)) return(0)
  filled <- rowSums(!is.na(m) & nzchar(trimws(m))); w <- max(filled)
  for (r in seq_len(min(nrow(m), 30))) {
    v <- trimws(m[r, ]); v <- v[!is.na(v) & nzchar(v)]
    if (length(v) >= max(2, .8 * w) && !anyDuplicated(v) && mean(is.na(suppressWarnings(as.numeric(v)))) >= .7) return(r - 1)
  }
  0
}
# Extra rows right below the header that are not data: question texts (SoSci, Qualtrics) and Qualtrics ImportId rows
qc_meta_rows <- function(d) {
  if (nrow(d) < 4) return(0)
  first <- seq_len(min(3, nrow(d) - 2))
  json <- first[vapply(first, function(r) mean(grepl('^\\{"?ImportId', unlist(d[r, ]))) > .5, logical(1))]
  rest <- if (length(json)) d[-json, , drop = FALSE] else d
  lab <- if (qc_has_label_row(rest)) setdiff(first, json)[1] else integer()
  k <- max(c(0, json, lab))
  if (k && !all(seq_len(k) %in% c(json, lab))) k <- 0     # only rows directly below the header
  k
}
qc_decimal_comma_likely <- function(d, sep) {
  if (sep == ",") return(FALSE)
  v <- unlist(utils::head(d, 200)); v <- v[!is.na(v) & nzchar(v)]
  sum(grepl("^-?[0-9]+,[0-9]+$", v)) > 0 && sum(grepl("^-?[0-9]+\\.[0-9]+$", v)) == 0
}

# Inspect a text file once: encoding, separator, header position, extra header rows, decimal mark
qc_text_format <- function(path) {
  enc <- qc_detect_encoding(path)
  txt <- qc_text_file(path, enc)
  all <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- utils::head(all[nzchar(all)], 60)
  sep <- qc_detect_sep(utils::tail(lines, 40))
  m <- tryCatch(as.matrix(utils::read.delim(text = paste(utils::head(all, 60), collapse = "\n"), sep = sep, header = FALSE, colClasses = "character",
                 na.strings = character(), quote = "\"", fill = TRUE, check.names = FALSE, blank.lines.skip = FALSE)), error = function(e) matrix(character()))
  skip <- qc_header_row(m)
  d <- utils::read.delim(text = paste(utils::head(all, 260), collapse = "\n"), skip = skip, sep = sep, colClasses = "character", check.names = FALSE,
                         na.strings = character(), quote = "\"", fill = TRUE)
  meta <- qc_meta_rows(d)
  dd <- if (meta) d[-seq_len(meta), , drop = FALSE] else d
  list(encoding = enc, sep = sep, skip = skip, meta_rows = meta, label_row = meta > 0, dec = if (qc_decimal_comma_likely(dd, sep)) "," else ".")
}

qc_excel_format <- function(path, sheet) {
  m <- as.matrix(suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text", n_max = 60)))
  skip <- qc_header_row(m)
  d <- as.data.frame(suppressMessages(readxl::read_excel(path, sheet = sheet, skip = skip, col_types = "text", n_max = 260)))
  list(skip = skip, meta_rows = qc_meta_rows(d))
}

qc_load_code <- function(path, sheet = NULL, fixes = character()) {
  file <- basename(path); kind <- qc_file_kind(path)
  na <- paste0("c(", paste0('"', qc_na_strings, '"', collapse = ", "), ")")
  tail_code <- function(f, from_text) c(
    if (f$meta_rows) sprintf('raw <- raw[-seq_len(%d), , drop = FALSE]   # %s below the header %s question texts / export information, not data',
                             f$meta_rows, if (f$meta_rows == 1) "the row" else paste(f$meta_rows, "rows"), if (f$meta_rows == 1) "holds" else "hold"),
    if (from_text && f$dec == ",") 'raw <- qc_decimal_comma(raw)   # numbers written with a decimal comma',
    'data <- qc_type_convert(raw); rownames(data) <- NULL')
  code <- switch(kind,
    excel = { if (is.null(sheet)) sheet <- readxl::excel_sheets(path)[1]; f <- qc_excel_format(path, sheet)
      c(sprintf('raw <- as.data.frame(readxl::read_excel("%s", sheet = "%s", skip = %d, col_types = "text", na = %s), check.names = FALSE)%s', file, sheet, f$skip, na,
                if (f$skip) sprintf("   # %d row(s) of notes above the header skipped", f$skip) else ""), tail_code(c(f, dec = "."), FALSE)) },
    ods = { if (is.null(sheet)) sheet <- 1
      sprintf('data <- as.data.frame(readODS::read_ods("%s", sheet = %s, na = %s))', file, if (is.numeric(sheet)) sheet else paste0('"', sheet, '"'), na) },
    spss = sprintf('data <- as.data.frame(haven::zap_labels(haven::%s("%s")))  # codes kept as numbers; labels go to the codebook',
                   if (tolower(tools::file_ext(path)) == "por") "read_por" else "read_sav", file),
    stata = sprintf('data <- as.data.frame(haven::zap_labels(haven::read_dta("%s")))', file),
    sas = sprintf('data <- as.data.frame(haven::zap_labels(haven::%s("%s")))', if (tolower(tools::file_ext(path)) == "xpt") "read_xpt" else "read_sas", file),
    text = { f <- qc_text_format(path)
      c(sprintf('raw <- utils::read.delim(text = qc_text_file("%s", "%s"), sep = "%s", skip = %d, colClasses = "character", fill = TRUE,', file, f$encoding, gsub("\t", "\\\\t", f$sep), f$skip),
        sprintf('                         check.names = FALSE, na.strings = %s, quote = "\\"")', na), tail_code(f, TRUE)) })
  if (length(fixes)) code <- c(code, "# Data preparation chosen in QuantCompass (recorded so the script reproduces it)", fixes)
  code
}

qc_read <- function(path, sheet = NULL, fixes = character()) {
  code <- qc_load_code(path, sheet, fixes)
  env <- new.env(parent = globalenv())
  old <- setwd(dirname(normalizePath(path))); on.exit(setwd(old))
  eval(parse(text = code), envir = env)
  env$data
}

# Labels stored in the file itself: SoSci/Qualtrics label row, or SPSS/Stata variable and value labels
qc_embedded_codebook <- function(path) {
  kind <- qc_file_kind(path)
  if (kind %in% c("spss", "stata", "sas")) {
    d <- switch(kind, spss = if (tolower(tools::file_ext(path)) == "por") haven::read_por(path) else haven::read_sav(path),
                stata = haven::read_dta(path), sas = haven::read_sas(path))
    rows <- lapply(names(d), function(v) { lab <- attr(d[[v]], "label"); vl <- attr(d[[v]], "labels")
      list(variable = v, label = if (is.null(lab)) NA_character_ else as.character(lab),
           values = if (is.null(vl)) NULL else stats::setNames(names(vl), as.character(unname(vl)))) })
    return(qc_codebook_from_list(rows, source = paste("labels stored in", basename(path))))
  }
  if (kind == "text") { f <- qc_text_format(path)
    if (f$label_row) { txt <- qc_text_file(path, f$encoding)
      d <- utils::read.delim(text = paste(utils::head(strsplit(txt, "\n", fixed = TRUE)[[1]], f$skip + 4), collapse = "\n"), sep = f$sep, skip = f$skip,
                             colClasses = "character", check.names = FALSE, na.strings = character(), quote = "\"", fill = TRUE)
      rows <- lapply(names(d), function(v) list(variable = v, label = d[1, v], values = NULL))
      return(qc_codebook_from_list(rows, source = "question texts in row 2 of the data file")) } }
  NULL
}

qc_sheets <- function(path) {
  kind <- qc_file_kind(path)
  if (kind == "excel") { sh <- readxl::excel_sheets(path)
    return(do.call(rbind, lapply(sh, function(s) { x <- readxl::read_excel(path, sheet = s)
      data.frame(sheet = s, rows = nrow(x), columns = ncol(x), first_columns = paste(utils::head(names(x), 5), collapse = ", ")) }))) }
  d <- qc_read(path)
  data.frame(sheet = "(single table)", rows = nrow(d), columns = ncol(d), first_columns = paste(utils::head(names(d), 5), collapse = ", "))
}
