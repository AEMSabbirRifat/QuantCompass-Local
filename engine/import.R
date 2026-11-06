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
  if (ext %in% c("xlsx", "xls", "xlsm")) "excel" else if (ext == "ods") "ods" else if (ext %in% c("sav", "zsav", "por")) "spss"
  else if (ext == "dta") "stata" else if (ext %in% c("sas7bdat", "xpt")) "sas" else "text"
}

# Inspect a text file once: encoding, separator, decimal mark, label row
qc_text_format <- function(path) {
  enc <- qc_detect_encoding(path)
  txt <- qc_text_file(path, enc)
  lines <- utils::head(strsplit(txt, "\n", fixed = TRUE)[[1]], 30); lines <- lines[nzchar(lines)]
  sep <- qc_detect_sep(lines)
  dec <- if (sep == ";") "," else "."
  d <- utils::read.delim(text = paste(utils::head(strsplit(txt, "\n", fixed = TRUE)[[1]], 200), collapse = "\n"),
                         sep = sep, colClasses = "character", check.names = FALSE, na.strings = character(), quote = "\"")
  list(encoding = enc, sep = sep, dec = dec, label_row = qc_has_label_row(d))
}

qc_load_code <- function(path, sheet = NULL, fixes = character()) {
  file <- basename(path); kind <- qc_file_kind(path)
  na <- paste0("c(", paste0('"', qc_na_strings, '"', collapse = ", "), ")")
  code <- switch(kind,
    excel = { if (is.null(sheet)) sheet <- readxl::excel_sheets(path)[1]
      sprintf('data <- as.data.frame(readxl::read_excel("%s", sheet = "%s", na = %s))', file, sheet, na) },
    ods = { if (is.null(sheet)) sheet <- 1
      sprintf('data <- as.data.frame(readODS::read_ods("%s", sheet = %s, na = %s))', file, if (is.numeric(sheet)) sheet else paste0('"', sheet, '"'), na) },
    spss = sprintf('data <- as.data.frame(haven::zap_labels(haven::%s("%s")))  # codes kept as numbers; labels go to the codebook',
                   if (tolower(tools::file_ext(path)) == "por") "read_por" else "read_sav", file),
    stata = sprintf('data <- as.data.frame(haven::zap_labels(haven::read_dta("%s")))', file),
    sas = sprintf('data <- as.data.frame(haven::zap_labels(haven::%s("%s")))', if (tolower(tools::file_ext(path)) == "xpt") "read_xpt" else "read_sas", file),
    text = { f <- qc_text_format(path)
      c(sprintf('raw <- utils::read.delim(text = qc_text_file("%s", "%s"), sep = "%s", colClasses = "character",', file, f$encoding, gsub("\t", "\\\\t", f$sep)),
        sprintf('                         check.names = FALSE, na.strings = %s, quote = "\\"")', na),
        if (f$label_row) 'raw <- raw[-1, , drop = FALSE]   # row 2 of the file holds the question texts (used as labels, not data)',
        if (f$dec == ",") 'raw[] <- lapply(raw, function(x) gsub(",", ".", x, fixed = TRUE))   # decimal commas',
        'data <- qc_type_convert(raw); rownames(data) <- NULL') })
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
      d <- utils::read.delim(text = paste(utils::head(strsplit(txt, "\n", fixed = TRUE)[[1]], 3), collapse = "\n"), sep = f$sep,
                             colClasses = "character", check.names = FALSE, na.strings = character())
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
