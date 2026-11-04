# Reading files and listing sheets.
# The code string returned by qc_load_code() is BOTH evaluated by the engine
# and written into the user's script, so the script loads data exactly the same way.

qc_na_strings <- c("", "NA", "N/A", "n/a", "-", "NULL")

# List the sheets of a workbook with a short preview of each
qc_sheets <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("xlsx", "xls")) {
    d <- qc_read(path)
    return(data.frame(sheet = "(single table)", rows = nrow(d), columns = ncol(d),
                      first_columns = paste(utils::head(names(d), 5), collapse = ", ")))
  }
  sh <- readxl::excel_sheets(path)
  do.call(rbind, lapply(sh, function(s) {
    x <- readxl::read_excel(path, sheet = s)
    data.frame(sheet = s, rows = nrow(x), columns = ncol(x),
               first_columns = paste(utils::head(names(x), 5), collapse = ", "))
  }))
}

# Detect separator and decimal mark of a CSV (handles German ';' + ',' exports)
qc_csv_format <- function(path) {
  first <- readLines(path, n = 20, warn = FALSE)
  semis <- sum(lengths(regmatches(first, gregexpr(";", first))))
  commas <- sum(lengths(regmatches(first, gregexpr(",", first))))
  if (semis > commas) list(sep = ";", dec = ",") else list(sep = ",", dec = ".")
}

qc_load_code <- function(path, sheet = NULL) {
  file <- basename(path)
  na <- paste0("c(", paste0('"', qc_na_strings, '"', collapse = ", "), ")")
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet)) sheet <- readxl::excel_sheets(path)[1]
    sprintf('data <- as.data.frame(readxl::read_excel("%s", sheet = "%s", na = %s))', file, sheet, na)
  } else {
    f <- qc_csv_format(path)
    sprintf('data <- utils::read.csv("%s", sep = "%s", dec = "%s", na.strings = %s, check.names = FALSE)',
            file, f$sep, f$dec, na)
  }
}

qc_read <- function(path, sheet = NULL) {
  code <- qc_load_code(path, sheet)
  env <- new.env()
  old <- setwd(dirname(normalizePath(path))); on.exit(setwd(old))
  eval(parse(text = code), envir = env)
  env$data
}
