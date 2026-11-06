# =====================================================================
#  QuantCompass - START HERE
#  1. Open this file in RStudio.
#  2. Press Ctrl + Shift + S (Mac: Cmd + Shift + S), or click "Source"
#     (top right of this window). Do not use "Run App".
#  The app opens in your browser. No code needed after that.
# =====================================================================

# Find the QuantCompass folder automatically (the folder this file is in)
qc_here <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
if (is.null(qc_here) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
  qc_here <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) NULL)
if (is.null(qc_here) || !file.exists(file.path(qc_here, "app", "app.R"))) {
  message("Please select the QuantCompass folder (the one that contains 'app' and 'engine').")
  qc_here <- if (.Platform$OS.type == "windows") utils::choose.dir() else readline("Folder path: ")
}
setwd(qc_here)

# Install missing packages once (pre-built versions, no Rtools needed)
qc_needed <- c("shiny", "bslib", "readxl", "ggplot2", "officer", "htmltools", "jsonlite", "haven", "pdftools", "writexl", "xml2")
qc_missing <- qc_needed[!vapply(qc_needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(qc_missing)) {
  message("First start: installing ", paste(qc_missing, collapse = ", "), " (one time only, a few minutes)...")
  utils::install.packages(qc_missing, repos = "https://cloud.r-project.org",
                          type = if (.Platform$OS.type == "windows") "binary" else getOption("pkgType"))
}

message("Starting QuantCompass... it opens in your browser. To stop it, press the red Stop button in RStudio's Console.")
qc_start <- get("runApp", envir = asNamespace("shiny"))   # written this way so RStudio shows "Source"
qc_start(file.path(qc_here, "app"), launch.browser = TRUE)
