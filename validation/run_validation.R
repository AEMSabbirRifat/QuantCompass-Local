# Run the full validation suite from the repository root:
#   Rscript validation/run_validation.R
# Any failing check exits with an error, so CI blocks the release.
options(qc.root = normalizePath("."), warn = 1)
source("engine/load.R")
testthat::test_file("validation/test_templates.R", reporter = "summary", stop_on_failure = TRUE)
testthat::test_file("validation/test_pipeline.R", reporter = "summary", stop_on_failure = TRUE)
testthat::test_file("validation/test_v03.R", reporter = "summary", stop_on_failure = TRUE)
testthat::test_file("validation/test_corpus.R", reporter = "summary", stop_on_failure = TRUE)
testthat::test_file("validation/test_v04.R", reporter = "summary", stop_on_failure = TRUE)
testthat::test_file("validation/test_ai.R", reporter = "summary", stop_on_failure = TRUE)
testthat::test_file("validation/test_measures.R", reporter = "summary", stop_on_failure = TRUE)
testthat::test_file("validation/test_v06.R", reporter = "summary", stop_on_failure = TRUE)
cat("\nAll validation checks passed.\n")
