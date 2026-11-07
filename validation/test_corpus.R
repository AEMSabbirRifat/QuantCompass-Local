# Accuracy checks on the test library (validation/corpus/files): every data format and every codebook format
# must be read correctly. Thresholds are set so that any regression in a reader fails the build.
source(file.path(getOption("qc.root", "."), "validation", "corpus", "score_corpus.R"))

test_that("every data format is read with the right rows, columns and numbers", {
  r <- qc_corpus_data_report()
  expect_equal(nrow(r), 9)
  for (i in seq_len(nrow(r))) {
    expect_true(r$rows_ok[i], info = r$file[i])
    expect_gte(r$columns_found[i], 1 - 1e-9)
    expect_gte(r$numbers_ok[i], 1 - 1e-9)
  }
})

test_that("every codebook format gives the right variables, labels, codes and missing codes", {
  r <- qc_corpus_codebook_report()
  expect_gte(nrow(r), 17)
  for (i in seq_len(nrow(r))) {
    expect_gte(r$variables[i], .95)
    expect_gte(r$labels[i], .95)
    expect_gte(r$codes[i], .95)
    if (!is.na(r$missing[i])) expect_gte(r$missing[i], .95)
    expect_true(isTRUE(r$attention[i]) || is.na(r$attention[i]), info = r$file[i])
  }
  with_rules <- c("C1_sosci_variable_view.txt", "C1_sosci_variable_view.pdf", "C2_word_table.docx", "C3_table.pdf",
                  "C4_word_paragraphs.docx", "C8_plain.txt", "C12_qualtrics.qsf", "C13_limesurvey.lss")
  for (f in with_rules) {
    row <- r[r$file == f, ]
    if (!grepl("qsf$|lss$", f)) expect_true(isTRUE(row$screen_rule), info = paste("screen-out rule:", f))   # these two files contain no screen-out
    expect_true(isTRUE(row$skip_rule), info = paste("skip rule:", f))
  }
})

test_that("filter conditions are never read as answer codes, and free-text fields stay free text", {
  expect_null(qc_parse_codes("blank unless DM02 = 5 (may be blank even then)"))
  expect_null(qc_parse_codes("only if JOB = 1"))
  expect_equal(unname(qc_parse_codes("1 = Yes; 2 = No")), c("Yes", "No"))
  expect_equal(names(qc_parse_codes("Yes (1), No (2)")), c("1", "2"))
  d <- data.frame(G = c(1, 2, 5, 5), G_5 = c("", "", "Agender", "Two-spirit"))
  cand <- list(vars = list(G = qc_cb_entry("G", "Gender", c(`1` = "Male", `2` = "Female", `5` = "Other")),
                           G_5 = qc_cb_entry("G_5", "Gender: other (free text)", c(`5` = "x"))), rules = list())
  cb <- qc_cb_merge(list(cand), d, "test")
  expect_equal(cb$vars$G_5$kind, "text")
  expect_length(cb$vars$G_5$values, 0)
  expect_equal(cb$vars$G$kind, "codes")
})
