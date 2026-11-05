# End-to-end example without the app. Run from the repository root:
#   Rscript examples/run_example.R
# Beginners: use START-QuantCompass.R instead - it opens the click-only app.
options(qc.root = normalizePath("."))
source("engine/load.R")
file <- "examples/example_survey.xlsx"
print(qc_sheets(file))

cb <- qc_read_codebook(file)                          # friendly names from the Codebook sheet
data <- qc_read(file, "Survey_Wave1")
prof <- qc_profile(data)
fixes <- prof$issues$fix[!is.na(prof$issues$fix)]     # e.g. treat 99 in age as missing
ctx <- list(path = file, sheet = "Survey_Wave1", fixes = fixes, labels = cb$labels,
            level_labels = cb$level_labels, out_dir = file.path("results", "example"))

hyps <- qc_read_hypothesis_sheet(file, qc_read(file, "Survey_Wave1", fixes))$hypotheses   # the 'Hypotheses' sheet
desc <- qc_describe_dataset(ctx, by = "remote_work")
res <- lapply(hyps, qc_run_hypothesis, ctx = ctx)
qc_summary_report(res, ctx, desc)

for (r in res) cat("\n", r$id, "-", r$test, "\n", r$paragraph, "\n")
cat("\nReports, charts and scripts saved in results/example/\n")
