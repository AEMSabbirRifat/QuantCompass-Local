# End-to-end example (no AI needed). Run from the repository root:
#   Rscript examples/run_example.R
# It walks through the same steps the app will guide students through.
options(qc.root = normalizePath("."))
source("engine/load.R")
file <- "examples/example_survey.xlsx"
dir.create("output", showWarnings = FALSE)

# Step 1 - which sheet? (the workbook has 3)
print(qc_sheets(file))
sheet <- "Survey_Wave1"

# Step 2 - profile the data and report problems before any test runs
data <- qc_read(file, sheet)
prof <- qc_profile(data)
cat("\nReadiness:", prof$verdict, "\n"); print(prof$issues)
cat("Item batteries found:", paste(names(prof$batteries), collapse = ", "), "\n")

# Step 3 - the student's hypotheses, mapped to columns
hypotheses <- list(
  H1 = list(text = "Job satisfaction items form a reliable scale",
            spec = list(goal = "reliability", items = prof$batteries$js, reverse = "js_3", scale_min = 1, scale_max = 5)),
  H2 = list(text = "Well-being differs between remote and on-site workers",
            spec = list(goal = "difference", outcome = "wellbeing", group = "remote_work")),
  H3 = list(text = "Workload predicts lower well-being",
            spec = list(goal = "prediction", outcome = "wellbeing", predictors = c("workload", "remote_work"))))

# Step 4 - rules engine suggests valid tests; the student picks one (here: the recommended one)
for (h in names(hypotheses)) {
  opts <- qc_suggest(data, hypotheses[[h]]$spec)
  cat("\n==", h, ":", hypotheses[[h]]$text, "\n")
  for (o in opts) cat(sprintf(" - [%s] %s (%s): %s %s\n", o$role, o$label, o$status, o$why, paste(o$messages, collapse = " ")))
  chosen <- opts[[1]]
  if (chosen$status == "blocked") next
  out <- qc_analyse(file, sheet, chosen, hypotheses[[h]]$text)
  writeLines(out$script, file.path("output", paste0(h, "_", chosen$test_id, ".R")))  # appendix-ready script
  cat("Result:", out$result$apa, "\n")
}
cat("\nScripts saved in output/ - each one reproduces its result exactly.\n")
