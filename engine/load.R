# Load the QuantCompass engine. Run from the repository root:
#   source("engine/load.R")
local({
  root <- getOption("qc.root", ".")
  for (f in c("helpers.R", "io.R", "import.R", "codebook.R", "prepare.R", "dissect.R", "profile.R", "rules.R", "render.R", "assumptions.R",
             "plots.R", "glossary.R", "report.R", "pipeline.R"))
    sys.source(file.path(root, "engine", f), envir = globalenv())
})
