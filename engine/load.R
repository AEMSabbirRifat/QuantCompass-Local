# Load the QuantCompass engine. Run from the repository root:
#   source("engine/load.R")
local({
  root <- getOption("qc.root", ".")
  for (f in c("helpers.R", "io.R", "import.R", "codebook.R", "codebook_general.R", "prepare.R", "dissect.R", "hypothesis_general.R", "profile.R", "rules.R", "render.R", "assumptions.R",
             "plots.R", "glossary.R", "report.R", "pipeline.R", "ai.R", "measures.R"))
    sys.source(file.path(root, "engine", f), envir = globalenv())
})
