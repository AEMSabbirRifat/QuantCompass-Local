# QuantCompass click app. Start it with START-QuantCompass.R (no code needed).
library(shiny)
root <- normalizePath(file.path(getwd(), ".."))
options(qc.root = root, shiny.maxRequestSize = 200 * 1024^2)
source(file.path(root, "engine", "load.R"))
results_root <- file.path(root, "results"); dir.create(results_root, showWarnings = FALSE)
addResourcePath("qcresults", results_root)

types_plain <- c("Compare groups" = "difference", "Before vs after (same cases)" = "paired_difference",
  "Relationship between two variables" = "relationship", "Association between two categories" = "association",
  "Predict an outcome" = "prediction", "Scale reliability" = "reliability")
type_help <- c(difference = "Example: Does house value differ between homes near and not near the river?",
  paired_difference = "Example: Did satisfaction change from before to after the training? (two columns, one row per person)",
  relationship = "Example: Is the number of rooms related to house value?",
  association = "Example: Is remote work (yes/no) associated with department?",
  prediction = "Example: Do rooms, crime rate and pupil-teacher ratio predict house value?",
  reliability = "Example: Do the four job-satisfaction items form a reliable scale?")

card <- function(...) div(class = "card mb-3", div(class = "card-body", ...))
step_note <- function(txt) p(class = "text-muted", txt)

brand <- tags$span(HTML(paste(readLines(file.path(root, "app", "www", "logo_inline.svg"), warn = FALSE), collapse = "")),
  tags$span(tags$span(class = "qc-word", HTML("Quant<b>Compass</b>")), tags$span(class = "qc-tag", "From hypothesis to thesis-ready statistics")))

ui <- navbarPage(
  title = brand, id = "nav", windowTitle = "QuantCompass",
  theme = bslib::bs_theme(version = 5, primary = "#2a78d6"),
  header = tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "logo.svg"),
    tags$link(rel = "stylesheet", href = "quantcompass.css"), tags$script(src = "quantcompass.js")),
  tabPanel("1. Your data",
    fluidRow(column(5,
      card(h5("Choose your data file"), step_note("CSV or Excel. The file stays on your computer."),
        fileInput("file", NULL, accept = c(".csv", ".xlsx", ".xls"), buttonLabel = "Choose file..."),
        uiOutput("sheet_ui")),
      card(h5("Data check"), uiOutput("verdict"), uiOutput("fix_ui")),
      card(h5("Friendly names (optional)"), step_note("Give columns and categories readable names. They are used in all sentences, tables and charts."),
        uiOutput("label_ui"), tableOutput("labels_tbl"))),
    column(7, card(h5("Sheets in this file"), tableOutput("sheets_tbl")),
      card(h5("Your columns"), tableOutput("cols_tbl")),
      card(h5("First rows"), div(style = "overflow-x:auto", tableOutput("head_tbl")))))),
  tabPanel("2. Describe dataset",
    card(h5("Describe my whole dataset"), step_note("Tables, charts and a ready-to-paste summary of every variable. No hypothesis needed."),
      uiOutput("by_ui"), actionButton("describe", "Describe my whole dataset", class = "btn-primary")),
    uiOutput("desc_out")),
  tabPanel("3. Hypotheses",
    fluidRow(column(6,
      card(h5("Option A: build a hypothesis by clicking"),
        uiOutput("ideas_ui"),
        radioButtons("qtype", "What do you want to find out?", choices = types_plain),
        uiOutput("qtype_help"), uiOutput("vars_ui"),
        textInput("htext", "Your hypothesis in your own words (optional)", placeholder = "e.g. House value differs by river location"),
        selectInput("choice", "Test choice", c("Automatic - based on assumption checks (recommended)" = "auto",
          "Always parametric" = "parametric", "Always non-parametric" = "non-parametric")),
        uiOutput("preview_out")),
      card(h5("Option B: many hypotheses from an Excel sheet"),
        step_note("Download the template, fill one row per hypothesis (column names must match your data), then upload it. A sheet called 'Hypotheses' inside your data workbook is also detected."),
        downloadButton("tpl", "Download hypothesis template", class = "btn-outline-primary"), br(), br(),
        fileInput("hfile", NULL, accept = c(".xlsx", ".csv"), buttonLabel = "Upload filled sheet..."),
        uiOutput("sheet_hyp_ui"), uiOutput("hfile_out"))),
    column(6, card(h5("Your hypotheses"), uiOutput("hyp_list"),
      uiOutput("remove_ui"), hr(),
      actionButton("run", "Run all hypotheses", class = "btn-run"),
      step_note("Each hypothesis: assumptions are screened, the test is chosen, the analysis runs, and a report with charts and the exact R script is created."))))),
  tabPanel("4. Results", uiOutput("results_out"))
)

server <- function(input, output, session) {
  rv <- reactiveValues(path = NULL, sheet = NULL, fixes = character(), labels = list(), level_labels = list(),
                       hyps = list(), prefill = NULL, results = NULL, desc = NULL, out_dir = NULL, run_name = NULL)

  # ---- Data ----
  observeEvent(input$file, {
    run <- format(Sys.time(), "%Y-%m-%d_%H%M%S"); d <- file.path(results_root, run); dir.create(file.path(d, "data"), recursive = TRUE)
    path <- file.path(d, "data", input$file$name); file.copy(input$file$datapath, path, overwrite = TRUE)
    rv$path <- path; rv$out_dir <- d; rv$run_name <- run; rv$fixes <- character(); rv$hyps <- list(); rv$results <- NULL; rv$desc <- NULL
    cb <- tryCatch(qc_read_codebook(path), error = function(e) list(labels = list(), level_labels = list()))
    rv$labels <- cb$labels; rv$level_labels <- cb$level_labels
    rv$sheet <- if (tolower(tools::file_ext(path)) %in% c("xlsx", "xls")) readxl::excel_sheets(path)[1] else NULL
  })
  sheets <- reactive({ req(rv$path); qc_sheets(rv$path) })
  output$sheets_tbl <- renderTable({ s <- sheets(); names(s) <- c("Sheet", "Rows", "Columns", "First columns"); s })
  output$sheet_ui <- renderUI({ req(rv$path); s <- sheets()
    if (nrow(s) > 1) selectInput("sheet", "Which sheet holds your data?", s$sheet, selected = rv$sheet) })
  observeEvent(input$sheet, { rv$sheet <- input$sheet; rv$fixes <- character() })
  dat <- reactive({ req(rv$path); qc_read(rv$path, rv$sheet, rv$fixes) })
  prof <- reactive(qc_profile(dat()))
  ctx <- reactive(list(path = rv$path, sheet = rv$sheet, fixes = rv$fixes, labels = rv$labels, level_labels = rv$level_labels, out_dir = rv$out_dir))

  output$verdict <- renderUI({ p <- prof()
    cls <- c(ready = "verdict-ready", needs_fixing = "verdict-fix", blocked = "verdict-blocked")[[p$verdict]]
    txt <- c(ready = "Ready: no problems found.", needs_fixing = "Needs fixing: see the suggestions below.", blocked = "Blocked: no usable data.")[[p$verdict]]
    tagList(p(class = cls, txt), p(sprintf("%d rows, %d columns. %d data fix(es) applied.", p$n_rows, p$n_columns, length(rv$fixes))),
      if (nrow(p$issues)) tags$ul(lapply(seq_len(nrow(p$issues)), function(i) tags$li(tags$b(p$issues$column[i]), ": ", p$issues$message[i])))) })
  output$fix_ui <- renderUI({ iss <- prof()$issues; iss <- iss[!is.na(iss$fix), ]
    if (!nrow(iss)) return(NULL)
    tagList(checkboxGroupInput("fixes", "Apply these fixes (they are written into your R script):",
      choiceNames = paste0(iss$column, ": ", iss$message), choiceValues = iss$fix, selected = iss$fix),
      actionButton("apply_fix", "Apply selected fixes", class = "btn-warning")) })
  observeEvent(input$apply_fix, { rv$fixes <- unique(c(rv$fixes, input$fixes)); showNotification("Fixes applied.", type = "message") })
  output$cols_tbl <- renderTable({ c <- prof()$columns
    data.frame(Column = c$column, `Friendly name` = sapply(c$column, function(v) if (!is.null(rv$labels[[v]])) rv$labels[[v]] else ""),
               Type = c$type, Missing = c$missing, `Distinct values` = c$unique, check.names = FALSE) })
  output$head_tbl <- renderTable({ h <- utils::head(dat(), 8)
    h[] <- lapply(h, function(x) if (is.numeric(x)) format(x, digits = 4, drop0trailing = TRUE) else x); h })

  output$label_ui <- renderUI({ req(rv$path)
    tagList(selectInput("lab_var", "Column", names(dat())), textInput("lab_txt", "What it means", placeholder = "e.g. house value"),
      textInput("lab_levels", "Category meanings (optional)", placeholder = "e.g. 0 = not near the river; 1 = near the river"),
      actionButton("lab_save", "Save name")) })
  observeEvent(input$lab_var, { updateTextInput(session, "lab_txt", value = if (!is.null(rv$labels[[input$lab_var]])) rv$labels[[input$lab_var]] else "")
    lv <- rv$level_labels[[input$lab_var]]
    updateTextInput(session, "lab_levels", value = if (!is.null(lv)) paste(names(lv), "=", lv, collapse = "; ") else "") })
  observeEvent(input$lab_save, { v <- input$lab_var
    labs <- rv$labels; labs[[v]] <- if (nzchar(input$lab_txt)) input$lab_txt else NULL; rv$labels <- labs
    ll <- rv$level_labels; ll[[v]] <- qc_parse_levels(input$lab_levels); rv$level_labels <- ll
    showNotification(paste("Saved name for", v), type = "message") })
  output$labels_tbl <- renderTable({ if (!length(rv$labels)) return(NULL)
    data.frame(Column = names(rv$labels), Name = unlist(rv$labels), Categories = sapply(names(rv$labels), function(v)
      if (!is.null(rv$level_labels[[v]])) paste(names(rv$level_labels[[v]]), "=", rv$level_labels[[v]], collapse = "; ") else "")) })

  # ---- Describe dataset ----
  output$by_ui <- renderUI({ req(rv$path); d <- dat()
    cand <- names(d)[sapply(d, function(x) { u <- length(unique(stats::na.omit(x))); u >= 2 && u <= 12 })]
    tagList(selectInput("by", "Also split numeric variables by group (optional)", c("No split" = "", stats::setNames(cand, sapply(cand, function(v) qc_lab_ui(v, rv$labels))))),
      p(class = "text-muted small", sprintf("Only columns with 2 to 12 different values can split the data into groups (here: %s). Columns with many different values, such as prices or ages, would create hundreds of tiny groups.",
        if (length(cand)) paste(cand, collapse = ", ") else "none"))) })
  observeEvent(input$describe, { req(rv$path)
    withProgress(message = "Describing your dataset...", {
      rv$desc <- tryCatch(qc_describe_dataset(ctx(), if (nzchar(input$by)) input$by else NULL), error = function(e) list(error = conditionMessage(e))) }) })
  output$desc_out <- renderUI({ d <- rv$desc; req(d)
    if (!is.null(d$error)) return(card(p(class = "text-danger", "Something went wrong: ", d$error)))
    rel <- sub(paste0("^", results_root, "/"), "", d$files[["html"]])
    tagList(card(h5("Ready-to-paste summary"), div(class = "paste", d$paragraph), br(),
      downloadButton("desc_docx", "Download Word report"), span(class = "text-muted ms-2", paste("Saved in:", dirname(d$files[["html"]])))),
      tags$iframe(src = paste0("qcresults/", rel))) })
  output$desc_docx <- downloadHandler(filename = "Descriptives_report.docx", content = function(f) file.copy(rv$desc$files[["docx"]], f))

  # ---- Hypotheses: option A ----
  output$qtype_help <- renderUI(p(class = "text-muted small", type_help[[input$qtype]]))
  output$vars_ui <- renderUI({ req(rv$path); d <- dat(); pc <- prof()$columns
    all <- setdiff(names(d), pc$column[pc$type %in% c("id", "empty", "constant")]); d <- d[all]
    num <- all[sapply(d, is.numeric)]; nl <- sapply(d, function(x) length(unique(stats::na.omit(x))))
    grp <- all[nl >= 2 & nl <= 12]; cats <- all[nl >= 2 & nl <= 20]
    kind <- function(v) { u <- nl[[v]]; if (u <= 12 && !(is.numeric(d[[v]]) && u > 7)) sprintf("%d groups", u)
      else if (is.numeric(d[[v]])) sprintf("numbers, %d values", u) else sprintf("text, %d values", u) }
    nm <- function(v) stats::setNames(v, sapply(v, function(x) paste0(qc_lab_ui(x, rv$labels), "  (", kind(x), ")")))
    pf <- rv$prefill; sel <- function(key, fallback) if (!is.null(pf) && identical(pf$qtype, input$qtype) && !is.null(pf[[key]])) pf[[key]] else fallback
    ph <- function(txt) list(placeholder = txt, plugins = list("remove_button"))
    switch(input$qtype,
      difference = tagList(selectInput("v_out", "What is measured? (outcome - numbers)", nm(num), selected = sel("v_out", num[1])),
        selectInput("v_grp", "Which groups? (grouping column)", nm(grp), selected = sel("v_grp", grp[1])),
        p(class = "text-muted small", "Only columns with 2 to 12 different values are listed as groups (e.g. yes/no, gender, department). To study a numeric column such as crime rate or age, choose 'Relationship between two variables' or 'Predict an outcome' instead - splitting numbers into groups throws information away.")),
      paired_difference = tagList(selectInput("v_1", "First measurement (e.g. before)", nm(num), selected = sel("v_1", num[1])),
        selectInput("v_2", "Second measurement (e.g. after)", nm(num), selected = sel("v_2", num[min(2, length(num))]))),
      relationship = tagList(selectInput("v_1", "First variable", nm(all), selected = sel("v_1", all[1])),
        selectInput("v_2", "Second variable", nm(all), selected = sel("v_2", all[min(2, length(all))]))),
      association = tagList(selectInput("v_1", "First category variable", nm(cats), selected = sel("v_1", cats[1])),
        selectInput("v_2", "Second category variable", nm(cats), selected = sel("v_2", cats[min(2, length(cats))]))),
      prediction = tagList(selectInput("v_out", "What do you want to predict? (outcome)", nm(all[sapply(d, is.numeric) | nl == 2]), selected = sel("v_out", NULL)),
        selectizeInput("v_preds", "Predictors (choose one or more)", nm(all), multiple = TRUE, selected = sel("v_preds", NULL), options = ph("Start typing a column name..."))),
      reliability = tagList(selectizeInput("v_items", "Items of the scale", nm(num), multiple = TRUE, selected = sel("v_items", NULL), options = ph("Choose two or more items, e.g. q1, q2, q3")),
        selectizeInput("v_rev", "Negatively worded items to reverse-code (optional)", nm(num), multiple = TRUE, selected = sel("v_rev", NULL), options = ph("Optional")))) })

  # Ideas based on column types only (never on results, to avoid choosing hypotheses after seeing the data)
  ideas <- reactive({ req(rv$path); d <- dat(); pc <- prof()$columns
    all <- setdiff(names(d), pc$column[pc$type %in% c("id", "empty", "constant", "text")])
    nl <- sapply(d[all], function(x) length(unique(stats::na.omit(x))))
    num <- all[sapply(d[all], is.numeric) & nl > 7]; grp <- all[nl >= 2 & nl <= 12]; L <- function(v) qc_lab(v)
    out <- list()
    if (length(num) && length(grp)) out[[length(out) + 1]] <- list(label = sprintf("Compare %s between %s groups", L(utils::tail(num, 1)), L(grp[1])),
      qtype = "difference", v_out = utils::tail(num, 1), v_grp = grp[1])
    if (length(num) >= 2) out[[length(out) + 1]] <- list(label = sprintf("Is %s related to %s?", L(num[1]), L(utils::tail(num, 1))),
      qtype = "relationship", v_1 = num[1], v_2 = utils::tail(num, 1))
    if (length(grp) >= 2) out[[length(out) + 1]] <- list(label = sprintf("Are %s and %s associated?", L(grp[1]), L(grp[2])),
      qtype = "association", v_1 = grp[1], v_2 = grp[2])
    if (length(num) >= 3) out[[length(out) + 1]] <- list(label = sprintf("Predict %s from %s", L(utils::tail(num, 1)), qc_and(sapply(utils::head(num, 2), L))),
      qtype = "prediction", v_out = utils::tail(num, 1), v_preds = utils::head(num, 2))
    for (b in names(prof()$batteries)) out[[length(out) + 1]] <- list(label = sprintf("Reliability of the %s items", b),
      qtype = "reliability", v_items = prof()$batteries[[b]])
    out })
  output$ideas_ui <- renderUI({ req(rv$path); qc_set_labels(ctx()); id <- ideas(); if (!length(id)) return(NULL)
    tagList(p(class = "small text-muted mb-1", "Ideas from your column types - click one to fill in the choices below. Choose hypotheses from your research question, not from what looks significant."),
      div(class = "ideas", lapply(seq_along(id), function(k) tags$button(type = "button", class = "idea", id[[k]]$label,
        onclick = sprintf("Shiny.setInputValue('idea', %d, {priority: 'event'})", k))))) })
  observeEvent(input$idea, { id <- ideas()[[input$idea]]; rv$prefill <- id
    updateRadioButtons(session, "qtype", selected = id$qtype) })

  spec_from_inputs <- function() switch(input$qtype,
    difference = list(goal = "difference", outcome = input$v_out, group = input$v_grp),
    paired_difference = list(goal = "paired_difference", var1 = input$v_1, var2 = input$v_2),
    relationship = list(goal = "relationship", var1 = input$v_1, var2 = input$v_2),
    association = list(goal = "association", var1 = input$v_1, var2 = input$v_2),
    prediction = list(goal = "prediction", outcome = input$v_out, predictors = setdiff(input$v_preds, input$v_out)),
    reliability = list(goal = "reliability", items = input$v_items, reverse = input$v_rev))
  live <- reactive({ req(rv$path); qc_set_labels(ctx()); spec <- spec_from_inputs(); req(spec)
    need <- unlist(spec[setdiff(names(spec), c("goal", "reverse"))])
    if (!length(need) || any(!nzchar(need))) return(list(problem = "Choose the columns above."))
    if (spec$goal %in% c("paired_difference", "relationship", "association") && identical(spec$var1, spec$var2)) return(list(problem = "Please choose two different variables."))
    if (spec$goal == "prediction" && !length(spec$predictors)) return(list(problem = "Choose at least one predictor."))
    if (spec$goal == "reliability" && length(spec$items) < 2) return(list(problem = "Choose at least two items."))
    pair <- tryCatch(qc_hypothesis_pair(spec, dat(), paste0("H", length(rv$hyps) + 1)), error = function(e) NULL)
    sentence <- tryCatch(qc_sentence(spec, dat()), error = function(e) NULL)
    if (is.null(pair) || is.null(sentence)) return(list(problem = "These columns cannot be combined this way."))
    list(spec = spec, sentence = sentence, pair = pair, choice = input$choice,
         text = if (nzchar(input$htext)) input$htext else sub("^We will test whether |^We will check whether ", "", sub("\\.$", "", sentence))) })
  output$preview_out <- renderUI({ pd <- live()
    if (!is.null(pd$problem)) return(p(class = "text-muted", pd$problem))
    tagList(div(class = "paste", tags$b(paste0("H", length(rv$hyps) + 1, ": ")), pd$sentence),
      div(class = "hpair", div(span(class = "lab lab-h1", pd$pair$lab1), "Research hypothesis: ", pd$pair$h1),
        div(span(class = "lab lab-h0", pd$pair$lab0), "Null hypothesis: ", pd$pair$h0),
        div(class = "small text-muted", pd$pair$note)),
      p(class = "mt-2 mb-2", "Is this what you want to test?"),
      actionButton("confirm", "Yes, add this hypothesis", class = "btn-success")) })
  observeEvent(input$confirm, { pd <- live(); req(pd$spec)
    rv$hyps[[length(rv$hyps) + 1]] <- list(id = paste0("H", length(rv$hyps) + 1), text = pd$text, sentence = pd$sentence, spec = pd$spec, choice = pd$choice)
    rv$prefill <- NULL; updateTextInput(session, "htext", value = "")
    showNotification(paste0("Added H", length(rv$hyps), "."), type = "message") })

  # ---- Hypotheses: option B ----
  output$tpl <- downloadHandler(filename = "QuantCompass_Hypotheses_Template.xlsx",
    content = function(f) file.copy(file.path(root, "app", "www", "QuantCompass_Hypotheses_Template.xlsx"), f))
  output$sheet_hyp_ui <- renderUI({ req(rv$path); s <- sheets()
    if ("Hypotheses" %in% s$sheet) actionButton("use_inner", "Use the 'Hypotheses' sheet in my data file", class = "btn-outline-primary") })
  parsed_b <- reactiveVal(NULL)
  observeEvent(input$hfile, { req(rv$path); qc_set_labels(ctx())
    parsed_b(tryCatch(qc_read_hypothesis_sheet(input$hfile$datapath, dat()), error = function(e) list(hypotheses = list(), problems = conditionMessage(e)))) })
  observeEvent(input$use_inner, { qc_set_labels(ctx()); parsed_b(qc_read_hypothesis_sheet(rv$path, dat())) })
  output$hfile_out <- renderUI({ pb <- parsed_b(); req(pb)
    tagList(if (length(pb$problems)) div(class = "text-danger", tags$ul(lapply(pb$problems, tags$li))),
      if (length(pb$hypotheses)) tagList(p(sprintf("%d hypothesis/hypotheses ready:", length(pb$hypotheses))),
        lapply(pb$hypotheses, function(h) div(class = "hyp", tags$b(h$id, ": "), qc_sentence(h$spec, dat()))),
        actionButton("add_b", "Add these hypotheses", class = "btn-success"))) })
  observeEvent(input$add_b, { pb <- parsed_b(); req(pb); qc_set_labels(ctx())
    for (h in pb$hypotheses) { h$id <- paste0("H", length(rv$hyps) + 1); h$sentence <- qc_sentence(h$spec, dat()); rv$hyps[[length(rv$hyps) + 1]] <- h }
    parsed_b(NULL); showNotification("Hypotheses added.", type = "message") })

  output$hyp_list <- renderUI({ if (!length(rv$hyps)) return(p(class = "text-muted", "No hypotheses yet. Add them with Option A or B."))
    qc_set_labels(ctx())
    lapply(rv$hyps, function(h) { pr <- tryCatch(qc_hypothesis_pair(h$spec, dat(), h$id), error = function(e) NULL)
      div(class = "hyp", tags$b(h$id, ": "), h$sentence, if (!identical(h$choice, "auto")) span(class = "badge bg-secondary ms-1", h$choice),
        if (!is.null(pr)) div(class = "small text-muted", paste0(pr$lab0, " (null): "), pr$h0)) }) })
  output$remove_ui <- renderUI({ if (length(rv$hyps)) tagList(br(), selectInput("rm_h", NULL, sapply(rv$hyps, `[[`, "id"), width = "120px"),
    actionButton("rm", "Remove", class = "btn-outline-danger btn-sm")) })
  observeEvent(input$rm, { rv$hyps <- Filter(function(h) h$id != input$rm_h, rv$hyps)
    for (i in seq_along(rv$hyps)) rv$hyps[[i]]$id <- paste0("H", i) })

  # ---- Run ----
  observeEvent(input$run, { req(rv$path)
    if (!length(rv$hyps)) { showNotification("Add at least one hypothesis first.", type = "warning"); return() }
    c0 <- ctx(); res <- list()
    withProgress(message = "Running analyses", value = 0, {
      for (i in seq_along(rv$hyps)) { h <- rv$hyps[[i]]
        incProgress(1 / (length(rv$hyps) + 1), detail = paste(h$id, "- screening assumptions and running the test"))
        res[[i]] <- tryCatch(qc_run_hypothesis(h, c0), error = function(e) list(id = h$id, sentence = h$sentence, blocked = TRUE,
          message = paste("The analysis could not be completed:", conditionMessage(e)))) }
      incProgress(1 / (length(rv$hyps) + 1), detail = "Writing the summary report")
      summ <- tryCatch(qc_summary_report(res, c0, if (!is.null(rv$desc) && is.null(rv$desc$error)) rv$desc else NULL), error = function(e) NULL) })
    rv$results <- list(items = res, summary = summ); updateNavbarPage(session, "nav", "4. Results") })

  output$results_out <- renderUI({ r <- rv$results
    if (is.null(r)) return(card(p(class = "text-muted", "Run your hypotheses in step 3 to see results here.")))
    tagList(card(h5("All results"), p("Everything is saved in: ", tags$code(rv$out_dir)),
        if (!is.null(r$summary)) downloadButton("sum_docx", "Download summary report (Word)"),
        actionButton("open_dir", "Open results folder", class = "btn-outline-secondary ms-2")),
      lapply(seq_along(r$items), function(i) { x <- r$items[[i]]
        if (isTRUE(x$blocked)) return(card(h5(x$id), p(x$sentence), p(class = "text-danger", x$message)))
        rel <- sub(paste0("^", results_root, "/"), "", x$files[["html"]])
        card(h5(paste0(x$id, ": ", x$test)),
          div(class = "hpair", div(span(class = "lab lab-h1", x$lab1), "Research hypothesis: ", x$h1), div(span(class = "lab lab-h0", x$lab0), "Null hypothesis: ", x$h0)),
          span(class = paste("decision", if (isTRUE(x$decision$rejected)) "rej" else "keep"), x$decision$short),
          p(tags$b("Why this test: "), x$reason),
          div(class = "paste", x$paragraph), br(),
          downloadButton(paste0("dl_docx_", i), "Word report"), downloadButton(paste0("dl_r_", i), "R script"),
          tags$details(class = "mt-3", tags$summary("Show full report with charts"), tags$iframe(src = paste0("qcresults/", rel)))) })) })
  observe({ r <- rv$results; req(r)
    for (i in seq_along(r$items)) local({ j <- i; x <- r$items[[j]]; if (isTRUE(x$blocked)) return()
      output[[paste0("dl_docx_", j)]] <- downloadHandler(filename = basename(x$files[["docx"]]), content = function(f) file.copy(x$files[["docx"]], f))
      output[[paste0("dl_r_", j)]] <- downloadHandler(filename = basename(x$files[["script"]]), content = function(f) file.copy(x$files[["script"]], f)) }) })
  output$sum_docx <- downloadHandler(filename = "QuantCompass_summary_report.docx", content = function(f) file.copy(rv$results$summary[["docx"]], f))
  observeEvent(input$open_dir, { d <- rv$out_dir
    if (.Platform$OS.type == "windows") shell.exec(d) else if (Sys.info()[["sysname"]] == "Darwin") system2("open", d) else system2("xdg-open", d, wait = FALSE) })
}

qc_lab_ui <- function(v, labels) if (!is.null(labels[[v]]) && nzchar(labels[[v]])) paste0(labels[[v]], " (", v, ")") else v

shinyApp(ui, server)
