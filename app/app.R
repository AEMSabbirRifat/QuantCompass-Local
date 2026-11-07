# QuantCompass click app. Start it with START-QuantCompass.R (no code needed).
# Steps: 1 Data & codebook -> 2 Prepare -> 3 Final data (OK / Needs editing) -> 4 Describe -> 5 Hypotheses -> 6 Results.
# Nothing is analysed before the final dataset is confirmed with "OK, use this data".
library(shiny)
root <- normalizePath(file.path(getwd(), ".."))
options(qc.root = root, shiny.maxRequestSize = 300 * 1024^2)
source(file.path(root, "engine", "load.R"))
results_root <- file.path(root, "results"); dir.create(results_root, showWarnings = FALSE)
addResourcePath("qcresults", results_root)

`%||%` <- function(a, b) if (is.null(a)) b else a
card <- function(...) div(class = "card mb-3", div(class = "card-body", ...))
note <- function(...) p(class = "text-muted small", ...)
sym <- function(s) span(class = paste0("sym sym-", s), c(ok = "✓", warn = "⚠", missing = "✗")[[s]])
lab_ui <- function(v, labels) if (!is.null(labels[[v]]) && nzchar(labels[[v]]) && labels[[v]] != v) paste0(labels[[v]], " (", v, ")") else v
named <- function(vs, labels) stats::setNames(vs, vapply(vs, lab_ui, "", labels = labels))
locked_card <- function() card(h5("Confirm your final dataset first"), p("Analyses use the final dataset only. Check it in step 3 and press ", tags$b("OK, use this data"), "."),
  actionButton("go_gate", "Go to step 3: Final data", class = "btn-primary"))

brand <- tags$span(HTML(paste(readLines(file.path(root, "app", "www", "logo_inline.svg"), warn = FALSE), collapse = "")),
  tags$span(tags$span(class = "qc-word", HTML("Quant<b>Compass</b>"))))

ui <- navbarPage(
  title = brand, id = "nav", windowTitle = "QuantCompass",
  theme = bslib::bs_theme(version = 5, primary = "#2a78d6"),
  header = tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "logo.svg"),
    tags$link(rel = "stylesheet", href = "quantcompass.css"), tags$script(src = "quantcompass.js")),

  tabPanel("1. Data & codebook",
    fluidRow(column(5,
      card(h5("Your data file"), note("CSV, TSV or TXT (any encoding, incl. SoSci Survey exports), Excel, OpenDocument, SPSS (.sav), Stata (.dta) or SAS. The file stays on your computer."),
        fileInput("file", NULL, buttonLabel = "Choose data file...", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".ods", ".sav", ".zsav", ".por", ".dta", ".sas7bdat", ".xpt")),
        uiOutput("sheet_ui"), uiOutput("data_info")),
      card(h5("Your codebook (recommended)"), note("PDF, Word, Excel/CSV, TXT/HTML, Qualtrics (.qsf), LimeSurvey (.lss) or the labels stored in an SPSS/Stata file. Scanned PDFs are reported, never guessed."),
        fileInput("cbfile", NULL, buttonLabel = "Choose codebook...", accept = c(".pdf", ".docx", ".xlsx", ".xls", ".csv", ".txt", ".html", ".htm", ".qsf", ".lss", ".sav", ".dta")),
        uiOutput("cb_embedded_ui"), uiOutput("cb_info")),
      card(h5("Friendly names"), note("Readable names for columns and categories. They appear in every sentence, table and chart."), uiOutput("label_ui"))),
    column(7,
      card(h5("Codebook check"), note("✓ matches the codebook  ⚠ needs your decision  ✗ not in the codebook. Nothing marked ⚠ is applied unless you tick it."),
        uiOutput("cb_check_ui")),
      card(h5("Survey rules found in the codebook"), uiOutput("rules_ui")),
      card(h5("First rows"), div(style = "overflow-x:auto", tableOutput("head_tbl")))))),

  tabPanel("2. Prepare",
    uiOutput("prep_gate"),
    fluidRow(column(6,
      card(h5("A. Exclude cases"), note("Suggested from the survey's own rules. Switch any step off or on; they are applied in this order."), uiOutput("excl_ui")),
      card(h5("B. Skip-logic and rule-break check"), note("Only rules defined in your survey are checked: skip/filter logic and valid answer codes."), uiOutput("rb_ui")),
      card(h5("C. Exclude cases by ID"), uiOutput("manual_ui"))),
    column(6,
      card(h5("D. Scales"), note("Combine items into one score. Check each item's answer direction before averaging."), uiOutput("scales_ui")),
      card(h5("E. Groups and recodes"), note("Build group variables from conditions, e.g. 'ES01 is 1,2 and ES02 is 1'. A case gets the first category whose condition it meets."), uiOutput("groups_ui")),
      card(actionButton("to_gate", "Continue to the final data check", class = "btn-run"))))),

  tabPanel("3. Final data", uiOutput("gate_ui")),
  tabPanel("4. Describe", uiOutput("describe_ui")),
  tabPanel("5. Hypotheses", uiOutput("hyp_ui")),
  tabPanel("6. Results", uiOutput("results_ui"))
)

server <- function(input, output, session) {
  rv <- reactiveValues(path = NULL, sheet = NULL, raw = NULL, out_dir = NULL, cb = NULL, cb_check = NULL,
    labels = list(), level_labels = list(), accept_na = character(), fixes_nocb = character(),
    excl = list(), rb_decisions = list(), rb_done = FALSE, hint_excl = character(), manual = data.frame(id = character(), reason = character()),
    scales = list(), groups = list(), locked = FALSE, final = NULL, saved = NULL,
    parts = list(), hyps = list(), results = NULL, desc = NULL, breakdowns = list())

  # ======================================================================= 1. data & codebook
  observeEvent(input$file, {
    run <- format(Sys.time(), "%Y-%m-%d_%H%M%S"); d <- file.path(results_root, run); dir.create(file.path(d, "data"), recursive = TRUE)
    path <- file.path(d, "data", input$file$name); file.copy(input$file$datapath, path, overwrite = TRUE)
    rv$path <- path; rv$out_dir <- d; rv$sheet <- if (qc_file_kind(path) == "excel") readxl::excel_sheets(path)[1] else NULL
    rv$cb <- NULL; rv$locked <- FALSE; rv$results <- NULL; rv$parts <- list(); rv$hyps <- list(); rv$scales <- list(); rv$groups <- list()
    rv$manual <- data.frame(id = character(), reason = character()); rv$rb_decisions <- list(); rv$rb_done <- FALSE
    load_raw()
    emb <- tryCatch(qc_embedded_codebook(path), error = function(e) NULL)
    if (qc_file_kind(path) == "excel" && any(grepl("(?i)codebook|variables|labels|dictionary", readxl::excel_sheets(path), perl = TRUE)))
      emb <- tryCatch(qc_read_any_codebook(path, rv$raw), error = function(e) emb)
    if (!is.null(emb) && length(emb$vars)) set_codebook(emb)
  })
  load_raw <- function() { rv$raw <- tryCatch(qc_read(rv$path, rv$sheet), error = function(e) { showNotification(paste("Could not read the file:", conditionMessage(e)), type = "error"); NULL }) }
  output$sheet_ui <- renderUI({ req(rv$path); if (qc_file_kind(rv$path) != "excel") return(NULL)
    s <- qc_sheets(rv$path); if (nrow(s) > 1) selectInput("sheet", "Which sheet holds your data?", s$sheet, selected = rv$sheet) })
  observeEvent(input$sheet, { if (!identical(input$sheet, rv$sheet)) { rv$sheet <- input$sheet; load_raw() } })
  output$data_info <- renderUI({ req(rv$raw); f <- if (qc_file_kind(rv$path) == "text") qc_text_format(rv$path) else NULL
    tagList(p(tags$b(sprintf("%d cases, %d columns", nrow(rv$raw), ncol(rv$raw)))),
      if (!is.null(f)) note(sprintf("Read as %s text, separated by %s%s.", f$encoding, c("\t" = "tabs", ";" = "semicolons", "," = "commas", "|" = "bars")[[f$sep]],
        if (f$label_row) "; row 2 holds question texts and was used as labels, not as data" else ""))) })

  set_codebook <- function(cb) {
    rv$cb <- cb; rv$cb_check <- qc_codebook_check(cb, rv$raw)
    ap <- qc_codebook_apply(cb, rv$raw); rv$labels <- utils::modifyList(ap$labels, rv$labels[setdiff(names(rv$labels), names(ap$labels))])
    rv$level_labels <- ap$level_labels; rv$accept_na <- character()
    rv$excl <- qc_exclusion_suggestions(rv$raw, cb); rv$locked <- FALSE
  }
  observeEvent(input$cbfile, { req(rv$raw)
    ext <- tools::file_ext(input$cbfile$name); p <- file.path(rv$out_dir, "data", input$cbfile$name); file.copy(input$cbfile$datapath, p, overwrite = TRUE)
    withProgress(message = "Reading the codebook...", { cb <- qc_read_any_codebook(p, rv$raw) })
    set_codebook(cb)
    showNotification(sprintf("Codebook read: %d variables, %d survey rules.", length(cb$vars), length(cb$rules)), type = "message") })
  output$cb_embedded_ui <- renderUI(NULL)
  output$cb_info <- renderUI({ cb <- rv$cb; if (is.null(cb)) return(note("No codebook yet. Without one, QuantCompass only checks the data itself (missing-value codes, types)."))
    tagList(p(sprintf("Source: %s. %d variables, %d survey rules.", cb$source, length(cb$vars), length(cb$rules))),
      if (length(cb$problems)) div(class = "text-danger", tags$ul(lapply(cb$problems, tags$li)))) })

  output$cb_check_ui <- renderUI({ req(rv$raw)
    if (is.null(rv$cb)) { pr <- qc_profile(rv$raw); iss <- pr$issues[!is.na(pr$issues$fix), ]
      return(tagList(note("No codebook loaded. Suggestions from the data only:"),
        if (nrow(iss)) checkboxGroupInput("nocb_fix", NULL, choiceNames = paste0(iss$column, ": ", iss$message), choiceValues = iss$fix, selected = rv$fixes_nocb)
        else p("No problems found."))) }
    ch <- rv$cb_check; tab <- table(factor(ch$status, c("ok", "warn", "missing")))
    warn <- ch[ch$status == "warn", ]; nas <- warn[nzchar(warn$na_codes), ]
    tagList(p(sym("ok"), sprintf(" %d match", tab[["ok"]]), "  ", sym("warn"), sprintf(" %d need a decision", tab[["warn"]]), "  ", sym("missing"), sprintf(" %d not in the codebook", tab[["missing"]])),
      if (nrow(nas)) checkboxGroupInput("accept_na", "Treat these codes as missing (they are not scores):",
        choiceNames = sprintf("%s: code %s = '%s'", nas$variable, nas$na_codes, nas$na_labels), choiceValues = nas$variable, selected = rv$accept_na),
      div(class = "cbtable", tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(""), tags$th("Variable"), tags$th("Label"), tags$th("Codes"), tags$th("Check"))),
        tags$tbody(lapply(order(match(ch$status, c("warn", "missing", "ok"))), function(i) tags$tr(class = paste0("row-", ch$status[i]),
          tags$td(sym(ch$status[i])), tags$td(tags$code(ch$variable[i])), tags$td(if (is.na(ch$label[i])) "" else ch$label[i]),
          tags$td(class = "small", ch$values[i]), tags$td(class = "small", ch$message[i])))))))
  })
  observeEvent(input$accept_na, ignoreNULL = FALSE, { if (is.null(rv$cb)) return()
    new <- if (is.null(input$accept_na)) character() else input$accept_na; if (identical(sort(new), sort(rv$accept_na))) return()
    rv$accept_na <- new
    ap <- qc_codebook_apply(rv$cb, rv$raw, rv$accept_na); rv$level_labels <- ap$level_labels; rv$locked <- FALSE })
  observeEvent(input$nocb_fix, ignoreNULL = FALSE, { rv$fixes_nocb <- if (is.null(input$nocb_fix)) character() else input$nocb_fix; rv$locked <- FALSE })

  output$rules_ui <- renderUI({ cb <- rv$cb; if (is.null(cb) || !length(cb$rules)) return(note("No skip or filter rules found."))
    tags$ul(lapply(cb$rules, function(r) tags$li(if (isTRUE(r$readable)) sprintf("Page %s: if %s, the survey goes to '%s'%s.", r$page, gsub("[()]", "", r$condition_php), r$target,
      if (isTRUE(r$screen_out)) " (end: respondent screened out)" else sprintf(" - skipped: %s", qc_and(utils::head(r$vars_skipped, 6))))
      else span(class = "text-warning", sprintf("Page %s: a rule could not be read automatically and is not checked: %s", r$page, substr(r$text, 1, 120)))))) })
  output$head_tbl <- renderTable({ req(rv$raw); h <- utils::head(rv$raw, 6); h[] <- lapply(h, function(x) if (is.numeric(x)) format(x, digits = 4, drop0trailing = TRUE) else x); h })

  output$label_ui <- renderUI({ req(rv$raw)
    tagList(selectInput("lab_var", "Column", named(c(names(rv$raw), vapply(rv$scales, `[[`, "", "name"), vapply(rv$groups, `[[`, "", "name")), rv$labels)),
      textInput("lab_txt", "Friendly name", placeholder = "e.g. mental well-being"),
      textInput("lab_levels", "Category meanings (optional)", placeholder = "e.g. 1 = Yes; 2 = No"),
      actionButton("lab_save", "Save name", class = "btn-outline-primary")) })
  observeEvent(input$lab_var, { updateTextInput(session, "lab_txt", value = if (!is.null(rv$labels[[input$lab_var]])) rv$labels[[input$lab_var]] else "")
    lv <- rv$level_labels[[input$lab_var]]; updateTextInput(session, "lab_levels", value = if (!is.null(lv)) paste(names(lv), "=", lv, collapse = "; ") else "") })
  observeEvent(input$lab_save, { v <- input$lab_var; l <- rv$labels; l[[v]] <- if (nzchar(input$lab_txt)) input$lab_txt else NULL; rv$labels <- l
    ll <- rv$level_labels; ll[v] <- list(if (nzchar(trimws(input$lab_levels))) qc_value_string(input$lab_levels) else NULL); rv$level_labels <- ll; showNotification(paste("Saved name for", v), type = "message") })

  # ======================================================================= 2. prepare
  id_var <- reactive({ req(rv$raw); qc_id_column(rv$raw) })
  cb_code <- reactive({ if (!is.null(rv$cb)) qc_codebook_apply(rv$cb, rv$raw, rv$accept_na)$code else rv$fixes_nocb })
  prep_obj <- function(manual = rv$manual, scales = rv$scales, groups = rv$groups) {
    ex <- lapply(rv$excl, function(x) { x$on <- isTRUE(input[[paste0("ex_", x$id)]]) || (is.null(input[[paste0("ex_", x$id)]]) && isTRUE(x$on)); x })
    list(codebook_code = cb_code(), exclusions = ex, manual = manual, scales = scales, groups = groups, id_var = id_var(), accept_na = rv$accept_na)
  }
  output$prep_gate <- renderUI({ if (is.null(rv$raw)) card(p("Load your data in step 1 first.")) })
  output$excl_ui <- renderUI({ req(rv$raw)
    if (!length(rv$excl)) return(note("No exclusion rules were found (no completion flag, screening rule or attention check)."))
    tagList(lapply(rv$excl, function(x) div(class = "excl", checkboxInput(paste0("ex_", x$id), tags$b(qc_cap_first(x$label)), value = isTRUE(x$on)), note(x$reason))),
      uiOutput("excl_flow")) })
  base_res <- reactive({ req(rv$raw); p <- prep_obj(manual = data.frame(id = character(), reason = character()), scales = list(), groups = list())
    tryCatch(qc_apply_prep(rv$raw, p), error = function(e) NULL) })
  output$excl_flow <- renderUI({ r <- base_res(); req(r); div(class = "flow", lapply(r$flow, div)) })
  observe({ for (x in rv$excl) input[[paste0("ex_", x$id)]]; isolate({ rv$locked <- FALSE; rv$rb_done <- FALSE }) })

  rule_breaks <- reactive({ r <- base_res(); req(r); if (is.null(rv$cb)) return(data.frame(id = character(), type = character(), why = character()))
    qc_rule_breaks(r$data, rv$cb, id_var()) })
  hints <- reactive({ r <- base_res(); req(r); if (is.null(rv$cb)) return(list()); qc_pattern_hints(r$data, rv$cb, id_var()) })
  output$rb_ui <- renderUI({ rb <- rule_breaks(); h <- hints()
    top <- if (!nrow(rb)) tagList(p(class = "verdict-ready", "No skip-logic or rule breaks were found."),
        actionButton("rb_continue", "No skip-logic or rule breaks, continue", class = "btn-success"))
      else tagList(p(sprintf("%d case(s) break a rule of your survey. Decide for each one:", nrow(rb))),
        lapply(seq_len(nrow(rb)), function(i) div(class = "rb", tags$b(sprintf("%s %s", id_var(), rb$id[i])), span(class = "badge bg-warning text-dark ms-1", rb$type[i]),
          radioButtons(paste0("rb_", i), NULL, c("Keep", "Exclude"), selected = if (is.null(rv$rb_decisions[[as.character(rb$id[i])]])) character(0) else rv$rb_decisions[[as.character(rb$id[i])]], inline = TRUE),
          tags$details(tags$summary("Why?"), p(class = "small", rb$why[i])))),
        actionButton("rb_apply", "Save decisions", class = "btn-primary"))
    tagList(top, if (length(h)) tagList(hr(), p(tags$b("Optional pattern hints"), " (kept by default; not rule breaks):"),
      lapply(names(h), function(b) checkboxInput(paste0("hint_", b), paste(h[[b]]$text, "Tick to exclude them."), value = b %in% rv$hint_excl)))) })
  observeEvent(input$rb_continue, { rv$rb_done <- TRUE; showNotification("Rule check done.", type = "message") })
  observeEvent(input$rb_apply, { rb <- rule_breaks(); dec <- list(); man <- rv$manual[!grepl("^breaks the survey|^answer code not defined", rv$manual$reason), ]
    for (i in seq_len(nrow(rb))) { v <- input[[paste0("rb_", i)]]
      if (is.null(v)) { showNotification(sprintf("Choose Keep or Exclude for %s %s.", id_var(), rb$id[i]), type = "warning"); return() }
      dec[[as.character(rb$id[i])]] <- v
      if (v == "Exclude") man <- rbind(man, data.frame(id = as.character(rb$id[i]), reason = if (rb$type[i] == "skip logic") "breaks the survey's skip logic" else "answer code not defined in the codebook")) }
    rv$rb_decisions <- dec; rv$manual <- unique(man); rv$rb_done <- TRUE; rv$locked <- FALSE; showNotification("Decisions saved.", type = "message") })
  observe({ h <- hints(); sel <- names(h)[vapply(names(h), function(b) isTRUE(input[[paste0("hint_", b)]]), logical(1))]
    isolate({ old <- rv$hint_excl; if (!identical(sort(sel), sort(old))) {
      man <- rv$manual[!grepl("^identical answers", rv$manual$reason), ]
      for (b in sel) man <- rbind(man, data.frame(id = as.character(h[[b]]$ids), reason = paste("identical answers on all", b, "items")))
      rv$manual <- unique(man); rv$hint_excl <- sel; rv$locked <- FALSE } }) })

  output$manual_ui <- renderUI({ req(rv$raw)
    tagList(fluidRow(column(4, textInput("man_id", sprintf("%s to exclude", id_var()), placeholder = "e.g. 624")),
      column(6, textInput("man_reason", "Reason", placeholder = "e.g. answered job questions without a job")),
      column(2, br(), actionButton("man_add", "Add", class = "btn-outline-primary"))),
      if (nrow(rv$manual)) tagList(tags$ul(lapply(seq_len(nrow(rv$manual)), function(i) tags$li(sprintf("%s %s - %s", id_var(), rv$manual$id[i], rv$manual$reason[i])))),
        selectInput("man_rm", NULL, rv$manual$id, width = "160px"), actionButton("man_del", "Remove", class = "btn-outline-danger btn-sm"))) })
  observeEvent(input$man_add, { id <- trimws(input$man_id); req(nzchar(id))
    if (!id %in% as.character(rv$raw[[id_var()]])) { showNotification(sprintf("%s %s is not in the data.", id_var(), id), type = "error"); return() }
    if (!nzchar(trimws(input$man_reason))) { showNotification("Please give a short reason (it goes into the methods section).", type = "warning"); return() }
    rv$manual <- unique(rbind(rv$manual, data.frame(id = id, reason = trimws(input$man_reason)))); rv$locked <- FALSE
    updateTextInput(session, "man_id", value = ""); updateTextInput(session, "man_reason", value = "") })
  observeEvent(input$man_del, { rv$manual <- rv$manual[rv$manual$id != input$man_rm, ]; rv$locked <- FALSE })

  # ---- scales
  scale_sugg <- reactive({ req(rv$raw); if (is.null(rv$cb)) return(list()); qc_scale_suggestions(rv$cb, rv$raw) })
  output$scales_ui <- renderUI({ req(rv$raw); sg <- scale_sugg(); num <- names(rv$raw)[vapply(rv$raw, is.numeric, logical(1))]
    tagList(
      if (length(rv$scales)) tagList(tags$b("Your scales"), tags$ul(lapply(seq_along(rv$scales), function(i) { s <- rv$scales[[i]]; a <- tryCatch(qc_scale_alpha(cur_res()$data, s), error = function(e) list(alpha = NA))
        tags$li(tags$code(s$name), sprintf(" %s: %d item(s), %s%s%s", s$label, length(s$items), s$method, if (length(s$reverse)) paste0(", reversed: ", paste(s$reverse, collapse = ", ")) else "",
          if (!is.na(a$alpha)) sprintf(", alpha = %s%s", qc_fmt_r(a$alpha), if (a$alpha < .7) " (below .70)" else "") else ""),
          actionLink(paste0("sc_del_", i), "remove", onclick = sprintf("Shiny.setInputValue('sc_del', %d, {priority:'event'})", i))) }))),
      if (length(sg) && any(vapply(sg, function(s) !(s$name %in% vapply(rv$scales, `[[`, "", "name") || all(s$items %in% unlist(lapply(rv$scales, `[[`, "items")))), logical(1))))
        tagList(tags$b("Suggested from the codebook"), lapply(seq_along(sg), function(i) { s <- sg[[i]]
        if (s$name %in% vapply(rv$scales, `[[`, "", "name") || all(s$items %in% unlist(lapply(rv$scales, `[[`, "items")))) return(NULL)
        div(class = "sugg", tags$code(paste(s$items, collapse = ", ")), if (!is.na(s$preset)) span(class = "badge bg-info ms-1", "WHO-5 detected"),
          tags$details(tags$summary("Answer directions"), tags$ul(class = "small", lapply(s$anchors, tags$li))),
          actionButton(paste0("sg_use_", i), "Use as starting point", class = "btn-sm btn-outline-primary",
            onclick = sprintf("Shiny.setInputValue('sg_use', %d, {priority:'event'})", i))) })),
      hr(), tags$b("Build or edit a scale"),
      fluidRow(column(6, textInput("sc_name", "Short name (no spaces)", placeholder = "e.g. fin_stress")), column(6, textInput("sc_label", "What it measures", placeholder = "e.g. subjective financial stress"))),
      selectizeInput("sc_items", "Items", named(num, rv$labels), multiple = TRUE),
      selectizeInput("sc_rev", "Reverse-code these items", named(num, rv$labels), multiple = TRUE),
      fluidRow(column(6, selectInput("sc_method", "Score", c("Mean of items" = "mean", "Sum of items" = "sum", "WHO-5 index 0-100" = "who5"))),
        column(3, numericInput("sc_min", "Lowest code", 1)), column(3, numericInput("sc_max", "Highest code", 5))),
      uiOutput("sc_preview"), actionButton("sc_add", "Add scale", class = "btn-primary")) })
  observeEvent(input$sg_use, { s <- scale_sugg()[[input$sg_use]]
    updateTextInput(session, "sc_name", value = s$name); updateSelectizeInput(session, "sc_items", selected = s$items)
    updateSelectInput(session, "sc_method", selected = s$method); updateNumericInput(session, "sc_min", value = s$min); updateNumericInput(session, "sc_max", value = s$max) })
  observeEvent(input$sc_items, { req(rv$cb); e <- rv$cb$vars[[input$sc_items[1]]]; if (is.null(e)) return()
    codes <- suppressWarnings(as.numeric(names(e$values))); if (input$sc_items[1] %in% rv$accept_na) codes <- codes[!grepl(qc_na_like_re, e$values, perl = TRUE)]
    if (length(codes)) { updateNumericInput(session, "sc_min", value = min(codes)); updateNumericInput(session, "sc_max", value = max(codes)) } })
  output$sc_preview <- renderUI({ req(length(input$sc_items) >= 1)
    s <- list(name = "tmp", items = input$sc_items, reverse = input$sc_rev, method = input$sc_method, min = input$sc_min, max = input$sc_max)
    d <- tryCatch({ x <- base_res()$data; e <- new.env(); assign("data", x, e); eval(parse(text = qc_scale_code(s)), e); get("data", e) }, error = function(e) NULL)
    if (is.null(d)) return(NULL); a <- qc_scale_alpha(d, s)
    note(sprintf("Preview: %d cases with a score%s.", sum(!is.na(d$tmp)), if (!is.na(a$alpha)) sprintf(", Cronbach's alpha = %s", qc_fmt_r(a$alpha)) else "")) })
  observeEvent(input$sc_add, { nm <- make.names(trimws(input$sc_name))
    if (!nzchar(input$sc_name) || !length(input$sc_items)) { showNotification("Give the scale a name and choose its items.", type = "warning"); return() }
    if (nm %in% names(rv$raw)) { showNotification("That name is already a column. Choose another.", type = "error"); return() }
    s <- list(name = nm, label = trimws(input$sc_label), items = input$sc_items, reverse = input$sc_rev, method = input$sc_method, min = input$sc_min, max = input$sc_max)
    rv$scales <- c(Filter(function(x) x$name != nm, rv$scales), list(s)); l <- rv$labels; if (nzchar(s$label)) l[[nm]] <- s$label; rv$labels <- l; rv$locked <- FALSE
    updateTextInput(session, "sc_name", value = ""); updateTextInput(session, "sc_label", value = "") })
  observeEvent(input$sc_del, { rv$scales <- rv$scales[-input$sc_del]; rv$locked <- FALSE })

  # ---- groups
  output$groups_ui <- renderUI({ req(rv$raw)
    tagList(
      if (length(rv$groups)) tagList(tags$b("Your groups"), tags$ul(lapply(seq_along(rv$groups), function(i) { g <- rv$groups[[i]]
        tags$li(tags$code(g$name), " ", g$label, tags$ul(class = "small", lapply(g$groups, function(r) tags$li(sprintf("%s: %s", r$label, r$text)))),
          actionLink(paste0("gr_del_", i), "remove", onclick = sprintf("Shiny.setInputValue('gr_del', %d, {priority:'event'})", i))) }))),
      fluidRow(column(6, textInput("gr_name", "Short name (no spaces)", placeholder = "e.g. job_type")), column(6, textInput("gr_label", "What it describes", placeholder = "e.g. employment situation"))),
      lapply(1:4, function(k) fluidRow(column(4, textInput(paste0("gr_l", k), paste("Category", k), placeholder = c("Field-related job", "Unskilled job", "Not working, seeking", "")[k])),
        column(8, textInput(paste0("gr_c", k), "Condition", placeholder = c("ES01 is 1,2 and ES02 is 1", "ES01 is 1,2 and ES02 is 3", "ES01 is 3", "")[k])))),
      note("Conditions: VARIABLE is 1,2 | is not 3 | < 5.5 | >= 2 | is missing | is answered, joined with 'and'. Scales you built can be used (e.g. fin_stress < 5.5)."),
      uiOutput("gr_codes"), actionButton("gr_add", "Add group variable", class = "btn-primary")) })
  output$gr_codes <- renderUI({ req(rv$raw); vs <- names(rv$level_labels)
    tags$details(tags$summary("Show answer codes"), div(class = "small codes", lapply(vs, function(v) div(tags$code(v), " ", paste(names(rv$level_labels[[v]]), "=", rv$level_labels[[v]], collapse = "; "))))) })
  observeEvent(input$gr_add, { nm <- make.names(trimws(input$gr_name))
    if (!nzchar(input$gr_name)) { showNotification("Give the group variable a name.", type = "warning"); return() }
    d <- tryCatch(cur_res(scales_only = TRUE)$data, error = function(e) rv$raw); cats <- list()
    for (k in 1:4) { l <- trimws(input[[paste0("gr_l", k)]]); ct <- trimws(input[[paste0("gr_c", k)]]); if (!nzchar(l) || !nzchar(ct)) next
      cc <- qc_parse_condition_text(ct, d); if (!is.null(cc$error)) { showNotification(paste0("Category ", k, ": ", cc$error), type = "error", duration = 8); return() }
      cats[[length(cats) + 1]] <- list(label = l, cond = cc, text = ct) }
    if (length(cats) < 2) { showNotification("A group variable needs at least two categories with conditions.", type = "warning"); return() }
    g <- list(name = nm, label = trimws(input$gr_label), groups = cats)
    rv$groups <- c(Filter(function(x) x$name != nm, rv$groups), list(g)); l <- rv$labels; if (nzchar(g$label)) l[[nm]] <- g$label; rv$labels <- l; rv$locked <- FALSE
    for (k in 1:4) { updateTextInput(session, paste0("gr_l", k), value = ""); updateTextInput(session, paste0("gr_c", k), value = "") } })
  observeEvent(input$gr_del, { rv$groups <- rv$groups[-input$gr_del]; rv$locked <- FALSE })

  cur_res <- function(scales_only = FALSE) qc_apply_prep(rv$raw, prep_obj(groups = if (scales_only) list() else rv$groups))
  observeEvent(input$to_gate, updateNavbarPage(session, "nav", "3. Final data"))
  observeEvent(input$go_gate, updateNavbarPage(session, "nav", "3. Final data"))

  # ======================================================================= 3. final data gate
  gate <- reactive({ req(rv$raw); p <- prep_obj(); r <- tryCatch(qc_apply_prep(rv$raw, p), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(r$error)) return(r); list(res = r, prep = p, summary = qc_prep_summary(r, p)) })
  output$gate_ui <- renderUI({ if (is.null(rv$raw)) return(card(p("Load your data in step 1 first.")))
    g <- gate(); if (!is.null(g$error)) return(card(p(class = "text-danger", "The preparation could not be applied: ", g$error)))
    s <- g$summary; need_rb <- nrow(rule_breaks()) > 0 && !rv$rb_done
    tagList(card(h5("This is your final dataset"),
      div(class = "gate-n", span(class = "big", s$n), " cases", span(class = "ms-3", s$variables, " variables")),
      h6("How the sample was filtered"), div(class = "flow", lapply(s$flow, div)),
      if (!is.null(s$scales)) tagList(h6("Scales"), tags$table(class = "table table-sm", tags$thead(tags$tr(tags$th("Scale"), tags$th("Measures"), tags$th("Items"), tags$th("Score"), tags$th("alpha"), tags$th("n"), tags$th("M (SD)"))),
        tags$tbody(lapply(seq_len(nrow(s$scales)), function(i) tags$tr(tags$td(tags$code(s$scales$scale[i])), tags$td(s$scales$label[i]), tags$td(s$scales$items[i]), tags$td(s$scales$method[i]),
          tags$td(if (is.na(s$scales$alpha[i])) "-" else qc_fmt_r(s$scales$alpha[i])), tags$td(s$scales$n[i]), tags$td(sprintf("%s (%s)", qc_fmt(s$scales$mean[i]), qc_fmt(s$scales$sd[i])))))))),
      if (!is.null(s$groups)) tagList(h6("Groups"), tags$ul(lapply(seq_len(nrow(s$groups)), function(i) tags$li(tags$code(s$groups$group[i]), " ", s$groups$label[i], ": ", s$groups$categories[i])))),
      if (need_rb) p(class = "verdict-fix", "Some cases break a survey rule and have no decision yet (step 2B)."),
      if (rv$locked) tagList(p(class = "verdict-ready", "✓ Confirmed. This dataset is locked and used for every analysis."),
          downloadButton("dl_xlsx", "Final data (Excel)"), downloadButton("dl_cb", "Codebook as applied"), downloadButton("dl_prep", "Preparation script (R)"), downloadButton("dl_proj", "Project file"),
          actionButton("unlock", "Needs editing", class = "btn-outline-secondary ms-2"))
      else tagList(actionButton("gate_ok", "OK, use this data", class = "btn-success btn-lg", disabled = if (need_rb) NA else NULL),
          actionButton("gate_edit", "Needs editing", class = "btn-outline-secondary btn-lg ms-2"),
          note("Nothing is analysed until you press OK. After 'Needs editing' you can change steps 2A-2E and come back here."))),
      card(h5("Preview of the final data"), div(style = "overflow-x:auto", tableOutput("final_head")))) })
  output$final_head <- renderTable({ g <- gate(); req(g$res); h <- utils::head(g$res$data, 5); h[] <- lapply(h, function(x) if (is.numeric(x)) format(x, digits = 4, drop0trailing = TRUE) else as.character(x)); h })
  observeEvent(input$gate_ok, { g <- gate(); req(g$res)
    rv$final <- list(data = g$res$data, flow = g$res$flow, counts = g$res$counts, prep = g$prep, summary = g$summary)
    rv$saved <- tryCatch(qc_save_prepared(g$res, g$prep, if (is.null(rv$cb)) list(source = "none") else rv$cb,
      if (is.null(rv$cb_check)) data.frame(variable = names(rv$raw), symbol = "", label = "", values = "", missing_codes = "", message = "") else rv$cb_check,
      ctx(), file.path(rv$out_dir, "final_data")), error = function(e) { showNotification(paste("Saving failed:", conditionMessage(e)), type = "error"); NULL })
    rv$locked <- TRUE; showNotification("Final dataset confirmed and saved.", type = "message") })
  observeEvent(input$gate_edit, updateNavbarPage(session, "nav", "2. Prepare"))
  observeEvent(input$unlock, { rv$locked <- FALSE; updateNavbarPage(session, "nav", "2. Prepare") })
  dlh <- function(key, name) downloadHandler(filename = name, content = function(f) file.copy(rv$saved[[key]], f))
  output$dl_xlsx <- dlh("excel", "final_dataset.xlsx"); output$dl_cb <- dlh("codebook", "codebook_applied.xlsx")
  output$dl_prep <- dlh("script", "prepare_data.R"); output$dl_proj <- dlh("project", "quantcompass_project.json")

  ctx <- function() list(path = rv$path, sheet = rv$sheet, fixes = qc_prep_code(if (!is.null(rv$final)) rv$final$prep else prep_obj()),
                         labels = rv$labels, level_labels = rv$level_labels, out_dir = rv$out_dir)
  fdata <- reactive({ req(rv$locked, rv$final); rv$final$data })
  catalog <- reactive({ qc_catalog(fdata(), rv$labels, rv$level_labels, rv$cb, rv$final$prep$scales, rv$final$prep$groups) })

  # ======================================================================= 4. describe
  output$describe_ui <- renderUI({ if (!rv$locked) return(locked_card()); d <- fdata()
    cand <- names(d)[vapply(d, function(x) { u <- length(unique(stats::na.omit(x))); u >= 2 && u <= 12 }, logical(1))]
    t1 <- intersect(c(grep("^DM[0-9]", names(d), value = TRUE), vapply(rv$final$prep$scales, `[[`, "", "name")), names(d))
    t1 <- t1[vapply(t1, function(v) { u <- length(unique(stats::na.omit(d[[v]]))); u >= 2 && (is.numeric(d[[v]]) || is.factor(d[[v]]) || u <= 12) }, logical(1))]   # no free-text or empty columns
    if (!is.null(rv$cb)) t1 <- t1[!vapply(t1, function(v) { e <- rv$cb$vars[[v]]; !is.null(e) && identical(e$kind, "text") }, logical(1))]
    tagList(card(h5("Describe the final dataset"), note("Tables, charts and a ready-to-paste summary, plus Table 1 for your thesis."),
      fluidRow(column(6, selectInput("by", "Split numeric variables by group (optional)", c("No split" = "", named(cand, rv$labels)))),
        column(6, selectizeInput("t1vars", "Variables for Table 1 (sample characteristics)", named(names(d), rv$labels), selected = t1, multiple = TRUE))),
      actionButton("describe", "Describe my dataset", class = "btn-primary")),
      card(h5("Reliability of your scales (Cronbach's alpha)"), note("For every scale with two or more items: alpha, corrected item-total correlations and alpha if an item is deleted, judged against the .70 benchmark. Reversed items are reversed first."),
        actionButton("rel_run", "Check reliability of all scales", class = "btn-outline-primary"), uiOutput("rel_out")),
      uiOutput("desc_out")) })
  observeEvent(input$rel_run, { scs <- Filter(function(s) length(s$items) >= 2, rv$final$prep$scales)
    if (!length(scs)) { showNotification("No scale with two or more items. Build scales in step 2D.", type = "warning"); return() }
    c0 <- ctx(); c0$out_dir <- file.path(rv$out_dir, "reliability")
    withProgress(message = "Cronbach's alpha...", { rv$rel <- lapply(scs, function(s) { r <- tryCatch(qc_run_hypothesis(list(id = paste0("Reliability_", s$name),
      text = sprintf("The items of %s form a reliable scale", if (nzchar(s$label %||% "")) s$label else s$name), spec = qc_reliability_spec(s)), c0), error = function(e) list(blocked = TRUE, message = conditionMessage(e)))
      r$scale <- s; r }) }) })
  output$rel_out <- renderUI({ rr <- rv$rel; req(rr)
    tagList(tags$table(class = "table table-sm mt-3", tags$thead(tags$tr(tags$th("Scale"), tags$th("Items"), tags$th("alpha"), tags$th("Verdict"), tags$th("Report"))),
      tags$tbody(lapply(seq_along(rr), function(i) { r <- rr[[i]]; s <- r$scale
        tags$tr(tags$td(tags$code(s$name), " ", s$label %||% ""), tags$td(length(s$items)),
          tags$td(if (isTRUE(r$blocked)) "-" else qc_fmt_r(r$result$alpha)),
          tags$td(if (isTRUE(r$blocked)) span(class = "text-danger", r$message) else span(class = paste("decision", if (isTRUE(r$decision$rejected)) "rej" else "keep"), if (isTRUE(r$decision$rejected)) "reliable (at least .70)" else "below .70")),
          tags$td(if (!isTRUE(r$blocked)) downloadButton(paste0("rel_dl_", i), "Word", class = "btn-sm"))) }))),
      lapply(seq_along(rr), function(i) { r <- rr[[i]]; if (isTRUE(r$blocked)) return(NULL); div(class = "paste mb-2", r$paragraph) })) })
  observe({ rr <- rv$rel; req(rr); for (i in seq_along(rr)) local({ j <- i; r <- rr[[j]]; if (isTRUE(r$blocked)) return()
    output[[paste0("rel_dl_", j)]] <- downloadHandler(filename = basename(r$files[["docx"]]), content = function(f) file.copy(r$files[["docx"]], f)) }) })
  observeEvent(input$describe, { withProgress(message = "Describing your dataset...", {
    qc_set_labels(ctx())
    rv$desc <- tryCatch({ d <- qc_describe_dataset(ctx(), if (nzchar(input$by)) input$by else NULL)
      d$table1 <- if (length(input$t1vars)) qc_table1(fdata(), input$t1vars) else NULL; d }, error = function(e) list(error = conditionMessage(e))) }) })
  output$desc_out <- renderUI({ d <- rv$desc; req(d); if (!is.null(d$error)) return(card(p(class = "text-danger", "Something went wrong: ", d$error)))
    rel <- sub(paste0("^", results_root, "/"), "", d$files[["html"]])
    tagList(card(h5("Ready-to-paste summary"), div(class = "paste", d$paragraph), br(),
      if (!is.null(d$table1)) tagList(h6(d$table1$title), tableOutput("t1_tbl")),
      downloadButton("desc_docx", "Download Word report")), tags$iframe(src = paste0("qcresults/", rel))) })
  output$t1_tbl <- renderTable(rv$desc$table1$df)
  output$desc_docx <- downloadHandler(filename = "Descriptives_report.docx", content = function(f) file.copy(rv$desc$files[["docx"]], f))

  # ======================================================================= 5. hypotheses
  output$hyp_ui <- renderUI({ if (!rv$locked) return(locked_card())
    fluidRow(column(5,
      card(h5("Paste your hypotheses"), note("One per line, exactly as written in your thesis, e.g. 'H1a: There is a negative association between ...'."),
        textAreaInput("htexts", NULL, rows = 9, placeholder = "H1: ...\nH2a: ...\nH2b: ..."),
        actionButton("dissect", "Break them down", class = "btn-primary"),
        note("Or upload an Excel/CSV sheet with columns 'ID' and 'Hypothesis'."), fileInput("hfile", NULL, accept = c(".xlsx", ".csv"), buttonLabel = "Upload sheet...")),
      card(h5("Plan a multiple regression"), note("Pick the outcome (dependent variable). QuantCompass then checks every other variable as a possible predictor. Choose predictors from your theory and hypotheses: the correlation with the outcome is shown only for information."),
        uiOutput("mlr_ui")),
      card(h5("Confirmed hypotheses"), uiOutput("confirmed_ui"), hr(),
        actionButton("run", "Run all confirmed hypotheses", class = "btn-run"),
        note("Each one: assumptions are screened, the test is run, and a Word + HTML report with charts and the exact R script is created."))),
    column(7, uiOutput("parts_ui"))) })
  read_hyp_lines <- function(txt) { ln <- trimws(strsplit(txt, "\n")[[1]]); ln <- ln[nzchar(ln)]
    ln <- sub("^[*\u2022\u2013-]+\\s*", "", ln); ln <- gsub("(?<=^H[0-9])[\u2080-\u2089]+|(?<=^H[0-9][0-9])[\u2080-\u2089]+|(?<=^H[0-9][a-z])[\u2080-\u2089]+", "", ln, perl = TRUE)
    ids <- ifelse(grepl("^H[0-9]+[a-z]?\\s*[:.)-]", ln, ignore.case = TRUE), toupper(sub("^(H[0-9]+[a-z]?).*$", "\\1", ln, ignore.case = TRUE)), paste0("H", seq_along(ln)))
    ids <- sub("^H", "H", sub("([0-9])([A-Z])$", "\\1\\L\\2", ids, perl = TRUE)); stats::setNames(sub("^H[0-9]+[a-z]?\\s*[:.)-]\\s*", "", ln, ignore.case = TRUE), ids) }
  start_breakdown <- function(H) { qc_set_labels(ctx()); qc_dissect_scales$scales <- rv$final$prep$scales
    B <- withProgress(message = "Breaking down your hypotheses...", qc_dissect_all(H, fdata(), catalog()))
    rv$breakdowns <- B; parts <- list()
    for (b in B) { if (!length(b$parts)) parts[[length(parts) + 1]] <- list(id = b$id, text = b$text, spec = list(goal = "relationship", var1 = NA, var2 = NA), questions = b$questions, expected = "")
      for (pp in b$parts) parts[[length(parts) + 1]] <- list(id = pp$id, text = b$text, spec = pp$spec, questions = b$questions, expected = pp$expected, mode = "recommended", option = 1) }
    rv$parts <- parts }
  observeEvent(input$dissect, { req(nzchar(input$htexts)); start_breakdown(read_hyp_lines(input$htexts)) })
  observeEvent(input$hfile, { x <- tryCatch(if (grepl("xlsx$", input$hfile$name)) as.data.frame(readxl::read_excel(input$hfile$datapath)) else utils::read.csv(input$hfile$datapath, check.names = FALSE), error = function(e) NULL)
    req(x); nm <- tolower(names(x)); i1 <- grep("^id", nm)[1]; i2 <- grep("hypothes", nm)[1]
    if (is.na(i2)) { showNotification("The sheet needs a column called 'Hypothesis'.", type = "error"); return() }
    H <- stats::setNames(as.character(x[[i2]]), if (is.na(i1)) paste0("H", seq_len(nrow(x))) else as.character(x[[i1]])); start_breakdown(H[!is.na(H) & nzchar(H)]) })

  # ---- multiple regression planner
  mlr_out <- reactive({ qc_mlr_outcomes(fdata(), catalog(), rv$final$prep$scales) })
  output$mlr_ui <- renderUI({ oc <- mlr_out(); use <- oc[oc$status != "no", ]
    ic <- c(ok = "\u2713 ", warn = "\u26a0 ")
    tagList(selectInput("mlr_y", "Outcome (dependent variable)", c("Choose..." = "", stats::setNames(use$variable, paste0(ic[use$status], vapply(use$variable, lab_ui, "", labels = rv$labels))))),
      uiOutput("mlr_y_note"), uiOutput("mlr_x_ui"), uiOutput("mlr_check_ui"),
      fluidRow(column(5, textInput("mlr_id", "Hypothesis ID", "H5")), column(7, br(), actionButton("mlr_add", "Add as hypothesis", class = "btn-primary")))) })
  output$mlr_y_note <- renderUI({ req(nzchar(input$mlr_y %||% "")); oc <- mlr_out(); r <- oc[oc$variable == input$mlr_y, ]; note(r$reason) })
  mlr_pred <- reactive({ req(nzchar(input$mlr_y %||% "")); qc_mlr_predictors(fdata(), catalog(), input$mlr_y, rv$final$prep$scales) })
  output$mlr_x_ui <- renderUI({ pr <- mlr_pred(); use <- pr[pr$status != "no", ]; no <- pr[pr$status == "no", ]
    ic <- c(ok = "\u2713", warn = "\u26a0")
    tagList(checkboxGroupInput("mlr_x", "Possible predictors (independent variables)",
        choiceNames = lapply(seq_len(nrow(use)), function(i) span(span(class = paste0("sym sym-", use$status[i]), ic[[use$status[i]]]), lab_ui(use$variable[i], rv$labels),
          span(class = "small text-muted", sprintf(" - %s%s", if (is.na(use$association[i])) "" else sprintf("association %s; ", qc_fmt_r(use$association[i])), use$reason[i])))),
        choiceValues = use$variable),
      if (nrow(no)) tags$details(tags$summary(sprintf("Not suitable (%d)", nrow(no))), tags$ul(class = "small", lapply(seq_len(nrow(no)), function(i) tags$li(lab_ui(no$variable[i], rv$labels), ": ", no$reason[i]))))) })
  output$mlr_check_ui <- renderUI({ req(nzchar(input$mlr_y %||% ""), length(input$mlr_x)); ck <- qc_mlr_check(fdata(), input$mlr_y, input$mlr_x)
    div(class = paste0("mt-2 mlr-", ck$status), tags$b(c(ok = "\u2713 Feasible. ", warn = "\u26a0 Check: ", no = "\u2717 Not feasible. ")[[ck$status]]), ck$verdict) })
  observeEvent(input$mlr_add, { req(nzchar(input$mlr_y %||% "")); if (!length(input$mlr_x)) { showNotification("Choose at least one predictor.", type = "warning"); return() }
    ck <- qc_mlr_check(fdata(), input$mlr_y, input$mlr_x); if (ck$status == "no") { showNotification("Too few complete cases for these predictors.", type = "error"); return() }
    sp <- list(goal = "prediction", outcome = input$mlr_y, predictors = input$mlr_x); id <- trimws(input$mlr_id); if (!nzchar(id)) id <- paste0("H", length(rv$parts) + 1)
    L <- function(v) if (!is.null(rv$labels[[v]])) rv$labels[[v]] else v
    txt <- sprintf("%s %s %s.", qc_cap_first(qc_and(vapply(input$mlr_x, L, ""))), if (length(input$mlr_x) > 1) "together predict" else "predicts", L(input$mlr_y))
    rv$parts <- c(Filter(function(p) p$id != id, rv$parts), list(list(id = id, text = txt, spec = sp, questions = character(), expected = qc_expected_text(sp, catalog()), mode = "recommended", option = 1)))
    showNotification(paste(id, "added below the other hypotheses. Check it and press Confirm."), type = "message") })

  goal_names <- c("Relationship between two variables" = "relationship", "Group difference" = "difference", "Mediation" = "mediation",
                  "Association between two categories" = "association", "Prediction (multiple regression)" = "prediction",
                  "Scale reliability (Cronbach's alpha)" = "reliability")
  part_inputs <- function(k, sp) { d <- fdata(); cat0 <- catalog(); L <- rv$labels
    allv <- named(cat0$variable, L); numv <- named(cat0$variable[cat0$kind %in% c("numeric", "ordinal", "binary")], L)
    grpv <- named(cat0$variable[lengths(cat0$levels) >= 2 & lengths(cat0$levels) <= 12], L)
    id <- function(f) paste0("pt", k, "_", f); val <- function(x) if (is.null(x) || all(is.na(x))) character(0) else x
    dir_sel <- if (is.null(sp[["direction"]]) || is.na(sp[["direction"]])) "none" else sp[["direction"]]
    body <- switch(sp$goal,
      relationship = tagList(fluidRow(column(6, selectInput(id("var1"), "First variable", allv, val(sp$var1))), column(6, selectInput(id("var2"), "Second variable", allv, val(sp$var2)))),
        selectInput(id("dir"), "Expected direction", c("No direction stated" = "none", "Positive" = "positive", "Negative" = "negative"), dir_sel)),
      difference = { lv <- if (!is.null(sp$group) && !is.na(sp$group) && sp$group %in% names(d)) { x <- d[[sp$group]]; k2 <- if (is.factor(x)) levels(x) else as.character(sort(unique(stats::na.omit(x))))
          stats::setNames(k2, vapply(k2, function(z) qc_level_lab(sp$group, z), "")) } else character()
        tagList(fluidRow(column(6, selectInput(id("outcome"), "Outcome (measured)", numv, val(sp$outcome))), column(6, selectInput(id("group"), "Group variable", grpv, val(sp$group)))),
          selectizeInput(id("compare"), "Groups to compare (in order: group 1 first)", lv, selected = val(sp$compare), multiple = TRUE),
          selectInput(id("higher"), "Two groups: which one is expected to be higher?", c("No direction stated" = "none", lv), if (is.null(sp$expect_higher)) "none" else sp$expect_higher),
          if (length(lv) > 2) tagList(
            selectInput(id("trend"), "Three or more ordered groups: expected trend", c("No order stated" = "none", "Rises from the first to the last group" = "increasing", "Falls from the first to the last group" = "decreasing"), sp$trend %||% "none"),
            fluidRow(column(8, selectInput(id("extreme"), "Group expected to be lowest or highest (optional)", c("None" = "", lv), sp$expect_extreme$level %||% "")),
                     column(4, selectInput(id("extreme_which"), "which", c("lowest", "highest"), sp$expect_extreme$which %||% "lowest"))))) },
      mediation = tagList(fluidRow(column(4, selectInput(id("var1"), "Predictor (X)", numv, val(sp$var1))), column(4, selectInput(id("mediator"), "Mediator (M)", numv, val(sp$mediator))),
          column(4, selectInput(id("outcome"), "Outcome (Y)", numv, val(sp$outcome)))),
        selectInput(id("dir"), "Expected direction of the indirect effect", c("No direction stated" = "none", "Positive" = "positive", "Negative" = "negative"), dir_sel)),
      association = fluidRow(column(6, selectInput(id("var1"), "First category variable", grpv, val(sp$var1))), column(6, selectInput(id("var2"), "Second category variable", grpv, val(sp$var2)))),
      prediction = tagList(selectInput(id("outcome"), "Outcome (dependent variable)", numv, val(sp$outcome)), selectizeInput(id("preds"), "Predictors (independent variables)", allv, val(sp$predictors), multiple = TRUE),
        selectizeInput(id("controls"), "Control variables (optional; entered first, which makes it a hierarchical regression)", allv, val(sp$controls), multiple = TRUE),
        textInput(id("dirs"), "Expected direction per predictor (optional)", value = if (length(sp$directions)) paste(names(sp$directions), sp$directions, collapse = ", ") else "", placeholder = "e.g. WUC negative, WUF positive"),
        if (!is.null(sp$outcome) && !is.na(sp$outcome) && length(sp$predictors)) { ck <- qc_mlr_check(d, sp$outcome, c(sp$controls, sp$predictors))
          div(class = paste0("small mlr-", ck$status), ck$verdict) }),
      reliability = { scs <- Filter(function(s) length(s$items) >= 2, rv$final$prep$scales)
        tagList(if (length(scs)) selectInput(id("relscale"), "Use the items of a scale you built", c("(choose items yourself)" = "", stats::setNames(vapply(scs, `[[`, "", "name"), vapply(scs, function(s) lab_ui(s$name, rv$labels), ""))), ""),
          selectizeInput(id("items"), "Items", numv, val(sp$items), multiple = TRUE),
          selectizeInput(id("reverse"), "Reverse-coded items", numv, val(sp$reverse), multiple = TRUE)) })
    tagList(selectInput(id("goal"), "Type", goal_names, sp$goal), body,
      textInput(id("subset"), "Only these cases (optional condition)", value = if (is.null(sp$subset)) "" else sp$subset$text, placeholder = "e.g. FC07 is 1"),
      checkboxInput(id("onesided"), "Test one-sided (only if the direction was planned before collecting the data)", value = !is.null(sp$alternative) && sp$alternative != "two.sided")) }
  spec_from_part <- function(k) { g <- function(f) input[[paste0("pt", k, "_", f)]]; goal <- g("goal"); sp <- list(goal = goal)
    nz <- function(x) if (is.null(x) || !length(x) || identical(x, "")) NA else x
    if (goal == "relationship") { sp$var1 <- nz(g("var1")); sp$var2 <- nz(g("var2")); sp[["direction"]] <- if (identical(g("dir"), "none")) NA else g("dir") }
    if (goal == "difference") { sp$outcome <- nz(g("outcome")); sp$group <- nz(g("group")); if (length(g("compare")) >= 2) sp$compare <- g("compare")
      if (!is.null(g("higher")) && g("higher") != "none") sp$expect_higher <- g("higher")
      if (isTRUE(g("trend") %in% c("increasing", "decreasing")) && length(g("compare")) != 2) sp$trend <- g("trend")
      if (nzchar(g("extreme") %||% "") && length(g("compare")) != 2) sp$expect_extreme <- list(level = g("extreme"), which = g("extreme_which") %||% "lowest") }
    if (goal == "mediation") { sp$var1 <- nz(g("var1")); sp$mediator <- nz(g("mediator")); sp$outcome <- nz(g("outcome")); sp[["direction"]] <- if (identical(g("dir"), "none")) NA else g("dir") }
    if (goal == "association") { sp$var1 <- nz(g("var1")); sp$var2 <- nz(g("var2")) }
    if (goal == "prediction") { sp$outcome <- nz(g("outcome")); sp$predictors <- setdiff(g("preds"), sp$outcome)
      ctl <- setdiff(g("controls"), c(sp$outcome, sp$predictors)); if (length(ctl)) sp$controls <- ctl
      dt <- trimws(strsplit(g("dirs") %||% "", "\\s*[,;]\\s*")[[1]]); dt <- dt[nzchar(dt)]
      if (length(dt)) { v <- sub("\\s+.*$", "", dt); w <- tolower(sub("^\\S+\\s+", "", dt)); ok <- v %in% sp$predictors & w %in% c("negative", "positive")
        if (any(!ok)) showNotification(sprintf("Directions not understood: %s (write e.g. 'WUC negative').", paste(dt[!ok], collapse = ", ")), type = "warning")
        if (any(ok)) sp$directions <- stats::setNames(w[ok], v[ok]) } }
    if (goal == "reliability") { rs <- g("relscale"); sc <- Filter(function(s) identical(s$name, rs), rv$final$prep$scales)
      if (length(sc)) sp <- qc_reliability_spec(sc[[1]]) else { sp$items <- g("items"); sp$reverse <- intersect(g("reverse"), g("items")) } }
    st <- trimws(if (is.null(g("subset"))) "" else g("subset"))
    if (nzchar(st)) { cc <- qc_parse_condition_text(st, fdata()); if (!is.null(cc$error)) showNotification(cc$error, type = "error") else sp$subset <- list(cond = cc, label = st, text = st) }
    if (isTRUE(g("onesided"))) { s <- NA
      if (goal %in% c("relationship") && !is.na(sp[["direction"]])) s <- if (sp[["direction"]] == "positive") "greater" else "less"
      if (goal == "difference" && !is.null(sp$expect_higher) && !is.null(sp$compare)) s <- if (sp$expect_higher == sp$compare[1]) "greater" else "less"
      if (!is.na(s)) sp$alternative <- s else showNotification("One-sided needs a stated direction (and, for groups, two groups to compare).", type = "warning") }
    sp }
  part_data <- function(sp) { d <- fdata()
    if (!is.null(sp$subset)) d <- d[eval(parse(text = qc_cond_code(sp$subset$cond)), list(data = d)) %in% TRUE, , drop = FALSE]
    if (!is.null(sp$compare) && !is.null(sp$group)) { d <- d[as.character(d[[sp$group]]) %in% sp$compare, , drop = FALSE]; d[[sp$group]] <- factor(as.character(d[[sp$group]]), levels = sp$compare) }
    d }
  output$parts_ui <- renderUI({ ps <- rv$parts; if (!length(ps)) return(card(h5("Breakdown"), p(class = "text-muted", "Paste your hypotheses and press 'Break them down'. Each one is split into its parts, matched to your variables, and given test options with reasons.")))
    qc_set_labels(ctx())
    tagList(lapply(seq_along(ps), function(k) { pt <- ps[[k]]; sp <- pt$spec
      ok <- !any(is.na(unlist(sp[intersect(names(sp), c("var1", "var2", "outcome", "group", "mediator"))])))
      pd <- if (ok) tryCatch(part_data(sp), error = function(e) NULL) else NULL
      pair <- if (!is.null(pd)) tryCatch(qc_hypothesis_pair(sp, pd, pt$id), error = function(e) NULL) else NULL
      opts <- if (!is.null(pd)) tryCatch(qc_part_options(sp, part_data(utils::modifyList(sp, list(compare = NULL)))), error = function(e) list()) else list()
      n_used <- if (!is.null(pd)) { vs <- intersect(unlist(sp[c("var1", "var2", "outcome", "group", "mediator", "predictors")]), names(pd)); sum(stats::complete.cases(pd[, vs, drop = FALSE])) } else NA
      confirmed <- pt$id %in% vapply(rv$hyps, `[[`, "", "id")
      card(div(class = "d-flex justify-content-between", h5(pt$id), if (confirmed) span(class = "decision rej", "confirmed")),
        p(class = "hyp", pt$text), if (nzchar(pt$expected)) p(class = "small", tags$b(pt$expected)),
        if (length(pt$questions)) div(class = "openq", tags$b("Open questions"), tags$ul(lapply(pt$questions, tags$li))),
        part_inputs(k, sp),
        if (!is.null(pair)) div(class = "hpair", div(span(class = "lab lab-h1", pair$lab1), "Research hypothesis: ", pair$h1),
          div(span(class = "lab lab-h0", pair$lab0), "Null hypothesis: ", pair$h0), div(class = "small text-muted", pair$note),
          if (!is.na(n_used)) div(class = "small", sprintf("Cases with complete data for this hypothesis: %d", n_used))),
        if (length(opts)) tagList(h6(class = "mt-3", "Test options"), lapply(seq_along(opts), function(j) div(class = "opt", tags$b(opts[[j]]$role, ": ", opts[[j]]$label),
            div(class = "small", opts[[j]]$why), div(class = "small text-muted", "Assumptions: ", opts[[j]]$assumptions))),
          radioButtons(paste0("pt", k, "_mode"), NULL, c("Use recommended" = "recommended", "Choose manually" = "manual", "Decide for me" = "auto"), selected = pt$mode, inline = TRUE),
          conditionalPanel(sprintf("input.pt%d_mode == 'manual'", k), radioButtons(paste0("pt", k, "_opt"), "Your choice", stats::setNames(seq_along(opts), vapply(opts, `[[`, "", "label")), selected = pt$option))),
        div(class = "mt-2", tags$button(type = "button", class = "btn btn-outline-primary btn-sm", "Update", onclick = sprintf("Shiny.setInputValue('pt_update', %d, {priority:'event'})", k)),
          tags$button(type = "button", class = "btn btn-success btn-sm ms-2", if (confirmed) "Confirm again" else "Confirm", onclick = sprintf("Shiny.setInputValue('pt_confirm', %d, {priority:'event'})", k)),
          tags$button(type = "button", class = "btn btn-outline-danger btn-sm ms-2", "Remove", onclick = sprintf("Shiny.setInputValue('pt_remove', %d, {priority:'event'})", k)))) })) })
  observeEvent(input$pt_update, { k <- input$pt_update; ps <- rv$parts; ps[[k]]$spec <- spec_from_part(k)
    ps[[k]]$mode <- input[[paste0("pt", k, "_mode")]] %||% "recommended"; ps[[k]]$option <- input[[paste0("pt", k, "_opt")]] %||% 1
    ps[[k]]$expected <- qc_expected_text(ps[[k]]$spec, catalog()); rv$parts <- ps })
  observeEvent(input$pt_confirm, { k <- input$pt_confirm; sp <- spec_from_part(k)
    if (any(is.na(unlist(sp[intersect(names(sp), c("var1", "var2", "outcome", "group", "mediator"))])))) { showNotification("Choose all variables first.", type = "warning"); return() }
    if (sp$goal == "prediction" && !length(sp$predictors)) { showNotification("Choose at least one predictor.", type = "warning"); return() }
    if (sp$goal == "reliability" && length(sp$items) < 2) { showNotification("Choose at least two items.", type = "warning"); return() }
    mode <- input[[paste0("pt", k, "_mode")]] %||% "recommended"; opts <- qc_part_options(sp, part_data(utils::modifyList(sp, list(compare = NULL)))); ch <- "auto"
    if (mode == "manual" && length(opts)) { o <- opts[[as.integer(input[[paste0("pt", k, "_opt")]] %||% 1)]]; ch <- o$choice; sp <- o$spec }
    ps <- rv$parts; ps[[k]]$spec <- sp; ps[[k]]$mode <- mode; rv$parts <- ps
    h <- list(id = ps[[k]]$id, text = ps[[k]]$text, spec = sp, choice = ch, mode = mode, expected = ps[[k]]$expected)
    rv$hyps <- c(Filter(function(x) x$id != h$id, rv$hyps), list(h)); showNotification(paste(h$id, "confirmed."), type = "message") })
  observeEvent(input$pt_remove, { k <- input$pt_remove; id <- rv$parts[[k]]$id; rv$parts <- rv$parts[-k]; rv$hyps <- Filter(function(x) x$id != id, rv$hyps) })
  output$confirmed_ui <- renderUI({ if (!length(rv$hyps)) return(note("Confirm the hypotheses on the right."))
    tags$ul(lapply(rv$hyps, function(h) tags$li(tags$b(h$id), ": ", h$text, span(class = "badge bg-secondary ms-1", c(recommended = "recommended test", manual = "your choice", auto = "decided automatically")[[h$mode]])))) })

  # ======================================================================= 6. run + results
  observeEvent(input$run, { if (!length(rv$hyps)) { showNotification("Confirm at least one hypothesis first.", type = "warning"); return() }
    c0 <- ctx(); c0$out_dir <- file.path(rv$out_dir, "analysis"); res <- list(); hs <- rv$hyps
    withProgress(message = "Running analyses", value = 0, {
      for (i in seq_along(hs)) { h <- hs[[i]]; incProgress(1 / (length(hs) + 1), detail = paste(h$id, "- screening assumptions and running the test"))
        res[[i]] <- tryCatch(qc_run_hypothesis(h, c0), error = function(e) list(id = h$id, sentence = h$text, blocked = TRUE, message = paste("The analysis could not be completed:", conditionMessage(e)))) }
      incProgress(1 / (length(hs) + 1), detail = "Writing the summary report")
      qc_set_labels(c0)
      meth <- tryCatch(qc_methods_text(rv$final$prep, rv$final$counts, rv$final$summary$scales, res, if (!is.null(rv$cb)) rv$cb$source else NULL), error = function(e) NULL)
      t1 <- if (!is.null(rv$desc$table1)) rv$desc$table1 else tryCatch(qc_table1(fdata(), intersect(c(grep("^DM[0-9]", names(fdata()), value = TRUE), vapply(rv$final$prep$scales, `[[`, "", "name")), names(fdata()))), error = function(e) NULL)
      summ <- tryCatch(qc_summary_report(res, c0, if (!is.null(rv$desc) && is.null(rv$desc$error)) rv$desc else NULL, meth, t1, rv$breakdowns), error = function(e) { showNotification(paste("Summary report:", conditionMessage(e)), type = "error"); NULL }) })
    rv$results <- list(items = res, summary = summ, methods = meth); updateNavbarPage(session, "nav", "6. Results") })
  output$results_ui <- renderUI({ if (!rv$locked) return(locked_card()); r <- rv$results
    if (is.null(r)) return(card(p(class = "text-muted", "Confirm and run your hypotheses in step 5 to see results here.")))
    tagList(card(h5("All results"), p("Everything is saved in: ", tags$code(file.path(rv$out_dir, "analysis"))),
        if (!is.null(r$summary)) downloadButton("sum_docx", "Summary report with methods section (Word)"),
        actionButton("open_dir", "Open results folder", class = "btn-outline-secondary ms-2"),
        tags$table(class = "table table-sm mt-3", tags$thead(tags$tr(tags$th("ID"), tags$th("Test"), tags$th("Decision"))),
          tags$tbody(lapply(r$items, function(x) tags$tr(tags$td(x$id), tags$td(if (isTRUE(x$blocked)) "-" else x$test),
            tags$td(if (isTRUE(x$blocked)) span(class = "text-danger", "not run") else span(class = paste("decision", if (isTRUE(x$decision$rejected)) "rej" else "keep"), x$decision$short))))))),
      if (!is.null(r$methods)) card(h5("Methods section (ready to paste and adapt)"), div(class = "paste", r$methods$sample), br(), if (nzchar(r$methods$measures)) div(class = "paste", r$methods$measures), br(), div(class = "paste", r$methods$analysis)),
      card(h5("Optional: check against numbers you already reported"), note("Paste lines such as 'H1a: r = -.47, p < .001'. Each number is looked up in QuantCompass's result for that hypothesis."),
        textAreaInput("val_txt", NULL, rows = 4), actionButton("val_go", "Compare", class = "btn-outline-primary"), uiOutput("val_out")),
      lapply(seq_along(r$items), function(i) { x <- r$items[[i]]
        if (isTRUE(x$blocked)) return(card(h5(x$id), p(x$sentence), p(class = "text-danger", x$message)))
        rel <- sub(paste0("^", results_root, "/"), "", x$files[["html"]])
        card(h5(paste0(x$id, ": ", x$test)),
          div(class = "hpair", div(span(class = "lab lab-h1", x$lab1), "Research hypothesis: ", x$h1), div(span(class = "lab lab-h0", x$lab0), "Null hypothesis: ", x$h0)),
          span(class = paste("decision", if (isTRUE(x$decision$rejected)) "rej" else "keep"), x$decision$short),
          p(tags$b("Why this test: "), x$reason), div(class = "paste", x$paragraph), br(),
          downloadButton(paste0("dl_docx_", i), "Word report"), downloadButton(paste0("dl_r_", i), "R script"),
          tags$details(class = "mt-3", tags$summary("Show full report with charts"), tags$iframe(src = paste0("qcresults/", rel)))) })) })
  observe({ r <- rv$results; req(r)
    for (i in seq_along(r$items)) local({ j <- i; x <- r$items[[j]]; if (isTRUE(x$blocked)) return()
      output[[paste0("dl_docx_", j)]] <- downloadHandler(filename = basename(x$files[["docx"]]), content = function(f) file.copy(x$files[["docx"]], f))
      output[[paste0("dl_r_", j)]] <- downloadHandler(filename = basename(x$files[["script"]]), content = function(f) file.copy(x$files[["script"]], f)) }) })
  output$sum_docx <- downloadHandler(filename = "QuantCompass_summary_report.docx", content = function(f) file.copy(rv$results$summary[["docx"]], f))
  observeEvent(input$val_go, { output$val_out <- renderUI({ ln <- trimws(strsplit(input$val_txt, "\n")[[1]]); ln <- ln[nzchar(ln)]; items <- rv$results$items
    rows <- lapply(ln, function(l) { id <- toupper(sub("^(H[0-9]+[a-z]?(\\([iv]+\\))?).*$", "\\1", l, ignore.case = TRUE)); id <- sub("([0-9])([A-Z])", "\\1\\L\\2", id, perl = TRUE)
      x <- Filter(function(z) tolower(z$id) == tolower(id), items); if (!length(x)) return(tags$tr(tags$td(id), tags$td(l), tags$td("no result with this ID")))
      num <- function(s) suppressWarnings(as.numeric(regmatches(s, gregexpr("-?\\d*\\.?\\d+", s))[[1]]))
      theirs <- num(sub("^H[0-9]+[a-z]?(\\([iv]+\\))?\\s*[:.)-]?", "", l, ignore.case = TRUE)); ours <- num(x[[1]]$paragraph)
      miss <- theirs[!vapply(theirs, function(t) any(abs(ours - t) < .0051), logical(1))]
      tags$tr(tags$td(id), tags$td(l), tags$td(if (!length(miss)) span(class = "verdict-ready", "✓ all numbers found") else span(class = "verdict-fix", "⚠ not found: ", paste(miss, collapse = ", ")))) })
    tags$table(class = "table table-sm mt-2", tags$tbody(rows)) }) })
  observeEvent(input$open_dir, { d <- file.path(rv$out_dir, "analysis")
    if (.Platform$OS.type == "windows") shell.exec(d) else if (Sys.info()[["sysname"]] == "Darwin") system2("open", d) else system2("xdg-open", d, wait = FALSE) })
}

shinyApp(ui, server)
