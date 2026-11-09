# QuantCompass click app. Start it with START-QuantCompass.R (no code needed).
# Steps: 1 Data & codebook -> 2 Hypotheses & variables -> 3 Prepare data (confirm) -> 4 Describe & reliability -> 5 Assumption checks -> 6 Results.
# Nothing is analysed before the final dataset is confirmed. With a connected AI key, the AI recommends from step 1 on.
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
nav_js <- function(step) sprintf("Shiny.setInputValue('qc_nav', '%s', {priority: 'event'}); return false;", step)
locked_card <- function() split1(card(h5("Confirm your final dataset first"), p("Analyses use the final dataset only. Check it in step 3 and press ", tags$b("Confirm data & continue"), "."),
  tags$button(type = "button", class = "btn btn-primary", onclick = nav_js("s3"), "Go to step 3: Prepare data")))

# ---- page layout: a step header, then two panes that scroll on their own (the summary and the Back/Continue bar stay put)
qc_steps <- c(s1 = "Data & codebook", s2 = "Hypotheses & variables", s3 = "Prepare data", s4 = "Describe & reliability", s5 = "Assumption checks", s6 = "Results")
qc_steps_short <- c(s1 = "Data", s2 = "Hypotheses", s3 = "Prepare", s4 = "Describe", s5 = "Assumptions", s6 = "Results")
step_title <- function(k) tagList(span(class = "qc-stepn", title = qc_steps[[k]], k), span(class = "qc-steplab", title = qc_steps[[k]], qc_steps_short[[k]]))
page_head <- function(k, what) div(class = "qc-page-head", span(class = "qc-kicker", sprintf("Step %d of 6", k)), h4(qc_steps[[k]]), p(what))
split2 <- function(left, right, cols = "5fr 7fr") div(class = "qc-split", style = sprintf("grid-template-columns: %s;", gsub("([0-9.]+fr)", "minmax(0, \\1)", cols)),
  div(class = "qc-pane", left), div(class = "qc-pane", right))
split1 <- function(...) div(class = "qc-split one", div(class = "qc-pane", ...))
step_tab <- function(k, what, ...) tabPanel(title = step_title(k), value = names(qc_steps)[k], page_head(k, what), ...)
fill <- function(id) uiOutput(id, class = "qc-fill")

brand <- tags$span(HTML(paste(readLines(file.path(root, "app", "www", "logo_inline.svg"), warn = FALSE), collapse = "")),
  tags$span(tags$span(class = "qc-word", HTML("Quant<b>Compass</b>"))))

ai_drawer <- div(class = "offcanvas offcanvas-end qc-ai-drawer", tabindex = "-1", id = "qc-ai", `aria-labelledby` = "qc-ai-title",
  div(class = "offcanvas-header", h5(class = "offcanvas-title", id = "qc-ai-title", "\u2726 AI assistant"),
    tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")),
  div(class = "offcanvas-body", uiOutput("ai_page")))

ui <- navbarPage(
  title = brand, id = "nav", windowTitle = "QuantCompass", selected = "s1",
  theme = bslib::bs_theme(version = 5, primary = "#2a78d6"),
  header = tagList(tags$head(tags$link(rel = "icon", type = "image/svg+xml", href = "logo.svg"),
      tags$link(rel = "stylesheet", href = "quantcompass.css"), tags$script(src = "quantcompass.js")),
    div(id = "qc-tools", class = "qc-tools",
      tags$button(type = "button", class = "qc-chip", id = "qc-ai-chip", `data-bs-toggle` = "offcanvas", `data-bs-target` = "#qc-ai",
        title = "Connect your own AI key: the AI then reads your codebook and recommends variables and scales for every hypothesis", uiOutput("ai_chip", inline = TRUE)),
      tags$button(type = "button", class = "qc-chip qc-chip-ask", id = "qc-ask-btn", title = "Ask the AI assistant anything about your data, hypotheses, tests or results - or tell it what to change",
        onclick = "QC.panel('chat')", span(class = "qc-ai-ico", "\u2726"), "Ask AI"),
      tags$button(type = "button", class = "qc-chip qc-chip-plain", id = "qc-sum-toggle", title = "Show or hide the side panel", "Summary")),
    ai_drawer),
  footer = tagList(
    tags$aside(id = "qc-summary", class = "qc-summary",
      div(class = "qs-tabs", tags$button(type = "button", class = "qs-tab active", id = "qs-tab-sel", onclick = "QC.panel('sel')", "Your selections"),
        tags$button(type = "button", class = "qs-tab", id = "qs-tab-chat", onclick = "QC.panel('chat')", span(class = "qc-ai-ico", "\u2726"), " AI chat")),
      div(id = "qs-sel", class = "qs-pane", div(class = "qs-sub mb-2", "Everything chosen so far - click a step to go there."), uiOutput("qc_summary")),
      div(id = "qs-chat", class = "qs-pane qc-chat",
        div(class = "qc-chat-head", uiOutput("chat_status"),
          div(class = "qc-chat-set", selectInput("chat_mode", NULL, stats::setNames(names(qc_assist_modes), qc_assist_modes), "ask", width = "100%"),
            selectInput("chat_access", NULL, stats::setNames(names(qc_assist_access), paste("AI sees:", qc_assist_access)), "full", width = "100%"))),
        div(id = "qc-chat-msgs", class = "qc-chat-msgs", uiOutput("chat_msgs")),
        div(class = "qc-chat-in", uiOutput("chat_chips"),
          tags$textarea(id = "qc-chat-text", class = "form-control", rows = 3, placeholder = "Ask anything - 'explain my codebook check', 'break down H2', 'what does p = .03 mean?' - or tell me what to change, e.g. 'use the codebook's scales for all hypotheses'."),
          div(class = "d-flex justify-content-between align-items-center mt-1", span(class = "small text-muted", "Enter = send \u00b7 Shift+Enter = new line"),
            tags$button(type = "button", class = "btn btn-ai btn-sm", onclick = "QC.send()", "Send"))))),
    div(id = "qc-footer", class = "qc-footer", uiOutput("qc_footer"))),

  step_tab(1, "Load your data file and, if you have it, the codebook. Connect the AI assistant here: it then helps you on every page.",
    split2(tagList(
      uiOutput("ai_card"),
      card(h5("Your data file"), note("CSV, TSV or TXT (any encoding, incl. SoSci Survey exports), Excel, OpenDocument, SPSS (.sav), Stata (.dta) or SAS. The file stays on your computer."),
        fileInput("file", NULL, buttonLabel = "Choose data file...", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".ods", ".sav", ".zsav", ".por", ".dta", ".sas7bdat", ".xpt")),
        uiOutput("sheet_ui"), uiOutput("data_info")),
      card(h5("Your codebook (recommended)"), note("PDF, Word, Excel/CSV, TXT/HTML, Qualtrics (.qsf), LimeSurvey (.lss) or the labels stored in an SPSS/Stata file. Scanned PDFs are reported, never guessed."),
        fileInput("cbfile", NULL, buttonLabel = "Choose codebook...", accept = c(".pdf", ".docx", ".xlsx", ".xls", ".csv", ".txt", ".html", ".htm", ".qsf", ".lss", ".sav", ".dta")),
        uiOutput("cb_embedded_ui"), uiOutput("cb_info"), uiOutput("cb_ai_ui")),
      card(h5("Friendly names"), note("Readable names for columns and categories. They appear in every sentence, table and chart."), uiOutput("label_ui"))),
    tagList(
      card(h5("Codebook check"), note("\u2713 matches the codebook  \u26a0 needs your decision  \u2717 not in the codebook. Nothing marked \u26a0 is applied unless you tick it."),
        uiOutput("cb_check_ui")),
      card(h5("Survey rules found in the codebook"), uiOutput("rules_ui")),
      card(h5("First rows"), div(style = "overflow-x:auto", tableOutput("head_tbl")))))),

  step_tab(2, "Write your hypotheses. For each one, QuantCompass (and the AI, if connected) recommends the dependent and independent variables and the columns that measure them. Click to accept, or choose your own.",
    fill("hyp_ui")),

  step_tab(3, "Decide which cases to exclude, check the scales your hypotheses use, then confirm the final dataset.",
    uiOutput("prep_gate"),
    split2(tagList(
      card(h5("A. Exclude cases"), note("Suggested from the survey's own rules. Switch any step off or on; they are applied in this order."), uiOutput("excl_ui")),
      card(h5("B. Skip-logic and rule-break check"), note("Only rules defined in your survey are checked: skip/filter logic and valid answer codes."), uiOutput("rb_ui")),
      card(h5("C. Exclude cases by ID"), uiOutput("manual_ui")),
      card(h5("E. Groups and recodes (optional)"), note("Build group variables from conditions, e.g. 'ES01 is 1,2 and ES02 is 1'. A case gets the first category whose condition it meets."), uiOutput("groups_ui"))),
    tagList(
      uiOutput("gate_ui"),
      card(h5("D. Scales"), note("The scales your hypotheses use are already here. Check the items and the reversed ones; you can also build more."), uiOutput("scales_ui"))), "1fr 1fr")),

  step_tab(4, "Describe your sample (Table 1) and check that every scale is reliable (Cronbach's alpha). Both run by themselves when you arrive here.", fill("describe_ui")),
  step_tab(5, "For every hypothesis, the assumptions of the test are checked on your final data. The test that fits is chosen for you, with the reason; you can switch to the alternative.", fill("assume_ui")),
  step_tab(6, "Decisions, reports, R scripts and a methods section for every hypothesis.", fill("results_ui"))
)

server <- function(input, output, session) {
  rv <- reactiveValues(path = NULL, sheet = NULL, raw = NULL, out_dir = NULL, cb = NULL, cb_check = NULL,
    labels = list(), level_labels = list(), accept_na = character(), fixes_nocb = character(),
    excl = list(), rb_decisions = list(), rb_done = FALSE, hint_excl = character(), manual = data.frame(id = character(), reason = character()),
    scales = list(), groups = list(), locked = FALSE, final = NULL, saved = NULL,
    parts = list(), hyps = list(), results = NULL, desc = NULL, breakdowns = list(), ran_hyps = NULL, rel = NULL, seen = "s1", cb_ai_done = FALSE, chat = list(), undo_last = NULL, user_h0 = list(), hyp_notes = character())

  # ======================================================================= 1. data & codebook
  observeEvent(input$file, {
    run <- format(Sys.time(), "%Y-%m-%d_%H%M%S"); d <- file.path(results_root, run); dir.create(file.path(d, "data"), recursive = TRUE)
    path <- file.path(d, "data", input$file$name); file.copy(input$file$datapath, path, overwrite = TRUE)
    rv$path <- path; rv$out_dir <- d; rv$sheet <- if (qc_file_kind(path) == "excel") readxl::excel_sheets(path)[1] else NULL
    rv$cb <- NULL; rv$locked <- FALSE; rv$results <- NULL; rv$parts <- list(); rv$hyps <- list(); rv$scales <- list(); rv$groups <- list(); rv$desc <- NULL; rv$rel <- NULL; rv$ran_hyps <- NULL
    rv$manual <- data.frame(id = character(), reason = character()); rv$rb_decisions <- list(); rv$rb_done <- FALSE
    load_raw()
    emb <- tryCatch(qc_embedded_codebook(path), error = function(e) NULL)
    if (qc_file_kind(path) == "excel" && any(grepl("(?i)codebook|variables|labels|dictionary", readxl::excel_sheets(path), perl = TRUE)))
      emb <- tryCatch(qc_read_any_codebook(path, rv$raw), error = function(e) emb)
    if (!is.null(emb) && length(emb$vars)) { rv$cb_path <- path; rv$cb_ai_done <- FALSE; set_codebook(emb) }
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
    rv$cb_path <- p
    withProgress(message = "Reading the codebook...", { cb <- qc_read_any_codebook(p, rv$raw) })
    rv$cb_ai_done <- FALSE; set_codebook(cb)
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
          tags$td(class = "small", ch$values[i]), tags$td(class = "small", ch$message[i],
            if (ch$status[i] != "ok") tags$a(href = "#", class = "qc-ask-link", onclick = sprintf("QC.ask(%s); return false;", jsonlite::toJSON(sprintf("Explain the codebook check for %s in simple words and tell me what I should do.", ch$variable[i]), auto_unbox = TRUE)), " \u2726 Explain"))))))))
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
      uiOutput("excl_flow"),
      div(class = "qc-say", tags$label(class = "small text-muted", `for` = "qc-say-excl", "Another rule? Describe it in your own words - the AI assistant sets it up:"),
        div(class = "d-flex gap-2", tags$input(type = "text", class = "form-control form-control-sm", id = "qc-say-excl", placeholder = "e.g. remove everyone who answered faster than 3 minutes"),
          tags$button(type = "button", class = "btn btn-ai-outline btn-sm", onclick = "QC.askFrom('qc-say-excl', 'Add an exclusion step: ')", "\u2726 Set it up")))) })
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
  scale_sugg <- reactive({ req(rv$raw); lapply(cands(), function(s) { s$preset <- s$preset %||% NA; s$anchors <- s$anchors %||% character(); s }) })
  output$scales_ui <- renderUI({ req(rv$raw); sg <- scale_sugg(); num <- names(rv$raw)[vapply(rv$raw, is.numeric, logical(1))]
    tagList(
      if (length(rv$scales)) tagList(tags$b("Your scales"), tags$ul(lapply(seq_along(rv$scales), function(i) { s <- rv$scales[[i]]; a <- tryCatch(qc_scale_alpha(cur_res()$data, s), error = function(e) list(alpha = NA))
        tags$li(tags$code(s$name), sprintf(" %s: %d item(s), %s%s%s", s$label, length(s$items), s$method, if (length(s$reverse)) paste0(", reversed: ", paste(s$reverse, collapse = ", ")) else "",
          if (!is.na(a$alpha)) sprintf(", alpha = %s%s", qc_fmt_r(a$alpha), if (a$alpha < .7) " (below .70)" else "") else ""),
          actionLink(paste0("sc_del_", i), "remove", onclick = sprintf("Shiny.setInputValue('sc_del', %d, {priority:'event'})", i))) }))),
      if (length(sg) && any(vapply(sg, function(s) !(s$name %in% vapply(rv$scales, `[[`, "", "name") || all(s$items %in% unlist(lapply(rv$scales, `[[`, "items")))), logical(1))))
        tagList(tags$b("Other scales found in your data (not used by a hypothesis)"), lapply(seq_along(sg), function(i) { s <- sg[[i]]
        if (s$name %in% vapply(rv$scales, `[[`, "", "name") || all(s$items %in% unlist(lapply(rv$scales, `[[`, "items")))) return(NULL)
        div(class = "sugg", tags$b(s$label), " ", tags$code(paste(s$items, collapse = ", ")), if (!is.na(s$preset)) span(class = "badge bg-info ms-1", "WHO-5 detected"),
          div(class = "small text-muted", s$why),
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
    updateSelectInput(session, "sc_method", selected = s$method); updateNumericInput(session, "sc_min", value = s$min); updateNumericInput(session, "sc_max", value = s$max)
    updateTextInput(session, "sc_label", value = s$label); updateSelectizeInput(session, "sc_rev", selected = s$reverse) })
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
  observeEvent(input$qc_nav, updateNavbarPage(session, "nav", input$qc_nav))

  # ======================================================================= 3. final data gate
  gate <- reactive({ req(rv$raw); p <- prep_obj(); r <- tryCatch(qc_apply_prep(rv$raw, p), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(r$error)) return(r); list(res = r, prep = p, summary = qc_prep_summary(r, p)) })
  output$gate_ui <- renderUI({ if (is.null(rv$raw)) return(card(p("Load your data in step 1 first.")))
    g <- gate(); if (!is.null(g$error)) return(card(p(class = "text-danger", "The preparation could not be applied: ", g$error)))
    s <- g$summary; need_rb <- nrow(rule_breaks()) > 0 && !rv$rb_done
    card(h5(if (rv$locked) "\u2713 Your final dataset (confirmed)" else "Your final dataset"),
      div(class = "gate-n", span(class = "big", s$n), " cases", span(class = "ms-3", s$variables, " variables")),
      h6("How the sample was filtered"), div(class = "flow", lapply(s$flow, div)),
      if (!is.null(s$scales)) tagList(h6("Scales"), tags$table(class = "table table-sm", tags$thead(tags$tr(tags$th("Scale"), tags$th("Measures"), tags$th("Items"), tags$th("Score"), tags$th("alpha"), tags$th("n"), tags$th("M (SD)"))),
        tags$tbody(lapply(seq_len(nrow(s$scales)), function(i) tags$tr(tags$td(tags$code(s$scales$scale[i])), tags$td(s$scales$label[i]), tags$td(s$scales$items[i]), tags$td(s$scales$method[i]),
          tags$td(if (is.na(s$scales$alpha[i])) "-" else qc_fmt_r(s$scales$alpha[i])), tags$td(s$scales$n[i]), tags$td(sprintf("%s (%s)", qc_fmt(s$scales$mean[i]), qc_fmt(s$scales$sd[i])))))))),
      if (!is.null(s$groups)) tagList(h6("Groups"), tags$ul(lapply(seq_len(nrow(s$groups)), function(i) tags$li(tags$code(s$groups$group[i]), " ", s$groups$label[i], ": ", s$groups$categories[i])))),
      if (need_rb) p(class = "verdict-fix", "Some cases break a survey rule and have no decision yet (B, on the left)."),
      tags$details(tags$summary("Preview the first rows"), div(style = "overflow-x:auto", tableOutput("final_head"))),
      if (rv$locked) tagList(p(class = "verdict-ready mt-2", "\u2713 Confirmed. This dataset is used for every analysis."),
          downloadButton("dl_xlsx", "Final data (Excel)"), downloadButton("dl_cb", "Codebook as applied"), downloadButton("dl_prep", "Preparation script (R)"), downloadButton("dl_proj", "Project file"),
          actionButton("unlock", "Change something", class = "btn-outline-secondary ms-2"))
      else tagList(div(class = "mt-2", actionButton("gate_ok", "Confirm this data", class = "btn-success", disabled = if (need_rb) NA else NULL)),
          note("Nothing is analysed until you confirm (here or with 'Confirm data & continue' below)."))) })
  output$final_head <- renderTable({ g <- gate(); req(g$res); h <- utils::head(g$res$data, 5); h[] <- lapply(h, function(x) if (is.numeric(x)) format(x, digits = 4, drop0trailing = TRUE) else as.character(x)); h })
  confirm_gate <- function() { g <- gate(); if (is.null(g$res)) return(FALSE)
    rv$final <- list(data = g$res$data, flow = g$res$flow, counts = g$res$counts, prep = g$prep, summary = g$summary)
    rv$saved <- tryCatch(qc_save_prepared(g$res, g$prep, if (is.null(rv$cb)) list(source = "none") else rv$cb,
      if (is.null(rv$cb_check)) data.frame(variable = names(rv$raw), symbol = "", label = "", values = "", missing_codes = "", message = "") else rv$cb_check,
      ctx(), file.path(rv$out_dir, "final_data")), error = function(e) { showNotification(paste("Saving failed:", conditionMessage(e)), type = "error"); NULL })
    rv$locked <- TRUE; rv$desc <- NULL; rv$rel <- NULL; showNotification("Final dataset confirmed and saved.", type = "message"); TRUE }
  observeEvent(input$gate_ok, confirm_gate())
  observeEvent(input$unlock, { rv$locked <- FALSE })
  dlh <- function(key, name) downloadHandler(filename = name, content = function(f) file.copy(rv$saved[[key]], f))
  output$dl_xlsx <- dlh("excel", "final_dataset.xlsx"); output$dl_cb <- dlh("codebook", "codebook_applied.xlsx")
  output$dl_prep <- dlh("script", "prepare_data.R"); output$dl_proj <- dlh("project", "quantcompass_project.json")

  ctx <- function() list(path = rv$path, sheet = rv$sheet, fixes = qc_prep_code(if (!is.null(rv$final)) rv$final$prep else prep_obj()),
                         labels = rv$labels, level_labels = rv$level_labels, out_dir = rv$out_dir)
  fdata <- reactive({ req(rv$locked, rv$final); rv$final$data })
  catalog <- reactive({ qc_catalog(fdata(), rv$labels, rv$level_labels, rv$cb, rv$final$prep$scales, rv$final$prep$groups) })

  # ======================================================================= 4. describe
  t1_default <- function() { d <- fdata()
    t1 <- intersect(c(grep("^DM[0-9]", names(d), value = TRUE), vapply(rv$final$prep$scales, `[[`, "", "name")), names(d))
    t1 <- t1[vapply(t1, function(v) { u <- length(unique(stats::na.omit(d[[v]]))); u >= 2 && (is.numeric(d[[v]]) || is.factor(d[[v]]) || u <= 12) }, logical(1))]   # no free-text or empty columns
    if (!is.null(rv$cb)) t1 <- t1[!vapply(t1, function(v) { e <- rv$cb$vars[[v]]; !is.null(e) && identical(e$kind, "text") }, logical(1))]
    t1 }
  output$describe_ui <- renderUI({ if (!rv$locked) return(locked_card()); d <- fdata()
    cand <- names(d)[vapply(d, function(x) { u <- length(unique(stats::na.omit(x))); u >= 2 && u <= 12 }, logical(1))]
    t1 <- t1_default()
    split2(tagList(card(h5("Describe the final dataset"), note("Tables, charts and a ready-to-paste summary, plus Table 1 for your thesis. Change the choices and press the button to update."),
      fluidRow(column(6, selectInput("by", "Split numeric variables by group (optional)", c("No split" = "", named(cand, rv$labels)))),
        column(6, selectizeInput("t1vars", "Variables for Table 1 (sample characteristics)", named(names(d), rv$labels), selected = t1, multiple = TRUE))),
      actionButton("describe", "Update the description", class = "btn-primary")),
      card(h5("Reliability of your scales (Cronbach's alpha)"), note("For every scale with two or more items: alpha, corrected item-total correlations and alpha if an item is deleted, judged against the .70 benchmark. Reversed items are reversed first."),
        uiOutput("rel_out"), actionButton("rel_run", "Check again", class = "btn-outline-primary btn-sm"))),
      uiOutput("desc_out"), "5fr 7fr") })
  run_rel <- function(quiet = FALSE) { scs <- Filter(function(s) length(s$items) >= 2, rv$final$prep$scales)
    if (!length(scs)) { rv$rel <- list(); if (!quiet) showNotification("No scale with two or more items. Your hypotheses use single columns only.", type = "warning"); return() }
    c0 <- ctx(); c0$out_dir <- file.path(rv$out_dir, "reliability")
    withProgress(message = "Cronbach's alpha...", { rv$rel <- lapply(scs, function(s) { r <- tryCatch(qc_run_hypothesis(list(id = paste0("Reliability_", s$name),
      text = sprintf("The items of %s form a reliable scale", if (nzchar(s$label %||% "")) s$label else s$name), spec = qc_reliability_spec(s)), c0), error = function(e) list(blocked = TRUE, message = conditionMessage(e)))
      r$scale <- s; r }) }) }
  observeEvent(input$rel_run, run_rel())
  output$rel_out <- renderUI({ rr <- rv$rel; if (is.null(rr)) return(note("Runs by itself when you arrive here."))
    if (!length(rr)) return(note("No scale with two or more items, so there is nothing to check. Single questions have no Cronbach's alpha."))
    tagList(tags$table(class = "table table-sm mt-3", tags$thead(tags$tr(tags$th("Scale"), tags$th("Items"), tags$th("alpha"), tags$th("Verdict"), tags$th("Report"))),
      tags$tbody(lapply(seq_along(rr), function(i) { r <- rr[[i]]; s <- r$scale
        tags$tr(tags$td(tags$code(s$name), " ", s$label %||% ""), tags$td(length(s$items)),
          tags$td(if (isTRUE(r$blocked)) "-" else qc_fmt_r(r$result$alpha)),
          tags$td(if (isTRUE(r$blocked)) span(class = "text-danger", r$message) else span(class = paste("decision", if (isTRUE(r$decision$rejected)) "rej" else "keep"), if (isTRUE(r$decision$rejected)) "reliable (at least .70)" else "below .70")),
          tags$td(if (!isTRUE(r$blocked)) downloadButton(paste0("rel_dl_", i), "Word", class = "btn-sm"))) }))),
      lapply(seq_along(rr), function(i) { r <- rr[[i]]; if (isTRUE(r$blocked)) return(NULL); div(class = "paste mb-2", r$paragraph) })) })
  observe({ rr <- rv$rel; req(rr); for (i in seq_along(rr)) local({ j <- i; r <- rr[[j]]; if (isTRUE(r$blocked)) return()
    output[[paste0("rel_dl_", j)]] <- downloadHandler(filename = basename(r$files[["docx"]]), content = function(f) file.copy(r$files[["docx"]], f)) }) })
  run_describe <- function(by = isolate(input$by) %||% "", t1 = isolate(input$t1vars) %||% t1_default()) { withProgress(message = "Describing your dataset...", {
    qc_set_labels(ctx())
    rv$desc <- tryCatch({ d <- qc_describe_dataset(ctx(), if (nzchar(by)) by else NULL)
      d$table1 <- if (length(t1)) qc_table1(fdata(), t1) else NULL; d }, error = function(e) list(error = conditionMessage(e))) }) }
  observeEvent(input$describe, run_describe())
  observeEvent(list(input$nav, rv$locked), { if (identical(input$nav, "s4") && isTRUE(rv$locked)) {   # newcomers get the description and the alpha without asking
    if (is.null(rv$rel)) run_rel(quiet = TRUE); if (is.null(rv$desc)) run_describe() } })
  output$desc_out <- renderUI({ d <- rv$desc; if (is.null(d)) return(card(h5("Description"), p(class = "text-muted", "The summary, Table 1 and the charts appear here in a moment."))); if (!is.null(d$error)) return(card(p(class = "text-danger", "Something went wrong: ", d$error)))
    rel <- sub(paste0("^", results_root, "/"), "", d$files[["html"]])
    tagList(card(h5("Ready-to-paste summary"), div(class = "paste", d$paragraph), br(),
      if (!is.null(d$table1)) tagList(h6(d$table1$title), tableOutput("t1_tbl")),
      downloadButton("desc_docx", "Download Word report")), tags$iframe(src = paste0("qcresults/", rel))) })
  output$t1_tbl <- renderTable(rv$desc$table1$df)
  output$desc_docx <- downloadHandler(filename = "Descriptives_report.docx", content = function(f) file.copy(rv$desc$files[["docx"]], f))

  # ======================================================================= 2. hypotheses & variables
  # Working data for this step: exclusions applied, your scales, and every scale QuantCompass found in the codebook and data.
  cands <- reactive({ req(rv$raw); d <- base_res()$data
    taken <- c(names(d), vapply(rv$scales, `[[`, "", "name"), vapply(rv$groups, `[[`, "", "name"))
    cs <- tryCatch(qc_measure_candidates(d, rv$cb, rv$labels, taken), error = function(e) list())
    own <- vapply(rv$scales, `[[`, "", "name")
    Filter(function(c0) !any(vapply(rv$scales, function(s) setequal(s$items, c0$items) || identical(s$name, c0$name), logical(1))) &&
                        !(identical(c0$source, "codebook") && isTRUE(c0$code %in% own)), cs) })       # a codebook scale you edited is replaced by your version
  wscales <- reactive(c(rv$scales, cands()))
  wdata <- reactive({ req(rv$raw); r <- tryCatch(qc_apply_prep(rv$raw, prep_obj()), error = function(e) NULL)
    qc_measure_data(if (is.null(r)) base_res()$data else r$data, cands()) })
  wlabels <- reactive({ l <- rv$labels; for (s in wscales()) if (is.null(l[[s$name]]) && nzchar(s$label %||% "")) l[[s$name]] <- s$label; l })
  wcat <- reactive(qc_measure_catalog(wdata(), wlabels(), rv$level_labels, rv$cb, wscales(), rv$groups))
  wctx <- function() { c0 <- ctx(); c0$labels <- wlabels(); c0 }
  sc_of <- function(v) { for (s in wscales()) if (identical(s$name, v)) return(s); NULL }
  var_desc <- function(v) { s <- sc_of(v)
    if (!is.null(s)) return(sprintf("Scale: %s of %d items (%s)%s", c(mean = "mean", sum = "sum", who5 = "WHO-5 index")[[s$method %||% "mean"]], length(s$items), paste(s$items, collapse = ", "),
      if (length(s$reverse)) paste0("; reversed: ", paste(s$reverse, collapse = ", ")) else ""))
    k <- wcat()$kind[wcat()$variable == v][1]; if (is.na(k)) return("")
    paste("Single column:", c(numeric = "numbers", ordinal = "rating / ordered answers", binary = "two categories", categorical = "categories")[[k]]) }
  goal_names <- c("Relationship between two variables" = "relationship", "Group difference" = "difference",
                  "Before / after (same people measured twice)" = "paired_difference", "Mediation (X affects Y through M)" = "mediation", "Moderation (W changes the effect of X on Y)" = "moderation",
                  "Association between two categories" = "association", "Prediction (multiple regression)" = "prediction",
                  "Scale reliability (Cronbach's alpha)" = "reliability")
  role_fields <- c("var1", "var2", "outcome", "group", "mediator", "moderator")
  # a sentence with two claims stays ONE hypothesis: its claims H4(i), H4(ii) are tested separately but counted once
  hyp_base <- function(id) sub("\\((i|ii|iii|iv|v|vi|vii|viii)\\)$", "", id)
  n_hyp_of <- function(ids) length(unique(vapply(ids, hyp_base, "")))
  new_uid <- function() paste0("u", paste(sample(c(letters, 0:9), 8, TRUE), collapse = ""))
  keep <- function(id, default) { v <- isolate(input[[id]]); if (is.null(v)) default else v }   # what you clicked survives a redraw

  output$hyp_ui <- renderUI({ if (is.null(rv$raw)) return(split1(card(h5("Load your data first"), p("Go to step 1 and choose your data file (and codebook)."),
      tags$button(type = "button", class = "btn btn-primary", onclick = nav_js("s1"), "Go to step 1"))))
    split2(tagList(
      card(h5("Your hypotheses"), note("One per line, as written in your thesis, e.g. 'H1: Higher financial stress is associated with lower well-being.'"),
        textAreaInput("htexts", NULL, rows = 8, placeholder = "H1: Higher financial strain is associated with lower well-being.\nH2: ...\n\nYou may paste H1\u2080 / H0 lines too: they are kept as the null hypothesis, never counted as extra hypotheses."),
        actionButton("dissect", "Recommend variables", class = "btn-primary"),
        uiOutput("hyp_read_note"), uiOutput("hyp_ai_note"),
        tags$details(class = "mt-2", tags$summary("Or upload a sheet (columns 'ID' and 'Hypothesis')"), fileInput("hfile", NULL, accept = c(".xlsx", ".csv"), buttonLabel = "Upload sheet..."))),
      card(h5("Confirmed"), uiOutput("confirmed_ui")),
      card(tags$details(tags$summary(tags$b("Plan a multiple regression (advanced)")), note("Pick the outcome (dependent variable). QuantCompass then checks every other variable as a possible predictor. Choose predictors from your theory: the correlation with the outcome is shown only for information."),
        uiOutput("mlr_ui")))),
      uiOutput("parts_ui")) })
  output$hyp_ai_note <- renderUI(div(class = "small text-muted mt-2", if (is.null(ai_cfg())) tagList("Recommendations come from QuantCompass's rules. ", tags$a(href = "#", onclick = "QC.openAI(); return false;", "Connect the AI assistant"), " for a second reading of every hypothesis.")
    else tagList(span(class = "qc-ai-ico", "\u2726"), " The AI assistant reads every hypothesis too. Its choices are marked \u2726 AI, QuantCompass's rules are marked App.")))
  read_hyp_lines <- function(txt) { ln <- trimws(strsplit(txt, "\n")[[1]]); ln <- ln[nzchar(ln)]
    ln <- sub("^[*•–-]+\\s*", "", ln); ln <- gsub("(?<=^H[0-9])[₀-₉]+|(?<=^H[0-9][0-9])[₀-₉]+|(?<=^H[0-9][a-z])[₀-₉]+", "", ln, perl = TRUE)
    ids <- ifelse(grepl("^H[0-9]+[a-z]?\\s*[:.)-]", ln, ignore.case = TRUE), toupper(sub("^(H[0-9]+[a-z]?).*$", "\\1", ln, ignore.case = TRUE)), paste0("H", seq_along(ln)))
    ids <- sub("^H", "H", sub("([0-9])([A-Z])$", "\\1\\L\\2", ids, perl = TRUE)); stats::setNames(sub("^H[0-9]+[a-z]?\\s*[:.)-]\\s*", "", ln, ignore.case = TRUE), ids) }
  start_breakdown <- function(H) { qc_set_labels(wctx()); qc_dissect_scales$scales <- wscales(); d <- wdata(); cat0 <- wcat()
    B <- withProgress(message = "Reading your hypotheses...", qc_dissect_all(H, d, cat0))
    rv$breakdowns <- B; parts <- list()
    for (b in B) { if (!length(b$parts)) parts[[length(parts) + 1]] <- list(id = b$id, uid = new_uid(), text = b$text, spec = list(goal = "relationship", var1 = NA, var2 = NA), phrases = list(),
          questions = b$questions, expected = "", confidence_label = "low")
      for (pp in b$parts) { sp <- qc_prefer_scales(pp$spec, wscales())
        parts[[length(parts) + 1]] <- list(id = pp$id, uid = new_uid(), text = b$text, spec = sp, phrases = pp$phrases, questions = b$questions,
          expected = tryCatch(qc_expected_text(sp, cat0), error = function(e) pp$expected %||% ""), confidence_label = pp$confidence_label %||% NA) } }
    for (k in seq_along(parts)) parts[[k]]$user_h0 <- rv$user_h0[[hyp_base(parts[[k]]$id)]] %||% ""
    for (k in seq_along(parts)) if (any(is.na(unlist(parts[[k]]$spec[intersect(names(parts[[k]]$spec), role_fields)])))) parts[[k]]$confidence_label <- "low"
    rv$parts <- parts
    if (!is.null(ai_cfg())) ask_ai(seq_along(parts), quiet = TRUE) }
  add_parts <- function(H, at = NULL) { qc_set_labels(wctx()); qc_dissect_scales$scales <- wscales(); d <- wdata(); cat0 <- wcat()
    B <- qc_dissect_all(H, d, cat0); new <- list()
    for (b in B) { if (!length(b$parts)) new[[length(new) + 1]] <- list(id = b$id, uid = new_uid(), text = b$text, spec = list(goal = "relationship", var1 = NA, var2 = NA), phrases = list(), questions = b$questions, expected = "", confidence_label = "low")
      for (pp in b$parts) { sp <- qc_prefer_scales(pp$spec, wscales())
        new[[length(new) + 1]] <- list(id = pp$id, uid = new_uid(), text = b$text, spec = sp, phrases = pp$phrases, questions = b$questions,
          expected = tryCatch(qc_expected_text(sp, cat0), error = function(e) ""), confidence_label = pp$confidence_label %||% NA) } }
    ps <- rv$parts; if (is.null(at) || is.na(at) || at > length(ps)) ps <- c(ps, new) else ps <- append(ps, new, after = at - 1); rv$parts <- ps }
  # ---- the AI reads every hypothesis once it is connected (and again when you ask)
  ask_ai <- function(ks, quiet = FALSE) { cfg <- ai_cfg()
    if (is.null(cfg)) { if (!quiet) open_ai("Connect the AI assistant first: choose a provider and paste your key."); return() }
    ps <- rv$parts; d <- wdata(); cat0 <- wcat(); qc_set_labels(wctx())
    withProgress(message = "The AI assistant is reading your hypotheses...", value = 0, for (k in ks) { pt <- ps[[k]]; incProgress(1 / length(ks), detail = pt$id)
      a <- tryCatch(qc_ai_dissect(pt$id, pt$text, d, cat0, cfg, rv$level_labels), error = function(e) list(ok = FALSE, problems = conditionMessage(e)))
      a$model <- sprintf("%s, %s", cfg$label, cfg$model)
      if (isTRUE(a$ok)) { a$spec <- qc_prefer_scales(a$spec, wscales()); a$expected <- tryCatch(qc_expected_text(a$spec, cat0), error = function(e) a$expected) }
      ps[[k]]$ai <- a
      app_missing <- any(is.na(unlist(pt$spec[intersect(names(pt$spec), role_fields)])))
      if (isTRUE(a$ok) && !isTRUE(pt$from_ai) && (app_missing || identical(pt$confidence_label, "low"))) {    # QuantCompass was unsure: start from the AI's reading
        ps[[k]]$app_spec <- pt$spec; ps[[k]]$spec <- a$spec; ps[[k]]$expected <- a$expected; ps[[k]]$from_ai <- TRUE; ps[[k]]$uid <- new_uid() } })
    rv$parts <- ps }
  open_ai <- function(msg = NULL) { session$sendCustomMessage("qc_open_ai", list()); if (!is.null(msg)) showNotification(msg, type = "warning", duration = 8) }
  observeEvent(input$pt_ask_ai, ask_ai(input$pt_ask_ai))
  observeEvent(input$dissect, { req(nzchar(input$htexts)); r <- qc_read_hyp_text(input$htexts)
    if (!length(r$hyps)) { showNotification("No hypotheses found. Write one per line, e.g. 'H1: ...'.", type = "warning"); return() }
    rv$user_h0 <- as.list(r$nulls); rv$hyp_notes <- c(sprintf("%d hypothes%s read: %s.", length(r$hyps), if (length(r$hyps) == 1) "is" else "es", paste(names(r$hyps), collapse = ", ")), r$notes)
    start_breakdown(r$hyps) })
  output$hyp_read_note <- renderUI({ n <- rv$hyp_notes; if (!length(n)) return(NULL); div(class = "small mt-2 qc-readnote", lapply(n, div)) })
  observeEvent(input$hfile, { x <- tryCatch(if (grepl("xlsx$", input$hfile$name)) as.data.frame(readxl::read_excel(input$hfile$datapath)) else utils::read.csv(input$hfile$datapath, check.names = FALSE), error = function(e) NULL)
    req(x); nm <- tolower(names(x)); i1 <- grep("^id", nm)[1]; i2 <- grep("hypothes", nm)[1]
    if (is.na(i2)) { showNotification("The sheet needs a column called 'Hypothesis'.", type = "error"); return() }
    H <- stats::setNames(as.character(x[[i2]]), if (is.na(i1)) paste0("H", seq_len(nrow(x))) else as.character(x[[i1]])); start_breakdown(H[!is.na(H) & nzchar(H)]) })

  # ---- multiple regression planner (on the working data)
  mlr_out <- reactive({ qc_mlr_outcomes(wdata(), wcat(), wscales()) })
  output$mlr_ui <- renderUI({ oc <- mlr_out(); use <- oc[oc$status != "no", ]; L <- wlabels()
    ic <- c(ok = "✓ ", warn = "⚠ ")
    tagList(selectInput("mlr_y", "Outcome (dependent variable)", c("Choose..." = "", stats::setNames(use$variable, paste0(ic[use$status], vapply(use$variable, lab_ui, "", labels = L))))),
      uiOutput("mlr_y_note"), uiOutput("mlr_x_ui"), uiOutput("mlr_check_ui"),
      fluidRow(column(5, textInput("mlr_id", "Hypothesis ID", "H5")), column(7, br(), actionButton("mlr_add", "Add as hypothesis", class = "btn-primary")))) })
  output$mlr_y_note <- renderUI({ req(nzchar(input$mlr_y %||% "")); oc <- mlr_out(); r <- oc[oc$variable == input$mlr_y, ]; note(r$reason) })
  mlr_pred <- reactive({ req(nzchar(input$mlr_y %||% "")); qc_mlr_predictors(wdata(), wcat(), input$mlr_y, wscales()) })
  output$mlr_x_ui <- renderUI({ pr <- mlr_pred(); use <- pr[pr$status != "no", ]; no <- pr[pr$status == "no", ]; L <- wlabels()
    ic <- c(ok = "✓", warn = "⚠")
    tagList(checkboxGroupInput("mlr_x", "Possible predictors (independent variables)",
        choiceNames = lapply(seq_len(nrow(use)), function(i) span(span(class = paste0("sym sym-", use$status[i]), ic[[use$status[i]]]), lab_ui(use$variable[i], L),
          span(class = "small text-muted", sprintf(" - %s%s", if (is.na(use$association[i])) "" else sprintf("association %s; ", qc_fmt_r(use$association[i])), use$reason[i])))),
        choiceValues = use$variable),
      if (nrow(no)) tags$details(tags$summary(sprintf("Not suitable (%d)", nrow(no))), tags$ul(class = "small", lapply(seq_len(nrow(no)), function(i) tags$li(lab_ui(no$variable[i], L), ": ", no$reason[i]))))) })
  output$mlr_check_ui <- renderUI({ req(nzchar(input$mlr_y %||% ""), length(input$mlr_x)); ck <- qc_mlr_check(wdata(), input$mlr_y, input$mlr_x)
    div(class = paste0("mt-2 mlr-", ck$status), tags$b(c(ok = "✓ Feasible. ", warn = "⚠ Check: ", no = "✗ Not feasible. ")[[ck$status]]), ck$verdict) })
  observeEvent(input$mlr_add, { req(nzchar(input$mlr_y %||% "")); if (!length(input$mlr_x)) { showNotification("Choose at least one predictor.", type = "warning"); return() }
    ck <- qc_mlr_check(wdata(), input$mlr_y, input$mlr_x); if (ck$status == "no") { showNotification("Too few complete cases for these predictors.", type = "error"); return() }
    sp <- list(goal = "prediction", outcome = input$mlr_y, predictors = input$mlr_x); id <- trimws(input$mlr_id); if (!nzchar(id)) id <- paste0("H", length(rv$parts) + 1)
    L <- function(v) if (!is.null(wlabels()[[v]])) wlabels()[[v]] else v
    txt <- sprintf("%s %s %s.", qc_cap_first(qc_and(vapply(input$mlr_x, L, ""))), if (length(input$mlr_x) > 1) "together predict" else "predicts", L(input$mlr_y))
    rv$parts <- c(Filter(function(p) p$id != id, rv$parts), list(list(id = id, uid = new_uid(), text = txt, spec = sp, phrases = list(), questions = character(), expected = qc_expected_text(sp, wcat()), confidence_label = "high")))
    showNotification(paste(id, "added on the right. Check it and press Confirm."), type = "message") })

  # ---- one card per hypothesis part: the roles with clickable recommendations, and your own choice
  part_data <- function(sp, d = fdata()) {
    if (!is.null(sp$subset)) d <- d[eval(parse(text = qc_cond_code(sp$subset$cond)), list(data = d)) %in% TRUE, , drop = FALSE]
    if (!is.null(sp$compare) && !is.null(sp$group)) { d <- d[as.character(d[[sp$group]]) %in% sp$compare, , drop = FALSE]; d[[sp$group]] <- factor(as.character(d[[sp$group]]), levels = sp$compare) }
    d }
  role_block <- function(k, r, pt) { L <- wlabels(); cat0 <- wcat(); pid <- function(x) sprintf("%s_%s_%s", pt$uid, x, r$field)
    app_v <- as.character(stats::na.omit(unlist((pt$app_spec %||% pt$spec)[[r$field]])))
    ai_v <- if (isTRUE(pt$ai$ok)) as.character(stats::na.omit(unlist(pt$ai$spec[[r$field]]))) else character()
    allv <- named(cat0$variable, L)
    head <- div(class = "qc-role-h", tags$b(r$label), if (length(r$phrase) && !is.na(r$phrase[1]) && nzchar(r$phrase[1])) span(class = "small text-muted", sprintf(" — in your hypothesis: “%s”", paste(r$phrase, collapse = "”, “"))))
    badges <- function(v) tagList(if (v %in% app_v) span(class = "qc-src app", "App"), if (v %in% ai_v) span(class = "qc-src ai", "✦ AI"))
    if (isTRUE(r$multi)) {
      return(div(class = "qc-role", head,
        selectizeInput(pid("pick"), NULL, allv, selected = keep(pid("pick"), r$chosen), multiple = TRUE, options = list(placeholder = if (isTRUE(r$optional)) "None (optional)" else "Choose columns or scales...")),
        if (length(r$chosen)) div(class = "small text-muted", lapply(r$chosen, function(v) div(tags$code(v), " ", badges(v), " ", var_desc(v)))),
        if (length(ai_v) && !setequal(ai_v, r$chosen)) div(class = "small", span(class = "qc-src ai", "✦ AI"), " suggests: ", paste(vapply(ai_v, lab_ui, "", labels = L), collapse = ", "), " ",
          tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('pt_ai_role', {k: %d, field: '%s', n: Date.now()}, {priority:'event'}); return false;", k, r$field), "use this")))) }
    opts <- unique(c(r$chosen, ai_v, r$alts)); opts <- opts[opts %in% cat0$variable]
    sel <- if (length(r$chosen) && r$chosen[1] %in% opts) r$chosen[1] else if (length(opts)) opts[1] else "__own"
    sel <- keep(pid("pick"), sel); if (!sel %in% c(opts, "__own")) sel <- "__own"
    div(class = "qc-role", head,
      radioButtons(pid("pick"), NULL, selected = sel,
        choiceNames = c(lapply(opts, function(v) span(class = "qc-opt", span(class = "qc-opt-name", lab_ui(v, L)), badges(v), span(class = "qc-opt-desc", var_desc(v)))),
                        list(span(class = "qc-opt", span(class = "qc-opt-name", "None of these — I choose my own column(s)")))),
        choiceValues = c(opts, "__own")),
      lapply(opts, function(v) { s <- sc_of(v); if (is.null(s)) return(NULL)
        conditionalPanel(sprintf("input['%s'] == '%s'", pid("pick"), v), measure_panel(s, sprintf("%s_m_%s", pid("x"), v), pt$id)) }),
      conditionalPanel(sprintf("input['%s'] == '__own'", pid("pick")),
        selectizeInput(pid("own"), "Your column(s): one column, or several items to build a scale from them", allv, selected = keep(pid("own"), NULL), multiple = TRUE, options = list(placeholder = "Type to search your columns...")),
        div(class = "qc-say", tags$label(class = "small text-muted", `for` = pid("say"), "...or describe it in your own words - the AI assistant sets it up:"),
          div(class = "d-flex gap-2", tags$input(type = "text", class = "form-control form-control-sm", id = pid("say_txt"), placeholder = "e.g. the six well-being items WB01 to WB06, averaged"),
            tags$button(type = "button", class = "btn btn-ai-outline btn-sm", onclick = sprintf("QC.askFrom('%s', %s)", pid("say_txt"),
              jsonlite::toJSON(sprintf("For %s, the %s should be: ", pt$id, tolower(r$label)), auto_unbox = TRUE)), "\u2726 Set it up"))))) }
  # how a construct is measured: the scale, its scoring and every item (with the question, reversed or not) - editable
  measure_panel <- function(s, key, hid = "") { d <- wdata(); L <- wlabels()
    a <- tryCatch(qc_scale_alpha_raw(d, s), error = function(e) list(alpha = NA, n = NA))
    num <- names(d)[vapply(d, is.numeric, logical(1))]; others <- setdiff(num, c(s$items, vapply(wscales(), `[[`, "", "name"), vapply(rv$groups, `[[`, "", "name")))
    src <- c(codebook = "from your codebook", suggested = "found in your data", own = "your scale")[[if ((s$source %||% "own") %in% c("codebook", "suggested")) s$source else "own"]]
    ilab <- function(it) { x <- L[[it]] %||% (rv$cb$vars[[it]]$label %||% it); qc_item_text(x) }
    meth <- c("Mean of the items" = "mean", "Sum of the items" = "sum", if (length(s$items) == 5) c("WHO-5 index (raw sum x 4, 0-100)" = "who5"))
    div(class = "qc-measure", `data-items` = paste(s$items, collapse = ","),
      div(class = "qc-m-head", span(class = "qc-m-kicker", "How it is measured"), " ", tags$b(s$label %||% s$name), " ", tags$code(s$name), " = ", qc_method_text(s$method), sprintf(" of %d items", length(s$items)),
        span(class = paste("qc-src", if (identical(src, "from your codebook")) "cb" else "app"), src)),
      tags$table(class = "table table-sm qc-items", tags$thead(tags$tr(tags$th("Use"), tags$th("Item"), tags$th("Question (item wording)"), tags$th("Reversed"))),
        tags$tbody(lapply(s$items, function(it) tags$tr(
          tags$td(tags$input(type = "checkbox", class = "qc-it-use", `data-item` = it, checked = NA, `aria-label` = paste("use", it))),
          tags$td(tags$code(it)), tags$td(class = "small", ilab(it)),
          tags$td(tags$input(type = "checkbox", class = "qc-it-rev", `data-item` = it, checked = if (it %in% s$reverse) NA else NULL, `aria-label` = paste("reverse", it))))))),
      div(class = "small qc-m-note", if (!is.na(a$alpha)) tagList("Cronbach's alpha = ", tags$b(qc_fmt_r(a$alpha)), sprintf(" (n = %d)", a$n), if (a$alpha < .7) span(class = "text-warning", " - below .70") else " - good internal consistency", ". ") else "",
        if (length(s$reverse)) sprintf("Reversed items are recoded first (%s + %s - answer), so a high score always means more of '%s'. ", qc_fmt_num(s$min %||% 1), qc_fmt_num(s$max %||% 5), s$label %||% s$name) else "",
        if (nzchar(s$why %||% "")) s$why),
      div(class = "qc-m-edit", tags$select(class = "form-select form-select-sm qc-m-method", `aria-label` = "Score", lapply(seq_along(meth), function(j) tags$option(value = meth[[j]], selected = if (identical(meth[[j]], s$method %||% "mean")) NA else NULL, names(meth)[j]))),
        tags$select(class = "form-select form-select-sm qc-m-add", `aria-label` = "Add an item to this scale",
          tags$option(value = "", selected = NA, "Add an item..."), lapply(others, function(v) tags$option(value = v, lab_ui(v, L)))),
        tags$button(type = "button", class = "btn btn-outline-primary btn-sm", onclick = sprintf("QC.scaleEdit(this, '%s')", s$name), "Update scale"),
        tags$button(type = "button", class = "btn btn-ai-outline btn-sm", onclick = sprintf("QC.ask(%s)", jsonlite::toJSON(sprintf("Explain the scale %s (%s) for %s: its items, why some are reversed, how the score is calculated and what its Cronbach's alpha means.", s$label %||% s$name, s$name, hid), auto_unbox = TRUE)), "\u2726 Explain"))) }
  observeEvent(input$sc_edit, { e <- input$sc_edit; s <- sc_of(e$name); req(s)
    items <- unique(c(intersect(s$items, unlist(e$use)), intersect(unlist(e$add), names(wdata())))); rev <- intersect(unlist(e$rev), items); m <- e$method %||% s$method %||% "mean"
    if (length(items) < 2) { showNotification("A scale needs at least two items.", type = "warning"); return() }
    if (m == "who5" && length(items) != 5) { showNotification("The WHO-5 index needs exactly 5 items; using the mean instead.", type = "warning"); m <- "mean" }
    x <- unlist(wdata()[, items, drop = FALSE]); x <- x[!is.na(x) & x >= 0]
    s2 <- utils::modifyList(s, list(items = items, reverse = rev, method = m, min = if (length(x)) min(x) else s$min, max = if (length(x)) max(x) else s$max, source = "own",
      why = sprintf("Your scale: %s of %s.", qc_method_text(m), paste(ifelse(items %in% rev, paste0(items, " (reversed)"), items), collapse = ", "))))
    s2 <- s2[intersect(names(s2), c("name", "label", "items", "reverse", "method", "min", "max", "text", "preset", "source", "why"))]
    rv$scales <- c(Filter(function(z) !identical(z$name, s$name), rv$scales), list(s2)); l <- rv$labels; if (is.null(l[[s2$name]])) l[[s2$name]] <- s2$label; rv$labels <- l; rv$locked <- FALSE
    a <- tryCatch(qc_scale_alpha_raw(wdata(), s2), error = function(e) list(alpha = NA))
    showNotification(sprintf("Scale %s updated: %s of %d items%s.", s2$name, qc_method_text(m), length(items), if (!is.na(a$alpha)) sprintf(", alpha = %s", qc_fmt_r(a$alpha)) else ""), type = "message") })
  part_details <- function(k, sp, uid) { d <- wdata(); L <- wlabels(); id <- function(f) paste0(uid, "_", f); val <- function(x) if (is.null(x) || all(is.na(x))) character(0) else x
    selectInput <- function(inputId, label, choices, selected = NULL, ...) shiny::selectInput(inputId, label, choices, keep(inputId, selected), ...)
    selectizeInput <- function(inputId, label, choices, selected = NULL, ...) shiny::selectizeInput(inputId, label, choices, keep(inputId, selected), ...)
    textInput <- function(inputId, label, value = "", ...) shiny::textInput(inputId, label, keep(inputId, value), ...)
    checkboxInput <- function(inputId, label, value = FALSE, ...) shiny::checkboxInput(inputId, label, keep(inputId, value), ...)
    dir_sel <- if (is.null(sp[["direction"]]) || is.na(sp[["direction"]])) "none" else sp[["direction"]]
    body <- switch(sp$goal,
      relationship = selectInput(id("dir"), "Expected direction", c("No direction stated" = "none", "Positive (more X, more Y)" = "positive", "Negative (more X, less Y)" = "negative"), dir_sel),
      mediation = selectInput(id("dir"), "Expected direction of the indirect effect", c("No direction stated" = "none", "Positive" = "positive", "Negative" = "negative"), dir_sel),
      difference = { lv <- if (!is.null(sp$group) && !is.na(sp$group) && sp$group %in% names(d)) { x <- d[[sp$group]]; k2 <- if (is.factor(x)) levels(x) else as.character(sort(unique(stats::na.omit(x))))
          stats::setNames(k2, vapply(k2, function(z) qc_level_lab(sp$group, z), "")) } else character()
        tagList(selectizeInput(id("compare"), "Groups to compare (in order: group 1 first; empty = all groups)", lv, selected = val(sp$compare), multiple = TRUE),
          selectInput(id("higher"), "Two groups: which one is expected to be higher?", c("No direction stated" = "none", lv), if (is.null(sp$expect_higher)) "none" else sp$expect_higher),
          if (length(lv) > 2) tagList(
            selectInput(id("trend"), "Three or more ordered groups: expected trend", c("No order stated" = "none", "Rises from the first to the last group" = "increasing", "Falls from the first to the last group" = "decreasing"), sp$trend %||% "none"),
            fluidRow(column(8, selectInput(id("extreme"), "Group expected to be lowest or highest (optional)", c("None" = "", lv), sp$expect_extreme$level %||% "")),
                     column(4, selectInput(id("extreme_which"), "which", c("lowest", "highest"), sp$expect_extreme$which %||% "lowest"))))) },
      paired_difference = selectInput(id("higher"), "Which measurement is expected to be higher?", c("No direction stated" = "none", named(c(sp$var1, sp$var2)[!is.na(c(sp$var1, sp$var2))], L)), if (is.null(sp$expect_higher)) "none" else sp$expect_higher),
      moderation = { wv <- if (!is.null(sp$moderator) && !is.na(sp$moderator) && sp$moderator %in% names(d)) sp$moderator else NA
        wlv <- if (!is.na(wv)) { x <- d[[wv]]; k2 <- if (is.factor(x)) levels(x) else as.character(sort(unique(stats::na.omit(x)))); if (length(k2) <= 6) stats::setNames(k2, vapply(k2, function(z) qc_level_lab(wv, z), "")) else character() } else character()
        st_sel <- if (!is.null(sp$stronger$at)) paste0("at:", sp$stronger$at) else if (!is.null(sp$stronger$level)) paste0("level:", sp$stronger$level) else if (!is.null(sp$stronger$weaker_level)) paste0("weaker:", sp$stronger$weaker_level) else "none"
        st_ch <- c("No pattern stated" = "none", if (length(wlv)) c(stats::setNames(paste0("level:", wlv), paste("Stronger for", names(wlv))), stats::setNames(paste0("weaker:", wlv), paste("Weaker for", names(wlv))))
                   else c("Stronger at high values of the moderator" = "at:high", "Stronger at low values of the moderator" = "at:low"))
        selectInput(id("stronger"), "Expected pattern (optional)", st_ch, st_sel) },
      prediction = textInput(id("dirs"), "Expected direction per predictor (optional)", value = if (length(sp$directions)) paste(names(sp$directions), sp$directions, collapse = ", ") else "", placeholder = "e.g. WUC negative, WUF positive"),
      NULL)
    tagList(body,
      textInput(id("subset"), "Only these cases (optional condition)", value = if (is.null(sp$subset)) "" else sp$subset$text, placeholder = "e.g. ES01 is 1,2"),
      checkboxInput(id("onesided"), "Test one-sided (only if the direction was planned before collecting the data)", value = !is.null(sp$alternative) && sp$alternative != "two.sided")) }
  output$parts_ui <- renderUI({ ps <- rv$parts
    if (!length(ps)) return(card(h5("Recommendations"), p(class = "text-muted", "Write your hypotheses on the left and press 'Recommend variables'. For each hypothesis you get:"),
      tags$ul(class = "text-muted", tags$li("what kind of hypothesis it is (relationship, group difference, mediation, ...)"), tags$li("the dependent and independent variables, and the columns or scales that measure them"),
        tags$li("other options to click, and a box to choose your own columns"))))
    qc_set_labels(wctx()); cat0 <- wcat(); d <- wdata(); ai_ready <- !is.null(ai_cfg()); conf_ids <- vapply(rv$hyps, `[[`, "", "id")
    tagList(
      if (ai_ready && any(vapply(ps, function(p) is.null(p$ai), logical(1)))) div(class = "qc-aibar", span(class = "qc-ai-ico", "✦"),
        div(tags$b("The AI assistant is connected."), div(class = "small", "Let it read the hypotheses it has not seen yet.")),
        tags$button(type = "button", class = "btn btn-ai btn-sm", onclick = "Shiny.setInputValue('pt_ask_ai_new', Date.now(), {priority:'event'})", "Ask the AI")),
      lapply(seq_along(ps), function(k) { pt <- ps[[k]]; sp <- pt$spec
        bases <- vapply(ps, function(p) hyp_base(p$id), ""); mem <- which(bases == hyp_base(pt$id)); pos <- match(k, mem)
        roles <- qc_recommend_roles(sp, if (isTRUE(pt$from_ai)) list() else pt$phrases, cat0, if (isTRUE(pt$ai$ok)) pt$ai$spec else NULL)
        ok <- !any(is.na(unlist(sp[intersect(names(sp), role_fields)])))
        pd <- if (ok) tryCatch(part_data(sp, d), error = function(e) NULL) else NULL
        pair <- if (!is.null(pd)) tryCatch(qc_hypothesis_pair(sp, pd, pt$id), error = function(e) NULL) else NULL
        n_used <- if (!is.null(pd)) { vs <- intersect(unlist(sp[c(role_fields, "predictors", "controls")]), names(pd)); if (length(vs)) sum(stats::complete.cases(pd[, vs, drop = FALSE])) else NA } else NA
        confirmed <- pt$id %in% conf_ids; conf <- pt$confidence_label %||% NA
        ai <- pt$ai; app0 <- pt$app_spec %||% sp
        ai_state <- if (is.null(ai)) NA else if (!isTRUE(ai$ok)) "none" else if (isTRUE(pt$from_ai)) "used" else if (identical(ai$spec$goal, app0$goal) && identical(unlist(ai$spec[role_fields]), unlist(app0[role_fields]))) "agree" else "differs"
        tagList(if (length(mem) > 1 && pos == 1) div(class = "qc-group-h", tags$b(sprintf("%s is one hypothesis with %d claims.", hyp_base(pt$id), length(mem))),
            sprintf(" Each claim needs its own test, so they are shown as %s - together they still count as one hypothesis.", paste(vapply(ps[mem], `[[`, "", "id"), collapse = " and ")),
            tags$a(href = "#", class = "ms-1", onclick = sprintf("QC.ask(%s); return false;", jsonlite::toJSON(sprintf("%s has %d claims. Explain why they are tested separately and whether one test could cover the whole hypothesis.", hyp_base(pt$id), length(mem)), auto_unbox = TRUE)), "\u2726 Ask why")),
        card(class = if (length(mem) > 1) "qc-claim" else NULL, div(class = "d-flex justify-content-between align-items-start", h5(if (length(mem) > 1) sprintf("%s \u00b7 claim %d of %d", hyp_base(pt$id), pos, length(mem)) else pt$id), div(
            if (!is.na(conf) && !isTRUE(pt$from_ai)) span(class = paste0("conf conf-", conf), title = "How sure QuantCompass's rules are about this reading. Always check the variables.", c(high = "✓ Sure", medium = "⚠ Check", low = "? Unsure")[[conf]]),
            if (!is.na(ai_state)) span(class = paste0("conf ms-1 conf-ai-", ai_state), c(agree = "✦ AI agrees", differs = "✦ AI reads it differently", none = "✦ AI: no answer", used = "✦ AI's reading")[[ai_state]]),
            if (confirmed) span(class = "decision rej ms-2", "✓ confirmed"))),
          p(class = "hyp", pt$text),
          div(class = "qc-type", tags$label(class = "small text-muted", `for` = paste0(pt$uid, "_goaltype"), "Type of hypothesis"),
            tags$select(id = paste0(pt$uid, "_goaltype"), class = "form-select form-select-sm", onchange = sprintf("Shiny.setInputValue('pt_goal', {k: %d, goal: this.value, n: Date.now()}, {priority:'event'})", k),
              lapply(seq_along(goal_names), function(j) tags$option(value = goal_names[[j]], selected = if (identical(goal_names[[j]], sp$goal)) NA else NULL, names(goal_names)[j])))),
          if (nzchar(pt$expected %||% "")) p(class = "small mt-2", tags$b(sub("^(Expected:)?\\s*", "", pt$expected) |> (\(x) paste("Expected:", x))())),
          if (isTRUE(pt$from_ai)) div(class = "small text-muted", span(class = "qc-src ai", "✦ AI"), " QuantCompass's rules were unsure, so this starts from the AI's reading. ", if (nzchar(ai$reason %||% "")) ai$reason),
          if (identical(ai_state, "differs") && !isTRUE(pt$from_ai)) div(class = "ai-sugg", tags$b("✦ The AI reads it as: "), names(goal_names)[goal_names == ai$spec$goal], " — ", ai$expected,
            if (nzchar(ai$reason %||% "")) div(class = "small text-muted", ai$reason),
            tags$button(type = "button", class = "btn btn-ai-outline btn-sm mt-1", "Use the AI's reading", onclick = sprintf("Shiny.setInputValue('pt_ai', %d, {priority:'event'})", k))),
          if (identical(ai_state, "none")) div(class = "small text-muted", "✦ AI assistant: ", paste(ai$problems, collapse = " ")),
          if (length(pt$questions) && !isTRUE(pt$from_ai)) div(class = "openq", tags$b("Please check"), tags$ul(lapply(pt$questions, tags$li))),
          lapply(roles, role_block, k = k, pt = pt),
          tags$details(class = "mt-2", tags$summary("Direction, groups and conditions"), part_details(k, sp, pt$uid)),
          if (!is.null(pair)) div(class = "hpair", div(span(class = "lab lab-h1", pair$lab1), "Research hypothesis: ", pair$h1),
            div(span(class = "lab lab-h0", pair$lab0), "Null hypothesis: ", pair$h0),
            if (nzchar(pt$user_h0 %||% "")) div(class = "small text-muted", "Your H0 as written: ", pt$user_h0),
            if (!is.na(n_used)) div(class = "small", sprintf("Cases with complete answers for this hypothesis (before the final exclusions): %d", n_used))),
          div(class = "mt-3", tags$button(type = "button", class = "btn btn-success btn-sm", if (confirmed) "Confirm again" else "✓ Confirm", onclick = sprintf("Shiny.setInputValue('pt_confirm', %d, {priority:'event'})", k)),
            tags$button(type = "button", class = "btn btn-outline-danger btn-sm ms-2", "Remove", onclick = sprintf("Shiny.setInputValue('pt_remove', %d, {priority:'event'})", k)),
            tags$button(type = "button", class = "btn btn-ai-outline btn-sm ms-2", title = "Let the AI read this hypothesis again and recommend variables",
              if (is.null(ai)) "✦ AI reading" else "✦ Read again", onclick = sprintf("Shiny.setInputValue('pt_ask_ai', %d, {priority:'event'})", k)),
            tags$button(type = "button", class = "btn btn-ai-outline btn-sm ms-2", onclick = sprintf("QC.ask(%s)", jsonlite::toJSON(sprintf("Break down %s for me in simple words: what it claims, which variables measure it (dependent and independent), and which test fits.", pt$id), auto_unbox = TRUE)),
              sprintf("✦ Ask about %s", pt$id))))) })) })
  observeEvent(input$pt_ask_ai_new, { k <- which(vapply(rv$parts, function(p) is.null(p$ai), logical(1))); if (length(k)) ask_ai(k) })

  # read the card back into an analysis spec; your own multi-column choice becomes a scale
  role_value <- function(uid, r) { pick <- input[[sprintf("%s_pick_%s", uid, r$field)]]
    if (isTRUE(r$multi)) return(as.character(pick %||% character()))
    if (is.null(pick) || !nzchar(pick)) return(NA_character_)
    if (!identical(pick, "__own")) return(pick)
    own <- input[[sprintf("%s_own_%s", uid, r$field)]]; if (!length(own)) return(NA_character_); if (length(own) == 1) return(own)
    d <- wdata(); items <- unique(unlist(lapply(own, function(v) { s <- sc_of(v); if (!is.null(s)) s$items else v })))
    old <- Filter(function(s) setequal(s$items, items), wscales()); if (length(old)) return(old[[1]]$name)
    x <- unlist(d[, items, drop = FALSE]); x <- x[!is.na(x) & x >= 0]
    nm <- qc_snake(if (length(r$phrase) && !is.na(r$phrase[1]) && nzchar(r$phrase[1])) r$phrase[1] else paste(r$field, "scale"), c(names(d), vapply(wscales(), `[[`, "", "name")))
    s <- list(name = nm, label = if (length(r$phrase) && !is.na(r$phrase[1])) r$phrase[1] else nm, items = items, reverse = qc_reverse_suggest(d, items), method = "mean",
              min = if (length(x)) min(x) else 1, max = if (length(x)) max(x) else 5, text = "")
    rv$scales <- c(rv$scales, list(s)); l <- rv$labels; l[[nm]] <- s$label; rv$labels <- l; rv$locked <- FALSE
    showNotification(sprintf("Scale '%s' built from %d items%s. Check it in step 3D.", nm, length(items), if (length(s$reverse)) sprintf(" (reversed: %s)", paste(s$reverse, collapse = ", ")) else ""), type = "message", duration = 8)
    nm }
  spec_from_part <- function(k) { pt <- rv$parts[[k]]; g <- function(f) input[[paste0(pt$uid, "_", f)]]; goal <- pt$spec$goal; sp <- list(goal = goal)
    roles <- qc_recommend_roles(pt$spec, if (isTRUE(pt$from_ai)) list() else pt$phrases, wcat())
    for (r in roles) { v <- role_value(pt$uid, r); if (isTRUE(r$multi)) { if (length(v)) sp[[r$field]] <- v } else sp[[r$field]] <- v }
    if (goal == "reliability") { sc <- Filter(function(s) setequal(s$items, sp$items), wscales())
      sp <- if (length(sc)) qc_reliability_spec(sc[[1]]) else list(goal = "reliability", items = sp$items, reverse = qc_reverse_suggest(wdata(), sp$items)) }
    if (goal %in% c("relationship", "mediation")) sp[["direction"]] <- if (is.null(g("dir")) || identical(g("dir"), "none")) pt$spec[["direction"]] %||% NA else g("dir")
    if (goal == "difference") { if (length(g("compare")) >= 2) sp$compare <- g("compare")
      h <- g("higher"); if (!is.null(h) && h != "none") sp$expect_higher <- h else if (is.null(h) && !is.null(pt$spec$expect_higher) && identical(pt$spec$group, sp$group)) sp$expect_higher <- pt$spec$expect_higher
      if (is.null(g("compare")) && !is.null(pt$spec$compare) && identical(pt$spec$group, sp$group)) sp$compare <- pt$spec$compare
      if (isTRUE(g("trend") %in% c("increasing", "decreasing")) && length(sp$compare) != 2) sp$trend <- g("trend")
      if (nzchar(g("extreme") %||% "") && length(sp$compare) != 2) sp$expect_extreme <- list(level = g("extreme"), which = g("extreme_which") %||% "lowest") }
    if (goal == "paired_difference") { h <- g("higher") %||% pt$spec$expect_higher; if (!is.null(h) && h %in% c(sp$var1, sp$var2)) sp$expect_higher <- h }
    if (goal == "moderation") { st <- g("stronger"); if (is.null(st)) sp$stronger <- pt$spec$stronger else if (st != "none") { k3 <- sub(":.*$", "", st); v3 <- sub("^[^:]*:", "", st)
      sp$stronger <- switch(k3, at = list(at = v3), level = list(level = v3), weaker = list(weaker_level = v3)) } }
    if (goal == "prediction") { sp$predictors <- setdiff(sp$predictors, sp$outcome); if (length(sp$controls)) sp$controls <- setdiff(sp$controls, c(sp$outcome, sp$predictors))
      dt <- trimws(strsplit(g("dirs") %||% "", "\\s*[,;]\\s*")[[1]]); dt <- dt[nzchar(dt)]
      if (!length(dt) && length(pt$spec$directions)) sp$directions <- pt$spec$directions[names(pt$spec$directions) %in% sp$predictors]
      if (length(dt)) { v <- sub("\\s+.*$", "", dt); w <- tolower(sub("^\\S+\\s+", "", dt)); ok <- v %in% sp$predictors & w %in% c("negative", "positive")
        if (any(!ok)) showNotification(sprintf("Directions not understood: %s (write e.g. 'WUC negative').", paste(dt[!ok], collapse = ", ")), type = "warning")
        if (any(ok)) sp$directions <- stats::setNames(w[ok], v[ok]) } }
    st <- trimws(g("subset") %||% (pt$spec$subset$text %||% ""))
    if (nzchar(st)) { cc <- qc_parse_condition_text(st, wdata()); if (!is.null(cc$error)) showNotification(cc$error, type = "error") else sp$subset <- list(cond = cc, label = st, text = st) }
    if (isTRUE(g("onesided"))) { s <- NA
      if (goal == "relationship" && !is.na(sp[["direction"]] %||% NA)) s <- if (sp[["direction"]] == "positive") "greater" else "less"
      if (goal == "difference" && !is.null(sp$expect_higher) && !is.null(sp$compare)) s <- if (sp$expect_higher == sp$compare[1]) "greater" else "less"
      if (goal == "paired_difference" && !is.null(sp$expect_higher)) s <- if (sp$expect_higher == sp$var1) "greater" else "less"
      if (!is.na(s)) sp$alternative <- s else showNotification("One-sided needs a stated direction (and, for groups, two groups to compare).", type = "warning") }
    sp }
  regoal <- function(sp, goal) { iv <- c(sp$var1, sp$group, sp$predictors); dv <- c(sp$outcome, sp$var2); iv <- iv[!is.na(iv)]; dv <- dv[!is.na(dv)]
    m <- c(sp$mediator, sp$moderator); m <- if (length(m[!is.na(m)])) m[!is.na(m)][1] else NA; first <- function(x) if (length(x)) x[1] else NA
    n <- list(goal = goal)
    switch(goal, relationship = , association = , paired_difference = { n$var1 <- first(iv); n$var2 <- first(dv) },
      difference = { n$group <- first(iv); n$outcome <- first(dv) }, prediction = { n$predictors <- iv; n$outcome <- first(dv) },
      mediation = { n$var1 <- first(iv); n$mediator <- m; n$outcome <- first(dv) }, moderation = { n$var1 <- first(iv); n$moderator <- m; n$outcome <- first(dv) },
      reliability = { s <- sc_of(first(c(dv, iv))); n <- if (!is.null(s)) qc_reliability_spec(s) else list(goal = "reliability", items = character()) })
    n }
  observeEvent(input$pt_goal, { k <- input$pt_goal$k; ps <- rv$parts; req(k <= length(ps)); ps[[k]]$spec <- regoal(ps[[k]]$spec, input$pt_goal$goal)
    ps[[k]]$phrases <- list(); ps[[k]]$app_spec <- NULL; ps[[k]]$uid <- new_uid(); ps[[k]]$expected <- tryCatch(qc_expected_text(ps[[k]]$spec, wcat()), error = function(e) ""); ps[[k]]$confidence_label <- NA; rv$parts <- ps })
  observeEvent(input$pt_ai_role, { k <- input$pt_ai_role$k; f <- input$pt_ai_role$field; a <- rv$parts[[k]]$ai; req(isTRUE(a$ok))
    updateSelectizeInput(session, sprintf("%s_pick_%s", rv$parts[[k]]$uid, f), selected = as.character(unlist(a$spec[[f]]))) })
  observeEvent(input$pt_confirm, { k <- input$pt_confirm; r <- confirm_part(k, spec_from_part(k)); if (!isTRUE(r)) showNotification(r, type = "warning") })
  confirm_part <- function(k, sp) { pt <- rv$parts[[k]]
    if (any(is.na(unlist(sp[intersect(names(sp), role_fields)])))) return(sprintf("%s: choose a column for every role first.", pt$id))
    used <- unlist(sp[intersect(names(sp), c(role_fields, "predictors"))]); if (anyDuplicated(used)) return(sprintf("%s: the same column is used for two roles.", pt$id))
    if (sp$goal == "prediction" && !length(sp$predictors)) return(sprintf("%s: choose at least one predictor.", pt$id))
    if (sp$goal == "reliability" && length(sp$items) < 2) return(sprintf("%s: choose at least two items.", pt$id))
    cat0 <- wcat(); expected <- tryCatch(qc_expected_text(sp, cat0), error = function(e) pt$expected)
    # scales QuantCompass found become your scales (you can check their items in step 3D)
    add <- Filter(function(s) s$name %in% unlist(sp[c(role_fields, "predictors", "controls")]) && !(s$name %in% vapply(rv$scales, `[[`, "", "name")), cands())
    if (length(add)) { sc <- c(rv$scales, lapply(add, function(s) s[intersect(names(s), c("name", "label", "items", "reverse", "method", "min", "max", "text", "preset", "source", "why"))]))
      rv$scales <- sc[!duplicated(vapply(sc, `[[`, "", "name"))]
      l <- rv$labels; for (s in add) if (is.null(l[[s$name]])) l[[s$name]] <- s$label; rv$labels <- l; rv$locked <- FALSE }
    ai <- pt$ai; app0 <- pt$app_spec %||% pt$spec
    ai_used <- if (isTRUE(ai$ok) && (isTRUE(pt$from_ai) || any(vapply(role_fields, function(f) { a <- ai$spec[[f]]; !is.null(a) && !is.na(a) && identical(sp[[f]], a) && !identical(app0[[f]], a) }, logical(1))))) ai$model else NULL
    old <- Filter(function(x) x$id == pt$id, rv$hyps)
    h <- list(id = pt$id, text = pt$text, spec = sp, choice = "auto", mode = "recommended", expected = expected, ai = ai_used)
    if (length(old) && identical(old[[1]]$spec, sp)) { h$choice <- old[[1]]$choice; h$mode <- old[[1]]$mode }
    ps <- rv$parts; ps[[k]]$spec <- sp; ps[[k]]$expected <- expected; if (!is.null(pt$app_spec) && !isTRUE(pt$from_ai)) ps[[k]]$app_spec <- pt$app_spec; rv$parts <- ps
    rv$hyps <- c(Filter(function(x) x$id != h$id, rv$hyps), list(h)); showNotification(paste(h$id, "confirmed."), type = "message"); TRUE }
  observeEvent(input$pt_ai, { k <- input$pt_ai; ps <- rv$parts; a <- ps[[k]]$ai; req(isTRUE(a$ok))
    ps[[k]]$app_spec <- ps[[k]]$spec; ps[[k]]$spec <- a$spec; ps[[k]]$expected <- a$expected; ps[[k]]$from_ai <- TRUE; ps[[k]]$uid <- new_uid(); rv$parts <- ps })
  observeEvent(input$pt_remove, { k <- input$pt_remove; id <- rv$parts[[k]]$id; rv$parts <- rv$parts[-k]; rv$hyps <- Filter(function(x) x$id != id, rv$hyps) })
  output$confirmed_ui <- renderUI({ if (!length(rv$hyps)) return(note("Nothing confirmed yet. Check the recommendations on the right and press Confirm."))
    L <- wlabels()
    tags$ul(class = "small", lapply(rv$hyps, function(h) { sp <- h$spec
      dv <- c(sp$outcome, if (sp$goal %in% c("relationship")) sp$var2); iv <- c(sp$group, sp$predictors, if (sp$goal %in% c("relationship", "mediation", "moderation")) sp$var1)
      tags$li(tags$b(h$id), ": ", names(goal_names)[goal_names == sp$goal],
        if (length(iv)) tagList(br(), "IV: ", paste(vapply(iv[!is.na(iv)], lab_ui, "", labels = L), collapse = ", ")),
        if (length(dv)) tagList(br(), "DV: ", paste(vapply(dv[!is.na(dv)], lab_ui, "", labels = L), collapse = ", ")),
        if (!is.null(h$ai)) span(class = "qc-src ai ms-1", "✦ AI")) })) })

  # ======================================================================= AI help (optional, the user's own key)
  ai_env <- qc_ai_read_env(file.path(root, ".env"))
  ai_cfg <- reactive({ if (!isTRUE(input$ai_on)) return(NULL)
    cfg <- tryCatch(qc_ai_config(input$ai_provider %||% "", trimws(input$ai_model %||% ""), trimws(input$ai_key %||% ""), trimws(input$ai_base %||% ""), env = ai_env), error = function(e) NULL)
    if (is.null(cfg) || !cfg$ready) NULL else cfg })
  output$ai_page <- renderUI({
    prov0 <- if (nzchar(ai_env$QC_AI_PROVIDER %||% "") && (ai_env$QC_AI_PROVIDER %in% names(qc_ai_providers))) ai_env$QC_AI_PROVIDER else "gemini"
    tagList(
      p("Connect an AI model with your own key and it works alongside QuantCompass on every page. Recommended for beginners."),
      div(class = "qc-when", h6("What the AI does for you"),
        tags$ul(class = "small",
          tags$li(tags$b("Step 1: "), "reads your codebook and describes the variables QuantCompass could not read."),
          tags$li(tags$b("Step 2: "), "reads every hypothesis and recommends the dependent and independent variables and the scales that measure them, next to QuantCompass's own recommendation (marked \u2726 AI and App)."),
          tags$li(tags$b("You stay in charge: "), "nothing is used until you click it and confirm. All statistics are computed in R, never by the AI."))),
      div(class = "qc-when", h6("What is sent"),
        tags$ul(class = "small", tags$li("Variable names, codebook labels and texts, your hypotheses. ", tags$b("Never your data values.")),
          tags$li("Every AI suggestion is checked against your variables; impossible ones are dropped."),
          tags$li("Reports and the methods section say where an AI suggestion was used."),
          tags$li("Your key is used for this session only and never saved. To keep it, write it into a file called .env in the QuantCompass folder (see .env.example); that file is never uploaded."))),
      h6("Connect"),
      selectInput("ai_provider", "Provider", stats::setNames(names(qc_ai_providers), vapply(qc_ai_providers, `[[`, "", "label")), prov0),
      uiOutput("ai_fields"),
      checkboxInput("ai_on", "Use the AI assistant in this session", TRUE),
      uiOutput("ai_status"),
      actionButton("ai_test", "Test the connection", class = "btn-outline-primary")) })
  outputOptions(output, "ai_page", suspendWhenHidden = FALSE)
  output$ai_chip <- renderUI({ cfg <- ai_cfg()
    if (is.null(cfg)) tagList(span(class = "qc-ai-ico", "\u2726"), "AI assistant", span(class = "qc-chip-state off", "connect"))
    else tagList(span(class = "qc-ai-ico", "\u2726"), "AI assistant", span(class = "qc-chip-state on", "on")) })
  output$ai_card <- renderUI({ cfg <- ai_cfg()
    if (is.null(cfg)) div(class = "qc-aicard", div(class = "d-flex align-items-center gap-2", span(class = "qc-ai-ico fs-5", "\u2726"), h5(class = "m-0", "AI assistant (recommended)")),
      p(class = "small mt-2 mb-2", "New to statistics? Connect an AI model with your own key (Gemini, Mistral, Claude, your university's AI, ...). It then reads your codebook and recommends the right variables for each hypothesis. Your data values are never sent."),
      tags$button(type = "button", class = "btn btn-ai btn-sm", onclick = "QC.openAI(); return false;", "Connect the AI assistant"),
      span(class = "small text-muted ms-2", "or continue without it"))
    else div(class = "qc-aicard on", div(class = "d-flex align-items-center gap-2", span(class = "qc-ai-ico fs-5", "\u2726"), h5(class = "m-0", "AI assistant is on")),
      p(class = "small mt-2 mb-0", sprintf("%s (%s). It reads your codebook and your hypotheses and makes recommendations; you confirm everything. ", cfg$label, cfg$model),
        tags$a(href = "#", onclick = "QC.openAI(); return false;", "Settings"))) })
  output$ai_fields <- renderUI({ pv <- input$ai_provider %||% "gemini"; pr <- qc_ai_providers[[pv]]; cfg0 <- qc_ai_config(pv, env = ai_env)
    tagList(if (length(pr$models)) tagList(selectizeInput("ai_model", "Model", c(pr$models), selected = cfg0$model, options = list(create = TRUE)),
        note("Recommended: llama-3.3-70b-instruct or qwen3-30b-a3b-instruct-2507. You can also type another model name. ", tags$a(href = pr$docs, target = "_blank", "Current model list")))
      else textInput("ai_model", "Model (leave empty for the default)", "", placeholder = cfg0$model),
      if (!is.na(pr$key_env)) passwordInput("ai_key", "API key", "", placeholder = if (nzchar(cfg0$key)) "Using the key from your .env file" else "Paste your key (this session only)"),
      if (!is.null(pr$base_env)) textInput("ai_base", "Address of the service", "", placeholder = cfg0$base)) })
  outputOptions(output, "ai_fields", suspendWhenHidden = FALSE)
  output$ai_status <- renderUI({ if (!isTRUE(input$ai_on)) return(note("The AI assistant is switched off for this session."))
    cfg <- ai_cfg(); if (is.null(cfg)) return(p(class = "text-danger small", "Paste your API key to connect."))
    div(class = "openq small", tags$b("The AI assistant is on. "), qc_ai_notice(cfg, input$chat_access %||% "full")) })
  # once connected, the AI catches up on what is already on screen (debounced: not while the key is being typed)
  ai_cfg_d <- debounce(ai_cfg, 2000)
  observeEvent(ai_cfg_d(), { cfg <- ai_cfg_d(); if (is.null(cfg)) return()
    if (!is.null(rv$cb) && !isTRUE(rv$cb_ai_done)) auto_cb_ai()
    k <- which(vapply(rv$parts, function(p) is.null(p$ai) || (!isTRUE(p$ai$ok) && grepl("(?i)connect|key|401|403|tunnel|timeout|resolve", paste(p$ai$problems, collapse = " "), perl = TRUE)), logical(1)))
    if (length(k)) ask_ai(k, quiet = TRUE) }, ignoreNULL = TRUE)
  observeEvent(input$ai_test, { cfg <- ai_cfg(); if (is.null(cfg)) { showNotification("Switch AI help on and enter a key first.", type = "warning"); return() }
    r <- tryCatch(qc_ai_chat(cfg, "Answer with JSON only.", 'Return {"ok": true}.'), error = function(e) e)
    if (inherits(r, "error")) showNotification(paste("Connection failed:", conditionMessage(r)), type = "error", duration = 12)
    else showNotification(sprintf("Connected to %s (%s).", cfg$label, cfg$model), type = "message") })
  cb_lines <- function() { if (is.null(rv$cb_path)) return(character())
    same <- identical(normalizePath(rv$cb_path), normalizePath(rv$path))      # codebook inside the data file
    if (same && qc_file_kind(rv$path) != "excel") return(character())          # never send the data itself
    ex <- tryCatch(qc_cb_extract(rv$cb_path, names(rv$raw)), error = function(e) NULL); if (is.null(ex)) return(character())
    if (same) { ex$lines <- character(); ex$tables <- ex$tables[grepl("(?i)codebook|variables|labels|dictionary", names(ex$tables), perl = TRUE)] }
    c(ex$lines, unlist(lapply(ex$tables, function(t) apply(as.matrix(t), 1, function(r) paste(stats::na.omit(r), collapse = " | "))))) }
  cb_gaps <- reactive({ req(rv$raw, rv$cb); d <- rv$raw
    Filter(function(v) { e <- rv$cb$vars[[v]]; !qc_is_system_var(v) && (is.null(e) || is.na(e$label) || !nzchar(e$label) ||
      (is.numeric(d[[v]]) && !length(e$values) && length(unique(stats::na.omit(d[[v]]))) <= 12 && !identical(e$kind, "text"))) }, names(d)) })
  output$cb_ai_ui <- renderUI({ req(rv$cb); g <- cb_gaps(); if (!length(g)) return(NULL)
    div(class = "qc-aibar", span(class = "qc-ai-ico", "\u2726"),
      div(tags$b(sprintf("%d variable(s) could not be described from the codebook", length(g))), div(class = "small", sprintf("%s%s. Name them yourself under 'Friendly names', or let the AI assistant read the codebook text for them%s.",
        paste(utils::head(g, 6), collapse = ", "), if (length(g) > 6) ", ..." else "", if (is.null(ai_cfg())) " (connect it first)" else ""))),
      actionButton("cb_ai", if (is.null(ai_cfg())) "Connect the AI" else "Ask the AI", class = "btn-ai btn-sm")) })
  auto_cb_ai <- function(quiet = TRUE) { cfg <- ai_cfg(); if (is.null(cfg) || is.null(rv$cb)) return(); rv$cb_ai_done <- TRUE
    if (!length(cb_gaps())) return()
    g <- withProgress(message = "The AI assistant is reading your codebook...", tryCatch(qc_ai_codebook_gaps(rv$cb, rv$raw, cb_lines(), cfg), error = function(e) e))
    if (inherits(g, "error")) { if (!quiet) showNotification(paste("AI assistant:", conditionMessage(g)), type = "error", duration = 12); return() }
    if (!length(g$vars)) { if (!quiet) showNotification(paste(c("The AI found no descriptions.", g$problems), collapse = " "), type = "warning"); return() }
    cb <- rv$cb; for (v in names(g$vars)) cb$vars[[v]] <- g$vars[[v]]
    cb$problems <- unique(c(cb$problems, sprintf("%d variable(s) were described by AI (%s, %s) from the codebook text and checked against the data: %s. Please check them.", length(g$vars), cfg$label, cfg$model, paste(names(g$vars), collapse = ", ")), g$problems))
    set_codebook(cb); showNotification(sprintf("The AI assistant described %d variable(s). Check them in the codebook check.", length(g$vars)), type = "message") }
  observeEvent(input$cb_ai, { if (is.null(ai_cfg())) { open_ai("Connect the AI assistant: choose a provider and paste your key."); return() }; auto_cb_ai(quiet = FALSE) })
  observeEvent(rv$cb, { if (!is.null(rv$cb) && !isTRUE(rv$cb_ai_done) && !is.null(isolate(ai_cfg()))) auto_cb_ai() }, ignoreNULL = TRUE)

  # ======================================================================= 5. assumption checks
  as_items <- reactive({ req(rv$locked); hs <- rv$hyps; d <- fdata(); qc_set_labels(ctx())
    lapply(hs, function(h) { sp <- h$spec
      need <- stats::na.omit(unlist(sp[c(role_fields, "predictors", "controls", "items")])); miss <- setdiff(need, names(d))
      if (length(miss)) return(list(h = h, error = sprintf("Not in the final data: %s. Was a scale or group removed in step 3? Confirm this hypothesis again in step 2.", paste(miss, collapse = ", "))))
      pd <- tryCatch(part_data(sp, d), error = function(e) NULL); if (is.null(pd)) return(list(h = h, error = "The condition for the cases could not be applied."))
      pdf <- if (exists("qc_as_factors")) qc_as_factors(pd, sp) else pd
      sc <- tryCatch(qc_screen(pdf, sp), error = function(e) list(error = conditionMessage(e)))
      if (!is.null(sc$error)) return(list(h = h, error = paste("The checks could not run:", sc$error)))
      opts <- tryCatch(qc_part_options(sp, part_data(utils::modifyList(sp, list(compare = NULL)), d)), error = function(e) list())
      list(h = h, data = pdf, screen = sc, opts = opts) }) })
  output$assume_ui <- renderUI({ if (!rv$locked) return(locked_card()); it <- as_items()
    if (!length(it)) return(split1(card(h5("No confirmed hypotheses yet"), p("Confirm your hypotheses in step 2. Their assumption checks appear here."),
      tags$button(type = "button", class = "btn btn-primary", onclick = nav_js("s2"), "Go to step 2"))))
    cards <- lapply(seq_along(it), function(i) { x <- it[[i]]; h <- x$h
      if (!is.null(x$error)) return(card(h5(h$id), p(class = "hyp", h$text), p(class = "text-danger", x$error)))
      ck <- x$screen$checks; dec <- x$screen$decision
      cur <- if (identical(h$mode, "recommended") || is.null(h$mode)) 1 else { j <- which(vapply(x$opts, function(o) identical(o$choice, h$choice) && identical(o$spec, h$spec), logical(1))); if (length(j)) j[1] else 1 }
      card(div(class = "d-flex justify-content-between", h5(h$id), span(class = "small text-muted", names(goal_names)[goal_names == h$spec$goal])), p(class = "hyp", h$text),
        h6("Assumption checks on your final data"),
        if (!is.null(ck) && nrow(ck)) tags$table(class = "table table-sm qc-checks", tags$thead(tags$tr(tags$th(""), tags$th("Assumption"), tags$th("How it was checked"), tags$th("Result"), tags$th("What it means"))),
          tags$tbody(lapply(seq_len(nrow(ck)), function(j) { m <- ck$met[j]
            tags$tr(tags$td(if (isTRUE(m)) sym("ok") else if (isFALSE(m)) sym("warn") else ""), tags$td(ck$check[j]), tags$td(class = "small", ck$method[j]), tags$td(class = "small", ck$value[j]), tags$td(class = "small", ck$meaning[j])) }))),
        div(class = "qc-testbox", tags$b("Test for this hypothesis: ", qc_test_labels[[dec$test_id]] %||% dec$test_id), div(class = "small", dec$reason),
          tags$button(type = "button", class = "btn btn-ai-outline btn-sm mt-2", onclick = sprintf("QC.ask(%s)", jsonlite::toJSON(sprintf("Explain the assumption checks for %s in simple words: what each one means, what my data shows, and why this test was chosen.", h$id), auto_unbox = TRUE)), "\u2726 Explain these checks")),
        if (length(x$opts) > 1) div(class = "mt-2", span(class = "small text-muted me-2", "Use:"),
          lapply(seq_along(x$opts), function(j) { o <- x$opts[[j]]
            tags$button(type = "button", class = paste("btn btn-sm me-2 mb-1", if (j == cur) "btn-primary" else "btn-outline-secondary"), title = o$why,
              onclick = sprintf("Shiny.setInputValue('as_choice', {i: %d, j: %d, n: Date.now()}, {priority:'event'})", i, j), sprintf("%s: %s", o$role, o$label)) }),
          if (cur > 1) div(class = "small text-muted", x$opts[[cur]]$why)),
        tags$details(class = "mt-2", tags$summary("Show the charts"), uiOutput(paste0("as_figs_", i)))) })
    split2(tagList(card(h5("What happens here"), tags$ul(class = "small",
        tags$li("Every statistical test has conditions (assumptions), e.g. normally distributed values or similar spread in the groups."),
        tags$li("QuantCompass checks them on your final data and picks the test whose conditions are met. If they are not met, it uses a robust or rank-based test instead."),
        tags$li("✓ = condition met, ⚠ = not met (the test choice already takes this into account)."),
        tags$li("You can switch to the alternative test, but only if you can justify it in your thesis.")),
        p(class = "small", "Next: press ", tags$b(sprintf("Run %d hypothes%s", length(it), if (length(it) == 1) "is" else "es")), " in the bar below.")),
      cards[seq(1, length(cards), by = 2)]), cards[seq_along(cards) %% 2 == 0], "1fr 1fr") })
  observe({ it <- tryCatch(as_items(), error = function(e) list()); for (i in seq_along(it)) local({ j <- i; x <- it[[j]]
    output[[paste0("as_figs_", j)]] <- renderUI({ if (!is.null(x$error)) return(NULL)
      figs <- tryCatch(qc_assumption_figures(x$data, x$h$spec, x$screen), error = function(e) list()); if (!length(figs)) return(note("No charts for this test."))
      tagList(lapply(seq_along(figs), function(f) { id <- sprintf("as_plot_%d_%d", j, f); output[[id]] <- renderPlot(figs[[f]]$plot, res = 96)
        div(plotOutput(id, height = "280px"), note(figs[[f]]$caption)) })) }) }) })
  observeEvent(input$as_choice, { i <- input$as_choice$i; j <- input$as_choice$j; it <- as_items(); req(i <= length(it)); o <- it[[i]]$opts[[j]]; req(o)
    hs <- rv$hyps; k <- which(vapply(hs, `[[`, "", "id") == it[[i]]$h$id); req(length(k))
    hs[[k]]$choice <- o$choice; hs[[k]]$spec <- o$spec; hs[[k]]$mode <- if (j == 1) "recommended" else "manual"; rv$hyps <- hs })

  # ======================================================================= 6. run + results
  run_all <- function() { if (!length(rv$hyps)) { showNotification("Confirm at least one hypothesis first.", type = "warning"); return() }
    c0 <- ctx(); c0$out_dir <- file.path(rv$out_dir, "analysis"); res <- list(); hs <- rv$hyps
    withProgress(message = "Running analyses", value = 0, {
      for (i in seq_along(hs)) { h <- hs[[i]]; incProgress(1 / (length(hs) + 1), detail = paste(h$id, "- screening assumptions and running the test"))
        res[[i]] <- tryCatch(qc_run_hypothesis(h, c0), error = function(e) list(id = h$id, sentence = h$text, blocked = TRUE, message = paste("The analysis could not be completed:", conditionMessage(e)))) }
      incProgress(1 / (length(hs) + 1), detail = "Writing the summary report")
      qc_set_labels(c0)
      meth <- tryCatch(qc_methods_text(rv$final$prep, rv$final$counts, rv$final$summary$scales, res, if (!is.null(rv$cb)) rv$cb$source else NULL), error = function(e) NULL)
      ai_ids <- vapply(Filter(function(h) !is.null(h$ai), hs), `[[`, "", "id")
      if (!is.null(meth) && length(ai_ids)) meth$analysis <- paste(meth$analysis, sprintf("For %s, the assignment of the hypothesis to variables was suggested by an AI system (%s), checked against the dataset and confirmed by the author; the AI did not compute any results.",
        qc_and(ai_ids), paste(unique(vapply(Filter(function(h) !is.null(h$ai), hs), `[[`, "", "ai")), collapse = "; ")))
      t1 <- if (!is.null(rv$desc$table1)) rv$desc$table1 else tryCatch(qc_table1(fdata(), intersect(c(grep("^DM[0-9]", names(fdata()), value = TRUE), vapply(rv$final$prep$scales, `[[`, "", "name")), names(fdata()))), error = function(e) NULL)
      summ <- tryCatch(qc_summary_report(res, c0, if (!is.null(rv$desc) && is.null(rv$desc$error)) rv$desc else NULL, meth, t1, rv$breakdowns), error = function(e) { showNotification(paste("Summary report:", conditionMessage(e)), type = "error"); NULL }) })
    rv$results <- list(items = res, summary = summ, methods = meth); rv$ran_hyps <- hs; updateNavbarPage(session, "nav", "s6") }
  observeEvent(input$run, run_all())
  output$results_ui <- renderUI({ if (!rv$locked) return(locked_card()); r <- rv$results
    if (is.null(r)) return(split1(card(h5("No results yet"), p(class = "text-muted", "Confirm your hypotheses in step 2, confirm the data in step 3, and press Run in step 5. The results appear here."),
      tags$button(type = "button", class = "btn btn-primary", onclick = nav_js("s5"), "Go to step 5: Assumption checks"))))
    split2(tagList(card(h5("All results"), p("Everything is saved in: ", tags$code(file.path(rv$out_dir, "analysis"))),
        if (!is.null(r$summary)) downloadButton("sum_docx", "Summary report with methods section (Word)"),
        actionButton("open_dir", "Open results folder", class = "btn-outline-secondary ms-2"),
        tags$table(class = "table table-sm mt-3", tags$thead(tags$tr(tags$th("ID"), tags$th("Test"), tags$th("Decision"))),
          tags$tbody(lapply(r$items, function(x) tags$tr(tags$td(x$id), tags$td(if (isTRUE(x$blocked)) "-" else x$test),
            tags$td(if (isTRUE(x$blocked)) span(class = "text-danger", "not run") else span(class = paste("decision", if (isTRUE(x$decision$rejected)) "rej" else "keep"), x$decision$short))))))),
      if (!is.null(r$methods)) card(h5("Methods section (ready to paste and adapt)"), div(class = "paste", r$methods$sample), br(), if (nzchar(r$methods$measures)) div(class = "paste", r$methods$measures), br(), div(class = "paste", r$methods$analysis)),
      card(h5("Optional: check against numbers you already reported"), note("Paste lines such as 'H1a: r = -.47, p < .001'. Each number is looked up in QuantCompass's result for that hypothesis."),
        textAreaInput("val_txt", NULL, rows = 4), actionButton("val_go", "Compare", class = "btn-outline-primary"), uiOutput("val_out"))),
      lapply(seq_along(r$items), function(i) { x <- r$items[[i]]
        if (isTRUE(x$blocked)) return(card(h5(x$id), p(x$sentence), p(class = "text-danger", x$message)))
        rel <- sub(paste0("^", results_root, "/"), "", x$files[["html"]])
        card(h5(paste0(x$id, ": ", x$test)),
          div(class = "hpair", div(span(class = "lab lab-h1", x$lab1), "Research hypothesis: ", x$h1), div(span(class = "lab lab-h0", x$lab0), "Null hypothesis: ", x$h0)),
          span(class = paste("decision", if (isTRUE(x$decision$rejected)) "rej" else "keep"), x$decision$short),
          tags$button(type = "button", class = "btn btn-ai-outline btn-sm ms-2", onclick = sprintf("QC.ask(%s)", jsonlite::toJSON(sprintf("Explain the result of %s in simple words: what the numbers mean, whether the hypothesis is supported, and how to report it.", x$id), auto_unbox = TRUE)), "\u2726 Explain this result"),
          p(tags$b("Why this test: "), x$reason), div(class = "paste", x$paragraph), br(),
          downloadButton(paste0("dl_docx_", i), "Word report"), downloadButton(paste0("dl_r_", i), "R script"),
          tags$details(class = "mt-3", tags$summary("Show full report with charts"), tags$iframe(src = paste0("qcresults/", rel)))) }), "5fr 7fr") })
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
  # ======================================================================= step status, Back / Continue bar, summary panel
  # ======================================================================= the AI assistant chat (on every page)
  chat_add <- function(m) { rv$chat <- c(rv$chat, list(m)); length(rv$chat) }
  part_index <- function(id) { ids <- vapply(rv$parts, `[[`, "", "id"); k <- which(ids == id); if (!length(k)) k <- which(vapply(ids, hyp_base, "") == id); if (length(k)) k[1] else NA }
  # everything the assistant may know about the project right now
  assist_state <- function() {
    cur <- isolate(input$nav) %||% "s1"; st <- list(step = sprintf("%s (step %s of 6)", qc_steps[[cur]], substring(cur, 2)), hyps = list(), n_hyp = 0)
    if (is.null(rv$raw)) return(st)
    d <- tryCatch(wdata(), error = function(e) rv$raw); cat0 <- tryCatch(wcat(), error = function(e) qc_catalog(d, rv$labels, rv$level_labels))
    ex <- tryCatch(prep_obj()$exclusions, error = function(e) rv$excl)
    used <- unlist(lapply(rv$hyps, function(h) h$spec[c(role_fields, "predictors", "controls")]))
    sc <- lapply(tryCatch(wscales(), error = function(e) rv$scales), function(s) { s$alpha <- tryCatch(qc_scale_alpha_raw(d, s)$alpha, error = function(e) NA); s$used <- s$name %in% used; s })
    conf <- vapply(rv$hyps, `[[`, "", "id")
    hy <- lapply(rv$parts, function(pt) { sp <- pt$spec; r <- unlist(sp[intersect(names(sp), c(role_fields, "predictors", "controls"))])
      list(id = pt$id, text = pt$text, goal = sp$goal, roles = paste(sprintf("%s = %s", names(r), r), collapse = ", "), confirmed = pt$id %in% conf, expected = pt$expected %||% "", user_h0 = pt$user_h0 %||% "") })
    as <- if (rv$locked) tryCatch(unlist(lapply(as_items(), function(x) { if (!is.null(x$error)) return(sprintf("%s: %s", x$h$id, x$error)); ck <- x$screen$checks; dec <- x$screen$decision
      c(sprintf("%s: test = %s. %s", x$h$id, qc_test_labels[[dec$test_id]] %||% dec$test_id, dec$reason),
        if (!is.null(ck) && nrow(ck)) sprintf("   %s %s: %s", ifelse(ck$met %in% TRUE, "met", ifelse(ck$met %in% FALSE, "NOT met", "info")), ck$check, ck$value)) })), error = function(e) character()) else character()
    res <- if (!is.null(rv$results)) unlist(lapply(rv$results$items, function(x) if (isTRUE(x$blocked)) sprintf("%s: not run - %s", x$id, x$message)
      else c(sprintf("%s (%s): %s", x$id, x$test, x$decision$short), sprintf("   %s", x$paragraph)))) else character()
    c(st, list(file = basename(rv$path), n_raw = nrow(rv$raw), n_cols = ncol(rv$raw), codebook = if (!is.null(rv$cb)) rv$cb$source else NULL,
      cb_problems = rv$cb$problems, data = d, cat = cat0, check = rv$cb_check, level_labels = rv$level_labels, exclusions = ex,
      flow = tryCatch(base_res()$flow, error = function(e) character()), manual = rv$manual, scales = sc, groups = rv$groups,
      hyps = hy, part_ids = vapply(rv$parts, `[[`, "", "id"), n_hyp = n_hyp_of(vapply(rv$parts, `[[`, "", "id")),
      final = if (rv$locked) sprintf("Confirmed: %d cases, %d variables.", nrow(rv$final$data), ncol(rv$final$data)) else "Not confirmed yet (step 3).",
      assumptions = as, results = res, id_var = tryCatch(id_var(), error = function(e) names(rv$raw)[1]), raw_ids = tryCatch(rv$raw[[id_var()]], error = function(e) NULL),
      raw_names = stats::setNames(names(rv$raw), names(rv$raw)), codebook_lines = rv$cb$lines %||% character()))
  }
  # a snapshot of everything the assistant can change, for Undo
  snap <- function() { ex <- tryCatch(prep_obj()$exclusions, error = function(e) rv$excl)
    list(labels = rv$labels, level_labels = rv$level_labels, cb = rv$cb, cb_check = rv$cb_check, accept_na = rv$accept_na, excl = ex, manual = rv$manual,
         scales = rv$scales, groups = rv$groups, parts = rv$parts, hyps = rv$hyps, locked = rv$locked, final = rv$final, htexts = isolate(input$htexts) %||% "") }
  restore <- function(z) { for (f in setdiff(names(z), c("excl", "htexts"))) rv[[f]] <- z[[f]]
    rv$excl <- z$excl; for (x in z$excl) updateCheckboxInput(session, paste0("ex_", x$id), value = isTRUE(x$on)); updateTextAreaInput(session, "htexts", value = z$htexts) }
  refresh_cb <- function() { if (is.null(rv$cb)) return(); rv$cb_check <- qc_codebook_check(rv$cb, rv$raw)
    ap <- qc_codebook_apply(rv$cb, rv$raw, rv$accept_na); rv$level_labels <- ap$level_labels; rv$locked <- FALSE }
  hyp_text_sync <- function() { ps <- rv$parts; b <- unique(vapply(ps, function(p) hyp_base(p$id), ""))
    txt <- vapply(b, function(x) { p <- ps[[which(vapply(ps, function(q) hyp_base(q$id), "") == x)[1]]]; sprintf("%s: %s", x, p$text) }, "")
    updateTextAreaInput(session, "htexts", value = paste(txt, collapse = "\n")) }
  ensure_cb_var <- function(v) { if (is.null(rv$cb)) rv$cb <- list(vars = list(), rules = list(), source = "codebook entries made in the app", problems = character())
    cb <- rv$cb; if (is.null(cb$vars[[v]])) cb$vars[[v]] <- qc_cb_entry(v, rv$labels[[v]] %||% NA_character_); cb }
  # one checked change -> the app's state
  apply_action <- function(x) { a <- x$action; tp <- a$type; d <- wdata()
    switch(tp,
      set_label = { l <- rv$labels; l[[a$variable]] <- a$label; rv$labels <- l },
      set_codes = { cb <- ensure_cb_var(a$variable); cd <- unlist(a$codes); cb$vars[[a$variable]]$values <- stats::setNames(as.character(cd), names(cd)); cb$vars[[a$variable]]$kind <- "codes"; rv$cb <- cb; refresh_cb() },
      clear_codes = { cb <- ensure_cb_var(a$variable); cb$vars[[a$variable]]$values <- character(); cb$vars[[a$variable]]$kind <- "none"; rv$cb <- cb; refresh_cb() },
      set_missing = { cb <- ensure_cb_var(a$variable); cb$vars[[a$variable]]$missing <- unique(c(cb$vars[[a$variable]]$missing, as.character(unlist(a$codes)))); rv$cb <- cb; refresh_cb() },
      exclusion_on = , exclusion_off = { on <- tp == "exclusion_on"; ex <- rv$excl; for (i in seq_along(ex)) if (identical(ex[[i]]$id, x$id)) ex[[i]]$on <- on
        rv$excl <- ex; updateCheckboxInput(session, paste0("ex_", x$id), value = on); rv$locked <- FALSE },
      add_exclusion = { cc <- qc_parse_condition_text(a$condition, base_res()$data); id <- paste0("ai_", length(rv$excl) + 1)
        rv$excl <- c(rv$excl, list(list(id = id, label = a$label %||% a$condition, cond = qc_cond_code(cc), on = TRUE, reason = sprintf("Added with the AI assistant: remove cases where %s.", a$condition), from = "assistant"))); rv$locked <- FALSE },
      exclude_cases = { rv$manual <- unique(rbind(rv$manual, data.frame(id = as.character(unlist(a$ids)), reason = a$reason))); rv$locked <- FALSE },
      create_scale = , update_scale = { it <- as.character(unlist(a$items)); xs <- unlist(d[, it, drop = FALSE]); xs <- xs[!is.na(xs) & xs >= 0]
        s <- list(name = a$name, label = a$label %||% a$name, items = it, reverse = intersect(as.character(unlist(a$reverse)), it), method = a$method %||% "mean",
                  min = min(xs), max = max(xs), text = "", source = "own", why = "Built with the AI assistant.")
        rv$scales <- c(Filter(function(z) !identical(z$name, s$name), rv$scales), list(s)); l <- rv$labels; l[[s$name]] <- s$label; rv$labels <- l; rv$locked <- FALSE },
      delete_scale = { rv$scales <- Filter(function(z) !identical(z$name, a$name), rv$scales); rv$locked <- FALSE },
      create_group = { g <- list(name = a$name, label = a$label %||% a$name, groups = lapply(a$groups, function(r) list(label = r$label, cond = qc_parse_condition_text(r$condition, base_res()$data))))
        rv$groups <- c(Filter(function(z) !identical(z$name, g$name), rv$groups), list(g)); l <- rv$labels; l[[g$name]] <- g$label; rv$labels <- l; rv$locked <- FALSE },
      add_hypothesis = { ids <- unique(vapply(rv$parts, function(p) hyp_base(p$id), "")); id <- a$id %||% ""; if (!nzchar(id)) { k <- length(ids) + 1; while (paste0("H", k) %in% ids) k <- k + 1; id <- paste0("H", k) }
        add_parts(stats::setNames(a$text, id)); hyp_text_sync() },
      edit_hypothesis = { k <- vapply(rv$parts, function(p) hyp_base(p$id) == a$id, logical(1)); old <- rv$parts; rv$hyps <- Filter(function(h) hyp_base(h$id) != a$id, rv$hyps)
        rv$parts <- old[!k]; add_parts(stats::setNames(a$text, a$id), at = which(k)[1]); hyp_text_sync() },
      remove_hypothesis = { rv$parts <- Filter(function(p) hyp_base(p$id) != a$id, rv$parts); rv$hyps <- Filter(function(h) hyp_base(h$id) != a$id, rv$hyps); hyp_text_sync() },
      set_hypothesis = { k <- part_index(a$id); ps <- rv$parts; sp <- x$spec
        ps[[k]]$app_spec <- ps[[k]]$app_spec %||% ps[[k]]$spec; ps[[k]]$spec <- sp; ps[[k]]$uid <- new_uid(); ps[[k]]$from_ai <- TRUE; ps[[k]]$confidence_label <- "high"
        ps[[k]]$ai <- list(ok = TRUE, spec = sp, model = sprintf("%s, %s", ai_cfg()$label, ai_cfg()$model), reason = "Set up in the AI chat.")
        ps[[k]]$expected <- tryCatch(qc_expected_text(sp, wcat()), error = function(e) ""); rv$parts <- ps; rv$hyps <- Filter(function(h) h$id != ps[[k]]$id, rv$hyps) },
      confirm_hypothesis = { ids <- vapply(rv$parts, `[[`, "", "id"); ks <- which(ids == a$id | vapply(ids, hyp_base, "") == a$id)
        for (k in ks) { r <- confirm_part(k, rv$parts[[k]]$spec); if (!isTRUE(r)) stop(r) } },
      confirm_data = { if (!isTRUE(confirm_gate())) stop("the final data could not be confirmed - check step 3") },
      run_analyses = run_all(),
      go_to_step = updateNavbarPage(session, "nav", paste0("s", as.integer(a$step))))
    invisible(TRUE) }
  apply_msg <- function(i, which = NULL) { m <- rv$chat[[i]]; idx <- which(vapply(m$actions, function(a) identical(a$status, "pending"), logical(1))); if (!is.null(which)) idx <- intersect(idx, which)
    if (!length(idx)) return()
    z <- snap()
    for (j in idx) { r <- tryCatch({ apply_action(m$actions[[j]]); "applied" }, error = function(e) paste("failed:", conditionMessage(e)))
      m$actions[[j]]$status <- if (identical(r, "applied")) "applied" else "failed"; if (!identical(r, "applied")) m$actions[[j]]$error <- sub("^failed: ", "", r) }
    m$snaps <- c(m$snaps, list(z)); ch <- rv$chat; ch[[i]] <- m; rv$chat <- ch; rv$undo_last <- i }
  observeEvent(input$chat_send, { txt <- trimws(input$chat_send$text %||% ""); if (!nzchar(txt)) return()
    chat_add(list(role = "user", text = txt))
    cfg <- ai_cfg()
    if (is.null(cfg)) { chat_add(list(role = "assistant", setup = TRUE, actions = list(),
      text = "I am not connected yet. Open **✦ AI assistant** in the top bar (or the AI card in step 1), choose a provider, paste your own key and tick *Use the AI assistant*. Then ask me again - I will see your whole project.")); return() }
    st <- tryCatch(assist_state(), error = function(e) list(step = "?"))
    hist <- lapply(utils::head(rv$chat, -1), function(m) list(role = m$role, text = m$text %||% ""))
    r <- withProgress(message = "The AI assistant is thinking...", value = .3,
      tryCatch(qc_assist_turn(cfg, st, hist, txt, input$chat_access %||% "full"), error = function(e) list(ok = FALSE, reply = paste("The AI service did not answer:", conditionMessage(e)), actions = list())))
    acts <- lapply(r$actions, function(a) { a$status <- if (isTRUE(a$ok)) "pending" else "invalid"; a })
    i <- chat_add(list(role = "assistant", text = r$reply, actions = acts, lookups = r$lookups, mode = input$chat_mode %||% "ask"))
    if (identical(input$chat_mode, "auto")) apply_msg(i) })
  observeEvent(input$chat_act, { e <- input$chat_act; i <- as.integer(e$i); req(i <= length(rv$chat)); m <- rv$chat[[i]]
    if (e$what == "allow_all") apply_msg(i)
    else if (e$what == "allow") apply_msg(i, as.integer(e$j))
    else if (e$what == "deny_all") { for (j in seq_along(m$actions)) if (identical(m$actions[[j]]$status, "pending")) m$actions[[j]]$status <- "denied"; ch <- rv$chat; ch[[i]] <- m; rv$chat <- ch }
    else if (e$what == "deny") { j <- as.integer(e$j); m$actions[[j]]$status <- "denied"; ch <- rv$chat; ch[[i]] <- m; rv$chat <- ch }
    else if (e$what == "undo" && length(m$snaps)) { restore(m$snaps[[1]]); for (j in seq_along(m$actions)) if (identical(m$actions[[j]]$status, "applied")) m$actions[[j]]$status <- "undone"
      m$snaps <- list(); ch <- rv$chat; ch[[i]] <- m; rv$chat <- ch; rv$undo_last <- NULL; showNotification("Changes undone.", type = "message") } })
  observeEvent(input$chat_clear, { rv$chat <- list(); rv$undo_last <- NULL })
  output$chat_status <- renderUI({ cfg <- ai_cfg()
    div(class = "qc-chat-status", if (is.null(cfg)) tagList(span(class = "qc-chip-state off", "off"), " Not connected - ", tags$a(href = "#", onclick = "QC.openAI(); return false;", "connect your AI"))
      else tagList(span(class = "qc-chip-state on", "on"), sprintf(" %s · %s", cfg$label, cfg$model)),
      if (length(rv$chat)) tags$a(href = "#", class = "ms-auto small", onclick = "Shiny.setInputValue('chat_clear', Date.now(), {priority:'event'}); return false;", "New chat")) })
  chat_examples <- reactive({ switch(input$nav %||% "s1",
    s1 = c("Explain my codebook check", "What is in my data?", "Which variables are scales?"),
    s2 = c("Break down all my hypotheses", "Use the codebook's scales for my hypotheses", "Which is the dependent variable in H1?"),
    s3 = c("Explain the exclusion steps", "Why are these cases removed?", "Are my scales built correctly?"),
    s4 = c("Describe my sample in simple words", "What does Cronbach's alpha mean for my scales?"),
    s5 = c("Explain the assumption checks", "Which test fits each hypothesis and why?"),
    s6 = c("Explain my results in simple words", "What does the p-value mean here?", "Help me write the results section")) })
  output$chat_chips <- renderUI(div(class = "qc-chat-chips", lapply(chat_examples(), function(x) tags$button(type = "button", class = "idea", onclick = sprintf("QC.ask(%s)", jsonlite::toJSON(x, auto_unbox = TRUE)), x))))
  output$chat_msgs <- renderUI({ ch <- rv$chat; mode <- input$chat_mode %||% "ask"
    if (!length(ch)) return(div(class = "qc-msg ai", div(class = "qc-msg-who", "✦ AI assistant"),
      div(class = "qc-msg-body", p("Hi! I can see your whole project: data, codebook, exclusions, scales, hypotheses, checks and results."),
        p("Ask me anything - I explain in simple words. Or tell me what to change, and I do it: for example fix a codebook entry, build a scale, choose the variables for a hypothesis, exclude cases or run the tests."),
        p(class = "small text-muted", "Changes need your OK (see the setting above). You can always undo them. Your original data file is never changed."))))
    tagList(lapply(seq_along(ch), function(i) { m <- ch[[i]]
      if (m$role == "user") return(div(class = "qc-msg user", div(class = "qc-msg-body", m$text)))
      acts <- m$actions %||% list(); pend <- which(vapply(acts, function(a) identical(a$status, "pending"), logical(1)))
      ico <- c(pending = "○", applied = "✓", failed = "✗", denied = "–", undone = "↶", invalid = "✗")
      div(class = "qc-msg ai", div(class = "qc-msg-who", "✦ AI assistant", if (length(m$lookups)) span(class = "qc-msg-look", sprintf(" · %s", paste(unique(m$lookups), collapse = ", ")))),
        div(class = "qc-msg-body", HTML(shiny::markdown(m$text %||% ""))),
        if (length(acts)) div(class = "qc-acts", div(class = "qc-acts-h", if (length(pend)) "Proposed changes - your OK is needed:" else "Changes:"),
          lapply(seq_along(acts), function(j) { a <- acts[[j]]
            div(class = paste("qc-act", a$status), span(class = "qc-act-ico", ico[[a$status]]), span(class = "qc-act-t", a$text),
              if (a$status %in% c("failed", "invalid")) span(class = "small text-danger", " - ", a$error %||% ""),
              if (identical(a$status, "pending") && identical(mode, "ask") && length(pend) > 1) span(class = "qc-act-btns",
                tags$button(type = "button", class = "btn btn-success btn-xs", onclick = sprintf("QC.act(%d,'allow',%d)", i, j), "Allow"),
                tags$button(type = "button", class = "btn btn-outline-secondary btn-xs", onclick = sprintf("QC.act(%d,'deny',%d)", i, j), "Deny"))) }),
          if (length(pend)) div(class = "qc-acts-btns", tags$button(type = "button", class = "btn btn-success btn-sm", onclick = sprintf("QC.act(%d,'allow_all',0)", i),
              if (length(pend) == 1) "Allow" else sprintf("Allow all %d", length(pend))),
            tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", onclick = sprintf("QC.act(%d,'deny_all',0)", i), "Deny")),
          if (length(m$snaps) && identical(rv$undo_last, i)) div(class = "qc-acts-btns", tags$button(type = "button", class = "btn btn-outline-secondary btn-sm", onclick = sprintf("QC.act(%d,'undo',0)", i), "↶ Undo these changes")))) })) })

  safe <- function(expr, alt = NULL) tryCatch(expr, error = function(e) alt)
  observeEvent(input$nav, { rv$seen <- union(rv$seen, input$nav) })
  needs_run <- reactive({ length(rv$hyps) > 0 && (is.null(rv$results) || !identical(rv$ran_hyps, rv$hyps)) })
  step_info <- reactive({ has <- !is.null(rv$raw)
    n_rb <- if (has && !is.null(rv$cb)) safe(nrow(rule_breaks()), 0) else 0; rb_open <- n_rb > 0 && !rv$rb_done
    pids <- vapply(rv$parts, `[[`, "", "id"); cids <- vapply(rv$hyps, `[[`, "", "id")
    n_parts <- n_hyp_of(pids); n_conf <- sum(vapply(unique(vapply(pids, hyp_base, "")), function(b) all(pids[vapply(pids, hyp_base, "") == b] %in% cids), logical(1)))
    list(
      s1 = list(done = has, ok = has, state = if (has) "done" else "todo",
        msg = if (!has) "To do: choose your data file on the left." else if (is.null(rv$cb)) "Data loaded. A codebook is optional but recommended (it labels variables and finds survey rules and scales)."
              else "Data and codebook loaded. Tick any \u26a0 codes you want treated as missing, then continue."),
      s2 = list(done = n_parts > 0 && n_conf == n_parts, ok = has, state = if (!has || !n_parts || n_conf < n_parts) "todo" else "done",
        msg = if (!has) "Load your data in step 1 first." else if (!n_parts) "To do: write your hypotheses and press 'Recommend variables'."
              else if (!n_conf) "To do: check the recommended variables for each hypothesis and press 'Confirm'."
              else if (n_conf < n_parts) sprintf("%d of %d confirmed. Unconfirmed hypotheses are not analysed.", n_conf, n_parts) else sprintf("All %d hypotheses confirmed.", n_parts)),
      s3 = list(done = rv$locked, ok = has && !rb_open, state = if (rv$locked) "done" else "todo",
        msg = if (!has) "Load your data in step 1 first." else if (rv$locked) "Confirmed. This dataset is used for every analysis."
              else if (rb_open) sprintf("To do: decide Keep or Exclude for the %d case(s) in B, then press 'Save decisions'.", n_rb)
              else "To do: check the exclusions and scales, then confirm the data."),
      s4 = list(done = !is.null(rv$desc) && is.null(rv$desc$error), ok = rv$locked, state = if (!rv$locked) "locked" else if (!is.null(rv$desc)) "done" else "todo",
        msg = if (!rv$locked) "Confirm the final data in step 3 first." else if (!is.null(rv$desc)) "Sample described and scales checked. Continue to the assumption checks."
              else "Describing your sample and checking your scales..."),
      s5 = list(done = !is.null(rv$results) && !needs_run(), ok = rv$locked && (length(rv$hyps) > 0 || !is.null(rv$results)),
        state = if (!rv$locked) "locked" else if (!is.null(rv$results) && !needs_run()) "done" else "todo",
        msg = if (!rv$locked) "Confirm the final data in step 3 first." else if (!length(rv$hyps)) "Confirm at least one hypothesis in step 2."
              else if (needs_run()) { nh <- n_hyp_of(vapply(rv$hyps, `[[`, "", "id")); sprintf("Assumptions checked for %d hypothes%s. Press Run to test them.", nh, if (nh == 1) "is" else "es") }
              else "All confirmed hypotheses have been run."),
      s6 = list(done = !is.null(rv$results), ok = FALSE, state = if (!is.null(rv$results)) "done" else if (rv$locked) "todo" else "locked",
        msg = if (is.null(rv$results)) "No results yet. Run your hypotheses in step 5." else sprintf("%d result(s). Everything is saved in your results folder.", length(rv$results$items)))) })

  output$qc_footer <- renderUI({ cur <- input$nav %||% "s1"; ks <- names(qc_steps); i <- match(cur, ks); if (is.na(i)) return(NULL)
    si <- step_info()[[cur]]; nxt <- if (i < length(ks)) ks[i + 1] else NA
    next_lab <- if (cur == "s3" && !rv$locked) "Confirm data & continue" else if (cur == "s5" && needs_run()) { nh <- n_hyp_of(vapply(rv$hyps, `[[`, "", "id")); sprintf("Run %d hypothes%s", nh, if (nh == 1) "is" else "es") }
      else if (cur == "s5" && is.null(rv$results)) "Run hypotheses"
      else if (!is.na(nxt)) paste("Continue:", qc_steps[[nxt]]) else NULL
    ico <- c(done = "✓", todo = "●", optional = "○", locked = "\U0001F512")[[si$state]]
    div(class = "qc-footer-in",
      div(class = "qc-f-left", if (i > 1) tags$button(type = "button", class = "btn btn-outline-secondary", onclick = "Shiny.setInputValue('qc_back', Date.now(), {priority:'event'})",
        HTML("&larr;"), paste(" Back:", qc_steps[[ks[i - 1]]]))),
      div(class = paste("qc-f-status", si$state), span(class = "qc-f-ico", ico), span(class = "qc-f-step", sprintf("Step %d of 6", i)), span(si$msg)),
      div(class = "qc-f-right", if (!is.null(next_lab)) tags$button(type = "button", class = paste("btn", if (si$ok) "btn-primary" else "btn-primary disabled"), disabled = if (!si$ok) NA,
        onclick = "Shiny.setInputValue('qc_next', Date.now(), {priority:'event'})", next_lab, HTML(" &rarr;")))) })
  observeEvent(input$qc_back, { ks <- names(qc_steps); i <- match(input$nav %||% "s1", ks); if (!is.na(i) && i > 1) updateNavbarPage(session, "nav", ks[i - 1]) })
  observeEvent(input$qc_next, { ks <- names(qc_steps); cur <- input$nav %||% "s1"; i <- match(cur, ks); if (is.na(i) || i >= length(ks)) return()
    si <- step_info()[[cur]]; if (!si$ok) { showNotification(si$msg, type = "warning"); return() }
    if (cur == "s3" && !rv$locked) { if (!isTRUE(confirm_gate())) return() }
    if (cur == "s5" && needs_run()) { run_all(); return() }
    updateNavbarPage(session, "nav", ks[i + 1]) })
  observe({ si <- step_info()
    session$sendCustomMessage("qc_steps", list(state = lapply(si, `[[`, "state"))) })

  # ---- summary panel: everything chosen so far, always in view
  qs_sec <- function(k, title, state, ...) { cur <- identical(input$nav %||% "s1", k)
    div(class = paste("qs-sec", state, if (cur) "cur"),
      tags$a(href = "#", class = "qs-head", onclick = nav_js(k), span(class = "qs-dot"), span(class = "qs-n", substring(k, 2)), title),
      div(class = "qs-body", ...)) }
  qs_row <- function(label, value) if (!is.null(value) && length(value) && !identical(value, "")) div(class = "qs-row", span(class = "qs-l", label), span(class = "qs-v", title = value, value))
  output$qc_summary <- renderUI({ si <- step_info(); has <- !is.null(rv$raw)
    exn <- if (has) safe(sum(vapply(prep_obj()$exclusions, function(x) isTRUE(x$on), logical(1))), 0) else 0
    g <- if (has) safe(gate()) else NULL; n_final <- if (rv$locked) rv$final$summary$n else safe(g$summary$n)
    ch <- rv$cb_check; tab <- if (!is.null(ch)) table(factor(ch$status, c("ok", "warn", "missing"))) else NULL
    conf_ids <- vapply(rv$hyps, `[[`, "", "id")
    cfg <- ai_cfg()
    tagList(
      qs_sec("s1", "Data & codebook", si$s1$state,
        if (!has) div(class = "qs-empty", "No data file yet") else tagList(
          qs_row("Data", basename(rv$path)), qs_row("Size", sprintf("%d cases \u00b7 %d columns", nrow(rv$raw), ncol(rv$raw))),
          qs_row("Codebook", if (is.null(rv$cb)) "none" else sprintf("%d variables \u00b7 %d rules", length(rv$cb$vars), length(rv$cb$rules))),
          if (!is.null(tab)) qs_row("Check", sprintf("\u2713 %d  \u26a0 %d  \u2717 %d", tab[["ok"]], tab[["warn"]], tab[["missing"]])),
          if (length(rv$accept_na)) qs_row("Missing codes", sprintf("set for %d variable(s)", length(rv$accept_na))))),
      qs_sec("s2", "Hypotheses & variables", si$s2$state,
        if (!has) div(class = "qs-empty", "Waiting for data") else if (!length(rv$parts)) div(class = "qs-empty", "None entered yet")
        else tagList(qs_row("Hypotheses", { pids <- vapply(rv$parts, `[[`, "", "id"); nb <- n_hyp_of(pids)
            nc <- sum(vapply(unique(vapply(pids, hyp_base, "")), function(b) all(pids[vapply(pids, hyp_base, "") == b] %in% conf_ids), logical(1))); sprintf("%d (%d confirmed)", nb, nc) }),
          div(class = "qs-hyps", lapply(rv$parts, function(pt) { ok <- pt$id %in% conf_ids; cl <- pt$confidence_label %||% NA
            span(class = paste("qs-h", if (ok) "ok" else if (isTRUE(cl %in% c("medium", "low"))) "check" else "open"),
              title = if (ok) "confirmed" else "not confirmed yet", pt$id) })),
          qs_row("Scales used", if (length(rv$scales)) paste(vapply(rv$scales, `[[`, "", "name"), collapse = ", ") else "none"))),
      qs_sec("s3", "Prepare data", si$s3$state,
        if (!has) div(class = "qs-empty", "Waiting for data") else tagList(
          qs_row("Exclusion rules", sprintf("%d of %d on", exn, length(rv$excl))),
          qs_row("Rule check", if (!is.null(rv$cb) && safe(nrow(rule_breaks()), 0) > 0) (if (rv$rb_done) "decided" else "open") else "nothing to decide"),
          qs_row("Excluded by ID", if (nrow(rv$manual)) as.character(nrow(rv$manual)) else "none"),
          qs_row("Groups", if (length(rv$groups)) paste(vapply(rv$groups, `[[`, "", "name"), collapse = ", ") else "none"),
          qs_row("Final cases", if (is.null(n_final)) "-" else as.character(n_final)),
          qs_row("Status", if (rv$locked) "\u2713 confirmed" else "not confirmed yet"))),
      qs_sec("s4", "Describe & reliability", si$s4$state,
        if (!rv$locked) div(class = "qs-empty", "Unlocks after step 3") else tagList(
          qs_row("Summary", if (!is.null(rv$desc) && is.null(rv$desc$error)) "\u2713 created" else "running..."),
          qs_row("Reliability", if (is.null(rv$rel)) "running..." else if (!length(rv$rel)) "no multi-item scale" else {
            a <- vapply(rv$rel, function(r) if (isTRUE(r$blocked)) NA_real_ else r$result$alpha, 0); sprintf("%d scale(s), %d below .70", length(a), sum(a < .7, na.rm = TRUE)) }))),
      qs_sec("s5", "Assumption checks", si$s5$state,
        if (!rv$locked) div(class = "qs-empty", "Unlocks after step 3") else if (!length(rv$hyps)) div(class = "qs-empty", "No confirmed hypotheses")
        else tagList(qs_row("Tests", sprintf("%d chosen", length(rv$hyps))), qs_row("Your own choice", { m <- sum(vapply(rv$hyps, function(h) identical(h$mode, "manual"), logical(1))); if (m) as.character(m) else "none" }))),
      qs_sec("s6", "Results", si$s6$state,
        if (is.null(rv$results)) div(class = "qs-empty", "Not run yet") else { it <- Filter(function(x) !isTRUE(x$blocked), rv$results$items)
          tagList(qs_row("Run", sprintf("%d hypothes%s", length(rv$results$items), if (length(rv$results$items) == 1) "is" else "es")),
            qs_row("H0 rejected", sprintf("%d of %d", sum(vapply(it, function(x) isTRUE(x$decision$rejected), logical(1))), length(it))),
            if (needs_run()) div(class = "qs-warn", "Hypotheses changed since the last run.")) }),
      div(class = "qs-sec ai", tags$a(href = "#", class = "qs-head", onclick = "QC.openAI(); return false;", span(class = "qc-ai-ico", "✦"), "AI assistant"),
        div(class = "qs-body", qs_row("Status", if (is.null(cfg)) "not connected" else "on"), if (!is.null(cfg)) qs_row("Model", sprintf("%s", cfg$model)),
          qs_row("Used for", { u <- c(if (any(grepl("described by AI", rv$cb$problems %||% character()))) "codebook", if (any(vapply(rv$hyps, function(h) !is.null(h$ai), logical(1)))) "hypotheses")
            if (length(u)) paste(u, collapse = ", ") else "nothing yet" })))) })
  for (o in c("chat_status", "chat_chips", "chat_msgs", "qc_summary")) outputOptions(output, o, suspendWhenHidden = FALSE)   # drawn even when their panel tab is hidden
}

shinyApp(ui, server)
