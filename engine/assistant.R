# The AI assistant chat: the user asks anything about their analysis; the assistant explains, looks things up in the
# data, and - when the user asks for it - proposes changes ("actions") that the app applies after the user's permission.
# Every change is checked here against the real data first. All statistics are still computed by the app in R.

qc_assist_modes <- c(ask = "Ask before every change", task = "Ask once per task", auto = "Change without asking")
qc_assist_access <- c(full = "Everything, incl. single answers", summaries = "Summaries only (counts, means, results)")

qc_assist_action_types <- c("set_label", "set_codes", "clear_codes", "set_missing", "exclusion_on", "exclusion_off", "add_exclusion",
  "exclude_cases", "create_scale", "update_scale", "delete_scale", "create_group", "add_hypothesis", "edit_hypothesis",
  "remove_hypothesis", "set_hypothesis", "confirm_hypothesis", "confirm_data", "run_analyses", "go_to_step")

qc_clip <- function(x, n) { x <- as.character(x); ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x) }

# ---------------------------------------------------------------- what the assistant sees
# st: a snapshot of the app built by the app (see app.R, assist_state()). Aggregates are always included; single rows only
# through the "rows" lookup, and only when access = "full".
qc_assist_var_line <- function(v, d, cat, check, level_labels) {
  x <- d[[v]]; i <- match(v, cat$variable); lab <- if (!is.na(i)) cat$label[i] else v; kind <- if (!is.na(i)) cat$kind[i] else ""
  lv <- level_labels[[v]]; ck <- if (!is.null(check)) check[check$variable == v, , drop = FALSE] else NULL
  stats <- if (is.null(x)) "" else {
    n <- sum(!is.na(x)); u <- unique(x[!is.na(x)])
    if (is.numeric(x) && length(u) > 10) sprintf("n=%d, mean=%s, sd=%s, min=%s, max=%s", n, qc_fmt(mean(x, na.rm = TRUE)), qc_fmt(stats::sd(x, na.rm = TRUE)), qc_fmt(min(x, na.rm = TRUE)), qc_fmt(max(x, na.rm = TRUE)))
    else if (length(u) <= 12) { t <- table(x, useNA = "no"); sprintf("n=%d, counts: %s", n, paste(names(t), t, sep = ":", collapse = ", ")) }
    else sprintf("n=%d, %d different answers", n, length(u)) }
  paste0(v, " | ", qc_clip(lab, 90), " | ", kind,
    if (length(lv)) paste0(" | codes: ", qc_clip(paste(names(lv), "=", lv, collapse = "; "), 160)) else "",
    if (!is.null(ck) && nrow(ck)) paste0(" | codebook check: ", ck$status[1], if (nzchar(ck$message[1] %||% "")) paste0(" - ", qc_clip(ck$message[1], 140)) else "") else "",
    " | ", stats)
}

qc_assist_context <- function(st, access = "full", max_vars = 260) {
  sec <- function(title, lines) if (length(lines)) c(paste0("## ", title), lines, "") else character()
  d <- st$data
  vars <- if (!is.null(d)) utils::head(unique(c(st$cat$variable, names(d))), max_vars) else character()
  vars <- vars[vars %in% names(d)]
  hy <- unlist(lapply(st$hyps, function(h) c(sprintf("%s: %s", h$id, h$text),
    sprintf("   type: %s | variables: %s | %s%s", h$goal, h$roles, if (isTRUE(h$confirmed)) "confirmed" else "not confirmed yet", if (nzchar(h$expected %||% "")) paste0(" | ", h$expected) else ""),
    if (nzchar(h$user_h0 %||% "")) sprintf("   user's own H0: %s", h$user_h0))))
  c("# CURRENT STATE OF THE USER'S QUANTCOMPASS PROJECT",
    sprintf("Page the user is on: %s.", st$step %||% "?"),
    sprintf("AI data access chosen by the user: %s.", qc_assist_access[[access]]),
    "",
    sec("Data", c(if (!is.null(st$file)) sprintf("File: %s, %d cases, %d columns (before exclusions).", st$file, st$n_raw %||% NA, st$n_cols %||% NA),
                  if (!is.null(st$codebook)) sprintf("Codebook: %s.", st$codebook) else "No codebook loaded.", st$cb_problems)),
    sec("Variables (name | label | kind | codes | codebook check | summary of the answers after exclusions)",
        if (length(vars)) vapply(vars, qc_assist_var_line, "", d = d, cat = st$cat, check = st$check, level_labels = st$level_labels %||% list())),
    sec("Exclusions (applied in this order)", c(vapply(st$exclusions %||% list(), function(x) sprintf("[%s] %s: %s%s", x$id, if (isTRUE(x$on)) "ON" else "off", x$label,
          if (!is.null(x$documented) && !is.na(x$documented)) sprintf(" (the codebook removes %s here)", x$documented) else ""), ""),
        st$flow, if (!is.null(st$manual) && nrow(st$manual)) sprintf("Excluded by ID: %s", paste(sprintf("%s (%s)", st$manual$id, st$manual$reason), collapse = "; ")))),
    sec("Scales (score = mean/sum of items; reversed items are recoded first)", vapply(st$scales %||% list(), function(s) sprintf("%s (%s): %s of %s%s%s%s", s$name, s$label %||% "", s$method %||% "mean",
          paste(s$items, collapse = ", "), if (length(s$reverse)) paste0("; reversed: ", paste(s$reverse, collapse = ", ")) else "",
          if (!is.null(s$alpha) && !is.na(s$alpha)) sprintf("; Cronbach's alpha = %s", qc_fmt_r(s$alpha)) else "", if (identical(s$source, "codebook")) " [defined in the codebook]" else if (isTRUE(s$used)) "" else " [found, not used yet]"), "")),
    sec("Groups / recodes", vapply(st$groups %||% list(), function(g) sprintf("%s: %s", g$name, paste(vapply(g$groups, function(r) sprintf("'%s' if %s", r$label, if (is.character(r$cond)) r$cond else qc_cond_code(r$cond)), ""), collapse = "; ")), "")),
    sec(sprintf("Hypotheses (%d, as the user wrote them)", st$n_hyp %||% length(st$hyps)), hy),
    sec("Final dataset", st$final),
    sec("Assumption checks", st$assumptions),
    sec("Results", st$results))
}

# ---------------------------------------------------------------- lookups the assistant may request
qc_assist_lookup <- function(lk, st, access = "full") {
  d <- st$data; tp <- lk$type %||% ""
  vv <- function(x) { x <- trimws(as.character(unlist(x))); x[x %in% names(d)] }
  out <- tryCatch(switch(tp,
    values = { v <- vv(lk$variable)[1]; if (is.na(v)) stop("unknown variable"); t <- table(d[[v]], useNA = "ifany"); sprintf("Answers of %s: %s", v, paste(names(t), t, sep = " -> ", collapse = "; ")) },
    describe = { vs <- utils::head(vv(lk$variables %||% lk$variable), 20); if (!length(vs)) stop("unknown variables")
      paste(vapply(vs, function(v) { x <- suppressWarnings(as.numeric(d[[v]])); sprintf("%s: n=%d mean=%s sd=%s min=%s median=%s max=%s", v, sum(!is.na(x)), qc_fmt(mean(x, na.rm = TRUE)), qc_fmt(stats::sd(x, na.rm = TRUE)),
        qc_fmt(min(x, na.rm = TRUE)), qc_fmt(stats::median(x, na.rm = TRUE)), qc_fmt(max(x, na.rm = TRUE))) }, ""), collapse = "\n") },
    crosstab = { a <- vv(lk$var1)[1]; b <- vv(lk$var2)[1]; if (is.na(a) || is.na(b)) stop("unknown variables"); paste(utils::capture.output(print(table(d[[a]], d[[b]], dnn = c(a, b), useNA = "ifany"))), collapse = "\n") },
    correlate = { vs <- utils::head(vv(lk$variables), 10); if (length(vs) < 2) stop("name two or more variables")
      m <- stats::cor(sapply(d[, vs, drop = FALSE], function(x) suppressWarnings(as.numeric(x))), use = "pairwise.complete.obs"); paste(utils::capture.output(print(round(m, 2))), collapse = "\n") },
    rows = { if (!identical(access, "full")) return("Not allowed: the user lets the assistant see summaries only, not single answers.")
      vs <- utils::head(vv(lk$variables), 12); idv <- st$id_var %||% names(d)[1]; vs <- unique(c(idv, vs))
      sel <- if (length(lk$ids)) as.character(d[[idv]]) %in% as.character(unlist(lk$ids)) else if (nzchar(lk$condition %||% "")) {
          cc <- qc_parse_condition_text(lk$condition, d); if (!is.null(cc$error)) stop(cc$error); eval(parse(text = qc_cond_code(cc)), list(data = d)) %in% TRUE } else rep(TRUE, nrow(d))
      r <- utils::head(d[sel, vs, drop = FALSE], 25); sprintf("%d case(s) match; first %d:\n%s", sum(sel), nrow(r), paste(utils::capture.output(print(r, row.names = FALSE)), collapse = "\n")) },
    codebook = { term <- lk$term %||% ""; L <- st$codebook_lines %||% character(); if (!nzchar(term)) stop("give a search term")
      hit <- grep(term, L, ignore.case = TRUE, fixed = FALSE); if (!length(hit)) return(sprintf("The codebook does not mention '%s'.", term))
      idx <- sort(unique(pmin(length(L), pmax(1, c(hit - 1, hit, hit + 1))))); paste(trimws(utils::head(L[idx], 30)), collapse = "\n") },
    stop("unknown lookup")), error = function(e) sprintf("Lookup failed: %s", conditionMessage(e)))
  paste0("[", tp, "] ", out)
}

# ---------------------------------------------------------------- checking a proposed change
# Returns list(ok, text = what will change in plain words, error).
qc_assist_check_action <- function(a, st) {
  d <- st$data; tp <- a$type %||% ""; known <- unique(c(names(d), st$cat$variable))
  fail <- function(msg) list(ok = FALSE, text = sprintf("%s: %s", tp, msg), error = msg)
  okv <- function(v) !is.null(v) && length(v) == 1 && !is.na(v) && v %in% known
  L <- function(v) { i <- match(v, st$cat$variable); if (is.na(i)) v else sprintf("%s (%s)", v, qc_clip(st$cat$label[i], 50)) }
  hyp_ids <- vapply(st$hyps %||% list(), `[[`, "", "id")
  if (!tp %in% qc_assist_action_types) return(fail("this kind of change is not available"))
  switch(tp,
    set_label = if (!okv(a$variable) || !nzchar(a$label %||% "")) fail("name a column and a label") else list(ok = TRUE, text = sprintf("Name %s '%s'", a$variable, a$label)),
    set_codes = { if (!okv(a$variable)) return(fail("unknown column")); cd <- unlist(a$codes); if (!length(cd) || is.null(names(cd))) return(fail("give codes as {\"1\": \"label\"}"))
      x <- d[[a$variable]]; obs <- unique(x[!is.na(x)]); miss <- setdiff(as.character(obs), names(cd))
      list(ok = TRUE, text = sprintf("Set the answer codes of %s: %s%s", L(a$variable), paste(names(cd), "=", cd, collapse = "; "),
        if (length(miss)) sprintf(" (note: the data also has %s)", paste(utils::head(miss, 8), collapse = ", ")) else "")) },
    clear_codes = if (!okv(a$variable)) fail("unknown column") else list(ok = TRUE, text = sprintf("Remove the answer codes of %s - its values are plain numbers", L(a$variable))),
    set_missing = { if (!okv(a$variable)) return(fail("unknown column")); cd <- as.character(unlist(a$codes)); if (!length(cd)) return(fail("give the codes"))
      list(ok = TRUE, text = sprintf("Treat %s in %s as missing (not an answer)", paste(cd, collapse = ", "), L(a$variable))) },
    exclusion_on = , exclusion_off = { ex <- Filter(function(x) identical(x$id, a$id) || identical(tolower(x$label), tolower(a$id %||% "")), st$exclusions %||% list())
      if (!length(ex)) return(fail(sprintf("no exclusion step '%s'", a$id %||% ""))); list(ok = TRUE, text = sprintf("Switch %s the exclusion step '%s'", if (tp == "exclusion_on") "on" else "off", ex[[1]]$label), id = ex[[1]]$id) },
    add_exclusion = { cc <- qc_parse_condition_text(a$condition %||% "", d); if (!is.null(cc$error)) return(fail(cc$error))
      n <- sum(eval(parse(text = qc_cond_code(cc)), list(data = d)) %in% TRUE)
      list(ok = TRUE, text = sprintf("Add an exclusion step '%s': remove cases where %s (%d case(s) now)", a$label %||% a$condition, a$condition, n)) },
    exclude_cases = { ids <- as.character(unlist(a$ids)); idv <- st$id_var %||% names(d)[1]; bad <- setdiff(ids, as.character(st$raw_ids %||% d[[idv]]))
      if (!length(ids)) return(fail("give the case IDs")); if (length(bad)) return(fail(sprintf("these IDs are not in the data: %s", paste(bad, collapse = ", "))))
      if (!nzchar(a$reason %||% "")) return(fail("give a reason (it goes into the methods section)"))
      list(ok = TRUE, text = sprintf("Exclude %s %s - %s", idv, paste(ids, collapse = ", "), a$reason)) },
    create_scale = , update_scale = { it <- as.character(unlist(a$items)); if (length(it) < 2) return(fail("a scale needs at least two items"))
      bad <- it[!it %in% names(d)]; if (length(bad)) return(fail(sprintf("unknown items: %s", paste(bad, collapse = ", "))))
      if (!all(vapply(it, function(v) is.numeric(d[[v]]), logical(1)))) return(fail("all items must hold numbers"))
      rv <- intersect(as.character(unlist(a$reverse)), it); m <- a$method %||% "mean"; if (!m %in% c("mean", "sum", "who5")) return(fail("method must be mean, sum or who5"))
      if (m == "who5" && length(it) != 5) return(fail("the WHO-5 index needs exactly 5 items"))
      nm <- a$name %||% ""; if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", nm)) return(fail("the scale needs a column name like WB_MEAN"))
      if (tp == "create_scale" && nm %in% names(st$raw_names %||% character())) return(fail("this name is already a column of the data"))
      list(ok = TRUE, text = sprintf("%s scale %s%s: %s of %s%s", if (tp == "create_scale") "Build" else "Change", nm, if (nzchar(a$label %||% "")) paste0(" (", a$label, ")") else "",
        c(mean = "mean", sum = "sum", who5 = "WHO-5 index (raw sum x 4)")[[m]], paste(it, collapse = ", "), if (length(rv)) paste0("; reversed: ", paste(rv, collapse = ", ")) else "")) },
    delete_scale = if (!(a$name %||% "") %in% vapply(st$scales %||% list(), `[[`, "", "name")) fail("no such scale") else list(ok = TRUE, text = sprintf("Remove the scale %s", a$name)),
    create_group = { g <- a$groups; if (!length(g)) return(fail("give the groups with a condition each"))
      for (r in g) { cc <- qc_parse_condition_text(r$condition %||% "", d); if (!is.null(cc$error)) return(fail(cc$error)) }
      if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", a$name %||% "")) return(fail("the group variable needs a column name"))
      list(ok = TRUE, text = sprintf("Build the group variable %s: %s", a$name, paste(vapply(g, function(r) sprintf("'%s' if %s", r$label, r$condition), ""), collapse = "; "))) },
    add_hypothesis = { if (!nzchar(a$text %||% "")) return(fail("give the hypothesis text")); id <- a$id %||% ""
      if (nzchar(id) && id %in% hyp_ids) return(fail(sprintf("%s already exists", id))); list(ok = TRUE, text = sprintf("Add hypothesis %s: %s", if (nzchar(id)) id else "(next number)", a$text)) },
    edit_hypothesis = if (!(a$id %||% "") %in% hyp_ids) fail("no such hypothesis") else if (!nzchar(a$text %||% "")) fail("give the new text") else list(ok = TRUE, text = sprintf("Rewrite %s: %s", a$id, a$text)),
    remove_hypothesis = if (!(a$id %||% "") %in% hyp_ids) fail("no such hypothesis") else list(ok = TRUE, text = sprintf("Remove hypothesis %s", a$id)),
    set_hypothesis = { if (!(a$id %||% "") %in% c(hyp_ids, st$part_ids)) return(fail("no such hypothesis"))
      v <- qc_ai_validate_spec(a, d, st$cat); if (!isTRUE(v$ok)) return(fail(paste(v$problems, collapse = " ")))
      r <- unlist(v$spec[intersect(names(v$spec), c("var1", "var2", "outcome", "group", "mediator", "moderator", "predictors", "controls"))])
      list(ok = TRUE, spec = v$spec, text = sprintf("Set up %s as %s with %s", a$id, v$spec$goal, paste(sprintf("%s = %s", names(r), r), collapse = ", "))) },
    confirm_hypothesis = if (!(a$id %||% "") %in% c(hyp_ids, st$part_ids)) fail("no such hypothesis") else list(ok = TRUE, text = sprintf("Confirm %s", a$id)),
    confirm_data = list(ok = TRUE, text = "Confirm the final dataset (the analysis steps unlock)"),
    run_analyses = list(ok = TRUE, text = "Run the confirmed hypotheses"),
    go_to_step = { s <- suppressWarnings(as.integer(a$step)); if (is.na(s) || s < 1 || s > 6) return(fail("step must be 1-6")); list(ok = TRUE, text = sprintf("Go to step %d", s)) })
}

# ---------------------------------------------------------------- one turn of the conversation
qc_assist_system <- function() paste(
  "You are the QuantCompass assistant, built into a statistics app that guides students and beginners through quantitative",
  "survey research: data and codebook -> hypotheses and the variables that measure them -> data preparation -> description and",
  "Cronbach's alpha -> assumption checks -> hypothesis tests and reports. You see the current state of the user's project below.",
  "\nHOW TO ANSWER: plain, friendly language for someone who knows no statistics; short paragraphs or a short list; explain every",
  "technical term the first time; refer to variables by their label and name, e.g. 'Well-being (WB_MEAN)'. Answer in the language of the user.",
  "Never invent numbers: use the numbers in the state, or request a lookup. All statistics are computed by the app in R, not by you.",
  "\nCHANGES: propose actions ONLY when the user asks you to change, fix, set up, add, remove, build, confirm, run or do something.",
  "When you notice a problem the user did not ask about, explain it and ask whether you should fix it - without actions.",
  "Never add, split, merge or remove hypotheses unless the user explicitly asks for that; keep exactly the hypotheses the user wrote.",
  "The user's original file is never changed; changes apply to the app's working copy and are written into the preparation script.",
  "\nLOOKUPS (read-only, use them when you need numbers that are not in the state): {\"type\":\"values\",\"variable\":\"X\"},",
  "{\"type\":\"describe\",\"variables\":[\"X\",\"Y\"]}, {\"type\":\"crosstab\",\"var1\":\"X\",\"var2\":\"Y\"}, {\"type\":\"correlate\",\"variables\":[...]},",
  "{\"type\":\"rows\",\"ids\":[...] or \"condition\":\"DE01 > 40\",\"variables\":[...]} (single answers; only if access allows),",
  "{\"type\":\"codebook\",\"term\":\"straight\"}.",
  "\nACTIONS: {\"type\":\"set_label\",\"variable\",\"label\"}, {\"type\":\"set_codes\",\"variable\",\"codes\":{\"1\":\"female\"}}, {\"type\":\"clear_codes\",\"variable\"},",
  "{\"type\":\"set_missing\",\"variable\",\"codes\":[-9]}, {\"type\":\"exclusion_on\"|\"exclusion_off\",\"id\"}, {\"type\":\"add_exclusion\",\"label\",\"condition\":\"TIME_RSI >= 2\"}",
  "(condition = the cases to REMOVE; syntax: VAR is 1,2 | VAR is not 3 | VAR < 5 | VAR >= 2 | VAR is missing, joined with 'and'),",
  "{\"type\":\"exclude_cases\",\"ids\":[..],\"reason\"}, {\"type\":\"create_scale\"|\"update_scale\",\"name\":\"WB_MEAN\",\"label\",\"items\":[..],\"reverse\":[..],\"method\":\"mean\"|\"sum\"|\"who5\"},",
  "{\"type\":\"delete_scale\",\"name\"}, {\"type\":\"create_group\",\"name\",\"label\",\"groups\":[{\"label\":\"employed\",\"condition\":\"DE06 is 2\"}]},",
  "{\"type\":\"add_hypothesis\",\"id\",\"text\"}, {\"type\":\"edit_hypothesis\",\"id\",\"text\"}, {\"type\":\"remove_hypothesis\",\"id\"},",
  "{\"type\":\"set_hypothesis\",\"id\",\"goal\":\"relationship|difference|paired_difference|association|prediction|mediation|moderation\",",
  "\"var1\",\"var2\",\"outcome\",\"group\",\"mediator\",\"moderator\",\"predictors\":[..],\"controls\":[..],\"direction\":\"positive|negative|none\",\"higher_group\",\"stronger\"},",
  "{\"type\":\"confirm_hypothesis\",\"id\"}, {\"type\":\"confirm_data\"}, {\"type\":\"run_analyses\"}, {\"type\":\"go_to_step\",\"step\":1-6}.",
  "Use scale columns (e.g. FC_MEAN) for constructs measured by several items, never a single item.",
  "\nAnswer with ONE JSON object only: {\"reply\": \"your answer in Markdown\", \"lookups\": [], \"actions\": []}.",
  "If you request lookups, leave actions empty; you will get the lookup results and answer again.")

qc_assist_turn <- function(cfg, st, history, message, access = "full", max_rounds = 3) {
  ctx <- paste(qc_assist_context(st, access), collapse = "\n")
  hist <- utils::tail(history, 10)
  conv <- paste(vapply(hist, function(m) sprintf("%s: %s", if (m$role == "user") "USER" else "ASSISTANT", qc_clip(m$text, 1500)), ""), collapse = "\n")
  looked <- character(); notes <- character()
  for (round in seq_len(max_rounds)) {
    user <- paste0(ctx, "\n\n# CONVERSATION SO FAR\n", if (nzchar(conv)) conv else "(none)",
      if (length(looked)) paste0("\n\n# LOOKUP RESULTS\n", paste(looked, collapse = "\n\n")) else "",
      "\n\n# USER'S NEW MESSAGE\n", message)
    a <- qc_ai_chat(cfg, qc_assist_system(), user)
    lk <- a$lookups %||% list(); if (is.list(lk) && !is.null(lk$type)) lk <- list(lk)
    if (length(lk) && round < max_rounds) { for (l in utils::head(lk, 6)) { looked <- c(looked, qc_assist_lookup(l, st, access)); notes <- c(notes, sprintf("looked up %s", l$type)) }; next }
    acts <- a$actions %||% list(); if (is.list(acts) && !is.null(acts$type)) acts <- list(acts)
    checked <- lapply(acts, function(x) { r <- tryCatch(qc_assist_check_action(x, st), error = function(e) list(ok = FALSE, text = x$type %||% "change", error = conditionMessage(e)))
      c(list(action = x), r) })
    reply <- a$reply %||% ""; if (!is.character(reply)) reply <- paste(unlist(reply), collapse = "\n")
    return(list(ok = TRUE, reply = reply, actions = checked, lookups = notes))
  }
  list(ok = TRUE, reply = "I could not finish looking this up. Please ask a more specific question.", actions = list(), lookups = notes)
}
