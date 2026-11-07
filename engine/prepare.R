# Data preparation: exclusions, rule-break check, scales, groups, final data gate.
# Every decision becomes a line of R code, so the prepared data can be rebuilt from the raw file.
#
# prep (a list) holds the user's decisions:
#   codebook_code : lines from qc_codebook_apply()
#   exclusions    : list of steps, each list(id, label, cond, on, reason)  (cond: TRUE = remove)
#   manual        : data.frame(id, reason)  - rule-break exclusions and manual exclusions
#   scales        : list of list(name, label, items, reverse, method, min, max)
#   groups        : list of list(name, label, groups = list(list(label, cond)))
#   id_var        : the case ID column

qc_id_column <- function(data) {
  for (v in c("CASE", "ResponseId", "id", "ID", "Id", "case", "respondent", "participant"))
    if (v %in% names(data) && !anyDuplicated(stats::na.omit(data[[v]]))) return(v)
  for (v in names(data)) if (is.numeric(data[[v]]) && !anyDuplicated(data[[v]]) && !anyNA(data[[v]])) return(v)
  NA_character_
}

qc_q <- function(v) sprintf('data[["%s"]]', v)
qc_vec <- function(x) if (is.numeric(x)) paste0("c(", paste(x, collapse = ", "), ")") else paste0("c(", paste0('"', x, '"', collapse = ", "), ")")

# Attention checks: an item whose text asks for one specific answer ("please select 'Agree'").
# The required answer must match one of the item's answer labels in the codebook, otherwise it is
# reported and not used.
qc_attention_items <- function(cb) {
  out <- list()
  for (e in cb$vars) {
    lab <- e$label; if (is.na(lab)) next
    if (!grepl("(?i)(attention|reading (the|these) questions carefully|to show (that )?you|demonstrate that you|please (select|choose|tick|mark|click|answer)|select specifically|quality check)", lab, perl = TRUE)) next
    # quoted answer: try each quote style separately, so 'Agree' inside "...select 'Agree'..." is found
    q <- unlist(lapply(c("\u2018([^\u2019]{1,40})\u2019", "'([^']{1,40})'", "\u201c([^\u201d]{1,40})\u201d", "\"([^\"]{1,40})\""),
      function(pt) { m <- regmatches(lab, gregexpr(pt, lab, perl = TRUE))[[1]]; substr(m, 2, nchar(m) - 1) }))
    hit <- names(e$values)[tolower(trimws(e$values)) %in% tolower(trimws(q))]
    out[[e$variable]] <- list(variable = e$variable, label = lab, required = q,
                              code = if (length(hit) == 1) as.numeric(hit) else NA, answer = if (length(hit) == 1) e$values[[hit]] else NA)
  }
  out
}

# Suggested exclusion steps, in the order they are applied. Each is a toggle for the user.
qc_exclusion_suggestions <- function(data, cb) {
  steps <- list()
  add <- function(id, label, cond, on, reason) steps[[length(steps) + 1]] <<- list(id = id, label = label, cond = cond, on = on, reason = reason)
  if ("FINISHED" %in% names(data)) add("unfinished", "unfinished questionnaires", '!(data[["FINISHED"]] %in% 1)', TRUE,
      "FINISHED = 0: the respondent left before the last page (SoSci Survey).")
  else if ("Finished" %in% names(data)) add("unfinished", "unfinished questionnaires", '!(data[["Finished"]] %in% c(1, TRUE, "1", "True", "TRUE"))', TRUE,
      "Finished = 0: the respondent did not complete the survey (Qualtrics).")
  else if ("submitdate" %in% names(data)) add("unfinished", "unfinished questionnaires", 'is.na(data[["submitdate"]]) | data[["submitdate"]] == ""', TRUE,
      "No submission date: the respondent did not submit the survey (LimeSurvey).")
  for (r in cb$rules) if (isTRUE(r$screen_out) && isTRUE(r$readable)) {
    vs <- regmatches(r$condition, gregexpr('data\\[\\["([^"]+)"\\]\\]', r$condition))[[1]]; vs <- unique(gsub('data\\[\\["|"\\]\\]', "", vs))
    what <- paste(vapply(vs, function(v) { e <- cb$vars[[v]]; if (is.null(e) || is.na(e$label)) v else sprintf("%s (%s)", v, e$label) }, ""), collapse = ", ")
    nice <- r$condition_php; for (v in vs) { e <- cb$vars[[v]]
      nice <- gsub(sprintf("value\\('%s'\\)\\s*==\\s*(-?[0-9]+)", v), sprintf("%s = \\1", v), nice) }
    codes <- sub("^=\\s*", "", regmatches(nice, gregexpr("=\\s*-?[0-9]+", nice))[[1]])
    vlab <- if (length(vs) == 1 && length(codes) == 1 && !is.null(cb$vars[[vs]]$values[codes]) && !is.na(cb$vars[[vs]]$values[codes])) sprintf(" '%s'", cb$vars[[vs]]$values[[codes]]) else ""
    add(paste0("screen_", vs[1]), sprintf("screened out by the survey (%s%s)", gsub("[()]", "", trimws(nice)), vlab), r$condition, TRUE,
        sprintf("The survey sends respondents with %s%s to the end (filter on page %s, %s). They were not eligible.", gsub("[()]", "", trimws(nice)), vlab, r$page, what))
  }
  for (a in qc_attention_items(cb)) if (!is.na(a$code))
    add(paste0("attention_", a$variable), sprintf("failed the attention check (%s not '%s')", a$variable, a$answer), sprintf('!(data[["%s"]] %%in%% %s)', a$variable, a$code), TRUE,
        sprintf("%s asks respondents to choose '%s'. Any other answer, or no answer, fails the check.", a$variable, a$answer))
  if ("TIME_RSI" %in% names(data)) add("speeders", "speeders (relative speed index 2 or higher)", 'data[["TIME_RSI"]] >= 2', FALSE,
      "TIME_RSI of 2 or more means the respondent was at least twice as fast as typical (Leiner, 2019). Optional - off by default; switch it on if your codebook or method says so.")
  steps
}

qc_answered <- function(x, e = NULL) !is.na(x) & !(if (is.null(e)) FALSE else x %in% suppressWarnings(as.numeric(e$missing)))

qc_value_text <- function(cb, v, x) {
  e <- cb$vars[[v]]; lab <- if (!is.null(e) && as.character(x) %in% names(e$values)) e$values[[as.character(x)]] else NULL
  if (is.null(lab)) sprintf("%s = %s", v, x) else sprintf("%s = %s (%s)", v, x, lab)
}

# Cases that break the survey's own rules: skip logic (answers on pages the survey skipped for them)
# and answers that are not valid codes in the codebook. Nothing else is flagged.
qc_rule_breaks <- function(data, cb, id_var) {
  out <- list()
  for (r in cb$rules) {
    if (isTRUE(r$screen_out) || !isTRUE(r$readable) || !length(r$vars_skipped)) next
    hit <- tryCatch(eval(parse(text = r$condition), list(data = data)) %in% TRUE, error = function(e) rep(FALSE, nrow(data)))
    sk <- intersect(r$vars_skipped, names(data))
    for (i in which(hit)) {
      ans <- sk[vapply(sk, function(v) qc_answered(data[[v]][i], cb$vars[[v]]), logical(1))]
      if (!length(ans)) next
      vs <- unique(gsub('data\\[\\["|"\\]\\]', "", regmatches(r$condition, gregexpr('data\\[\\["([^"]+)"\\]\\]', r$condition))[[1]]))
      why <- sprintf("%s. The survey skips %s for this answer (filter on page %s), but this case has answers there: %s.",
                     paste(vapply(vs, function(v) qc_value_text(cb, v, data[[v]][i]), ""), collapse = ", "),
                     paste(utils::head(r$vars_skipped, 3), collapse = ", "), r$page,
                     paste(vapply(utils::head(ans, 4), function(v) sprintf("%s = %s", v, data[[v]][i]), ""), collapse = ", "))
      out[[length(out) + 1]] <- data.frame(id = data[[id_var]][i], type = "skip logic", why = why)
    }
  }
  for (v in intersect(names(cb$vars), names(data))) {
    e <- cb$vars[[v]]; x <- data[[v]]
    if (!e$kind %in% c("codes", "range") || !is.numeric(x)) next
    codes <- suppressWarnings(as.numeric(names(e$values))); mc <- suppressWarnings(as.numeric(e$missing))
    allowed <- if (e$kind == "range") c(seq(min(codes), max(codes)), mc) else c(codes, mc)
    for (i in which(!is.na(x) & !x %in% allowed))
      out[[length(out) + 1]] <- data.frame(id = data[[id_var]][i], type = "invalid code",
        why = sprintf("%s = %s is not a code in the codebook (valid: %s).", v, x[i], if (e$kind == "range") paste(min(codes), "to", max(codes)) else paste(codes, collapse = ", ")))
  }
  if (!length(out)) return(data.frame(id = character(), type = character(), why = character()))
  do.call(rbind, out)
}

# Optional hints (kept by default): identical answers across every item of a battery
qc_pattern_hints <- function(data, cb, id_var) {
  items <- names(data)[grepl("_[0-9]+$", names(data))]
  att <- names(qc_attention_items(cb))
  bat <- split(setdiff(items, att), sub("_[0-9]+$", "", setdiff(items, att)))
  out <- list()
  for (b in names(bat)) { vs <- bat[[b]]; if (length(vs) < 4) next
    m <- as.matrix(data[, vs]); full <- stats::complete.cases(m)
    same <- full & apply(m, 1, function(r) length(unique(r)) == 1)
    if (any(same)) out[[b]] <- list(battery = b, items = vs, ids = data[[id_var]][same],
      text = sprintf("%d cases gave the same answer to all %d %s items. This can be genuine; they are kept unless you exclude them.", sum(same), length(vs), b)) }
  out
}

# ---------------------------------------------------------------- scales
qc_scale_suggestions <- function(cb, data) {
  att <- names(qc_attention_items(cb))
  vars <- Filter(function(e) e$kind %in% c("codes", "range") && e$variable %in% names(data) && !e$variable %in% att, cb$vars)
  key <- function(e) paste(names(e$values), collapse = ",")
  stem <- vapply(vars, function(e) sub("_[0-9]+$", "", e$variable), "")
  out <- list()
  for (s in unique(stem[duplicated(stem)])) {
    vs <- names(stem)[stem == s]; if (length(unique(vapply(vars[vs], key, ""))) > 1) next
    out[[s]] <- list(name = s, items = vs)
  }
  # single items on one page that share the same answer scale (e.g. a set of 1-10 ratings)
  singles <- names(stem)[!stem %in% names(out)]
  singles <- singles[vapply(vars[singles], function(e) !is.na(e$page), logical(1))]   # only when the codebook gives pages
  pg <- vapply(vars[singles], function(e) paste(e$page, key(e), e$kind), "")
  for (k in unique(pg[duplicated(pg)])) { vs <- names(pg)[pg == k]
    if (length(vs) >= 3 && all(vapply(vars[vs], function(e) e$kind == "range" || length(e$values) >= 4, logical(1))))
      out[[paste(vs[1], "set")]] <- list(name = paste0(sub("[0-9]+$", "", sub("_[0-9]+$", "", vs[1])), "_scale"), items = vs) }
  for (i in seq_along(out)) {
    e <- vars[[out[[i]]$items[1]]]; codes <- as.numeric(names(e$values))
    out[[i]]$min <- min(codes); out[[i]]$max <- max(codes)
    out[[i]]$anchors <- vapply(out[[i]]$items, function(v) { ee <- vars[[v]]; k <- names(ee$values)
      sprintf("%s: %s = %s ... %s = %s", v, k[1], ee$values[1], k[length(k)], ee$values[length(k)]) }, "")
    labs <- tolower(paste(vapply(vars[out[[i]]$items], `[[`, "", "label"), collapse = " "))
    who5 <- length(out[[i]]$items) == 5 && grepl("cheerful", labs) && grepl("calm", labs) && grepl("active|vigorous", labs)
    out[[i]]$preset <- if (who5) "who5" else NA
    out[[i]]$method <- if (who5) "who5" else "mean"
    if (who5) out[[i]]$name <- "WHO5_index"
  }
  unname(out)
}

qc_scale_code <- function(s) {
  items <- s$items; rev <- intersect(s$reverse, items)
  lines <- sprintf("# Scale %s%s: %s", s$name, if (!is.null(s$label) && nzchar(s$label)) paste0(" (", s$label, ")") else "", paste(items, collapse = ", "))
  src <- items
  if (length(rev)) { lines <- c(lines, sprintf('for (v in %s) data[[paste0(v, "_r")]] <- %s - data[[v]]   # reversed: %s + %s - answer', qc_vec(rev), s$min + s$max, s$min, s$max))
    src <- ifelse(items %in% rev, paste0(items, "_r"), items) }
  m <- sprintf("data[, %s, drop = FALSE]", qc_vec(src))
  lines <- c(lines, switch(s$method,
    sum = sprintf('data[["%s"]] <- rowSums(%s)   # sum score (needs every item answered)', s$name, m),
    who5 = sprintf('data[["%s"]] <- (rowSums(%s) - %d * %s) * 4   # WHO-5 index 0-100 (Topp et al., 2015)', s$name, m, length(items), s$min),
    sprintf('data[["%s"]] <- rowMeans(%s)   # mean score (needs every item answered)', s$name, m)))
  lines
}

qc_scale_alpha <- function(data, s) {
  src <- ifelse(s$items %in% s$reverse, paste0(s$items, "_r"), s$items)
  m <- stats::na.omit(as.matrix(data[, src, drop = FALSE]))
  k <- ncol(m); if (k < 2 || nrow(m) < 3) return(list(alpha = NA, n = nrow(m), k = k))
  list(alpha = qc_alpha(m), n = nrow(m), k = k)
}

# ---------------------------------------------------------------- groups / recodes
# A group variable is built from rules: each rule = one category label + a condition.
# Conditions are written as list(var, op, values) and turned into R code.
qc_cond_code <- function(cond) {
  parts <- vapply(cond, function(c) {
    v <- qc_q(c$var); vals <- c$values; num <- suppressWarnings(as.numeric(vals)); if (!anyNA(num)) vals <- num
    switch(c$op, "is" = sprintf("%s %%in%% %s", v, qc_vec(vals)), "is not" = sprintf("!(%s %%in%% %s)", v, qc_vec(vals)),
           "<" = sprintf("%s < %s", v, vals[1]), "<=" = sprintf("%s <= %s", v, vals[1]), ">" = sprintf("%s > %s", v, vals[1]),
           ">=" = sprintf("%s >= %s", v, vals[1]), "is missing" = sprintf("is.na(%s)", v), "is answered" = sprintf("!is.na(%s)", v))
  }, "")
  paste(parts, collapse = " & ")
}

qc_group_code <- function(g) {
  lines <- c(sprintf("# Group variable %s%s", g$name, if (!is.null(g$label) && nzchar(g$label)) paste0(" (", g$label, ")") else ""),
             sprintf('data[["%s"]] <- NA_character_', g$name))
  for (r in g$groups) {
    cc <- if (is.character(r$cond)) r$cond else qc_cond_code(r$cond)
    lines <- c(lines, sprintf('data[["%s"]][is.na(data[["%s"]]) & ((%s) %%in%% TRUE)] <- "%s"', g$name, g$name, cc, r$label))
  }
  c(lines, sprintf('data[["%s"]] <- factor(data[["%s"]], levels = %s)   # order used in tables and one-sided tests', g$name, g$name,
                   qc_vec(vapply(g$groups, `[[`, "", "label"))))
}

# ---------------------------------------------------------------- whole preparation
qc_prep_steps <- function(prep) {
  st <- list()
  if (length(prep$codebook_code)) st[[length(st) + 1]] <- list(kind = "codebook", code = c("# Codebook: missing-value codes", prep$codebook_code))
  for (x in prep$exclusions) if (isTRUE(x$on)) st[[length(st) + 1]] <- list(kind = "exclude", label = x$label,
    code = c(sprintf("# Exclude %s - %s", x$label, x$reason), sprintf("data <- data[!((%s) %%in%% TRUE), , drop = FALSE]", x$cond)))
  man <- prep$manual
  if (!is.null(man) && nrow(man)) for (rs in unique(man$reason)) {
    ids <- man$id[man$reason == rs]; idv <- suppressWarnings(as.numeric(ids)); idc <- if (anyNA(idv)) qc_vec(ids) else qc_vec(idv)
    lab <- if (length(ids) <= 5) sprintf("%s (%s %s: %s)", if (length(ids) == 1) "case" else "cases", prep$id_var, paste(ids, collapse = ", "), rs) else sprintf("cases (%s)", rs)
    who <- if (length(ids) <= 5) sprintf("%s %s", prep$id_var, paste(ids, collapse = ", ")) else sprintf("%d cases", length(ids))
    st[[length(st) + 1]] <- list(kind = "exclude", label = lab,
      code = c(sprintf("# Exclude %s - %s", who, rs), sprintf('data <- data[!(data[["%s"]] %%in%% %s), , drop = FALSE]', prep$id_var, idc)))
  }
  for (s in prep$scales) st[[length(st) + 1]] <- list(kind = "scale", name = s$name, code = qc_scale_code(s))
  for (g in prep$groups) st[[length(st) + 1]] <- list(kind = "group", name = g$name, code = qc_group_code(g))
  st
}

qc_prep_code <- function(prep) unlist(lapply(qc_prep_steps(prep), `[[`, "code"))

# Run the preparation on the raw data and record the case flow (one line per filtering step)
qc_apply_prep <- function(raw, prep) {
  data <- raw; n0 <- nrow(raw); flow <- sprintf("%d cases in the data file", n0); counts <- data.frame(step = "data file", n = n0, removed = 0)
  env <- new.env(parent = globalenv())
  for (s in qc_prep_steps(prep)) {
    before <- nrow(data); assign("data", data, envir = env)
    eval(parse(text = s$code), envir = env); data <- get("data", envir = env)
    if (s$kind == "exclude") { rm <- before - nrow(data)
      flow <- c(flow, sprintf("%d after removing %d %s", nrow(data), rm, s$label))
      counts <- rbind(counts, data.frame(step = s$label, n = nrow(data), removed = rm)) }
  }
  rownames(data) <- NULL
  list(data = data, flow = flow, counts = counts)
}

qc_prep_summary <- function(res, prep, labels = list()) {
  d <- res$data
  sc <- do.call(rbind, lapply(prep$scales, function(s) { a <- qc_scale_alpha(d, s)
    data.frame(scale = s$name, label = if (is.null(s$label)) "" else s$label, items = length(s$items), method = s$method,
               alpha = a$alpha, n = sum(!is.na(d[[s$name]])), mean = mean(d[[s$name]], na.rm = TRUE), sd = stats::sd(d[[s$name]], na.rm = TRUE)) }))
  gr <- do.call(rbind, lapply(prep$groups, function(g) { t <- table(d[[g$name]], useNA = "ifany")
    data.frame(group = g$name, label = if (is.null(g$label)) "" else g$label, categories = paste(sprintf("%s: %d", ifelse(is.na(names(t)), "not assigned", names(t)), as.integer(t)), collapse = "; ")) }))
  list(n = nrow(d), variables = ncol(d), flow = res$flow, scales = sc, groups = gr)
}

# Save everything the user needs to reproduce or share the prepared data
qc_save_prepared <- function(res, prep, cb, check, ctx, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  d <- res$data
  lab_tab <- data.frame(variable = names(d), label = vapply(names(d), function(v) if (!is.null(ctx$labels[[v]])) ctx$labels[[v]] else "", ""),
    values = vapply(names(d), function(v) { l <- ctx$level_labels[[v]]; if (is.null(l)) "" else paste(sprintf("%s = %s", names(l), l), collapse = "; ") }, ""))
  flow_tab <- data.frame(step = seq_along(res$flow), description = res$flow)
  xl <- file.path(out_dir, "final_dataset.xlsx")
  writexl::write_xlsx(list(data = as.data.frame(lapply(d, function(x) if (is.factor(x)) as.character(x) else x), check.names = FALSE),
                           variables = lab_tab, case_flow = flow_tab), xl)
  cbx <- file.path(out_dir, "codebook_applied.xlsx")
  writexl::write_xlsx(list(codebook = check[, c("variable", "symbol", "label", "values", "missing_codes", "message")]), cbx)
  script <- file.path(out_dir, "prepare_data.R")
  helpers <- paste(readLines(file.path(getOption("qc.root", "."), "engine", "helpers.R"), warn = FALSE), collapse = "\n")
  writeLines(c("# QuantCompass - data preparation script. It rebuilds the final dataset from the raw file.",
               sprintf("# Created %s. Put this script next to %s and run it.", format(Sys.time(), "%Y-%m-%d %H:%M"), basename(ctx$path)),
               helpers, qc_load_code(ctx$path, ctx$sheet), qc_prep_code(prep),
               "", "# Case flow", paste0("#   ", res$flow), "cat(nrow(data), 'cases in the final dataset\\n')"), script, useBytes = FALSE)
  proj <- file.path(out_dir, "project.json")
  jsonlite::write_json(list(data_file = basename(ctx$path), sheet = ctx$sheet, codebook = cb$source, prep = prep[c("id_var", "exclusions", "scales", "groups")],
                            manual = prep$manual, accept_na = prep$accept_na, flow = res$flow, created = format(Sys.time())), proj, auto_unbox = TRUE, pretty = TRUE)
  c(excel = xl, codebook = cbx, script = script, project = proj)
}

# Plain-language conditions, e.g. "ES01 is 1,2 and fin_stress < 5.5" or "DM06 is not 1".
# Returns a list of conditions for qc_cond_code(), or an error message naming the problem.
qc_parse_condition_text <- function(txt, data) {
  parts <- trimws(strsplit(txt, "\\s+and\\s+", perl = TRUE)[[1]]); parts <- parts[nzchar(parts)]
  if (!length(parts)) return(list(error = "Write at least one condition, e.g. ES01 is 1,2"))
  out <- list()
  for (p in parts) {
    m <- regmatches(p, regexec("^([A-Za-z][A-Za-z0-9_.]*)\\s+(is not|is missing|is answered|is|=|!=|<=|>=|<|>)\\s*(.*)$", p))[[1]]
    if (length(m) != 4) return(list(error = sprintf("Could not read '%s'. Use: VARIABLE is 1,2 | is not 3 | < 5.5 | >= 2 | is missing | is answered", p)))
    v <- m[2]; if (!v %in% names(data)) return(list(error = sprintf("'%s' is not a column of the prepared data.", v)))
    op <- c("=" = "is", "!=" = "is not")[m[3]]; op <- if (is.na(op)) m[3] else unname(op)
    vals <- trimws(strsplit(gsub("^[\"']|[\"']$", "", m[4]), "\\s*[,;]\\s*")[[1]]); vals <- gsub("^[\"']|[\"']$", "", vals[nzchar(vals)])
    if (!op %in% c("is missing", "is answered") && !length(vals)) return(list(error = sprintf("'%s' needs a value.", p)))
    if (op %in% c("<", "<=", ">", ">=") && is.na(suppressWarnings(as.numeric(vals[1])))) return(list(error = sprintf("'%s' needs a number.", p)))
    out[[length(out) + 1]] <- list(var = v, op = op, values = vals)
  }
  out
}
