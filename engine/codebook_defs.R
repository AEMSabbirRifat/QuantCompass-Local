# Definitions a codebook writes down in words: how scales are scored ("Money stress (MS_MEAN): recode MS03 (6 - x);
# mean of MS01-MS04") and how the sample was screened ("S1 Incomplete | FINISHED = 1 | 6 removed | 144 remaining").
# Both are read from the codebook's text (PDF/TXT lines, or table rows of Word/Excel files) and checked against the data.

# Variables named in a piece of text: single names, ranges (SL01-SL05, TIME001-TIME006), name families ("MS, SL, AB
# items") and reverse-coded copies (MS03R, MS03_r) that point back to their item.
qc_expand_vars <- function(txt, known) {
  out <- character(); rev <- character()
  if (is.null(txt) || is.na(txt) || !nzchar(txt)) return(list(vars = out, reverse = rev))
  num <- function(v) suppressWarnings(as.numeric(sub("^.*?([0-9]+)$", "\\1", v)))
  stem <- function(v) sub("_?[0-9]+$", "", v)
  # ranges first: A01-A05, A01 to A05, A01 - A05
  rg <- regmatches(txt, gregexpr("\\b([A-Za-z][A-Za-z_]*_?[0-9]+)\\s*(?:-|\u2013|to)\\s*([A-Za-z][A-Za-z_]*_?[0-9]+)\\b", txt, perl = TRUE))[[1]]
  for (r in rg) { ab <- trimws(strsplit(r, "\\s*(?:-|\u2013|to)\\s*", perl = TRUE)[[1]]); if (length(ab) != 2) next
    if (stem(ab[1]) != stem(ab[2])) next
    fam <- known[stem(known) == stem(ab[1]) & grepl("[0-9]$", known)]
    fam <- fam[num(fam) >= num(ab[1]) & num(fam) <= num(ab[2])]
    out <- c(out, fam[order(num(fam))]); txt <- sub(r, " ", txt, fixed = TRUE) }
  tok <- regmatches(txt, gregexpr("[A-Za-z][A-Za-z0-9_\\[\\]]*", txt, perl = TRUE))[[1]]
  for (t in tok) {
    if (t %in% known) { out <- c(out, t); next }
    base <- sub("(_r|_R|R|_rev|rev)$", "", t)
    if (base != t && base %in% known) { out <- c(out, base); rev <- c(rev, base); next }
  }
  # families: "MS, SL, AB items" / "the MS items"
  fm <- regmatches(txt, gregexpr("\\b([A-Z]{2,6})\\b(?=(?:\\s*(?:,|and|&)\\s*[A-Z]{2,6}\\b)*\\s*(?:items|scale|questions)\\b)", txt, perl = TRUE))[[1]]
  fm <- unique(c(fm, if (grepl("(?i)\\bitems\\b", txt, perl = TRUE)) regmatches(txt, gregexpr("\\b[A-Z]{2,6}\\b(?=\\s*(?:,|and|&|\\)|items))", txt, perl = TRUE))[[1]]))
  for (f in fm) { fam <- known[grepl(sprintf("^%s_?[0-9]+$", f), known)]; out <- c(out, fam[order(num(fam))]) }
  list(vars = unique(out), reverse = unique(rev))
}

# Lines of a layout text joined into records: a row of a table continues on the next lines when they are indented,
# start with "(" or with a lower-case word.
qc_cb_records <- function(lines) {
  rec <- character()
  for (l in lines) {
    if (!nzchar(trimws(l))) { rec <- c(rec, ""); next }
    ind <- nchar(l) - nchar(sub("^\\s+", "", l)); t <- trimws(l)
    if (length(rec) && nzchar(rec[length(rec)]) && (ind >= 6 || grepl("^[(a-z.]", t))) rec[length(rec)] <- paste(rec[length(rec)], t)
    else rec <- c(rec, t)
  }
  rec[nzchar(rec)]
}

# Items marked as reverse-keyed anywhere in the codebook: "MS03 (R)", a label ending in "Reverse-keyed",
# "Recode MS03 (6 - x)", "MS03R  MS03 reverse-coded (6 - MS03)".
qc_cb_reverse_marks <- function(lines, known, cb = NULL) {
  txt <- paste(lines, collapse = "\n"); out <- character()
  for (v in known) {
    q <- gsub("([^A-Za-z0-9_])", "\\\\\\1", v)
    if (grepl(sprintf("\\b%s\\s*\\((R|r|rev|reversed)\\)", q), txt, perl = TRUE) ||
        grepl(sprintf("(?i)\\brecode\\s+%s\\b", q), txt, perl = TRUE) ||
        grepl(sprintf("(?i)\\b%s\\s+(is\\s+)?reverse[- ]?(keyed|coded|scored)", q), txt, perl = TRUE) ||
        grepl(sprintf("(?i)\\b%s(_r|R)\\b[^\n]{0,40}reverse", q), txt, perl = TRUE)) out <- c(out, v)
    e <- if (!is.null(cb)) cb$vars[[v]] else NULL
    if (!is.null(e) && !is.na(e$label) && grepl("(?i)reverse[- ]?(keyed|coded|scored)|\\(R\\)\\s*$", e$label, perl = TRUE)) out <- c(out, v)
  }
  unique(out)
}

# Scale definitions: a record that says how a score is computed ("mean of", "sum of", "average of", WHO-5 index).
qc_cb_scale_defs <- function(lines, known, cb = NULL, data = NULL) {
  recs <- qc_cb_records(lines); revs <- qc_cb_reverse_marks(lines, known, cb); out <- list()
  sys <- if (exists("qc_is_system_var")) qc_is_system_var(known) else rep(FALSE, length(known)); items_ok <- known[!sys]
  for (r in recs) {
    if (!grepl("(?i)\\b(mean|sum|average|total|index)\\b(\\s+score)?\\s+(of|across|over)\\b|\\bmean of\\b|who-?5", r, perl = TRUE)) next
    if (grepl("(?i)\\b(SD|standard deviation) across\\b", r, perl = TRUE)) next             # a screening rule, not a score
    after <- sub("(?i)^.*?\\b(mean|sum|average|total|index)\\b(\\s+score)?\\s+(of|across|over)\\b", "", r, perl = TRUE)
    ev <- qc_expand_vars(after, items_ok)
    if (length(ev$vars) < 2) ev <- qc_expand_vars(r, items_ok)
    if (length(ev$vars) < 2) next
    code <- regmatches(r, regexpr("\\(([A-Za-z][A-Za-z0-9_]*)\\)", r)); code <- if (length(code)) gsub("[()]", "", code) else NA
    if (!is.na(code) && (code %in% known || code %in% c("R", "r") || !grepl("[A-Za-z]{2}", code))) code <- NA
    first <- sub("\\s.*$", "", r)
    if (is.na(code) && grepl("^[A-Z][A-Za-z0-9]*_[A-Za-z0-9_]+$", first) && !first %in% known) code <- first
    lab <- r
    if (!is.na(code) && startsWith(r, code)) lab <- trimws(sub(sprintf("^%s", code), "", r))
    lab <- trimws(sub("(?i)\\s*(:|\\(|\\b[A-Za-z]+_?[0-9]+\\b|\\bmean of|\\bsum of|\\baverage of|\\brecode\\b).*$", "", lab, perl = TRUE))
    lab <- trimws(sub("\\s{2,}.*$", "", lab)); if (!nzchar(lab) || nchar(lab) > 60) lab <- if (!is.na(code)) code else paste(ev$vars[1], "scale")
    method <- if (grepl("(?i)who-?5", r, perl = TRUE) && length(ev$vars) == 5 && grepl("(?i)(x\\s*4|\\*\\s*4|times 4|multipl|0\\s*-\\s*100|index)", r, perl = TRUE)) "who5"
              else if (grepl("(?i)\\b(sum|total)\\b(\\s+score)?\\s+(of|across)", r, perl = TRUE)) "sum" else "mean"
    rv <- intersect(unique(c(ev$reverse, revs)), ev$vars)
    out[[length(out) + 1]] <- list(code = code, label = lab, items = ev$vars, reverse = rv, method = method, text = r)
  }
  # the same item set described twice (a scale table and a variable list): keep one, with a code and a label
  keep <- list()
  for (d in out) { j <- which(vapply(keep, function(k) setequal(k$items, d$items) || (!is.na(k$code) && identical(k$code, d$code)), logical(1)))
    if (length(j) && length(d$items) > length(keep[[j]]$items)) { d0 <- d; d <- keep[[j]]; keep[[j]] <- d0 }   # the fuller reading of the same scale wins
    if (!length(j)) { keep[[length(keep) + 1]] <- d; next }
    k <- keep[[j]]
    if (is.na(k$code) && !is.na(d$code)) k$code <- d$code
    if ((k$label == k$code %in% TRUE || grepl(" scale$", k$label)) && !grepl(" scale$", d$label) && !identical(d$label, d$code)) k$label <- d$label
    k$reverse <- unique(c(k$reverse, d$reverse)); if (d$method != "mean") k$method <- d$method
    keep[[j]] <- k }
  if (!is.null(data)) keep <- Filter(function(d) all(vapply(d$items, function(v) is.numeric(data[[v]]), logical(1))), keep)
  keep
}

# Screening steps: a table with a rule per step and the cases removed / remaining.
qc_cb_screening <- function(lines, known) {
  s <- grep("(?i)(screening|filtration|exclusion|inclusion criteria|data cleaning|sample selection)", lines, perl = TRUE)
  if (!length(s)) return(list())
  steps <- list(); raw_n <- NA
  for (l in lines[s[1]:min(length(lines), s[1] + 60)]) {
    m <- regmatches(l, regexec("^\\s*(.+?)\\s{2,}(.+?)\\s{2,}(-|[0-9]+)\\s{2,}([0-9]+)\\s*$", l))[[1]]
    if (length(m) != 5) next
    lab <- trimws(m[2]); rule <- trimws(m[3]); rem <- suppressWarnings(as.integer(m[4])); left <- as.integer(m[5])
    if (grepl("(?i)^(raw|all cases|downloaded|export)", lab, perl = TRUE) || grepl("(?i)all cases", rule, perl = TRUE)) { raw_n <- left; next }
    if (grepl("(?i)^(final|total|analysis sample)", lab, perl = TRUE)) break
    steps[[length(steps) + 1]] <- c(list(label = lab, rule = rule, removed = rem, remaining = left), qc_screen_rule(lab, rule, lines, known))
  }
  if (!length(steps)) return(list())
  list(steps = steps, raw_n = raw_n)
}

# One screening rule in words -> R code that marks the cases to remove (cond) and a plain description.
qc_screen_rule <- function(lab, rule, lines, known) {
  q <- function(v) sprintf('data[["%s"]]', v)
  # simple conditions joined by and/or: FINISHED = 1, CO01 = 1 and SC01 = 1, TIME_RSI < 2.0
  parts <- strsplit(rule, "(?i)\\s+(and|or|&|,)\\s+", perl = TRUE)[[1]]; joins <- regmatches(rule, gregexpr("(?i)\\s+(and|or|&|,)\\s+", rule, perl = TRUE))[[1]]
  pat <- "^\\s*([A-Za-z][A-Za-z0-9_]*)\\s*(==|=|!=|<>|<=|>=|<|>|\u2264|\u2265)\\s*(-?[0-9]+(?:\\.[0-9]+)?)\\s*$"
  pm <- lapply(parts, function(p) regmatches(p, regexec(pat, p))[[1]])
  if (length(parts) && all(lengths(pm) == 4) && all(vapply(pm, `[[`, "", 2) %in% known)) {
    ex <- vapply(pm, function(m) { op <- switch(m[3], "=" = , "==" = "==", "<>" = "!=", "\u2264" = "<=", "\u2265" = ">=", m[3])
      if (op == "==") sprintf("%s %%in%% %s", q(m[2]), m[4]) else if (op == "!=") sprintf("!(%s %%in%% %s)", q(m[2]), m[4]) else sprintf("(%s %s %s)", q(m[2]), op, m[4]) }, "")
    keep <- ex[1]; for (i in seq_along(joins)) keep <- paste(keep, if (grepl("(?i)or", joins[i], perl = TRUE)) "|" else "&", ex[i + 1])
    return(list(cond = sprintf("!((%s) %%in%% TRUE)", keep), kind = "condition", vars = vapply(pm, `[[`, "", 2), keep_text = rule)) }
  # straight-lining: the same answer on every item
  if (grepl("(?i)straight|\\bSD across|same answer", paste(lab, rule), perl = TRUE)) {
    it <- qc_expand_vars(rule, known)$vars
    if (length(it) < 2) { rr <- qc_cb_records(lines); note <- rr[grepl("(?i)straight", rr, perl = TRUE) & !grepl("(?i)SD across", rr, perl = TRUE)]
      note <- sub("(?i)^.*?(straight)", "\\1", note, perl = TRUE)                # only the sentence about straight-lining, not its neighbours
      it <- qc_expand_vars(paste(note, collapse = " "), known)$vars }
    k <- suppressWarnings(as.integer(regmatches(paste(rule, lab), regexpr("[0-9]+(?=\\s+(scale\\s+)?items)", paste(rule, lab), perl = TRUE))))
    if (length(k) && !is.na(k) && length(it) != k) it <- character()          # "all 15 items" must name 15 items, or the rule is not used automatically
    if (length(it) >= 2) return(list(cond = sprintf("apply(data[, %s, drop = FALSE], 1, function(r) { r <- r[!is.na(r)]; length(r) > 1 && length(unique(r)) == 1 })", qc_vec(it)),
      kind = "straightlining", vars = it, keep_text = sprintf("not the same answer on all %d items (%s)", length(it), qc_span(it)))) }
  # missing answers on listed variables
  if (grepl("(?i)missing", paste(lab, rule), perl = TRUE)) {
    it <- qc_expand_vars(sub("(?i)^.*?\\b(on|in|for)\\b", "", rule, perl = TRUE), known)$vars
    if (length(it) >= 1) return(list(cond = sprintf("!stats::complete.cases(data[, %s, drop = FALSE])", qc_vec(it)),
      kind = "missing", vars = it, keep_text = sprintf("no missing answer on %s", qc_span(it)))) }
  list(cond = NA_character_, kind = "unread", vars = character(), keep_text = rule)
}

# "MS01, MS02, MS03, SL01" -> "MS01-MS03, SL01" (short form for labels)
qc_span <- function(vs) {
  st <- sub("_?[0-9]+$", "", vs); out <- character()
  for (s in unique(st)) { g <- vs[st == s]; out <- c(out, if (length(g) > 2) paste0(g[1], "-", g[length(g)]) else paste(g, collapse = ", ")) }
  paste(out, collapse = ", ")
}
