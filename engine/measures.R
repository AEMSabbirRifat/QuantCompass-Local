# Measures: which columns can measure the constructs of a hypothesis.
# Finds multi-item scales (items that share a heading in the codebook, or a column-name stem, and the same answer scale),
# suggests reverse-coded items from the answers themselves, and turns a hypothesis breakdown into role recommendations
# (dependent / independent variable, mediator, moderator, ...) with alternatives the user can click.

# "Work-University Conflict: Because of my job ..." -> "Work-University Conflict"; "Subjective FS 3: How ..." -> "Subjective FS"
qc_construct_of <- function(label) {
  if (is.null(label) || is.na(label) || !nzchar(label)) return(NA_character_)
  m <- regmatches(label, regexpr("^[^:]{2,60}:", label)); if (!length(m)) return(NA_character_)
  k <- trimws(sub(":$", "", m))
  k <- sub("^(?i)(filter|screening)\\.\\s*", "", k, perl = TRUE)
  k <- trimws(sub("\\s+(item\\s*)?[0-9]+[a-z]?$", "", k, ignore.case = TRUE))
  if (nchar(k) < 2) NA_character_ else k
}
qc_item_text <- function(label) trimws(sub("^[^:]{2,60}:\\s*", "", label %||% ""))

qc_snake <- function(x, taken = character()) {
  s <- tolower(gsub("[^A-Za-z0-9]+", "_", iconv(x, to = "ASCII//TRANSLIT", sub = "")))
  s <- gsub("^_+|_+$", "", s); if (!nzchar(s)) s <- "scale"; if (grepl("^[0-9]", s)) s <- paste0("s_", s)
  s <- substr(s, 1, 28); base <- s; i <- 2
  while (s %in% taken) { s <- paste0(base, "_", i); i <- i + 1 }
  s
}

# Items that run the other way: negative correlation with the sum of the other items (needs 3+ items).
qc_reverse_suggest <- function(data, items) {
  X <- data[, items, drop = FALSE]; X[X < 0] <- NA; X <- X[stats::complete.cases(X), , drop = FALSE]   # negative codes are missing-value codes
  if (length(items) < 3 || nrow(X) < 10) return(character())
  X <- as.data.frame(lapply(X, as.numeric))
  r <- vapply(seq_along(items), function(i) suppressWarnings(stats::cor(X[[i]], rowSums(X[, -i, drop = FALSE]))), 0)
  neg <- items[!is.na(r) & r < 0]
  if (length(neg) > length(items) / 2) neg <- setdiff(items, neg)     # reverse the minority
  neg
}

# Candidate multi-item scales from the codebook headings, the column names and the answers.
qc_measure_candidates <- function(data, cb = NULL, labels = list(), taken = names(data)) {
  att <- if (!is.null(cb)) tryCatch(names(qc_attention_items(cb)), error = function(e) character()) else character()
  e_of <- function(v) if (!is.null(cb)) cb$vars[[v]] else NULL
  lab_of <- function(v) { if (!is.null(labels[[v]])) return(labels[[v]]); e <- e_of(v); if (!is.null(e) && !is.na(e$label)) e$label else NA_character_ }
  codes_of <- function(v) { e <- e_of(v); k <- if (!is.null(e) && length(e$values)) suppressWarnings(as.numeric(names(e$values))) else numeric()
    if (!is.null(e) && length(e$values)) k <- k[!grepl(qc_na_like_re, e$values, perl = TRUE)]
    k <- k[!is.na(k)]
    if (length(k) >= 2) sort(k) else { x <- data[[v]][!is.na(data[[v]])]; if (length(x)) seq(min(x), max(x)) else numeric() } }
  likert <- vapply(names(data), function(v) { x <- data[[v]]; e <- e_of(v)
    if (!is.numeric(x) || qc_is_system_var(v) || v %in% att || grepl("_r$", v)) return(FALSE)
    if (!is.null(e) && identical(e$kind, "text")) return(FALSE)
    nn <- x[!is.na(x)]; k <- codes_of(v); if (!is.null(e) && length(e$values) && length(k) >= 2) nn <- nn[nn >= min(k) & nn <= max(k)]   # missing codes like -9 are not answers
    length(nn) >= 5 && all(nn == round(nn)) && length(unique(nn)) >= 3 && length(unique(nn)) <= 11 && (max(nn) - min(nn)) <= 10 }, logical(1))
  vs <- names(data)[likert]; if (length(vs) < 2) return(list())
  key <- vapply(vs, function(v) { k <- qc_construct_of(lab_of(v)); if (is.na(k)) paste0("#stem:", sub("_[0-9]+$", "", v)) else k }, "")
  out <- list()
  for (k in unique(key[duplicated(key)])) {
    it <- vs[key == k]
    if (startsWith(k, "#stem:") && !all(grepl("_[0-9]+$", it))) next           # a stem group needs numbered items (X_01, X_02 ...)
    cs <- vapply(it, function(v) { e <- e_of(v); paste(c(codes_of(v), if (!is.null(e)) c(e$kind, length(e$values) > 0, if (identical(e$kind, "codes")) tolower(unname(e$values)))), collapse = ",") }, "")   # answer labels must match too
    modal <- names(sort(table(cs), decreasing = TRUE))[1]; it <- it[cs == modal]
    if (length(it) < 2) next
    codes <- codes_of(it[1])
    lv <- if (!is.null(cb) && !is.null(e_of(it[1]))) e_of(it[1])$values else character()
    if (length(lv) >= 3 && identical(e_of(it[1])$kind, "codes") && !qc_ordered_labels(unname(lv[!grepl(qc_na_like_re, lv, perl = TRUE)]))) next   # categories (job type ...) are not a scale
    construct <- if (startsWith(k, "#stem:")) sub("^#stem:", "", k) else k
    out[[length(out) + 1]] <- list(name = NA, label = construct, items = it, method = "mean", min = min(codes), max = max(codes),
      text = paste(vapply(it, function(v) qc_item_text(lab_of(v)), ""), collapse = " / "), source = "suggested")
  }
  # codebook presets (e.g. WHO-5) win when they describe the same items
  sg <- if (!is.null(cb)) tryCatch(qc_scale_suggestions(cb, data), error = function(e) list()) else list()
  for (s in sg) { j <- which(vapply(out, function(o) setequal(o$items, s$items), logical(1)))
    if (length(j) && !is.na(s$preset)) { out[[j]]$method <- s$method; out[[j]]$preset <- s$preset; out[[j]]$name <- s$name }
    else if (!length(j) && all(s$items %in% vs) && !any(vapply(out, function(o) length(intersect(o$items, s$items)) > 0, logical(1))))
      out[[length(out) + 1]] <- list(name = NA, label = s$name, items = s$items, method = s$method, min = s$min, max = s$max,
        text = paste(vapply(s$items, function(v) qc_item_text(lab_of(v)), ""), collapse = " / "), source = "suggested", preset = s$preset) }
  used <- taken
  for (i in seq_along(out)) { o <- out[[i]]
    if (is.na(o$name)) o$name <- qc_snake(o$label, used); used <- c(used, o$name)
    o$reverse <- if (identical(o$method, "who5")) character() else qc_reverse_suggest(data, o$items)
    o$why <- paste0(sprintf("%d items with the same answer scale (%s-%s)%s", length(o$items), qc_fmt_num(o$min), qc_fmt_num(o$max),
        if (!grepl("^[A-Za-z]+[0-9]*$", o$label) || !is.null(cb)) sprintf(" under the heading '%s'", o$label) else ""),
      if (length(o$reverse)) sprintf("; %s run%s the other way in your answers and %s reversed", paste(o$reverse, collapse = ", "), if (length(o$reverse) == 1) "s" else "", if (length(o$reverse) == 1) "is" else "are") else "", ".")
    out[[i]] <- o }
  out
}
qc_fmt_num <- function(x) format(x, trim = TRUE, drop0trailing = TRUE)

# Data with every candidate scale computed (the same code that the final data uses).
qc_measure_data <- function(data, cands) {
  if (!length(cands)) return(data)
  e <- new.env(); assign("data", data, e)
  for (s in cands) tryCatch(eval(parse(text = qc_scale_code(s)), e), error = function(err) NULL)
  d <- get("data", e); d[, setdiff(names(d), paste0(unlist(lapply(cands, `[[`, "reverse")), "_r")), drop = FALSE]
}

# Catalog for matching: scales carry their item wording, so "financial stress" finds a scale whose items ask about money worries.
qc_measure_catalog <- function(data, labels, level_labels, cb, scales, groups) {
  cat0 <- qc_catalog(data, labels, level_labels, cb, scales, groups)
  cat0$note <- ""
  for (s in scales) { i <- which(cat0$variable == s$name); if (!length(i)) next
    if (nzchar(s$text %||% "")) cat0$question[i] <- s$text
    cat0$note[i] <- sprintf("scale: %s of %d items (%s)%s", if (identical(s$method, "sum")) "sum" else "mean", length(s$items), paste(s$items, collapse = ", "),
                            if (length(s$reverse)) paste0(", reversed: ", paste(s$reverse, collapse = ", ")) else "") }
  cat0
}

# Roles of each analysis type, in the words a beginner knows.
qc_roles <- function(goal) switch(goal,
  relationship = list(list(field = "var1", label = "Independent variable (X)", want = "any"), list(field = "var2", label = "Dependent variable (Y)", want = "any")),
  difference = list(list(field = "group", label = "Independent variable: the groups", want = "categorical"), list(field = "outcome", label = "Dependent variable (compared between the groups)", want = "numeric")),
  paired_difference = list(list(field = "var1", label = "First measurement (e.g. before)", want = "numeric"), list(field = "var2", label = "Second measurement (e.g. after)", want = "numeric")),
  association = list(list(field = "var1", label = "First category variable", want = "categorical"), list(field = "var2", label = "Second category variable", want = "categorical")),
  prediction = list(list(field = "predictors", label = "Independent variables (predictors)", want = "any", multi = TRUE), list(field = "outcome", label = "Dependent variable (outcome)", want = "numeric"),
                    list(field = "controls", label = "Control variables (optional)", want = "any", multi = TRUE, optional = TRUE)),
  mediation = list(list(field = "var1", label = "Independent variable (X)", want = "numeric"), list(field = "mediator", label = "Mediator (M): the path through which X acts", want = "numeric"),
                   list(field = "outcome", label = "Dependent variable (Y)", want = "numeric")),
  moderation = list(list(field = "var1", label = "Independent variable (X)", want = "numeric"), list(field = "moderator", label = "Moderator (W): changes the strength of X's effect", want = "any"),
                    list(field = "outcome", label = "Dependent variable (Y)", want = "numeric")),
  reliability = list(list(field = "items", label = "Items of the scale", want = "numeric", multi = TRUE)),
  list())

# The words of the hypothesis behind each role (from the rule-based breakdown).
qc_role_phrases <- function(goal, phrases) {
  ph <- function(x) if (is.list(x) && !is.null(x$phrase) && nzchar(x$phrase)) x$phrase else NA_character_
  switch(goal,
    relationship = list(var1 = ph(phrases$a), var2 = ph(phrases$b)),
    association = list(var1 = ph(phrases$a), var2 = ph(phrases$b)),
    difference = list(outcome = ph(phrases$outcome), group = if (!is.null(phrases$g1)) paste(stats::na.omit(c(phrases$g1, phrases$g2)), collapse = " vs ") else NA),
    mediation = list(var1 = ph(phrases$x), mediator = ph(phrases$m), outcome = ph(phrases$y)),
    moderation = list(var1 = ph(phrases$x), moderator = ph(phrases$w), outcome = ph(phrases$y)),
    prediction = c(list(outcome = ph(phrases$outcome)), list(predictors = unname(vapply(phrases[grepl("^predictor", names(phrases))], ph, "")))),
    list())
}

# For one part: every role with the recommended column (or scale), the words it came from, and alternatives to click.
qc_recommend_roles <- function(spec, phrases, cat, ai_spec = NULL, top = 3) {
  roles <- qc_roles(spec$goal); rp <- qc_role_phrases(spec$goal, phrases %||% list())
  lapply(roles, function(r) {
    f <- r$field; chosen <- spec[[f]]; chosen <- if (is.null(chosen)) character() else as.character(stats::na.omit(chosen))
    ai <- if (!is.null(ai_spec)) as.character(stats::na.omit(unlist(ai_spec[[f]]))) else character()
    phrase <- rp[[f]]; phrase <- if (is.null(phrase)) NA else phrase
    alts <- character()
    if (!isTRUE(r$multi)) {
      p1 <- if (length(phrase) && !is.na(phrase[1]) && nzchar(phrase[1])) phrase[1] else if (length(chosen)) cat$label[cat$variable == chosen[1]][1] else NA
      if (!is.na(p1)) { m <- tryCatch(qc_match_variable(p1, cat, r$want, top = top + 2), error = function(e) data.frame())
        if (nrow(m)) alts <- setdiff(m$variable, c(chosen, ai)) }
      alts <- utils::head(alts, top) }
    c(r, list(phrase = phrase, chosen = chosen, ai = ai, alts = alts))
  })
}

# A construct is measured by its whole scale, not by one of the scale's items.
qc_prefer_scales <- function(spec, scales) {
  if (!length(scales) || spec$goal %in% c("reliability", "paired_difference")) return(spec)
  owner <- function(v) { for (s in scales) if (v %in% s$items) return(s$name); v }
  for (f in c("var1", "var2", "outcome", "mediator", "moderator")) if (!is.null(spec[[f]]) && !is.na(spec[[f]])) spec[[f]] <- owner(spec[[f]])
  for (f in c("predictors", "controls")) if (length(spec[[f]])) spec[[f]] <- unique(vapply(spec[[f]], owner, ""))
  used <- unlist(spec[c("var1", "var2", "outcome", "mediator", "moderator")])
  if (anyDuplicated(stats::na.omit(used))) return(spec)
  spec
}
