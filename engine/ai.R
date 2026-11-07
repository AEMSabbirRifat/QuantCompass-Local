# Optional AI help, with the user's own key. QuantCompass works fully without it.
#
# Rules (see ai/README.md):
#   - Only variable names, codebook labels / texts and the hypotheses are sent. Never data values or rows.
#   - The AI never computes statistics. It only proposes a breakdown or codebook entries.
#   - Every proposal is checked against the real variables and data in R before it is shown, and the user
#     decides whether to use it. Proposals that fail the checks are dropped with a reason.
#   - Keys come from the session (typed in the app) or from a local .env file that git ignores. Nothing is stored.
#
# Providers: Google Gemini, Mistral, Anthropic Claude, any OpenAI-compatible endpoint (OpenAI, a university
# gateway, LM Studio ...) and Ollama on the user's own computer.

qc_ai_providers <- list(
  gemini = list(label = "Google Gemini", key_env = "GEMINI_API_KEY", base = "https://generativelanguage.googleapis.com/v1beta", model = "gemini-2.5-flash", style = "gemini"),
  mistral = list(label = "Mistral", key_env = "MISTRAL_API_KEY", base = "https://api.mistral.ai/v1", model = "mistral-small-latest", style = "openai"),
  anthropic = list(label = "Anthropic Claude", key_env = "ANTHROPIC_API_KEY", base = "https://api.anthropic.com/v1", model = "claude-haiku-4-5", style = "anthropic"),
  openai_compatible = list(label = "OpenAI or OpenAI-compatible (e.g. a university AI gateway)", key_env = "OPENAI_COMPATIBLE_API_KEY", base_env = "OPENAI_COMPATIBLE_BASE_URL",
                           base = "https://api.openai.com/v1", model = "gpt-4o-mini", style = "openai"),
  academiccloud = list(label = "GWDG Academic Cloud - Chat AI / SAIA (German universities)", key_env = "ACADEMICCLOUD_API_KEY", base_env = "ACADEMICCLOUD_BASE_URL",
                       base = "https://chat-ai.academiccloud.de/v1", model = "llama-3.3-70b-instruct", style = "openai",
                       # chat models as of March 2026 (embedding models left out); current list: https://docs.hpc.gwdg.de/services/chat-ai/models/index.html
                       models = c("llama-3.3-70b-instruct", "qwen3-30b-a3b-instruct-2507", "mistral-large-3-675b-instruct-2512", "openai-gpt-oss-120b",
                                  "qwen3-235b-a22b", "qwen3-32b", "glm-4.7", "gemma-3-27b-it", "apertus-70b-instruct-2509", "meta-llama-3.1-8b-instruct",
                                  "devstral-2-123b-instruct-2512", "deepseek-r1-distill-llama-70b", "qwen3-30b-a3b-thinking-2507", "qwen3-coder-30b-a3b-instruct",
                                  "medgemma-27b-it", "internvl3.5-30b-a3b", "qwen3-omni-30b-a3b-instruct", "qwen3-vl-30b-a3b-instruct", "teuken-7b-instruct-research"),
                       docs = "https://docs.hpc.gwdg.de/services/chat-ai/models/index.html"),
  ollama = list(label = "Ollama (runs on your own computer, no key)", key_env = NA, base_env = "OLLAMA_BASE_URL", base = "http://localhost:11434", model = "llama3.1", style = "ollama"))

# Read KEY=VALUE lines of a .env file (comments and quotes allowed). Returns a named list; nothing is exported.
qc_ai_read_env <- function(path = file.path(qc_root(), ".env")) {
  if (!file.exists(path)) return(list())
  ln <- trimws(readLines(path, warn = FALSE)); ln <- ln[nzchar(ln) & !startsWith(ln, "#") & grepl("=", ln)]
  k <- trimws(sub("=.*$", "", ln)); v <- trimws(sub("\\s+#.*$", "", sub("^[^=]*=", "", ln)))
  v <- gsub("^[\"']|[\"']$", "", v); stats::setNames(as.list(v), k)
}

# Settings for one provider. Typed values win over the .env file; empty values fall back to the defaults.
qc_ai_config <- function(provider = NULL, model = "", key = "", base_url = "", env = qc_ai_read_env()) {
  get_env <- function(k) { if (is.null(k) || is.na(k)) return(""); v <- env[[k]]; if (is.null(v) || !nzchar(v)) Sys.getenv(k, "") else v }
  if (is.null(provider) || !nzchar(provider)) provider <- get_env("QC_AI_PROVIDER")
  if (!nzchar(provider)) provider <- "gemini"
  pr <- qc_ai_providers[[provider]]; if (is.null(pr)) stop("Unknown AI provider: ", provider)
  key <- if (nzchar(key)) key else get_env(pr$key_env)
  base <- if (nzchar(base_url)) base_url else { b <- get_env(pr$base_env); if (nzchar(b)) b else pr$base }
  model <- if (nzchar(model)) model else { m <- get_env("QC_AI_MODEL"); if (nzchar(m)) m else pr$model }
  list(provider = provider, label = pr$label, style = pr$style, model = model, key = key, base = sub("/+$", "", base),
       ready = nzchar(key) || identical(pr$style, "ollama"))
}

qc_ai_notice <- function(cfg) sprintf(paste(
  "You are interacting with an AI system (%s, model %s) when this is switched on.",
  "QuantCompass sends it only your variable names, codebook labels and texts, and your hypotheses - never your data values.",
  "The AI only suggests; every suggestion is checked against your variables and you decide whether to use it. All statistics are computed in R."), cfg$label, cfg$model)

# ---------------------------------------------------------------- transport
# One HTTPS POST. Tests replace it with options(qc.ai.transport = function(url, headers, body) "...").
qc_ai_post <- function(url, headers, body) {
  tr <- getOption("qc.ai.transport"); json <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  if (is.function(tr)) return(tr(url, headers, json))
  if (!requireNamespace("curl", quietly = TRUE)) stop("The 'curl' package is needed for AI help: install.packages('curl')")
  h <- curl::new_handle(); curl::handle_setopt(h, postfields = as.character(json), timeout = 60)
  curl::handle_setheaders(h, .list = c(list(`Content-Type` = "application/json"), headers))
  r <- curl::curl_fetch_memory(url, handle = h); txt <- rawToChar(r$content); Encoding(txt) <- "UTF-8"
  if (r$status_code >= 300) {
    msg <- tryCatch({ j <- jsonlite::fromJSON(txt, simplifyVector = FALSE); m <- j$error$message %||% j$message %||% ""; as.character(m) }, error = function(e) "")
    stop(sprintf("The AI service answered with error %d%s", r$status_code, if (nzchar(msg)) paste0(": ", substr(msg, 1, 200)) else "."))
  }
  txt
}

# Ask for one JSON answer. Returns the parsed JSON (a list).
qc_ai_chat <- function(cfg, system, user) {
  if (!isTRUE(cfg$ready)) stop("No API key for ", cfg$label, ". Enter it in the AI settings or in the .env file.")
  if (cfg$style == "gemini") {
    txt <- qc_ai_post(sprintf("%s/models/%s:generateContent", cfg$base, cfg$model), list(`x-goog-api-key` = cfg$key),
      list(systemInstruction = list(parts = list(list(text = system))), contents = list(list(role = "user", parts = list(list(text = user)))),
           generationConfig = list(temperature = 0, responseMimeType = "application/json")))
    j <- jsonlite::fromJSON(txt, simplifyVector = FALSE); out <- j$candidates[[1]]$content$parts[[1]]$text
  } else if (cfg$style == "anthropic") {
    txt <- qc_ai_post(paste0(cfg$base, "/messages"), list(`x-api-key` = cfg$key, `anthropic-version` = "2023-06-01"),
      list(model = cfg$model, max_tokens = 2000, temperature = 0, system = system, messages = list(list(role = "user", content = user))))
    j <- jsonlite::fromJSON(txt, simplifyVector = FALSE); out <- j$content[[1]]$text
  } else {
    url <- if (cfg$style == "ollama") paste0(cfg$base, "/v1/chat/completions") else paste0(cfg$base, "/chat/completions")
    hd <- if (nzchar(cfg$key)) list(Authorization = paste("Bearer", cfg$key)) else list()
    body <- list(model = cfg$model, temperature = 0, messages = list(list(role = "system", content = system), list(role = "user", content = user)),
                 response_format = list(type = "json_object"))
    txt <- tryCatch(qc_ai_post(url, hd, body), error = function(e) { if (grepl("error 4", conditionMessage(e))) { body$response_format <- NULL; qc_ai_post(url, hd, body) } else stop(e) })
    j <- jsonlite::fromJSON(txt, simplifyVector = FALSE); out <- j$choices[[1]]$message$content
  }
  if (is.null(out) || !nzchar(out)) stop("The AI returned an empty answer.")
  qc_ai_json(out)
}

qc_ai_json <- function(txt) {
  txt <- gsub("(?s)<think>.*?</think>", "", txt, perl = TRUE)          # reasoning models may write their thinking first
  txt <- gsub("^```(?:json)?\\s*|\\s*```$", "", trimws(txt), perl = TRUE)
  s <- regexpr("\\{", txt); e <- max(gregexpr("\\}", txt)[[1]]); if (s < 0 || e < s) stop("The AI answer was not JSON.")
  jsonlite::fromJSON(substr(txt, s, e), simplifyVector = FALSE)
}

# ---------------------------------------------------------------- what is sent: names, labels and codebook text only
# Category names are sent only when they come from the codebook (level_labels), never from the data.
qc_ai_catalog_text <- function(cat, level_labels = list(), max_vars = 250) {
  cat <- utils::head(cat, max_vars)
  vapply(seq_len(nrow(cat)), function(i) { v <- cat$variable[i]; lv <- level_labels[[v]]
    paste0(v, " | ", substr(cat$label[i], 1, 120), " | ", cat$kind[i],
           if (length(lv)) paste0(" | categories: ", substr(paste(names(lv), "=", lv, collapse = "; "), 1, 200)) else "") }, "")
}

# ---------------------------------------------------------------- 1. hypothesis breakdown
qc_ai_goal_fields <- list(relationship = c("var1", "var2"), difference = c("outcome", "group"), paired_difference = c("var1", "var2"),
  association = c("var1", "var2"), prediction = c("outcome", "predictors"), mediation = c("var1", "mediator", "outcome"), moderation = c("var1", "moderator", "outcome"))

qc_ai_hyp_prompt <- function(text, cat, level_labels) {
  sys <- paste("You help students map one research hypothesis onto the variables of their own survey data set, so that a statistics program can test it.",
    "You never compute or invent results. Use only variable names from the list. Answer with one JSON object and nothing else.")
  usr <- paste0("VARIABLES (name | label | kind | categories from the codebook):\n", paste(qc_ai_catalog_text(cat, level_labels), collapse = "\n"),
    "\n\nHYPOTHESIS:\n", text,
    "\n\nReturn JSON with these keys:\n",
    '{"goal": one of "relationship" (two numeric/ordinal variables), "difference" (numeric outcome compared between groups of one category variable), ',
    '"paired_difference" (the same people measured twice, e.g. before/after), "association" (two category variables), "prediction" (regression of one outcome on several predictors, optionally after control variables), ',
    '"mediation" (X affects Y through M), "moderation" (the effect of X on Y depends on W),\n',
    ' "var1": "", "var2": "", "outcome": "", "group": "", "mediator": "", "moderator": "", "predictors": [], "controls": [],\n',
    ' "direction": "positive" | "negative" | "none",\n',
    ' "higher_group": "words of the hypothesis naming the group expected to be higher, or empty", "other_group": "words naming the other group, or empty",\n',
    ' "higher_measure": "for paired_difference: the variable expected to be higher, or empty",\n',
    ' "stronger": "for moderation: high | low | words naming the moderator group where the effect is stronger | none",\n',
    ' "subgroup": "words naming a subgroup the hypothesis is limited to, or empty",\n',
    ' "confidence": number from 0 to 1, "reason": "one short sentence"}\n',
    "Use the empty string for keys that do not apply.")
  list(system = sys, user = usr)
}

# Turn an AI answer into a spec and check it against the real variables and data.
qc_ai_validate_spec <- function(a, data, cat) {
  probs <- character(); g <- a$goal %||% ""
  if (!g %in% names(qc_ai_goal_fields)) return(list(ok = FALSE, problems = sprintf("Unknown analysis type '%s'.", g)))
  s1 <- function(x) { x <- unlist(x); if (is.null(x) || !length(x) || !nzchar(x[1])) NA_character_ else trimws(as.character(x[1])) }
  vec <- function(x) { x <- trimws(as.character(unlist(x))); x[nzchar(x)] }
  sp <- list(goal = g)
  for (f in c("var1", "var2", "outcome", "group", "mediator", "moderator")) if (f %in% qc_ai_goal_fields[[g]]) sp[[f]] <- s1(a[[f]])
  if (g == "prediction") { sp$predictors <- vec(a$predictors); ct <- vec(a$controls); if (length(ct)) sp$controls <- ct }
  used <- unlist(sp[setdiff(names(sp), "goal")])
  bad <- used[is.na(used) | !used %in% cat$variable]
  if (length(bad) || anyNA(used)) return(list(ok = FALSE, problems = sprintf("The AI named variables that are not in your data set: %s.", paste(unique(stats::na.omit(bad)), collapse = ", "))))
  if (anyDuplicated(used)) return(list(ok = FALSE, problems = "The AI used the same variable twice."))
  k <- function(v) cat$kind[cat$variable == v][1]; catk <- c("binary", "categorical"); num <- c("numeric", "ordinal", "binary")
  ok_kind <- switch(g,
    relationship = !(k(sp$var1) %in% catk && k(sp$var2) %in% catk),
    difference = length(cat$levels[[which(cat$variable == sp$group)]]) %in% 2:12,
    paired_difference = k(sp$var1) %in% num && k(sp$var2) %in% num,
    association = k(sp$var1) %in% c(catk, "ordinal") && k(sp$var2) %in% c(catk, "ordinal"),
    prediction = length(sp$predictors) > 0 && k(sp$outcome) %in% num,
    mediation = all(vapply(c(sp$var1, sp$mediator, sp$outcome), k, "") %in% num),
    moderation = k(sp$outcome) %in% c("numeric", "ordinal") && k(sp$var1) %in% num)
  if (!isTRUE(ok_kind)) return(list(ok = FALSE, problems = sprintf("The variable types do not fit a %s analysis.", g)))
  d <- a$direction %||% ""; if (d %in% c("positive", "negative") && g %in% c("relationship", "mediation", "moderation")) sp[["direction"]] <- d
  if (g == "prediction" && d %in% c("positive", "negative") && length(sp$predictors) == 1) sp$directions <- stats::setNames(d, sp$predictors)
  if (g == "difference" && nzchar(a$higher_group %||% "")) {
    gm <- qc_match_groups(a$higher_group, if (nzchar(a$other_group %||% "")) a$other_group else "other", cat[cat$variable == sp$group, ])
    lv <- cat$levels[[which(cat$variable == sp$group)]]; ls <- vapply(lv, function(l) qc_level_score(a$higher_group, l), 0)
    if (!is.null(gm) && gm$score >= .35) { sp$expect_higher <- gm$level1; if (gm$n_levels > 2 && nzchar(a$other_group %||% "")) sp$compare <- c(gm$level1, gm$level2) }
    else if (length(ls) && max(ls) >= .3 && sum(ls == max(ls)) == 1) { sp$expect_higher <- names(lv)[which.max(ls)]
      if (length(lv) > 2) probs <- c(probs, sprintf("Choose the group that '%s' is compared with.", a$higher_group)) }
    else probs <- c(probs, sprintf("The group '%s' could not be matched to a category of %s; choose it below.", a$higher_group, sp$group))
  }
  if (g == "paired_difference" && (a$higher_measure %||% "") %in% c(sp$var1, sp$var2)) sp$expect_higher <- a$higher_measure
  if (g == "moderation") { st <- tolower(a$stronger %||% "")
    if (st %in% c("high", "low")) sp$stronger <- list(at = st)
    else if (nzchar(st) && st != "none") { lv <- cat$levels[[which(cat$variable == sp$moderator)]]
      if (length(lv)) { ls <- vapply(lv, function(l) qc_level_score(st, l), 0); if (max(ls) > 0) sp$stronger <- list(level = names(lv)[which.max(ls)]) } } }
  if (g %in% c("relationship", "difference") ) {   # re-route like the rule engine does
    if (g == "relationship" && xor(k(sp$var1) %in% catk, k(sp$var2) %in% catk)) { grp <- if (k(sp$var1) %in% catk) sp$var1 else sp$var2
      sp <- list(goal = "difference", outcome = setdiff(c(sp$var1, sp$var2), grp), group = grp) }
  }
  blocked <- tryCatch({ o <- qc_suggest(if (exists("qc_as_factors")) qc_as_factors(data, sp) else data, sp); identical(o[[1]]$status, "blocked") }, error = function(e) TRUE)
  if (blocked) return(list(ok = FALSE, problems = "The suggested analysis is not possible with these variables in your data."))
  list(ok = TRUE, spec = sp, problems = probs, confidence = suppressWarnings(as.numeric(a$confidence %||% NA)), reason = a$reason %||% "")
}

qc_ai_dissect <- function(id, text, data, cat, cfg, level_labels = list()) {
  pr <- qc_ai_hyp_prompt(text, cat, level_labels)
  a <- qc_ai_chat(cfg, pr$system, pr$user)
  v <- qc_ai_validate_spec(a, data, cat)
  if (!v$ok) return(v)
  v$expected <- qc_expected_text(v$spec, cat); v
}

# ---------------------------------------------------------------- 2. codebook gaps
# Variables that the readers could not describe: the AI reads the codebook text around each mention and proposes a
# label, answer codes and missing codes. Codes are kept only if they fit the data (every observed value is a code).
qc_ai_codebook_gaps <- function(cb, data, lines, cfg, max_vars = 40) {
  known <- names(data); have <- names(cb$vars)
  need <- known[!vapply(known, function(v) qc_is_system_var(v), logical(1))]
  need <- need[vapply(need, function(v) { e <- cb$vars[[v]]; is.null(e) || is.na(e$label) || !nzchar(e$label) ||
    (is.numeric(data[[v]]) && !length(e$values) && length(unique(stats::na.omit(data[[v]]))) <= 12 && !identical(e$kind, "text")) }, logical(1))]
  need <- utils::head(need, max_vars); if (!length(need)) return(list(vars = list(), problems = character(), asked = character()))
  ctx <- unlist(lapply(need, function(v) { i <- grep(sprintf("(?<![A-Za-z0-9_])\\Q%s\\E(?![A-Za-z0-9_])", v), lines, perl = TRUE)
    if (!length(i)) return(NULL); r <- sort(unique(unlist(lapply(i[1:min(2, length(i))], function(k) max(1, k - 2):min(length(lines), k + 8)))))
    c(sprintf("--- around %s:", v), substr(lines[r], 1, 200)) }))
  if (!length(ctx)) return(list(vars = list(), problems = "The codebook text does not mention the variables that are still undescribed.", asked = need))
  sys <- "You read survey codebooks. You only copy what the codebook says; you never invent labels or codes. Answer with one JSON object."
  usr <- paste0("Variables to describe: ", paste(need, collapse = ", "), "\n\nCODEBOOK TEXT:\n", paste(utils::head(ctx, 900), collapse = "\n"),
    '\n\nReturn {"variables": [{"name": "", "label": "", "codes": {"1": "label", "2": "label"}, "missing": ["-9"], "free_text": false}]} ',
    "with only the variables the text describes. Leave codes empty for numeric measurements without answer options.")
  a <- qc_ai_chat(cfg, sys, usr); out <- list(); probs <- character()
  for (x in a$variables) { v <- x$name %||% ""; if (!v %in% need) next
    lab <- trimws(as.character(x$label %||% "")); codes <- unlist(x$codes); miss <- as.character(unlist(x$missing))
    e <- qc_cb_entry(v, if (nzchar(lab)) lab else NA_character_, if (length(codes)) stats::setNames(as.character(codes), names(codes)) else NULL,
                     kind = if (isTRUE(x$free_text)) "text" else NULL)
    e$missing <- miss[grepl("^-?[0-9]+$", miss)]
    if (length(e$values) && is.numeric(data[[v]])) {
      obs <- unique(stats::na.omit(data[[v]])); obs <- obs[!as.character(obs) %in% e$missing]
      if (!all(obs %in% suppressWarnings(as.numeric(names(e$values))))) { probs <- c(probs, sprintf("%s: the AI's answer codes do not match the values in the data, so only the label was used.", v)); e$values <- character(); e$kind <- "none" } }
    e$source <- "AI (checked against the data)"; out[[v]] <- e }
  list(vars = out, problems = probs, asked = need)
}
