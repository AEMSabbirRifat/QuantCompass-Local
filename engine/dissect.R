# Hypothesis breakdown: plain sentence -> type, constructs, variables, groups, direction, test options.
# Rule-based and transparent (no AI). Every match comes with a score and alternatives, and the user
# confirms or changes it. Anything that cannot be matched is turned into an open question, never guessed.

# ---------------------------------------------------------------- text normalisation and concepts
qc_norm_text <- function(x) {
  x <- tolower(paste(x, collapse = " "))
  x <- gsub("→|->|=>", " to ", x)
  reps <- c("well[- ]?being" = "wellbeing", "who[- ]?5" = "who5", "work[- ]?study|work (and|&) study|job[- ]study" = "workstudy", "work[- ]?life" = "worklife",
            "non[- ]?eu\\b" = "noneu", "outside (the )?eu" = "noneu", "non[- ]?working|not working|not currently working|never worked|unemployed|jobless|without (a )?job" = "notworking",
            "low[- ]skilled|unskilled|low skill" = "unskilled", "field[- ]related|related to (my |their )?field( of study)?" = "fieldrelated",
            "part[- ]time" = "parttime", "full[- ]time" = "fulltime", "student[- ]related" = "student")
  for (k in names(reps)) x <- gsub(k, reps[[k]], x, perl = TRUE)
  x <- gsub("[^a-z0-9 ]", " ", x)
  gsub("\\s+", " ", trimws(x))
}

qc_concept_map <- list(
  wellbeing = c("wellbeing", "who5", "happiness", "happy", "mental", "cheerful", "flourishing"),
  finance = c("financial", "finance", "finances", "money", "income", "funding", "economic"),
  stress = c("stress", "stressed", "constraint", "constraints", "constrained", "strain", "pressure", "hardship", "burden", "difficulty", "difficulties"),
  worry = c("worry", "worries", "worried", "concern", "concerned", "concerns", "anxious", "anxiety"),
  debt = c("debt", "debts", "loan", "loans", "borrowed", "owe", "indebted"),
  job = c("job", "jobs", "work", "working", "employment", "employed", "workplace", "employ"),
  notworking = c("notworking", "nonworking"),
  seeking = c("seeking", "looking", "searching", "actively"),
  fieldrelated = c("fieldrelated"), unskilled = c("unskilled"), skilled = c("skilled"),
  workstudy = c("workstudy"),
  conflict = c("conflict", "imbalance", "interference", "clash", "imbalanced"),
  balance = c("balance", "facilitation", "enrichment"),
  academic = c("academic", "study", "studies", "studying", "university", "exam", "exams", "course"),
  performance = c("performance", "grade", "grades", "gpa", "outcome", "outcomes", "achievement", "results", "marks", "success"),
  german = c("german", "language"), proficiency = c("proficiency", "cefr", "fluency", "fluent"),
  eu = c("eu", "european"), noneu = c("noneu"),
  region = c("citizenship", "nationality", "region", "country", "origin"),
  age = c("age", "old", "older", "younger"), gender = c("gender", "sex", "male", "female", "women", "men"),
  satisfaction = c("satisfaction", "satisfied", "dissatisfied"),
  low = c("low", "lower", "little", "less"), high = c("high", "higher", "more", "greater"))
qc_concept_of <- local({ m <- character(); for (k in names(qc_concept_map)) m[qc_concept_map[[k]]] <- k; m })
qc_stopwords <- c("the", "a", "an", "of", "in", "on", "is", "are", "be", "been", "being", "with", "and", "or", "students", "student",
  "their", "there", "will", "have", "has", "had", "than", "who", "which", "that", "it", "its", "about", "for", "to", "by", "as", "at",
  "they", "them", "those", "these", "this", "do", "does", "significantly", "statistically", "more", "positively", "negatively",
  "associated", "correlated", "related", "relationship", "association", "correlation", "between", "among", "international",
  "germany", "currently", "current", "s", "e", "g", "how", "you", "your", "please", "indicate", "following", "what", "would", "describe",
  "level", "total", "general", "personal", "holding", "hold", "having", "experience", "experienced")

qc_concepts <- function(x, keep_level_words = FALSE) {
  tok <- strsplit(qc_norm_text(x), " ", fixed = TRUE)[[1]]
  tok <- tok[nzchar(tok) & !tok %in% qc_stopwords]
  tok <- ifelse(nchar(tok) > 4 & grepl("s$", tok) & !grepl("ss$", tok), sub("s$", "", tok), tok)
  con <- ifelse(tok %in% names(qc_concept_of), qc_concept_of[tok], tok)
  con <- unique(unname(con))
  if (!keep_level_words) con <- setdiff(con, c("low", "high"))
  con
}

# ---------------------------------------------------------------- catalogue of analysable variables
# One row per variable that could stand for a construct: scales and groups built in preparation,
# then single variables. Battery items rank lower than the scale built from them.
qc_catalog <- function(data, labels = list(), level_labels = list(), cb = NULL, scales = list(), groups = list()) {
  sc_items <- unlist(lapply(scales, `[[`, "items"))
  rows <- lapply(names(data), function(v) {
    x <- data[[v]]; nn <- x[!is.na(x)]
    if (!length(nn) || qc_is_system_var(v) || grepl("_r$", v)) return(NULL)
    e <- if (!is.null(cb)) cb$vars[[v]] else NULL
    if (!is.null(e) && e$kind == "text") return(NULL)
    kind <- if (is.factor(x) || is.character(x)) "categorical" else if (length(unique(nn)) <= 2) "binary" else if (length(unique(nn)) <= 12 && all(nn == round(nn))) "ordinal" else "numeric"
    src <- if (v %in% vapply(scales, `[[`, "", "name")) "scale" else if (v %in% vapply(groups, `[[`, "", "name")) "group" else if (v %in% sc_items || grepl("_[0-9]+$", v)) "item" else "variable"
    lab <- if (!is.null(labels[[v]])) labels[[v]] else if (!is.null(e) && !is.na(e$label)) e$label else v
    q <- if (!is.null(e) && !is.na(e$question)) e$question else ""
    lv <- if (is.factor(x)) stats::setNames(levels(x), levels(x)) else if (!is.null(level_labels[[v]])) level_labels[[v]] else if (kind %in% c("binary", "categorical")) stats::setNames(as.character(sort(unique(nn))), sort(unique(nn))) else character()
    data.frame(variable = v, label = lab, question = q, kind = kind, source = src, n = length(nn),
               levels = I(list(lv)), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

qc_match_score <- function(phrase, label, question = "", name = "") {
  pc <- qc_concepts(phrase); if (!length(pc)) return(0)
  lc <- c(qc_concepts(label), qc_concepts(gsub("_", " ", name))); qcn <- qc_concepts(question)
  sum(ifelse(pc %in% lc, 1, ifelse(pc %in% qcn, .6, 0))) / length(pc)
}

qc_match_variable <- function(phrase, cat, want = c("any", "numeric", "categorical"), top = 4) {
  want <- match.arg(want)
  cand <- cat
  if (want == "numeric") cand <- cand[cand$kind %in% c("numeric", "ordinal", "binary"), ]
  if (want == "categorical") cand <- cand[cand$kind %in% c("binary", "categorical", "ordinal") & lengths(cand$levels) >= 2, ]
  if (!nrow(cand)) return(data.frame())
  w <- c(scale = 1.15, group = 1.15, variable = 1, item = .8)[cand$source]
  s <- mapply(qc_match_score, phrase, cand$label, cand$question, cand$variable) * w
  # "holding / having X" names a yes-no state: prefer the two-category variable
  if (grepl("^(holding|having|has|have|owning|with)\\b", tolower(trimws(phrase)))) s <- s + .1 * (cand$kind == "binary" & s > 0)
  o <- order(-s); out <- cand[o, ][seq_len(min(top, nrow(cand))), ]; out$score <- round(s[o][seq_len(nrow(out))], 2)
  out[out$score > 0, , drop = FALSE]
}

# Find a categorical variable with two different categories that match the two group phrases
qc_match_groups <- function(g1, g2, cat) {
  cand <- cat[lengths(cat$levels) >= 2 & cat$kind %in% c("binary", "categorical", "ordinal"), ]
  best <- NULL
  for (i in seq_len(nrow(cand))) {
    lv <- cand$levels[[i]]; keys <- names(lv)
    s1 <- vapply(lv, function(l) qc_match_score(g1, l), numeric(1)); s2 <- vapply(lv, function(l) qc_match_score(g2, l), numeric(1))
    # a phrase like "non-EU" or "working" must not match "EU" or "not working": penalise subset matches
    if (all(s1 == 0) || all(s2 == 0)) next
    j1 <- which.max(s1); s2b <- s2; s2b[j1] <- -1; j2 <- which.max(s2b)
    sc <- min(s1[j1], s2[j2]) * c(scale = 1, group = 1.15, variable = 1, item = .8)[cand$source[i]] +
          .2 * qc_match_score(paste(g1, g2), cand$label[i], cand$question[i])
    if (s2[j2] <= 0) next
    if (is.null(best) || sc > best$score) best <- list(variable = cand$variable[i], label = cand$label[i], level1 = keys[j1], level2 = keys[j2],
      level1_label = lv[[j1]], level2_label = lv[[j2]], score = round(sc, 2), n_levels = length(lv))
  }
  best
}

# ---------------------------------------------------------------- sentence patterns
qc_strip_lead <- function(x) trimws(gsub("^(that|there is|there are|we expect that|we expect|it is expected that|hypothesis:?)\\s+", "", trimws(x), ignore.case = TRUE))

qc_parse_hypothesis <- function(text) {
  t <- gsub("\\s+", " ", trimws(sub("^\\s*H[0-9]+[a-z]?\\s*[:.)-]\\s*", "", text, ignore.case = TRUE)))
  t <- sub("[.;]\\s*$", "", gsub("→|->", " to ", t)); lt <- tolower(t)
  dirw <- function(s) if (grepl("negativ|inverse", s)) "negative" else if (grepl("positiv", s)) "positive" else NA_character_
  # mediation
  m <- regmatches(t, regexec("(?i)^(.*?)\\bmediates?\\b\\s+(?:the\\s+)?(?:relationship|association|link|effect)?\\s*(?:between|of|from)?\\s*(.*?)\\s+(?:and|on|to)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) return(list(type = "mediation", mediator = qc_strip_lead(m[2]), x = m[3], y = m[4], direction = dirw(lt)))
  m <- regmatches(t, regexec("(?i)^(?:the\\s+)?(?:effect|relationship|association|link)\\s+(?:of|between)\\s+(.*?)\\s+(?:on|and)\\s+(.*?)\\s+is\\s+mediated\\s+by\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) return(list(type = "mediation", x = m[2], y = m[3], mediator = m[4], direction = dirw(lt)))
  # group difference: "A have higher Y than B" / "Y is higher in A than in B"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:have|has|show|shows|report|reports|experience|experiences|exhibit|exhibits|score|scores)\\s+(?:significantly\\s+)?(higher|greater|more|better|lower|less|worse|poorer)\\s+(.*?)\\s+than\\s+(?:do\\s+|those\\s+|students\\s+)?(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 5) { up <- tolower(m[3]) %in% c("higher", "greater", "more", "better")
    return(list(type = "difference", g1 = qc_strip_lead(m[2]), g2 = m[5], outcome = m[4], higher = if (up) 1 else 2)) }
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:is|are)\\s+(?:significantly\\s+)?(higher|greater|better|lower|worse|poorer)\\s+(?:among|for|in|amongst)\\s+(.*?)\\s+than\\s+(?:among|for|in)?\\s*(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 5) { up <- tolower(m[3]) %in% c("higher", "greater", "better")
    return(list(type = "difference", g1 = m[4], g2 = m[5], outcome = qc_strip_lead(m[2]), higher = if (up) 1 else 2)) }
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+differs?\\s+(?:significantly\\s+)?between\\s+(.*?)\\s+and\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) return(list(type = "difference", g1 = m[3], g2 = m[4], outcome = qc_strip_lead(m[2]), higher = NA))
  # reliability: "the items of X form a reliable scale" / "Cronbach's alpha of X is at least .70"
  if (grepl("(?i)reliab|cronbach|internal consistency|consistent scale", t, perl = TRUE)) {
    sc <- sub("(?i)^.*?(?:items? of|reliability of|alpha (?:of|for)|scale (?:of|for))\\s+(?:the\\s+)?", "", t, perl = TRUE)
    sc <- sub("(?i)\\s+(?:is|are|forms?|has|have|shows?|scale|items?)\\b.*$", "", sc, perl = TRUE)
    return(list(type = "reliability", scale = qc_strip_lead(sc)))
  }
  # prediction: "A, B and C (together) predict D" / "D is predicted by A, B and C" / "regression of D on A, B"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:is|are)\\s+(?:significantly\\s+)?predicted\\s+by\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 3) return(list(type = "prediction", outcome = qc_strip_lead(m[2]), predictors = m[3]))
  m <- regmatches(t, regexec("(?i)regression\\s+(?:of|predicting)\\s+(.*?)\\s+(?:on|from|by|with)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 3) return(list(type = "prediction", outcome = m[2], predictors = m[3]))
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:together\\s+|jointly\\s+|significantly\\s+)*predicts?\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 3 && grepl(",|\\band\\b|as well as", m[2])) return(list(type = "prediction", outcome = m[3], predictors = qc_strip_lead(m[2])))
  # relationship: "(positive) association between A and B"
  m <- regmatches(t, regexec("(?i)(positive|negative|significant)?\\s*(?:association|correlation|relationship|link)\\s+between\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 3) return(list(type = "relationship", pair = m[3], direction = dirw(tolower(m[2])), split_pair = TRUE))
  # "A is/are (positively) associated with B"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:is|are)\\s+(?:significantly\\s+)?(positively|negatively)?\\s*(?:and\\s+significantly\\s+)?(?:associated|correlated|related|linked)\\s+(?:with|to)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) return(list(type = "relationship", a = qc_strip_lead(m[2]), b = m[4], direction = dirw(tolower(m[3]))))
  # "the higher A, the lower B"
  m <- regmatches(t, regexec("(?i)the\\s+(higher|more|greater|lower|less)\\s+(.*?),?\\s+the\\s+(higher|more|greater|lower|less|better|worse)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 5) { s1 <- tolower(m[2]) %in% c("higher", "more", "greater"); s2 <- tolower(m[4]) %in% c("higher", "more", "greater", "better")
    return(list(type = "relationship", a = m[3], b = m[5], direction = if (s1 == s2) "positive" else "negative")) }
  # "A increases/decreases/predicts B"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(increases?|raises?|improves?|decreases?|reduces?|lowers?|predicts?|affects?|influences?|impacts?)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) { v <- tolower(m[3])
    return(list(type = "relationship", a = qc_strip_lead(m[2]), b = m[4], direction = if (grepl("^(increase|raise|improve)", v)) "positive" else if (grepl("^(decrease|reduce|lower)", v)) "negative" else NA)) }
  list(type = "unknown")
}

# Split "A and B" when the halves stand for two different variables better than the whole phrase does
qc_split_compound <- function(phrase, cat) {
  parts <- strsplit(phrase, "\\s+(?:and|as well as|&)\\s+", perl = TRUE)[[1]]
  if (length(parts) < 2) return(list(phrase))
  # resolve "it/them" in later parts with the main words of the first part
  key <- paste(setdiff(qc_concepts(parts[1]), c("objective", "subjective")), collapse = " ")
  parts <- c(parts[1], vapply(parts[-1], function(p) if (grepl("\\b(it|them|this|that)\\b", p, ignore.case = TRUE)) paste(p, key) else p, ""))
  whole <- qc_match_variable(phrase, cat, top = 1); mp <- lapply(parts, qc_match_variable, cat = cat, top = 1)
  ok <- all(vapply(mp, function(x) nrow(x) > 0 && x$score[1] >= .55, logical(1)))
  distinct <- ok && length(unique(vapply(mp, function(x) x$variable[1], ""))) == length(parts)
  if (distinct && (nrow(whole) == 0 || whole$score[1] < max(vapply(mp, function(x) x$score[1], 0)) + .05)) as.list(parts) else list(phrase)
}

# Which side of a two-category variable is the "present" state named in the phrase (e.g. holding debt -> Yes)?
qc_present_level <- function(lv) {
  yes <- which(grepl("(?i)^\\s*(yes|ja|true|present|have|has)\\b", lv, perl = TRUE)); no <- which(grepl("(?i)^\\s*(no|nein|false|none|not)\\b", lv, perl = TRUE))
  if (length(yes) == 1 && length(no) == 1) list(present = names(lv)[yes], absent = names(lv)[no]) else NULL
}

# ---------------------------------------------------------------- build the breakdown
qc_dissect <- function(id, text, data, cat) {
  p <- qc_parse_hypothesis(text)
  parts <- list(); questions <- character()
  pick <- function(phrase, want = "any") { m <- qc_match_variable(phrase, cat, want)
    list(phrase = phrase, best = if (nrow(m)) m$variable[1] else NA, score = if (nrow(m)) m$score[1] else 0,
         alternatives = if (nrow(m)) m$variable else character(), weak = !nrow(m) || m$score[1] < .5) }
  ask_weak <- function(x, role) if (x$weak) questions <<- c(questions, sprintf("No variable clearly matches '%s' (%s). Choose it from the list, or build it in Prepare (scales or groups).", x$phrase, role))
  lab <- function(v) { r <- cat[cat$variable == v, ]; if (nrow(r)) r$label[1] else v }
  if (p$type == "relationship") {
    if (isTRUE(p$split_pair)) {        # "between A and B": try every 'and' split, keep the best-matching one
      sp <- gregexpr("\\s+and\\s+", p$pair)[[1]]; best <- NULL
      for (k in seq_along(sp)) { a <- substr(p$pair, 1, sp[k] - 1); b <- substring(p$pair, sp[k] + attr(sp, "match.length")[k])
        sc <- sum(pick(a)$score, pick(b)$score); if (is.null(best) || sc > best$sc) best <- list(a = a, b = b, sc = sc) }
      if (is.null(best)) return(list(id = id, text = text, type = "unknown", parts = list(), questions = "The two constructs could not be separated."))
      p$a <- best$a; p$b <- best$b
    }
    subjects <- qc_split_compound(p$a, cat); objects <- qc_split_compound(p$b, cat)
    for (sa in subjects) for (ob in objects) {
      a <- pick(sa); b <- pick(ob); ask_weak(a, "first construct"); ask_weak(b, "second construct")
      spec <- list(goal = "relationship", var1 = a$best, var2 = b$best, direction = p$direction)
      ka <- cat$kind[cat$variable %in% a$best]; kb <- cat$kind[cat$variable %in% b$best]
      # a two-category variable: the relationship is a group comparison; direction becomes "which group is higher"
      if (length(ka) && ka %in% c("binary", "categorical") && !(length(kb) && kb %in% c("binary", "categorical"))) {
        lv <- cat$levels[[which(cat$variable == a$best)]]; pr <- qc_present_level(lv)
        spec <- list(goal = "difference", outcome = b$best, group = a$best)
        if (!is.null(pr) && !is.na(p$direction)) spec$expect_higher <- if (p$direction == "positive") pr$present else pr$absent
        else if (!is.na(p$direction)) questions <- c(questions, sprintf("'%s' has categories, so the relationship is tested as a group difference. Which group do you expect to be higher?", a$phrase))
      }
      parts[[length(parts) + 1]] <- list(spec = spec, phrases = list(a = a, b = b))
    }
  } else if (p$type == "difference") {
    y <- pick(p$outcome, "numeric"); ask_weak(y, "outcome")
    gm <- qc_match_groups(p$g1, p$g2, cat)
    spec <- list(goal = "difference", outcome = y$best)
    if (is.null(gm) || gm$score < .45) {
      questions <- c(questions, sprintf("No grouping variable has categories that clearly match '%s' and '%s'. Build this group in Prepare > Groups, then come back.", p$g1, p$g2))
      spec$group <- if (!is.null(gm)) gm$variable else NA
    } else {
      spec$group <- gm$variable
      if (gm$n_levels > 2) spec$compare <- c(gm$level1, gm$level2)
      else spec$compare <- c(gm$level1, gm$level2)     # sets the order: group 1 = first group named
      if (!is.na(p$higher)) spec$expect_higher <- if (p$higher == 1) gm$level1 else gm$level2
    }
    parts[[1]] <- list(spec = spec, phrases = list(outcome = y, g1 = p$g1, g2 = p$g2, groups = gm))
  } else if (p$type == "mediation") {
    x <- pick(p$x); mm <- pick(p$mediator); y <- pick(p$y)
    ask_weak(x, "predictor"); ask_weak(mm, "mediator"); ask_weak(y, "outcome")
    parts[[1]] <- list(spec = list(goal = "mediation", var1 = x$best, mediator = mm$best, outcome = y$best, direction = p$direction),
                       phrases = list(x = x, m = mm, y = y))
  } else if (p$type == "prediction") {
    y <- pick(p$outcome, "numeric"); ask_weak(y, "outcome")
    prs <- trimws(strsplit(p$predictors, "\\s*,\\s*|\\s+and\\s+|\\s+as well as\\s+", perl = TRUE)[[1]]); prs <- prs[nzchar(prs)]
    px <- lapply(prs, pick); for (x in px) ask_weak(x, "predictor")
    pv <- unique(stats::na.omit(vapply(px, `[[`, "", "best"))); pv <- setdiff(pv, y$best)
    parts[[1]] <- list(spec = list(goal = "prediction", outcome = y$best, predictors = pv), phrases = c(list(outcome = y), stats::setNames(px, paste0("predictor", seq_along(px)))))
  } else if (p$type == "reliability") {
    sc <- if (is.null(scales_for_dissect())) list() else scales_for_dissect()
    nm <- vapply(sc, `[[`, "", "name"); lb <- vapply(sc, function(s) if (is.null(s$label)) "" else s$label, "")
    sco <- if (length(sc)) mapply(function(a, b) qc_match_score(p$scale, b, "", a), nm, lb) else numeric()
    if (length(sco) && max(sco) >= .5) parts[[1]] <- list(spec = qc_reliability_spec(sc[[which.max(sco)]]), phrases = list())
    else { questions <- c(questions, sprintf("No scale built in Prepare matches '%s'. Build the scale first (Prepare > Scales), or choose the items below.", p$scale))
      parts[[1]] <- list(spec = list(goal = "reliability", items = character()), phrases = list()) }
  } else questions <- c(questions, "The type of this hypothesis could not be recognised. Choose it below (relationship, group difference or mediation).")
  # labels and expected outcomes
  for (i in seq_along(parts)) {
    sp <- parts[[i]]$spec
    parts[[i]]$id <- if (length(parts) > 1) sprintf("%s(%s)", id, c("i", "ii", "iii", "iv")[i]) else id
    parts[[i]]$expected <- qc_expected_text(sp, cat)
  }
  list(id = id, text = text, type = p$type, direction = p$direction, compound = length(parts) > 1, parts = parts, questions = unique(questions))
}

qc_expected_text <- function(sp, cat) {
  L <- function(v) { if (is.null(v) || is.na(v)) return("?"); r <- cat[cat$variable == v, ]; if (nrow(r)) r$label[1] else v }
  LV <- function(v, k) { r <- cat[cat$variable == v, ]; if (nrow(r) && length(r$levels[[1]]) && k %in% names(r$levels[[1]])) r$levels[[1]][[k]] else k }
  switch(sp$goal,
    relationship = if (is.null(sp$direction) || is.na(sp$direction)) sprintf("Expected: %s and %s are related.", L(sp$var1), L(sp$var2))
      else sprintf("Expected: the higher %s, the %s %s (%s correlation).", L(sp$var1), if (sp$direction == "positive") "higher" else "lower", L(sp$var2), sp$direction),
    difference = if (!is.null(sp$expect_higher)) { other <- setdiff(if (!is.null(sp$compare)) sp$compare else names(cat$levels[[which(cat$variable == sp$group)]]), sp$expect_higher)[1]
        sprintf("Expected: %s is higher for '%s' than for '%s'.", L(sp$outcome), LV(sp$group, sp$expect_higher), LV(sp$group, other)) }
      else sprintf("Expected: %s differs between the groups of %s.", L(sp$outcome), L(sp$group)),
    prediction = sprintf("Expected: %s %s %s.", qc_and(vapply(sp$predictors, L, "")), if (length(sp$predictors) > 1) "together predict" else "predicts", L(sp$outcome)),
    reliability = sprintf("Expected: the %d items form a reliable scale (Cronbach's alpha of at least .70).", length(sp$items)),
    mediation = sprintf("Expected: %s affects %s, which in turn affects %s (indirect effect%s).", L(sp$var1), L(sp$mediator), L(sp$outcome),
                        if (!is.null(sp$direction) && !is.na(sp$direction)) paste0(", ", sp$direction) else ""),
    "")
}

# ---------------------------------------------------------------- test options for a confirmed part
qc_test_assumptions <- c(
  ttest_independent = "Numeric outcome; roughly normal in each group, or at least 30 per group; no extreme outliers. Equal variances are not needed (Welch).",
  mann_whitney = "Ordinal or numeric outcome; independent groups. Compares whether one group tends to have higher values (ranks).",
  anova_oneway = "Numeric outcome; roughly normal in each group or at least 30 per group. Equal variances are not needed (Welch).",
  kruskal_wallis = "Ordinal or numeric outcome; independent groups; compares ranks across all groups.",
  correlation_pearson = "Both numeric; straight-line relationship; roughly normal; no extreme outliers.",
  correlation_spearman = "Ordinal or numeric; the relationship only needs to go consistently up or down (monotonic).",
  chisq_independence = "Two categorical variables; expected counts of at least 5 in most cells (otherwise Fisher's exact test).",
  regression_linear = "Numeric outcome; linear relationships; constant residual spread (otherwise robust standard errors).",
  mediation = "Numeric predictor, mediator and outcome; linear paths. The indirect effect is tested with a bootstrap interval.",
  regression_logistic = "Two-category outcome; enough cases in the smaller category (10 per predictor).",
  reliability_alpha = "Items measure the same construct and are coded in the same direction.")

qc_part_options <- function(spec, data) {
  if (any(is.na(unlist(spec[intersect(names(spec), c("var1", "var2", "outcome", "group", "mediator"))])))) return(list())
  if (spec$goal == "reliability" && length(spec$items) < 2) return(list())
  if (spec$goal == "prediction" && !length(spec$predictors)) return(list())
  dd <- data
  if (!is.null(spec$compare) && !is.null(spec$group)) { dd <- dd[as.character(dd[[spec$group]]) %in% as.character(spec$compare), , drop = FALSE]
    dd[[spec$group]] <- factor(as.character(dd[[spec$group]]), levels = as.character(spec$compare)) }
  sc <- tryCatch(qc_screen(dd, spec), error = function(e) NULL); if (is.null(sc)) return(list())
  rec <- sc$decision$test_id
  opts <- list(list(test_id = rec, label = qc_test_labels[[rec]], role = "Recommended", why = sc$decision$reason, assumptions = qc_test_assumptions[[rec]],
                    choice = "auto", spec = spec))
  if (rec %in% names(qc_alternatives)) { alt <- qc_alternatives[[rec]]
    opts[[2]] <- list(test_id = alt, label = qc_test_labels[[alt]], role = "Alternative",
      why = if (alt %in% qc_parametric_ids) "Parametric version: uses means and assumes the conditions listed. Choose it only if you can justify those assumptions." else "Rank-based version: makes fewer assumptions about the shape of the data.",
      assumptions = qc_test_assumptions[[alt]], choice = if (alt %in% qc_parametric_ids) "parametric" else "non-parametric", spec = spec) }
  if (spec$goal == "difference" && !is.null(spec$compare) && length(unique(stats::na.omit(data[[spec$group]]))) > 2) {
    s2 <- spec; s2$compare <- NULL; s2$expect_higher <- NULL
    opts[[length(opts) + 1]] <- list(test_id = "anova_oneway", label = "Compare all groups (Welch's ANOVA or Kruskal-Wallis)", role = "Broader option",
      why = "Tests all categories of the group variable at once, with post-hoc comparisons. Use it if the hypothesis is about all groups, not only the two named.",
      assumptions = qc_test_assumptions[["anova_oneway"]], choice = "auto", spec = s2)
  }
  opts
}

# Break down several hypotheses at once. A mediation hypothesis without a stated direction takes it
# from the component hypotheses (X -> M and M -> Y) when both are in the list: sign = product of the two.
qc_dissect_all <- function(hyps, data, cat) {
  out <- lapply(names(hyps), function(id) qc_dissect(id, hyps[[id]], data, cat)); names(out) <- names(hyps)
  rel <- list()
  for (b in out) for (pp in b$parts) if (pp$spec$goal == "relationship" && !is.null(pp$spec$direction) && !is.na(pp$spec$direction))
    rel[[length(rel) + 1]] <- list(a = pp$spec$var1, b = pp$spec$var2, sign = if (pp$spec$direction == "positive") 1 else -1, id = pp$id)
  find <- function(a, b) { for (r in rel) if ((r$a == a && r$b == b) || (r$a == b && r$b == a)) return(r); NULL }
  for (k in seq_along(out)) for (i in seq_along(out[[k]]$parts)) {
    sp <- out[[k]]$parts[[i]]$spec
    if (sp$goal != "mediation" || (!is.null(sp$direction) && !is.na(sp$direction)) || anyNA(c(sp$var1, sp$mediator, sp$outcome))) next
    r1 <- find(sp$var1, sp$mediator); r2 <- find(sp$mediator, sp$outcome)
    if (!is.null(r1) && !is.null(r2)) {
      sp$direction <- if (r1$sign * r2$sign > 0) "positive" else "negative"
      out[[k]]$parts[[i]]$spec <- sp
      out[[k]]$parts[[i]]$expected <- paste(qc_expected_text(sp, cat), sprintf("Direction taken from %s and %s.", r1$id, r2$id))
    }
  }
  out
}

# ---------------------------------------------------------------- multiple regression planner
# Suggests which variables can be the outcome (dependent variable) and which can be predictors.
# Predictors should come from theory and hypotheses: the correlation with the outcome is shown
# for information only and is never used to pick predictors automatically (that would be data dredging).
qc_scale_of <- function(v, scales) { for (s in scales) if (v %in% c(s$items, paste0(s$items, "_r"))) return(s$name); NA_character_ }

qc_mlr_outcomes <- function(data, cat, scales = list()) {
  rows <- lapply(seq_len(nrow(cat)), function(i) {
    v <- cat$variable[i]; k <- cat$kind[i]; n <- cat$n[i]; x <- data[[v]]; nl <- length(unique(stats::na.omit(x)))
    sc <- qc_scale_of(v, scales)
    st <- if (!is.na(sc)) c("no", sprintf("Item of the scale %s - use the scale score instead.", sc))
      else if (k == "numeric" && cat$source[i] %in% c("scale")) c("ok", "Scale score: the best kind of regression outcome.")
      else if (k == "numeric") c("ok", "Numeric variable.")
      else if (k == "ordinal" && nl >= 5) c("warn", sprintf("Single rating item with %d levels; usable, but a multi-item scale is a better outcome.", nl))
      else if (k == "binary") c("warn", "Two categories: QuantCompass will use logistic regression instead of linear regression.")
      else c("no", if (k == "categorical") "Several categories without an order: not a regression outcome." else sprintf("Only %d levels: too few for linear regression.", nl))
    if (st[1] != "no" && n < 30) st <- c("no", sprintf("Only %d answers.", n))
    data.frame(variable = v, label = cat$label[i], kind = k, n = n, status = st[1], reason = st[2], rank = c(ok = 1, warn = 2, no = 3)[[st[1]]] - .5 * (cat$source[i] == "scale"))
  })
  out <- do.call(rbind, rows); out[order(out$rank, -out$n), setdiff(names(out), "rank")]
}

# Do the category labels of a coded variable have an order (agree...disagree, A1...C2, grades)?
qc_ordered_labels <- function(lv) {
  if (length(lv) < 3) return(TRUE)
  ord <- "(?i)agree|disagree|never|always|often|rarely|sometimes|seldom|very|not at all|extremely|slightly|moderately|somewhat|poor|fair|good|excellent|satisf|less than|more than|up to|at least|[0-9]|beginner|intermediate|advanced|fluent|native|^\\s*[abc][12]\\b|low|medium|high|none|little|much|fail|pass"
  mean(grepl(ord, lv, perl = TRUE)) >= .5
}

qc_mlr_predictors <- function(data, cat, outcome, scales = list()) {
  y <- data[[outcome]]; n_y <- sum(!is.na(y)); out_sc <- qc_scale_of(outcome, scales)
  out_items <- if (outcome %in% vapply(scales, `[[`, "", "name")) unlist(lapply(Filter(function(s) s$name == outcome, scales), `[[`, "items")) else character()
  rows <- lapply(seq_len(nrow(cat)), function(i) {
    v <- cat$variable[i]; if (v == outcome) return(NULL)
    x <- data[[v]]; k <- cat$kind[i]; ok <- !is.na(x) & !is.na(y); n_pair <- sum(ok)
    nlev <- length(unique(x[ok])); top <- if (n_pair) max(table(x[ok])) / n_pair else 1
    terms <- if (k %in% c("categorical") || is.factor(x) || is.character(x)) max(1, nlev - 1) else 1
    assoc <- if (n_pair < 5 || nlev < 2) NA else if (terms == 1 && is.numeric(x)) stats::cor(x[ok], as.numeric(y[ok]))
      else sqrt(summary(stats::lm(as.numeric(y[ok]) ~ factor(x[ok])))$r.squared)
    sc <- qc_scale_of(v, scales)
    st <- if (v %in% out_items || (!is.na(out_sc) && identical(sc, out_sc))) c("no", "Part of the outcome scale: it would predict itself.")
      else if (!is.na(sc)) c("no", sprintf("Item of the scale %s - use the scale score instead.", sc))
      else if (nlev < 2) c("no", "Has only one value among these cases.")
      else if (terms == 1 && k %in% c("ordinal", "categorical") && length(cat$levels[[i]]) >= 3 && !qc_ordered_labels(cat$levels[[i]]))
        c("warn", sprintf("Coded categories without an order (%s ...). As a predictor it would be treated as a number; build a group variable (Prepare > Groups) instead.",
                          paste(utils::head(sprintf("%s = %s", names(cat$levels[[i]]), cat$levels[[i]]), 2), collapse = ", ")))
      else if (top > .9) c("warn", sprintf("%.0f%% of cases share one value: little information.", 100 * top))
      else if (n_pair < .8 * n_y) c("warn", sprintf("Answered by only %d of %d cases (a filtered question); using it drops the others.", n_pair, n_y))
      else if (terms > 5) c("warn", sprintf("%d categories become %d dummy terms; consider merging categories.", nlev, terms))
      else c("ok", if (terms > 1) sprintf("Categorical: enters as %d dummy terms.", terms) else if (k == "binary" || is.factor(x) || is.character(x)) "Two categories: enters as one 0/1 term." else "Numeric.")
    data.frame(variable = v, label = cat$label[i], kind = k, n_with_outcome = n_pair, terms = terms,
               association = if (is.na(assoc)) NA else round(assoc, 2), status = st[1], reason = st[2])
  })
  out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  out[order(match(out$status, c("ok", "warn", "no")), -abs(ifelse(is.na(out$association), 0, out$association))), ]
}

# Checks a chosen set of predictors: sample size (Green, 1991: N >= 50 + 8m), multicollinearity (VIF, |r| > .80)
qc_mlr_check <- function(data, outcome, predictors) {
  if (!length(predictors)) return(list(verdict = "Choose at least one predictor.", status = "warn"))
  d <- qc_complete(data, c(outcome, predictors))$data; n <- nrow(d)
  terms <- sum(vapply(predictors, function(v) if (is.numeric(d[[v]])) 1 else max(1, length(unique(d[[v]])) - 1), numeric(1)))
  need <- 50 + 8 * terms; max_terms <- max(0, floor((n - 50) / 8))
  vif <- NULL; pairs <- character()
  if (n > terms + 2) {
    num <- predictors[vapply(d[predictors], is.numeric, logical(1))]
    if (length(num) >= 2) { r <- stats::cor(d[num]); idx <- which(abs(r) > .8 & upper.tri(r), arr.ind = TRUE)
      pairs <- if (nrow(idx)) sprintf("%s and %s (r = %s)", num[idx[, 1]], num[idx[, 2]], qc_fmt_r(r[idx])) else character()
      vif <- vapply(num, function(v) { o <- setdiff(num, v); 1 / (1 - summary(stats::lm(stats::reformulate(sprintf("`%s`", o), sprintf("`%s`", v)), d))$r.squared) }, numeric(1)) }
  }
  msgs <- c(sprintf("%d complete cases for %d predictor term(s). Recommended minimum: %d (50 + 8 x %d; Green, 1991). With %d cases, up to %d terms are supported.", n, terms, need, terms, n, max_terms),
    if (length(pairs)) sprintf("Very highly correlated predictors: %s. Keep only one of each pair.", paste(pairs, collapse = "; ")),
    if (!is.null(vif) && any(vif > 5)) sprintf("High VIF (above 5): %s.", paste(sprintf("%s = %.1f", names(vif)[vif > 5], vif[vif > 5]), collapse = ", ")))
  status <- if (n <= terms + 1) "no" else if (n < need || length(pairs) || (!is.null(vif) && any(vif > 5))) "warn" else "ok"
  list(n = n, terms = terms, need = need, max_terms = max_terms, vif = vif, status = status, verdict = paste(msgs, collapse = " "))
}

# Reliability specs for built scales (items as entered, reversed items passed to the template)
qc_reliability_spec <- function(s) list(goal = "reliability", items = s$items, reverse = intersect(s$reverse, s$items), scale_min = s$min, scale_max = s$max)

# Scales known to the breakdown (set by the app from the confirmed preparation)
qc_dissect_scales <- new.env()
scales_for_dissect <- function() qc_dissect_scales$scales
