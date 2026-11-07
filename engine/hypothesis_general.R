# General hypothesis understanding, layered on top of dissect.R.
#   1. qc_hyp_normalise() rewrites the many ways people phrase a hypothesis into a few canonical forms
#      ("X and Y are correlated", "X will be ...", "X has a negative effect on Y", "associated with lower Y").
#   2. qc_parse_hypothesis() adds patterns for moderation, mediation "through", comparatives ("lonelier",
#      "older students sleep less"), "more likely to", "students who ... have higher ..." and then falls back
#      to the sentence patterns of dissect.R.
#   3. qc_dissect() adds what needs the variables themselves: before/after pairs of the same measure
#      (paired comparison), moderation roles, two category variables (chi-square), a fallback that finds
#      variable names and labels anywhere in the sentence, and a confidence level for every breakdown.
# Everything stays rule-based and visible; the user confirms every part.

# ---------------------------------------------------------------- 1. canonical wording
qc_hyp_normalise <- function(text) {
  t <- gsub("\\s+", " ", trimws(text))
  t <- sub("^[*•–-]+\\s*", "", t)
  t <- sub("(?i)^H[0-9]+[a-z]?[₀-₉]*\\s*[:.)-]\\s*", "", t, perl = TRUE)
  t <- gsub("–|—", "-", gsub("[‘’]", "'", t))
  t <- sub("(?i)^(?:it is (?:expected|hypothesi[sz]ed|predicted|assumed|anticipated) that|(?:we|i) (?:expect|hypothesi[sz]e|predict|assume|anticipate)(?: that)?|hypothesis:?)\\s+", "", t, perl = TRUE)
  t <- gsub("(?i)\\b(?:statistically\\s+)?significant(?:ly)?\\s+", "", t, perl = TRUE)
  t <- gsub("(?i)\\bwill be\\b", "is", t, perl = TRUE)
  t <- gsub("(?i)\\b(?:will|would|should|may|might|tend to|tends to)\\s+(?=[a-z])", "", t, perl = TRUE)
  t <- gsub("(?i)\\bthe (?:proportion|percentage|share|rate|likelihood|probability) of\\s+", "", t, perl = TRUE)
  t <- gsub("(?i)\\bthere (?:is|exists) (?:a |an )?", "there is a ", t, perl = TRUE)
  # "X and Y are (positively) correlated (with each other)" -> "there is a (positive) association between X and Y"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+and\\s+(.*?)\\s+(?:are|is)\\s+(positively|negatively)?\\s*(?:correlated|related|associated|linked|connected)(?:\\s+(?:with|to) (?:each other|one another))?\\s*[.;]?$", t, perl = TRUE))[[1]]
  if (length(m) == 4) t <- sprintf("there is a %s association between %s and %s", sub("ly$", "", tolower(m[4])), m[2], m[3])
  # "X correlates (positively) with Y" -> "X is (positively) correlated with Y"
  t <- sub("(?i)^(.*?)\\s+(?:correlates|correlate|covaries|covary)\\s+(positively|negatively)?\\s*with\\s+", "\\1 is \\2 correlated with ", t, perl = TRUE)
  t <- sub("(?i)^(.*?)\\s+(positively|negatively)\\s+(?:correlates|correlate|relates|relate)\\s+(?:with|to)\\s+", "\\1 is \\2 correlated with ", t, perl = TRUE)
  t <- gsub("\\s+", " ", t)
  trimws(sub("[.;]\\s*$", "", t))
}

# sign of a leading comparative ("higher stress" +1, "lower sleep" -1); the word is removed
qc_polarity <- function(x) {
  up <- "higher|greater|more|increased|increasing|better|stronger|larger|bigger|longer|elevated|high"
  dn <- "lower|less|fewer|reduced|decreased|decreasing|worse|weaker|smaller|shorter|poorer|low|diminished"
  x <- trimws(x)
  m <- regmatches(x, regexec(sprintf("(?i)^(?:a |an |the )?(%s|%s)\\s+(?:levels? of\\s+|amounts? of\\s+|degrees? of\\s+)?(.*)$", up, dn), x, perl = TRUE))[[1]]
  if (length(m) == 3) return(list(sign = if (grepl(sprintf("(?i)^(%s)$", up), m[2], perl = TRUE)) 1 else -1, text = m[3]))
  m <- regmatches(x, regexec(sprintf("(?i)^(.*?)\\s+(%s)$", "more|less|longer|shorter|better|worse|fewer"), x, perl = TRUE))[[1]]   # "sleep more"
  if (length(m) == 3) return(list(sign = if (tolower(m[3]) %in% c("more", "longer", "better")) 1 else -1, text = m[2]))
  list(sign = NA, text = x)
}
qc_dir_of <- function(s1, s2) { s1 <- if (is.na(s1)) 1 else s1; if (is.na(s2)) NA_character_ else if (s1 * s2 > 0) "positive" else "negative" }

# comparative adjectives that name a construct: "older" -> age (+), "lonelier" -> lonely (+)
qc_comparative <- function(w) {
  w <- tolower(w)
  fixed <- list(older = c("age", 1), younger = c("age", -1), richer = c("income", 1), poorer = c("income", -1), wealthier = c("income", 1),
    healthier = c("health", 1), happier = c("happiness", 1), busier = c("workload", 1), heavier = c("weight", 1), lighter = c("weight", -1),
    taller = c("height", 1), shorter = c("height", -1))
  if (!is.null(fixed[[w]])) return(list(text = fixed[[w]][1], sign = as.numeric(fixed[[w]][2])))
  if (grepl("ier$", w) && nchar(w) > 5) return(list(text = sub("ier$", "y", w), sign = 1))
  if (grepl("[^e]er$", w) && nchar(w) > 5 && !w %in% c("other", "either", "neither", "whether", "under", "after", "never", "rather", "together", "number", "member", "gender", "however", "power", "paper", "teacher", "worker", "manager", "partner", "leader", "parter"))
    return(list(text = sub("er$", "", w), sign = 1))
  NULL
}

qc_group_nouns <- "students|student|people|persons|employees|workers|participants|respondents|individuals|children|adults|adolescents|patients|men|women|those|staff|managers|teachers|users|customers|learners|pupils"
qc_rel_nouns <- "relationship|association|link|effect|correlation|impact|influence|connection|relation"

# ---------------------------------------------------------------- 2. sentence patterns
qc_parse_hypothesis_base <- if (exists("qc_parse_hypothesis_base")) qc_parse_hypothesis_base else qc_parse_hypothesis

qc_parse_hypothesis <- function(text) {
  t <- qc_hyp_normalise(text); lt <- tolower(t)
  dirw <- function(s) if (grepl("(?i)negativ|inverse", s, perl = TRUE)) "negative" else if (grepl("(?i)positiv", s, perl = TRUE)) "positive" else NA_character_
  extra <- list(among = NA_character_, controls = NA_character_, trend = NA_character_, extreme = NULL)
  REL <- sprintf("(?:the\\s+)?(?:(positive|negative|inverse)\\s+)?(?:%s)", qc_rel_nouns)
  XY <- "(?:between\\s+(.*?)\\s+and\\s+(.*?)|of\\s+(.*?)\\s+(?:on|with)\\s+(.*?))"
  xy <- function(m, i) { x <- if (nzchar(m[i])) m[i] else m[i + 2]; y <- if (nzchar(m[i])) m[i + 1] else m[i + 3]; list(x = trimws(x), y = trimws(y)) }
  mod <- function(x, y, w, dir = NA, stronger = NULL) c(list(type = "moderation", x = x, y = y, w = qc_strip_lead(w), direction = dir, stronger = stronger), extra)
  # ---- moderation
  m <- regmatches(t, regexec(sprintf("(?i)^(.*?)\\s+(?:moderates?|conditions?)\\s+%s\\s+%s$", REL, XY), t, perl = TRUE))[[1]]
  if (length(m) == 7) { p <- xy(m, 4); return(mod(p$x, p$y, m[2], dirw(m[3]))) }
  m <- regmatches(t, regexec(sprintf("(?i)^%s\\s+%s\\s+(?:is|are)\\s+moderated\\s+by\\s+(.*)$", REL, XY), t, perl = TRUE))[[1]]
  if (length(m) == 7) { p <- xy(m, 3); return(mod(p$x, p$y, m[7], dirw(m[2]))) }
  m <- regmatches(t, regexec(sprintf("(?i)^%s\\s+%s\\s+(?:depends?\\s+on|varies\\s+(?:by|with|across|depending\\s+on)|differs?\\s+(?:by|across|depending\\s+on)|is\\s+conditional\\s+on|changes\\s+with)\\s+(.*)$", REL, XY), t, perl = TRUE))[[1]]
  if (length(m) == 7) { p <- xy(m, 3); return(mod(p$x, p$y, m[7], dirw(m[2]))) }
  m <- regmatches(t, regexec(sprintf("(?i)^(.*?)\\s+(buffers?|attenuates?|weakens?|reduces?|dampens?|mitigates?|lessens?|protects? against|strengthens?|amplifies|amplify|intensif(?:y|ies)|exacerbates?|increases?|enhances?|magnif(?:y|ies))\\s+%s\\s+%s$", REL, XY), t, perl = TRUE))[[1]]
  if (length(m) == 8) { p <- xy(m, 5); weak <- grepl("(?i)^(buffer|attenuat|weaken|reduc|dampen|mitigat|lessen|protect)", m[3], perl = TRUE)
    return(mod(p$x, p$y, m[2], dirw(m[4]), list(at = if (weak) "low" else "high", how = tolower(m[3])))) }
  m <- regmatches(t, regexec(sprintf("(?i)^%s\\s+%s\\s+(?:is|are|becomes?)\\s+(stronger|weaker|larger|smaller|more pronounced|less pronounced)\\s+(?:for|among|in|when|at|with|amongst)\\s+(.*?)(?:\\s+than\\s+(?:for|among|in|when|at|with)?\\s*(.*))?$", REL, XY), t, perl = TRUE))[[1]]
  if (length(m) == 9) { p <- xy(m, 3); up <- grepl("(?i)stronger|larger|more", m[7], perl = TRUE)
    return(mod(p$x, p$y, m[8], dirw(m[2]), list(phrase = m[8], other = m[9], up = up))) }
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+interacts?\\s+with\\s+(.*?)\\s+(?:in\\s+)?(?:predicting|to predict|on|in explaining)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) return(mod(qc_strip_lead(m[2]), m[4], m[3]))
  # ---- mediation: "X affects Y through M", "M explains the relationship between X and Y"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:affects?|influences?|increases?|decreases?|reduces?|raises?|lowers?|improves?|predicts?|leads? to|is (?:related|linked|associated) (?:to|with)|impacts?|has an? (?:positive |negative )?(?:effect|impact|influence) on)\\s+(.*?)\\s+(?:indirectly\\s+)?(?:through|via|by way of|by means of|by increasing|by reducing|by lowering|by raising)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) { mm <- qc_polarity(m[4])$text
    return(c(list(type = "mediation", x = qc_strip_lead(m[2]), y = m[3], mediator = mm, direction = dirw(lt)), extra)) }
  m <- regmatches(t, regexec(sprintf("(?i)^(.*?)\\s+(?:explains|accounts for|underlies|carries)\\s+%s\\s+%s$", REL, XY), t, perl = TRUE))[[1]]
  if (length(m) == 7) { p <- xy(m, 4); return(c(list(type = "mediation", x = p$x, y = p$y, mediator = qc_strip_lead(m[2]), direction = dirw(m[3])), extra)) }
  m <- regmatches(t, regexec(sprintf("(?i)^%s\\s+%s\\s+is\\s+(?:explained|carried|transmitted)\\s+(?:in part\\s+|partly\\s+)?(?:by|through|via)\\s+(.*)$", REL, XY), t, perl = TRUE))[[1]]
  if (length(m) == 7) { p <- xy(m, 3); return(c(list(type = "mediation", x = p$x, y = p$y, mediator = m[7], direction = dirw(m[2])), extra)) }
  # ---- "X has a (positive) effect on Y" / "X positively affects Y"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:has|have|exerts?)\\s+(?:a |an )?(?:(positive|negative|beneficial|detrimental|adverse|harmful)\\s+)?(?:direct\\s+)?(?:effect|impact|influence)\\s+(?:on|upon)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) { d <- tolower(m[3]); d <- if (d %in% c("positive", "beneficial")) "positive" else if (d %in% c("negative", "detrimental", "adverse", "harmful")) "negative" else NA
    return(c(list(type = "relationship", a = qc_strip_lead(m[2]), b = m[4], direction = d, causal = TRUE), extra)) }
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(positively|negatively)\\s+(affects?|influences?|impacts?|predicts?|relates? to|is related to|are related to)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 5) { if (grepl("(?i)predict", m[4], perl = TRUE)) return(c(list(type = "prediction", outcome = m[5], predictors = qc_strip_lead(m[2]), directions = sub("ly$", "", tolower(m[3]))), extra))
    return(c(list(type = "relationship", a = qc_strip_lead(m[2]), b = m[5], direction = sub("ly$", "", tolower(m[3]))), extra)) }
  # ---- "A are more/less likely to B"
  m <- regmatches(t, regexec("(?i)^(.*?)\\s+(?:are|is)\\s+(more|less)\\s+likely\\s+(?:to\\s+(?:be\\s+|have\\s+|report\\s+|experience\\s+)?|than\\s+.*?\\s+to\\s+)(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 4) return(c(list(type = "relationship", a = qc_strip_lead(m[2]), b = m[4], direction = if (tolower(m[3]) == "more") "positive" else "negative"), extra))
  # ---- "the longer/older/more A, the higher B" (any comparative)
  CMP <- "higher|more|greater|lower|less|longer|shorter|older|younger|bigger|smaller|better|worse|fewer|stronger|weaker"
  m <- regmatches(t, regexec(sprintf("(?i)^the\\s+(%s)\\s+(.*?),?\\s+the\\s+(%s)\\s+(.*)$", CMP, CMP), t, perl = TRUE))[[1]]
  if (length(m) == 5) { up <- c("higher", "more", "greater", "longer", "older", "bigger", "better", "stronger")
    a <- m[3]; if (tolower(m[2]) %in% c("older", "younger") && !grepl("(?i)\\bage\\b", a, perl = TRUE)) a <- paste("age", a)
    a <- sub(sprintf("(?i)^(?:%s)\\s+(?:stay|work|have been|are|is)\\b", qc_group_nouns), "", a, perl = TRUE)
    b <- sub("(?i)^(?:their|his|her|its|the)\\s+", "", m[5], perl = TRUE); b <- sub("(?i)\\s+(?:they|he|she|it)\\s+(?:are|is|get|become|feel)$", "", b, perl = TRUE)
    return(c(list(type = "relationship", a = a, b = b, direction = if ((tolower(m[2]) %in% up) == (tolower(m[4]) %in% up)) "positive" else "negative"), extra)) }
  # ---- "students who sleep more have a higher GPA" / "students with higher mindfulness experience less burnout"
  VB <- "have|has|report|reports|show|shows|experience|experiences|feel|feels|are|is|get|gets|achieve|achieves|perform|score|scores|display|exhibit"
  m <- regmatches(t, regexec(sprintf("(?i)^(?:%s)\\s+(?:who|that|with|having)\\s+(.*?)\\s+(?:%s)\\s+(?:a |an )?(.*)$", qc_group_nouns, VB), t, perl = TRUE))[[1]]
  if (length(m) == 3 && !grepl("(?i)\\bthan\\b", m[3], perl = TRUE)) { a <- qc_polarity(m[2]); b <- qc_polarity(m[3])
    if (!is.na(b$sign)) return(c(list(type = "relationship", a = a$text, b = b$text, direction = qc_dir_of(a$sign, b$sign)), extra)) }
  # ---- "older students sleep less" (comparative adjective + group noun + verb + more/less)
  m <- regmatches(t, regexec(sprintf("(?i)^(\\w+)\\s+(?:%s)\\s+(.*?)\\s+(more|less|longer|shorter|better|worse)$", qc_group_nouns), t, perl = TRUE))[[1]]
  if (length(m) == 4) { cmp <- qc_comparative(m[2])
    if (!is.null(cmp)) return(c(list(type = "relationship", a = cmp$text, b = m[3], direction = qc_dir_of(cmp$sign, if (tolower(m[4]) %in% c("more", "longer", "better")) 1 else -1)), extra)) }
  # ---- group comparison with a comparative adjective: "students living alone feel lonelier than students living with parents"
  m <- regmatches(t, regexec(sprintf("(?i)^(.*?)\\s+(?:%s|do|perform)?\\s*(?:a |an )?(\\w+(?:ier|er))\\s+than\\s+(?:do\\s+|those\\s+)?(.*)$", VB), t, perl = TRUE))[[1]]
  if (length(m) == 4 && !tolower(m[3]) %in% c("higher", "lower", "greater", "better", "worse", "fewer", "other", "rather")) { cmp <- qc_comparative(m[3])
    if (!is.null(cmp)) return(c(list(type = "difference", g1 = qc_strip_lead(m[2]), g2 = m[4], outcome = cmp$text, higher = if (cmp$sign > 0) 1 else 2), extra)) }
  # ---- "there is a difference in Y between A and B" / "... between students with and without X"
  m <- regmatches(t, regexec("(?i)^(?:there is a\\s+)?(?:a\\s+)?differences?\\s+(?:in|on|regarding)\\s+(.*?)\\s+(?:between|across|among)\\s+(.*)$", t, perl = TRUE))[[1]]
  if (length(m) == 3) { gp <- m[3]
    ww <- regmatches(gp, regexec("(?i)^(.*?)\\s*\\bwith\\s+and\\s+without\\s+(.*)$", gp, perl = TRUE))[[1]]
    if (length(ww) == 3) return(c(list(type = "difference", g1 = paste(ww[2], "with", ww[3]), g2 = paste(ww[2], "without", ww[3]), outcome = m[2], higher = NA), extra))
    if (grepl("\\band\\b", gp)) { sp <- strsplit(gp, "\\s+and\\s+")[[1]]; return(c(list(type = "difference", g1 = sp[1], g2 = paste(sp[-1], collapse = " and "), outcome = m[2], higher = NA), extra)) }
    return(c(list(type = "difference_multi", outcome = m[2], group = sub("\\s+groups?$", "", sub("(?i)^(?:the\\s+)?(?:different\\s+)?", "", gp, perl = TRUE)), k = NA), extra)) }
  # "Y differs between students with and without X"
  t2 <- sub("(?i)\\bbetween\\s+(.*?)\\s*\\bwith\\s+and\\s+without\\s+(.*)$", "between \\1 with \\2 and \\1 without \\2", t, perl = TRUE)
  p <- qc_parse_hypothesis_base(t2)
  # directions carried by comparatives inside the two constructs: "higher loneliness is related to lower satisfaction"
  if (identical(p$type, "relationship") && !is.null(p$a) && !is.null(p$b)) {
    pa <- qc_polarity(p$a); pb <- qc_polarity(p$b)
    if (!is.na(pa$sign) || !is.na(pb$sign)) { p$a <- pa$text; p$b <- pb$text
      if (is.null(p[["direction"]]) || is.na(p[["direction"]])) p[["direction"]] <- qc_dir_of(pa$sign, pb$sign) }
  }
  p
}

# ---------------------------------------------------------------- 3a. before/after pairs of the same measure
qc_time_re <- c(early = "\\b(before|pre|pretest|pre test|baseline|time 1|t1|wave 1|first|initial|start|beginning|prior)\\b",
                late = "\\b(after|post|posttest|post test|follow up|followup|time 2|t2|wave 2|second|final|end|later)\\b")
qc_time_of <- function(x) { x <- qc_norm_text(x); e <- grepl(qc_time_re[["early"]], x, perl = TRUE); l <- grepl(qc_time_re[["late"]], x, perl = TRUE)
  if (e && !l) "early" else if (l && !e) "late" else NA_character_ }
qc_untimed <- function(x) { x <- qc_norm_text(x); for (r in qc_time_re) x <- gsub(r, " ", x, perl = TRUE); qc_concepts(x) }

qc_time_pairs <- function(cat) {
  cand <- cat[cat$kind %in% c("numeric", "ordinal", "binary"), ]; out <- list()
  if (nrow(cand) < 2) return(out)
  tm <- vapply(seq_len(nrow(cand)), function(i) { a <- qc_time_of(cand$label[i]); if (is.na(a)) qc_time_of(gsub("_", " ", cand$variable[i])) else a }, "")
  for (i in which(tm == "early")) for (j in which(tm == "late")) {
    a <- qc_untimed(cand$label[i]); b <- qc_untimed(cand$label[j]); if (!length(a) || !length(b)) next
    if (length(intersect(a, b)) / length(union(a, b)) >= .6) out[[length(out) + 1]] <- list(early = cand$variable[i], late = cand$variable[j], concepts = intersect(a, b), label = cand$label[i])
  }
  out
}

qc_detect_paired <- function(text, cat) {
  t <- qc_norm_text(qc_hyp_normalise(text))
  cue <- grepl(paste(qc_time_re, collapse = "|"), t, perl = TRUE) || grepl("\\b(over time|from .* to|change|changes|improves?|declines?|pre post)\\b", t, perl = TRUE)
  if (!cue) return(NULL)
  pairs <- qc_time_pairs(cat); if (!length(pairs)) return(NULL)
  tc <- qc_untimed(t)
  sc <- vapply(pairs, function(p) mean(p$concepts %in% tc | qc_stem_hit(p$concepts, tc)), 0)
  if (max(sc) < .5) return(NULL)
  p <- pairs[[which.max(sc)]]
  # which measurement is expected to be higher?
  hi <- NULL
  up <- "\\b(higher|more|greater|better|increase|increases|increased|improve|improves|improved|rise|rises|grow|grows|gain|gains)\\b"
  dn <- "\\b(lower|less|fewer|worse|decrease|decreases|decreased|reduce|reduces|reduced|decline|declines|drop|drops|fall|falls|diminish|diminishes)\\b"
  left <- if (grepl("\\bthan\\b", t)) sub("\\bthan\\b.*$", "", t) else t
  dir_up <- grepl(up, left, perl = TRUE); dir_dn <- grepl(dn, left, perl = TRUE)
  if (xor(dir_up, dir_dn)) {
    subj_time <- if (grepl("\\bthan\\b", t)) qc_time_of(left) else "late"     # without "than", the change is towards the later measurement
    if (is.na(subj_time)) subj_time <- "late"
    later_higher <- (subj_time == "late") == dir_up
    hi <- if (later_higher) p$late else p$early
  }
  list(var1 = p$early, var2 = p$late, expect_higher = hi, score = max(sc))
}

# ---------------------------------------------------------------- 3b. finding variables anywhere in a sentence
# Scans word sequences of the sentence for the best-matching variables (labels, names, acronyms); used when no
# sentence pattern fits. Returns non-overlapping matches in the order they appear.
qc_mentions <- function(text, cat, min_score = .8) {
  w <- strsplit(qc_norm_text(qc_hyp_normalise(text)), " ")[[1]]; if (!length(w)) return(data.frame())
  hits <- list()
  for (n in 4:1) for (i in seq_len(max(0, length(w) - n + 1))) {
    ph <- paste(w[i:(i + n - 1)], collapse = " "); if (!length(qc_concepts(ph))) next
    m <- qc_match_variable(ph, cat, top = 1); if (!nrow(m) || m$score[1] < min_score) next
    hits[[length(hits) + 1]] <- data.frame(variable = m$variable[1], start = i, end = i + n - 1, score = m$score[1] + .02 * n, stringsAsFactors = FALSE)
  }
  if (!length(hits)) return(data.frame())
  h <- do.call(rbind, hits); h <- h[order(-h$score), ]; keep <- logical(nrow(h)); used <- integer()
  for (k in seq_len(nrow(h))) { r <- h$start[k]:h$end[k]
    if (!any(r %in% used) && !h$variable[k] %in% h$variable[keep]) { keep[k] <- TRUE; used <- c(used, r) } }
  h <- h[keep, ]; h[order(h$start), ]
}

# ---------------------------------------------------------------- 3c. the breakdown
qc_dissect_base <- if (exists("qc_dissect_base")) qc_dissect_base else qc_dissect
qc_match_groups_base <- if (exists("qc_match_groups_base")) qc_match_groups_base else qc_match_groups

# "students with debt" vs "students without debt", "employed" vs "not employed": a yes/no variable named by the
# shared words; the phrase with a negation is the "absent" category
qc_match_groups <- function(g1, g2, cat) {
  best <- qc_match_groups_base(g1, g2, cat)
  if (!is.null(best) && best$score >= .5) return(best)
  neg <- "(?i)\\b(without|no|not|non|never|don't|do not|does not|lacking|free of|un)\\b"
  n1 <- grepl(neg, g1, perl = TRUE); n2 <- grepl(neg, g2, perl = TRUE)
  if (n1 == n2) return(best)
  core <- qc_concepts(gsub(neg, " ", paste(g1, g2), perl = TRUE)); core <- setdiff(core, "absent")
  cand <- cat[cat$kind == "binary" & lengths(cat$levels) == 2, ]; alt <- NULL
  for (i in seq_len(nrow(cand))) { pr <- qc_present_level(cand$levels[[i]]); lv <- cand$levels[[i]]
    sc <- qc_match_score(paste(core, collapse = " "), cand$label[i], cand$question[i], cand$variable[i])
    if (is.null(pr)) { # labels like "Employed" / "Not employed"
      ab <- which(grepl(neg, lv, perl = TRUE)); if (length(ab) != 1) next
      pr <- list(present = names(lv)[-ab], absent = names(lv)[ab]); sc <- max(sc, max(vapply(lv, function(l) qc_level_score(paste(core, collapse = " "), l), 0))) }
    if (sc >= .5 && (is.null(alt) || sc > alt$score)) {
      l1 <- if (n1) pr$absent else pr$present; l2 <- if (n2) pr$absent else pr$present
      alt <- list(variable = cand$variable[i], label = cand$label[i], level1 = l1, level2 = l2, level1_label = lv[[l1]], level2_label = lv[[l2]], score = round(sc, 2), n_levels = 2) } }
  if (!is.null(alt) && (is.null(best) || alt$score > best$score)) alt else best
}

qc_confidence_label <- function(x) if (is.na(x)) "low" else if (x >= .8) "high" else if (x >= .55) "medium" else "low"

qc_dissect <- function(id, text, data, cat) {
  p <- qc_parse_hypothesis(text)
  pick <- function(phrase, want = "any") {
    if (is.null(phrase) || is.na(phrase) || !nzchar(trimws(phrase))) return(list(phrase = "", best = NA, score = 0, alternatives = character(), weak = TRUE))
    ac <- regmatches(phrase, regexec("\\(([A-Za-z][A-Za-z0-9_]*)\\)", phrase))[[1]]
    if (length(ac) == 2) { hit <- cat$variable[tolower(cat$variable) == tolower(ac[2])]
      if (length(hit)) return(list(phrase = phrase, best = hit[1], score = 1, alternatives = hit[1], weak = FALSE)) }
    m <- qc_match_variable(phrase, cat, want)
    list(phrase = phrase, best = if (nrow(m)) m$variable[1] else NA, score = if (nrow(m)) m$score[1] else 0,
         alternatives = if (nrow(m)) m$variable else character(), weak = !nrow(m) || m$score[1] < .5) }
  kind <- function(v) { k <- cat$kind[cat$variable %in% v]; if (length(k)) k[1] else NA }
  catk <- c("binary", "categorical")
  finish <- function(b, conf) {
    for (i in seq_along(b$parts)) {
      sp <- b$parts[[i]]$spec
      # two category variables: the "relationship" is an association (chi-square / Fisher)
      if (sp$goal == "relationship" && kind(sp$var1) %in% catk && kind(sp$var2) %in% catk) sp <- list(goal = "association", var1 = sp$var1, var2 = sp$var2, subset = sp$subset)
      if (sp$goal == "difference" && kind(sp$outcome) %in% catk && kind(sp$group) %in% catk) sp <- list(goal = "association", var1 = sp$group, var2 = sp$outcome, subset = sp$subset)
      sp <- Filter(Negate(is.null), sp)
      b$parts[[i]]$spec <- sp; b$parts[[i]]$expected <- qc_expected_text(sp, cat)
      ph <- b$parts[[i]]$phrases
      # group matches score category words (0.5 = a clear match), variable matches score label words (1 = exact)
      sc <- unlist(lapply(names(ph) %||% rep("", length(ph)), function(nm) { x <- ph[[nm]]; if (!is.list(x) || is.null(x$score)) return(NULL)
        min(1, if (nm == "groups") x$score + .35 else x$score) }))
      if (!length(names(ph))) sc <- unlist(lapply(ph, function(x) if (is.list(x) && !is.null(x$score)) min(1, x$score)))
      c0 <- if (length(sc)) min(sc) * conf else conf * .6
      if (anyNA(unlist(sp[intersect(names(sp), c("var1", "var2", "outcome", "group", "mediator", "moderator"))]))) c0 <- min(c0, .3)
      b$parts[[i]]$confidence <- round(c0, 2); b$parts[[i]]$confidence_label <- qc_confidence_label(c0)
    }
    if (any(vapply(b$parts, function(x) identical(x$confidence_label, "low"), logical(1))))
      b$questions <- unique(c(b$questions, "QuantCompass is not sure about this breakdown. Please check the type and every variable below."))
    b
  }
  # ---- before/after: two measurements of the same construct
  pd <- qc_detect_paired(text, cat)
  if (!is.null(pd) && !p$type %in% c("moderation", "mediation", "prediction")) {
    sp <- list(goal = "paired_difference", var1 = pd$var1, var2 = pd$var2); if (!is.null(pd$expect_higher)) sp$expect_higher <- pd$expect_higher
    b <- list(id = id, text = text, type = "paired", direction = NA, compound = FALSE, questions = character(),
              parts = list(list(id = id, spec = sp, phrases = list(pair = list(score = .6 + .4 * pd$score)))))   # time words + same measure: strong evidence
    return(finish(b, 1))
  }
  # ---- moderation
  if (identical(p$type, "moderation")) {
    x <- pick(p$x); y <- pick(p$y, "numeric"); questions <- character(); stronger <- NULL
    w <- if (!is.null(p$stronger$phrase)) {
      ph <- p$stronger$phrase; pol <- qc_polarity(ph)
      gm <- if (nzchar(p$stronger$other %||% "")) qc_match_groups(ph, p$stronger$other, cat) else NULL
      if (!is.null(gm) && gm$score >= .35) { stronger <- list(level = if (p$stronger$up) gm$level1 else gm$level2, other = if (p$stronger$up) gm$level2 else gm$level1)
        list(phrase = ph, best = gm$variable, score = gm$score + .3, alternatives = gm$variable, weak = FALSE) }
      else { core <- sub(sprintf("(?i)^(?:%s)\\s+(?:with|who have|having|who report)\\s+", qc_group_nouns), "", ph, perl = TRUE); pol <- qc_polarity(core)
        ww <- pick(pol$text)
        if (!is.na(pol$sign) && kind(ww$best) %in% c("numeric", "ordinal")) stronger <- list(at = if ((pol$sign > 0) == isTRUE(p$stronger$up)) "high" else "low")
        else if (kind(ww$best) %in% c(catk, "ordinal")) { lv <- cat$levels[[which(cat$variable == ww$best)]]; ls <- vapply(lv, function(l) qc_level_score(ph, l), 0)
          if (max(ls) > 0) stronger <- if (isTRUE(p$stronger$up)) list(level = names(lv)[which.max(ls)]) else list(weaker_level = names(lv)[which.max(ls)]) }
        ww }
    } else pick(p$w)
    if (is.null(stronger) && !is.null(p$stronger$at)) stronger <- list(at = p$stronger$at)
    for (z in list(list(x, "predictor (X)"), list(y, "outcome (Y)"), list(w, "moderator (W)"))) if (z[[1]]$weak)
      questions <- c(questions, sprintf("No variable clearly matches '%s' (%s). Choose it below.", z[[1]]$phrase, z[[2]]))
    sp <- list(goal = "moderation", var1 = x$best, moderator = w$best, outcome = y$best, direction = p[["direction"]])
    if (!is.null(stronger)) sp$stronger <- stronger
    b <- list(id = id, text = text, type = "moderation", direction = p[["direction"]], compound = FALSE, questions = questions,
              parts = list(list(id = id, spec = sp, phrases = list(x = x, w = w, y = y))))
    return(finish(b, 1))
  }
  b <- qc_dissect_base(id, text, data, cat)
  ok <- length(b$parts) && !any(vapply(b$parts, function(pp) anyNA(unlist(pp$spec[intersect(names(pp$spec), c("var1", "var2", "outcome", "group", "mediator"))])), logical(1)))
  if (ok) return(finish(b, 1))
  # ---- fallback: find the variables named anywhere in the sentence and decide the analysis from their kinds
  mt <- qc_mentions(text, cat)
  if (nrow(mt) >= 2) {
    lt <- tolower(text); v <- mt$variable
    dirn <- if (grepl("\\b(negativ|inverse|reduc|decreas|lower|less|fewer)", lt)) "negative" else if (grepl("\\b(positiv|increas|higher|more|greater|improv)", lt)) "positive" else NA
    sp <- if (nrow(mt) >= 3 && grepl("mediat|through|via|indirect", lt)) list(goal = "mediation", var1 = v[1], mediator = v[3], outcome = v[2])
      else if (nrow(mt) >= 3 && grepl("moderat|depend|interact|buffer|stronger|weaker", lt)) list(goal = "moderation", var1 = v[1], moderator = v[3], outcome = v[2])
      else if (nrow(mt) >= 3 && grepl("predict|explain", lt)) list(goal = "prediction", outcome = v[length(v)], predictors = v[-length(v)])
      else { k1 <- kind(v[1]); k2 <- kind(v[2])
        if (k1 %in% catk && !k2 %in% catk) list(goal = "difference", outcome = v[2], group = v[1])
        else if (k2 %in% catk && !k1 %in% catk) list(goal = "difference", outcome = v[1], group = v[2])
        else list(goal = "relationship", var1 = v[1], var2 = v[2], direction = dirn) }
    phrases <- lapply(seq_len(nrow(mt)), function(i) list(score = mt$score[i]))
    b <- list(id = id, text = text, type = sp$goal, direction = NA, compound = FALSE,
              questions = c("The sentence did not match a known pattern, so QuantCompass used the variables it names. Check the type and the roles below."),
              parts = list(list(id = id, spec = sp, phrases = phrases)))
    return(finish(b, .7))
  }
  finish(b, 1)
}

# ---------------------------------------------------------------- coded categories without an order
# Numeric codes whose labels have no order (gender 1-3, region 1-4) must enter models as categories (dummy terms).
qc_nominal_coded <- function(vars, data) {
  ll <- if (exists("qc_level_labels", envir = globalenv())) get("qc_level_labels", envir = globalenv()) else list()
  vars <- unique(vars[!is.na(vars)])
  vars[vapply(vars, function(v) v %in% names(data) && is.numeric(data[[v]]) && length(ll[[v]]) >= 3 && !qc_ordered_labels(ll[[v]]) &&
                length(unique(stats::na.omit(data[[v]]))) >= 3, logical(1))]
}
qc_model_vars <- function(spec) unique(c(spec$predictors, spec$controls, if (identical(spec$goal, "moderation")) c(spec$moderator, spec$var1)))
qc_as_factors <- function(data, spec) { for (v in qc_nominal_coded(qc_model_vars(spec), data)) data[[v]] <- factor(data[[v]]); data }
