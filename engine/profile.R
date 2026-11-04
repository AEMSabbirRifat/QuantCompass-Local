# Data profiler: detects variable types and data problems before any analysis runs.

qc_missing_codes <- c(-9, -99, -999, 99, 999, 9999, -1)

# Measurement type of one variable
qc_vartype <- function(x) {
  x <- x[!is.na(x)]
  u <- length(unique(x))
  if (u == 0) return("empty")
  if (is.numeric(x)) {
    if (u == 1) return("constant")
    if (u == 2) return("binary")
    if (u <= 7 && all(x == round(x))) return("ordinal")
    return("continuous")
  }
  if (u == 1) return("constant")
  if (u == 2) return("binary")
  if (u <= max(10, 0.5 * length(x))) return("categorical")
  "text"
}

qc_profile <- function(data) {
  n <- nrow(data)
  issues <- list()
  add <- function(col, level, msg) issues[[length(issues) + 1]] <<-
    data.frame(column = col, level = level, message = msg)

  cols <- lapply(names(data), function(nm) {
    x <- data[[nm]]
    type <- qc_vartype(x)
    nonmiss <- x[!is.na(x)]
    if (is.character(x) && length(nonmiss) > 0) {
      if (all(grepl("^-?[0-9]+,[0-9]+$|^-?[0-9]+$", nonmiss)) && any(grepl(",", nonmiss)))
        add(nm, "fix", "Numbers use a decimal comma (e.g. 3,5) and were read as text.")
      else {
        num_share <- mean(!is.na(suppressWarnings(as.numeric(nonmiss))))
        if (num_share >= 0.8 && num_share < 1)
          add(nm, "fix", sprintf("Mostly numbers, but %d entries are text (e.g. '%s').",
              sum(is.na(suppressWarnings(as.numeric(nonmiss)))),
              nonmiss[is.na(suppressWarnings(as.numeric(nonmiss)))][1]))
      }
    }
    if (is.numeric(x) && length(nonmiss) > 5) {
      codes <- intersect(unique(nonmiss), qc_missing_codes)
      for (cd in codes) {
        rest <- nonmiss[nonmiss != cd]
        if (length(rest) > 2 && (cd >= max(rest) * 1.5 || cd < 0 && min(rest) >= 0))
          add(nm, "fix", sprintf("Value %s appears %d times and looks like a missing-value code.",
              cd, sum(nonmiss == cd)))
      }
    }
    if (type == "constant") add(nm, "info", "Only one value - cannot be analysed.")
    if (type == "empty") add(nm, "info", "Column is empty.")
    if (type == "ordinal") add(nm, "check",
      "Few whole-number values: confirm whether this is a rating-scale item or category codes.")
    is_id <- length(nonmiss) == n && length(unique(nonmiss)) == n &&
      (grepl("id", nm, ignore.case = TRUE) || (is.numeric(x) && all(diff(sort(nonmiss)) == 1)))
    data.frame(column = nm, type = if (is_id) "id" else type,
               missing = sum(is.na(x)), unique = length(unique(nonmiss)),
               example = if (length(nonmiss)) as.character(nonmiss[1]) else NA)
  })
  cols <- do.call(rbind, cols)

  # Item batteries: >= 3 columns sharing a prefix followed by a number (js_1, js_2, ...)
  stems <- sub("[_. ]?[0-9]+$", "", cols$column)
  has_num <- grepl("[0-9]+$", cols$column)
  tab <- table(stems[has_num])
  batteries <- lapply(names(tab[tab >= 3]), function(s) cols$column[has_num & stems == s])
  names(batteries) <- names(tab[tab >= 3])

  issues <- if (length(issues)) do.call(rbind, issues) else
    data.frame(column = character(), level = character(), message = character())
  verdict <- if (n == 0 || ncol(data) < 1) "blocked" else
    if (any(issues$level == "fix")) "needs_fixing" else "ready"
  list(n_rows = n, n_columns = ncol(data), columns = cols, batteries = batteries,
       issues = issues, verdict = verdict)
}
