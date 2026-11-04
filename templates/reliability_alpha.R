{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Select items, remove incomplete responses, reverse-code where needed ----
items     <- {{ITEMS_VEC}}
reverse   <- {{REVERSE_VEC}}         # negatively worded items
scale_min <- {{SCALE_MIN}}
scale_max <- {{SCALE_MAX}}
cc <- qc_complete(data, items); X <- as.data.frame(lapply(cc$data[items], as.numeric))
names(X) <- items
for (it in reverse) X[[it]] <- (scale_min + scale_max) - X[[it]]   # e.g. on 1-5: 1 <-> 5, 2 <-> 4
k <- length(items)

# ---- 3. Cronbach's alpha = k/(k-1) * (1 - sum of item variances / variance of total score) ----
alpha_of <- function(M) { M <- as.matrix(M); kk <- ncol(M)
  kk / (kk - 1) * (1 - sum(apply(M, 2, stats::var)) / stats::var(rowSums(M))) }
alpha <- alpha_of(X)

# ---- 4. Item analysis ----
total <- rowSums(X)
item_stats <- data.frame(item = items, reversed = items %in% reverse,
  mean = sapply(X, mean), sd = sapply(X, stats::sd),
  corrected_item_total_r = sapply(items, function(it) stats::cor(X[[it]], total - X[[it]])),
  alpha_if_deleted = if (k > 2) sapply(items, function(it) alpha_of(X[setdiff(items, it)])) else NA,
  row.names = NULL)
scale_score <- rowMeans(X)   # mean score per respondent, on the original 1-to-max metric

# ---- 5. Results ----
label <- if (alpha >= .9) "excellent" else if (alpha >= .8) "good" else if (alpha >= .7) "acceptable" else
  if (alpha >= .6) "questionable" else "poor"
apa <- sprintf("The %d-item scale (%s) showed %s internal consistency, Cronbach's alpha = %s, N = %d%s.",
  k, paste(items, collapse = ", "), label, qc_fmt_r(alpha), nrow(X),
  if (length(reverse)) paste0("; reverse-coded items: ", paste(reverse, collapse = ", ")) else "")
result <- list(test = "reliability_alpha", n_used = cc$n_used, n_excluded = cc$n_excluded,
  alpha = alpha, items = item_stats, scale_mean = mean(scale_score), scale_sd = stats::sd(scale_score),
  apa = apa, session = qc_session())
cat(apa, "\n"); print(item_stats, digits = 3, row.names = FALSE)
