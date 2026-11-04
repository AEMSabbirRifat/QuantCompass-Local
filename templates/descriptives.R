{{HEADER}}

{{HELPERS}}

# ---- 1. Load data ----
{{DATA_LOAD}}

# ---- 2. Variables to describe ----
variables <- {{VARIABLES_VEC}}

# ---- 3. Numeric variables: N, mean, SD, median, IQR, range, skewness, kurtosis ----
skew <- function(x) { m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
kurt <- function(x) { m <- mean(x); mean((x - m)^4) / mean((x - m)^2)^2 - 3 }   # excess kurtosis
num <- variables[sapply(data[variables], is.numeric)]
numeric_table <- do.call(rbind, lapply(num, function(v) { x <- data[[v]]; x <- x[!is.na(x)]
  data.frame(variable = v, n = length(x), missing = sum(is.na(data[[v]])), mean = mean(x), sd = stats::sd(x),
             median = stats::median(x), iqr = stats::IQR(x), min = min(x), max = max(x),
             skewness = skew(x), kurtosis = kurt(x)) }))

# ---- 4. Categorical variables: counts and percentages ----
cat_vars <- setdiff(variables, num)
categorical_table <- do.call(rbind, lapply(cat_vars, function(v) { t <- table(data[[v]], useNA = "ifany")
  data.frame(variable = v, category = ifelse(is.na(names(t)), "(missing)", names(t)),
             n = as.vector(t), percent = 100 * as.vector(t) / sum(t)) }))

# ---- 5. Results ----
apa <- sprintf("Descriptive statistics were computed for %d variables (%d numeric, %d categorical); N = %d rows.",
  length(variables), length(num), length(cat_vars), nrow(data))
result <- list(test = "descriptives", n_used = nrow(data), n_excluded = 0,
  numeric = numeric_table, categorical = categorical_table, apa = apa, session = qc_session())
cat(apa, "\n"); if (!is.null(numeric_table)) print(numeric_table, digits = 3, row.names = FALSE)
if (!is.null(categorical_table)) print(categorical_table, digits = 3, row.names = FALSE)
