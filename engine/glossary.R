# Plain-language definitions of statistical terms, attached to reports
# (Word comments + HTML hover notes on the first occurrence, and a "Key terms" section).
# pattern = Perl regex used to find the term in report text (first match per report).

qc_glossary <- list(
  "p-value" = list(pattern = "\\bp(?= (?:=|<) ?\\.?\\d)|\\bp-values?\\b|\\bp(?= (?:<|>=) \\.05\\))",
    def = "The probability of getting a result at least this extreme if there were really no effect. Small values (below .05) mean the result is unlikely to be chance alone. Example: p = .004 means a 0.4% chance under 'no difference'. It does not tell you how big or important the effect is."),
  "statistically significant" = list(pattern = "(?i)\\bstatistically significant\\b",
    def = "Shorthand for p < .05: the result is unlikely to be due to chance alone. It does not mean the effect is large or important - check the effect size for that."),
  "effect size" = list(pattern = "(?i)\\beffect sizes?\\b|\\b(?:small|medium|large|negligible) effect\\b",
    def = "How big a difference or relationship is, independent of sample size. Conventional labels (Cohen, 1988): small, medium, large. Example: a d of 0.8 is a large difference between two groups."),
  "Cohen's d" = list(pattern = "Cohen's d\\b|\\bd(?= = -?\\d)",
    def = "The difference between two group means in standard-deviation units. Example: d = 0.5 means the groups differ by half a standard deviation. Rough guide: 0.2 small, 0.5 medium, 0.8 large."),
  "d_z" = list(pattern = "\\bd_z\\b",
    def = "Cohen's d for paired data: the average change divided by the standard deviation of the changes. Same rough guide as Cohen's d."),
  "95% confidence interval" = list(pattern = "95% (?:CI|confidence interval)",
    def = "A range of plausible values for the true effect. If the study were repeated many times, 95% of such intervals would contain the true value. Example: 95% CI [0.3, 1.1] - the true effect is plausibly anywhere in that range; an interval not containing 0 matches p < .05."),
  "mean (M)" = list(pattern = "\\bM(?= = )|(?i)\\bthe (?:means?|averages?) of\\b|\\bmean (?:difference|score)s?\\b",
    def = "The arithmetic average: add all values and divide by how many there are. Sensitive to extreme values."),
  "standard deviation (SD)" = list(pattern = "\\bSD(?= = )|(?i)\\bstandard deviation\\b",
    def = "How spread out values are around the mean. About two-thirds of values lie within one SD of the mean when data are roughly normal."),
  "median (Mdn)" = list(pattern = "\\bMdn(?= = )|(?i)\\bmedians?\\b",
    def = "The middle value when all values are sorted. Half the cases are below it, half above. Not affected by extreme values, so it suits skewed data."),
  "IQR" = list(pattern = "\\bIQR\\b",
    def = "Interquartile range: the spread of the middle 50% of values (75th minus 25th percentile). Used with the median."),
  "degrees of freedom (df)" = list(pattern = "\\b[tFH](?=\\(\\d)|\\bdf\\b",
    def = "The number in brackets after a test statistic, e.g. t(36.88) or F(2, 117). It depends on the sample size and the design and is needed to judge the statistic. Readers expect it to be reported."),
  "normality" = list(pattern = "(?i)\\bnormality\\b|\\bnormal distribution\\b|\\bnormally distributed\\b",
    def = "Whether values follow the bell-shaped normal distribution. Mean-based tests (t-test, ANOVA, Pearson) assume it; rank-based tests do not."),
  "Shapiro-Wilk test" = list(pattern = "Shapiro-Wilk",
    def = "A test of normality for small samples. p < .05 means the data depart significantly from a normal distribution. QuantCompass uses it below n = 50 (Kim, 2013)."),
  "skewness" = list(pattern = "(?i)\\bskew(?:ness|ed)\\b",
    def = "How lopsided a distribution is. 0 = symmetric; positive = a long tail to the right (e.g. income); negative = a long tail to the left."),
  "kurtosis" = list(pattern = "(?i)\\bkurtosis\\b",
    def = "How heavy the tails of a distribution are compared with a normal distribution. High values mean more extreme values than expected."),
  "Q-Q plot" = list(pattern = "Q-Q plot",
    def = "A chart comparing your values with those expected under a normal distribution. Points on the straight line indicate normality; curves away from it indicate skew or heavy tails."),
  "outlier" = list(pattern = "(?i)\\boutliers?\\b",
    def = "A value far from the others. 'Extreme' outliers here lie more than 3 x IQR beyond the quartiles (Tukey, 1977). They can distort means and correlations."),
  "equal variances (Levene's test)" = list(pattern = "Levene's test|(?i)\\bequal variances\\b",
    def = "Whether groups have similar spread. Levene's test checks it; p < .05 means the spreads differ. Welch's tests do not need equal variances."),
  "parametric test" = list(pattern = "(?i)(?<!non-)\\bparametric\\b",
    def = "A test that compares means and assumes roughly normal data (e.g. t-test, ANOVA, Pearson). Usually the most powerful choice when its assumptions hold."),
  "non-parametric test" = list(pattern = "(?i)\\bnon-parametric\\b|\\brank-based\\b",
    def = "A test that works on ranks (order) instead of raw values, so it does not need normal data and is robust to outliers (e.g. Mann-Whitney, Kruskal-Wallis, Spearman)."),
  "central limit theorem" = list(pattern = "(?i)central limit theorem",
    def = "With larger samples (roughly 30+ per group), averages are approximately normal even when the raw data are not, so mean-based tests stay valid."),
  "Welch's t-test" = list(pattern = "Welch's (?:independent-samples )?t-test",
    def = "Compares the means of two independent groups without assuming equal variances. Example: average house value of homes near vs not near the river."),
  "paired-samples t-test" = list(pattern = "(?i)paired(?:-samples)? t-test",
    def = "Compares two measurements of the same cases, e.g. satisfaction before and after a training, by analysing each person's change."),
  "Mann-Whitney U test" = list(pattern = "Mann-Whitney",
    def = "Rank-based alternative to the t-test for two independent groups. It checks whether values in one group tend to be higher than in the other."),
  "Wilcoxon signed-rank test" = list(pattern = "Wilcoxon signed-rank",
    def = "Rank-based alternative to the paired t-test for two measurements of the same cases."),
  "rank-biserial r" = list(pattern = "rank-biserial",
    def = "Effect size for rank-based tests, from -1 to +1. Example: r = .32 means that in a random pair of cases, one group is higher noticeably more often than the other. Guide: .1 small, .3 medium, .5 large."),
  "ANOVA" = list(pattern = "\\bANOVA\\b",
    def = "Analysis of variance: compares the means of three or more groups in one test. Welch's version does not assume equal variances."),
  "Games-Howell post-hoc test" = list(pattern = "Games-Howell|(?i)post-hoc",
    def = "After a significant ANOVA, compares every pair of groups while controlling the overall error rate, without assuming equal variances."),
  "Kruskal-Wallis test" = list(pattern = "Kruskal-Wallis",
    def = "Rank-based alternative to ANOVA for three or more groups."),
  "eta-squared / epsilon-squared" = list(pattern = "eta-squared|epsilon-squared|omega-squared",
    def = "The share of differences in the outcome explained by group membership. Example: .06 = 6%. Guide: .01 small, .06 medium, .14 large."),
  "Pearson correlation (r)" = list(pattern = "Pearson|\\br(?=\\(\\d)",
    def = "Strength of a straight-line relationship between two numeric variables, from -1 to +1. Example: r = .63 = a strong positive relationship. Correlation does not prove causation."),
  "Spearman correlation (rho)" = list(pattern = "Spearman|\\brho\\b|\\brs(?=\\()",
    def = "Rank-based correlation, from -1 to +1. Measures whether two variables rise or fall together, even in curved (monotonic) relationships; suits rating scales and outliers."),
  "linearity" = list(pattern = "(?i)\\blinear(?:ity)?\\b(?! regression)",
    def = "Whether the relationship follows a straight line. If it curves, straight-line methods (Pearson, linear regression) misdescribe it."),
  "chi-square test" = list(pattern = "(?i)chi-square",
    def = "Tests whether two categorical variables are related, by comparing observed counts with the counts expected if they were unrelated."),
  "Fisher's exact test" = list(pattern = "Fisher's exact",
    def = "Used instead of chi-square when expected counts are small (below 5 in more than 20% of cells); it calculates the exact probability."),
  "expected count" = list(pattern = "(?i)expected (?:cell )?counts?",
    def = "The number of cases a table cell would have if the two variables were unrelated. Small expected counts make chi-square unreliable."),
  "Cramer's V" = list(pattern = "Cramer's V",
    def = "Effect size for chi-square, from 0 (no association) to 1 (perfect association). Guide (2 x 2 tables): .1 small, .3 medium, .5 large."),
  "linear regression" = list(pattern = "(?i)linear regression",
    def = "Estimates how a numeric outcome changes with one or more predictors, each while the others are held constant. Example: house value predicted from rooms and crime rate."),
  "b (regression coefficient)" = list(pattern = "\\bb(?= = -?\\d)|\\bb values?\\b",
    def = "Expected change in the outcome for a one-unit increase in a predictor, holding other predictors constant. Example: b = 4.52 for rooms = each extra room goes with 4.52 units higher value."),
  "beta (standardised coefficient)" = list(pattern = "\\bbeta\\b",
    def = "The regression coefficient in standard-deviation units, so predictors measured on different scales can be compared. Larger absolute beta = stronger unique effect."),
  "R-squared" = list(pattern = "(?:adjusted )?R-squared",
    def = "Share of the variation in the outcome explained by the model. Example: R-squared = .68 means 68%. Adjusted R-squared corrects for the number of predictors."),
  "residuals" = list(pattern = "(?i)\\bresiduals?\\b",
    def = "The difference between each observed value and the value the model predicts. Regression assumptions are checked on residuals."),
  "homoscedasticity" = list(pattern = "(?i)homoscedasticity|constant variance|Breusch-Pagan",
    def = "Whether residuals have a similar spread across all predicted values. If not (heteroscedasticity), ordinary standard errors are wrong and robust ones are used."),
  "HC3 robust standard errors" = list(pattern = "HC3",
    def = "Standard errors that stay valid when residual variance is not constant (Long & Ervin, 2000). They change p-values and CIs, not the coefficients."),
  "multicollinearity (VIF)" = list(pattern = "(?i)multicollinearity|\\bVIF\\b",
    def = "When predictors overlap strongly, their separate effects cannot be told apart. VIF above 5 is a warning sign."),
  "Cook's distance" = list(pattern = "Cook's distance|(?i)influential cases?",
    def = "How much the whole regression would change if one case were removed. Cases above 4/n are worth checking for data errors."),
  "logistic regression" = list(pattern = "(?i)logistic regression",
    def = "Predicts a yes/no outcome (e.g. passed vs failed) from one or more predictors, reporting odds ratios."),
  "odds ratio" = list(pattern = "(?i)odds ratios?|\\bOR\\b",
    def = "How the odds of the outcome change with a one-unit increase in a predictor. 1 = no change; 2 = odds double; 0.5 = odds halve."),
  "Nagelkerke R-squared" = list(pattern = "Nagelkerke",
    def = "An approximate share of explained variation for logistic regression, from 0 to 1. Interpret it more loosely than R-squared in linear regression."),
  "Cronbach's alpha" = list(pattern = "Cronbach's alpha",
    def = "How consistently several items measure the same thing (0 to 1). Common guide: .70 acceptable, .80 good, .90 excellent."),
  "item-total correlation" = list(pattern = "(?i)item-total",
    def = "How strongly one item relates to the sum of the other items. Below .30, the item may not fit the scale."),
  "reverse-coding" = list(pattern = "(?i)reverse-cod(?:ed|ing)",
    def = "Flipping a negatively worded item (e.g. 'I dislike my job') so high scores mean the same thing as on the other items. On a 1-5 scale: 1 becomes 5, 2 becomes 4."),
  "mediation analysis" = list(pattern = "(?i)\\bmediation (?:analysis|model)\\b|\\bmediates\\b|\\bmediator\\b",
    def = "Tests whether a predictor (X) is linked to an outcome (Y) partly through a third variable, the mediator (M): X changes M, and M changes Y. Example: work-study conflict lowers grades, and lower grades lower well-being."),
  "indirect effect" = list(pattern = "(?i)\\bindirect effect\\b",
    def = "The part of the link between X and Y that runs through the mediator, calculated as a x b (path X to M times path M to Y). If its confidence interval excludes zero, the mediation is statistically significant."),
  "direct effect" = list(pattern = "(?i)\\bdirect effect\\b|\\bc'(?= =)",
    def = "What remains of the link between X and Y after the mediator is taken into account (c'). If it is near zero while the indirect effect is significant, the mediator explains most of the link."),
  "bootstrap" = list(pattern = "(?i)\\bbootstrap(?:ped|ping)?\\b",
    def = "Repeatedly re-draws the sample (here 5,000 times, with replacement) and re-calculates the result each time. The middle 95% of these results form the confidence interval. It does not assume a normal distribution."),
  "one-sided test" = list(pattern = "(?i)\\bone-sided\\b",
    def = "A test of a direction stated in advance (e.g. 'group A scores higher'). It has more power for that direction but cannot detect an effect in the opposite direction. Only use it if the direction was planned before seeing the data.")
)

# Split text into plain segments and glossary-term segments (first occurrence per report only)
qc_term_segments <- function(text, used) {
  hits <- NULL
  for (k in setdiff(names(qc_glossary), used)) {
    m <- regexpr(qc_glossary[[k]]$pattern, text, perl = TRUE)
    if (m > 0) hits <- rbind(hits, data.frame(key = k, start = as.integer(m), len = attr(m, "match.length")))
  }
  if (is.null(hits)) return(list(segs = list(list(text = text, key = NA)), used = used))
  hits <- hits[order(hits$start, -hits$len), ]; keep <- integer(); end <- 0
  for (i in seq_len(nrow(hits))) if (hits$start[i] > end) { keep <- c(keep, i); end <- hits$start[i] + hits$len[i] - 1 }
  hits <- hits[keep, ]; segs <- list(); pos <- 1
  for (i in seq_len(nrow(hits))) {
    if (hits$start[i] > pos) segs[[length(segs) + 1]] <- list(text = substr(text, pos, hits$start[i] - 1), key = NA)
    segs[[length(segs) + 1]] <- list(text = substr(text, hits$start[i], hits$start[i] + hits$len[i] - 1), key = hits$key[i])
    pos <- hits$start[i] + hits$len[i]
  }
  if (pos <= nchar(text)) segs[[length(segs) + 1]] <- list(text = substring(text, pos), key = NA)
  list(segs = segs, used = c(used, hits$key))
}

# All glossary terms that appear anywhere in a block of text
qc_terms_in <- function(text) names(qc_glossary)[vapply(qc_glossary, function(g) grepl(g$pattern, text, perl = TRUE), logical(1))]
