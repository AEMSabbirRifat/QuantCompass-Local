# Hypothesis test library: two invented studies with codebook-style labels and ~60 hypotheses written the way
# students and researchers actually write them. Each hypothesis has the known correct answer:
#   family  = test family (correlation, group_diff, paired, categorical, regression, mediation, moderation);
#             several are allowed where more than one analysis is defensible
#   vars    = the variables the analysis must use (order free)
#   dir     = expected direction for correlations ("positive"/"negative")
#   higher  = expected higher group code (group differences) or the variable expected to be higher (paired)
# qc_hyp_corpus_report() breaks every hypothesis down with qc_dissect() and scores it.

qc_hyp_study <- function(which = c("students", "work", "health"), n = 240, seed = 11) {
  which <- match.arg(which); set.seed(seed)
  r <- function(m, s, lo, hi) pmin(hi, pmax(lo, round(rnorm(n, m, s), 2)))
  if (which == "students") {
    d <- data.frame(STRESS = r(3, .7, 1, 5), SLEEP_H = r(7, 1.1, 3, 11), LS = r(4.5, 1.1, 1, 7), SUPPORT = r(3.6, .8, 1, 5), LONELY = r(2.4, .7, 1, 4),
      GPA = r(2.3, .6, 1, 4), AGE = sample(18:39, n, TRUE), GENDER = sample(1:3, n, TRUE, c(.45, .5, .05)), EMP = sample(1:2, n, TRUE), DEBT = sample(1:2, n, TRUE),
      REGION = sample(1:4, n, TRUE), LEVEL = sample(1:3, n, TRUE, c(.5, .4, .1)), WORKH = sample(1:4, n, TRUE), ANX_PRE = r(3.2, .8, 1, 5), ANX_POST = r(2.9, .8, 1, 5),
      DROPINT = sample(1:2, n, TRUE, c(.2, .8)), HOUSING = sample(1:4, n, TRUE), MINDF = r(3.3, .7, 1, 5), BURN = r(2.8, .8, 1, 5))
    labels <- list(STRESS = "Perceived stress (PSS mean score)", SLEEP_H = "Sleep duration (hours per night)", LS = "Life satisfaction (SWLS mean)",
      SUPPORT = "Perceived social support", LONELY = "Loneliness (UCLA short form)", GPA = "Grade point average", AGE = "Age in years", GENDER = "Gender",
      EMP = "Employment status", DEBT = "Student debt", REGION = "Region of origin", LEVEL = "Study level", WORKH = "Weekly working hours",
      ANX_PRE = "Anxiety before the workshop", ANX_POST = "Anxiety after the workshop", DROPINT = "Intention to drop out", HOUSING = "Type of housing",
      MINDF = "Mindfulness (MAAS)", BURN = "Academic burnout")
    levels <- list(GENDER = c(`1` = "Male", `2` = "Female", `3` = "Non-binary"), EMP = c(`1` = "Employed", `2` = "Not employed"), DEBT = c(`1` = "Yes", `2` = "No"),
      REGION = c(`1` = "European Union", `2` = "Other Europe", `3` = "Asia", `4` = "Africa"), LEVEL = c(`1` = "Bachelor", `2` = "Master", `3` = "PhD"),
      WORKH = c(`1` = "0 hours", `2` = "1-10 hours", `3` = "11-20 hours", `4` = "More than 20 hours"), DROPINT = c(`1` = "Yes", `2` = "No"),
      HOUSING = c(`1` = "Student dorm", `2` = "Shared flat", `3` = "With parents", `4` = "Living alone"))
    H <- list(
      list("Perceived stress is negatively associated with sleep duration.", "correlation", c("STRESS", "SLEEP_H"), dir = "negative"),
      list("There is a positive correlation between social support and life satisfaction.", "correlation", c("SUPPORT", "LS"), dir = "positive"),
      list("The more students sleep, the less stressed they are.", "correlation", c("SLEEP_H", "STRESS"), dir = "negative"),
      list("Higher levels of loneliness are related to lower life satisfaction.", "correlation", c("LONELY", "LS"), dir = "negative"),
      list("Stress and burnout are positively correlated.", "correlation", c("STRESS", "BURN"), dir = "positive"),
      list("Mindfulness reduces perceived stress.", "correlation", c("MINDF", "STRESS"), dir = "negative"),
      list("H1: Social support is positively related to life satisfaction.", "correlation", c("SUPPORT", "LS"), dir = "positive"),
      list("We expect that burnout will be positively correlated with stress.", "correlation", c("BURN", "STRESS"), dir = "positive"),
      list("Students who sleep more have a higher grade point average.", "correlation", c("SLEEP_H", "GPA"), dir = "positive"),
      list("Stress has a negative effect on GPA.", c("correlation", "regression"), c("STRESS", "GPA"), dir = "negative"),
      list("Mindfulness is associated with lower burnout.", "correlation", c("MINDF", "BURN"), dir = "negative"),
      list("Life satisfaction increases as social support increases.", "correlation", c("LS", "SUPPORT"), dir = "positive"),
      list("Students with higher mindfulness experience less burnout.", "correlation", c("MINDF", "BURN"), dir = "negative"),
      list("Age is positively related to GPA.", "correlation", c("AGE", "GPA"), dir = "positive"),
      list("There is no relationship between age and life satisfaction.", "correlation", c("AGE", "LS")),
      list("Older students sleep less.", "correlation", c("AGE", "SLEEP_H"), dir = "negative"),
      list("Female students report higher stress than male students.", "group_diff", c("STRESS", "GENDER"), higher = "2"),
      list("Stress is higher among employed students than among students who are not employed.", "group_diff", c("STRESS", "EMP"), higher = "1"),
      list("Students with debt have lower life satisfaction than students without debt.", "group_diff", c("LS", "DEBT"), higher = "2"),
      list("Life satisfaction differs between the four regions of origin.", "group_diff", c("LS", "REGION")),
      list("Burnout differs by study level.", "group_diff", c("BURN", "LEVEL")),
      list("Sleep duration differs across types of housing.", "group_diff", c("SLEEP_H", "HOUSING")),
      list("GPA is higher for Master students than for Bachelor students.", "group_diff", c("GPA", "LEVEL"), higher = "2"),
      list("There is a significant difference in stress between students with and without debt.", "group_diff", c("STRESS", "DEBT")),
      list("Loneliness differs significantly between men and women.", "group_diff", c("LONELY", "GENDER")),
      list("Debt is associated with higher stress.", "group_diff", c("DEBT", "STRESS"), higher = "1"),
      list("Gender has an effect on burnout.", "group_diff", c("GENDER", "BURN")),
      list("Students living alone feel lonelier than students living with their parents.", "group_diff", c("LONELY", "HOUSING"), higher = "4"),
      list("Anxiety decreases from before to after the workshop.", "paired", c("ANX_PRE", "ANX_POST"), higher = "ANX_PRE"),
      list("Anxiety after the workshop is lower than anxiety before the workshop.", "paired", c("ANX_PRE", "ANX_POST"), higher = "ANX_PRE"),
      list("Participants report less anxiety after the workshop than before it.", "paired", c("ANX_PRE", "ANX_POST"), higher = "ANX_PRE"),
      list("The workshop reduces anxiety (before vs after).", "paired", c("ANX_PRE", "ANX_POST"), higher = "ANX_PRE"),
      list("Gender is associated with the intention to drop out.", "categorical", c("GENDER", "DROPINT")),
      list("Students with debt are more likely to intend to drop out.", c("categorical", "regression"), c("DEBT", "DROPINT")),
      list("Employment status and type of housing are related.", "categorical", c("EMP", "HOUSING")),
      list("The proportion of students intending to drop out differs between regions.", "categorical", c("DROPINT", "REGION")),
      list("Stress, sleep duration and social support predict life satisfaction.", "regression", c("STRESS", "SLEEP_H", "SUPPORT", "LS")),
      list("Life satisfaction is predicted by stress, loneliness and support.", "regression", c("LS", "STRESS", "LONELY", "SUPPORT")),
      list("Stress predicts burnout after controlling for age and gender.", "regression", c("STRESS", "BURN", "AGE", "GENDER")),
      list("Stress is a positive predictor of burnout.", "regression", c("STRESS", "BURN")),
      list("Stress predicts the intention to drop out.", c("regression", "group_diff"), c("STRESS", "DROPINT")),
      list("Sleep duration significantly predicts GPA.", c("regression", "correlation"), c("SLEEP_H", "GPA")),
      list("Sleep mediates the relationship between stress and GPA.", "mediation", c("STRESS", "SLEEP_H", "GPA")),
      list("The effect of stress on life satisfaction is mediated by loneliness.", "mediation", c("STRESS", "LONELY", "LS")),
      list("Stress increases burnout through reduced sleep.", "mediation", c("STRESS", "SLEEP_H", "BURN")),
      list("Social support buffers the negative effect of stress on life satisfaction.", "moderation", c("SUPPORT", "STRESS", "LS")),
      list("The relationship between stress and GPA is moderated by social support.", "moderation", c("STRESS", "GPA", "SUPPORT")),
      list("Employment status moderates the association between stress and sleep duration.", "moderation", c("EMP", "STRESS", "SLEEP_H")),
      list("The impact of loneliness on GPA depends on social support.", "moderation", c("LONELY", "GPA", "SUPPORT")),
      list("The negative relationship between loneliness and life satisfaction is stronger for students with low social support.", "moderation", c("LONELY", "LS", "SUPPORT")))
  } else if (which == "health") {
    # held-out study: written after the patterns, used to measure how well the reader generalises
    d <- data.frame(BMI = r(25, 4, 16, 45), STEPS = round(r(7000, 2500, 500, 20000)), PA_MIN = r(150, 60, 0, 600), DEP = r(1.8, .6, 1, 4), SE = r(3.1, .5, 1, 4),
      QOL = r(65, 15, 0, 100), SMOKE = sample(1:2, n, TRUE, c(.25, .75)), EDU = sample(1:4, n, TRUE), SEX = sample(1:2, n, TRUE), AGEGRP = sample(1:3, n, TRUE),
      PAIN_T0 = r(5.5, 1.8, 0, 10), PAIN_T1 = r(4.6, 1.8, 0, 10), ADH = r(3.6, .8, 1, 5), TRUST = r(3.8, .7, 1, 5), CHRONIC = sample(1:2, n, TRUE), SLEEPQ = r(2.9, .7, 1, 5),
      GROUP = sample(1:2, n, TRUE), INCOME = sample(1:5, n, TRUE))
    labels <- list(BMI = "Body mass index (kg/m2)", STEPS = "Average daily steps", PA_MIN = "Minutes of physical activity per week", DEP = "Depressive symptoms (PHQ-9 mean)",
      SE = "Exercise self-efficacy", QOL = "Health-related quality of life (0-100)", SMOKE = "Current smoker", EDU = "Highest education", SEX = "Sex",
      AGEGRP = "Age group", PAIN_T0 = "Pain intensity at baseline", PAIN_T1 = "Pain intensity at follow-up", ADH = "Medication adherence",
      TRUST = "Trust in the physician", CHRONIC = "Chronic illness", SLEEPQ = "Sleep quality", GROUP = "Study group", INCOME = "Household income")
    levels <- list(SMOKE = c(`1` = "Yes", `2` = "No"), EDU = c(`1` = "Compulsory school", `2` = "Vocational training", `3` = "High school", `4` = "University degree"),
      SEX = c(`1` = "Male", `2` = "Female"), AGEGRP = c(`1` = "18-34", `2` = "35-54", `3` = "55 and older"), CHRONIC = c(`1` = "Yes", `2` = "No"),
      GROUP = c(`1` = "Intervention", `2` = "Control"), INCOME = c(`1` = "Under 1000 EUR", `2` = "1000-1999 EUR", `3` = "2000-2999 EUR", `4` = "3000-3999 EUR", `5` = "4000 EUR or more"))
    H <- list(
      list("Physical activity is inversely related to depressive symptoms.", "correlation", c("PA_MIN", "DEP"), dir = "negative"),
      list("People who walk more steps per day have a lower BMI.", "correlation", c("STEPS", "BMI"), dir = "negative"),
      list("Exercise self-efficacy correlates positively with weekly physical activity.", "correlation", c("SE", "PA_MIN"), dir = "positive"),
      list("Smokers report a lower quality of life than non-smokers.", "group_diff", c("QOL", "SMOKE"), higher = "2"),
      list("Quality of life differs across education levels.", "group_diff", c("QOL", "EDU")),
      list("The intervention group shows higher medication adherence than the control group.", "group_diff", c("ADH", "GROUP"), higher = "1"),
      list("Pain intensity decreases from baseline to follow-up.", "paired", c("PAIN_T0", "PAIN_T1"), higher = "PAIN_T0"),
      list("Pain at follow-up is lower than pain at baseline.", "paired", c("PAIN_T0", "PAIN_T1"), higher = "PAIN_T0"),
      list("Trust in the physician predicts medication adherence.", c("regression", "correlation"), c("TRUST", "ADH")),
      list("Depressive symptoms, sleep quality and BMI predict quality of life.", "regression", c("DEP", "SLEEPQ", "BMI", "QOL")),
      list("Self-efficacy mediates the effect of trust in the physician on adherence.", "mediation", c("TRUST", "SE", "ADH")),
      list("Sleep quality explains the link between depressive symptoms and quality of life.", "mediation", c("DEP", "SLEEPQ", "QOL")),
      list("Chronic illness moderates the relationship between physical activity and quality of life.", "moderation", c("CHRONIC", "PA_MIN", "QOL")),
      list("The association between BMI and depressive symptoms is stronger in women than in men.", "moderation", c("BMI", "DEP", "SEX")),
      list("Smoking status is associated with sex.", "categorical", c("SMOKE", "SEX")),
      list("Chronic illness is more common among smokers.", "categorical", c("CHRONIC", "SMOKE")),
      list("Higher household income goes along with better quality of life.", c("correlation", "group_diff"), c("INCOME", "QOL"), dir = "positive"),
      list("Depressive symptoms are higher for people with a chronic illness than for people without one.", "group_diff", c("DEP", "CHRONIC"), higher = "1"),
      list("Sleep quality is positively associated with quality of life after controlling for age group and sex.", "regression", c("SLEEPQ", "QOL", "AGEGRP", "SEX")),
      list("Women and men differ in their daily steps.", "group_diff", c("STEPS", "SEX")),
      list("BMI is a negative predictor of exercise self-efficacy.", "regression", c("BMI", "SE")),
      list("Poorer sleep quality is linked to more depressive symptoms.", "correlation", c("SLEEPQ", "DEP"), dir = "negative"),
      list("The effect of self-efficacy on physical activity depends on sex.", "moderation", c("SE", "PA_MIN", "SEX")),
      list("Pain improves over time, from baseline to follow-up.", "paired", c("PAIN_T0", "PAIN_T1")),
      list("Medication adherence differs by age group.", "group_diff", c("ADH", "AGEGRP")),
      list("Participants in the intervention group are less likely to smoke.", c("categorical", "regression"), c("GROUP", "SMOKE")))
  } else {
    d <- data.frame(JS = r(3.5, .8, 1, 5), WL = r(3.1, .8, 1, 5), AUT = r(3.4, .9, 1, 5), TI = r(2.5, .9, 1, 5), TEN = r(6, 4, 0, 30), DEPT = sample(1:3, n, TRUE),
      ARR = sample(1:2, n, TRUE), LQ = r(3.6, .8, 1, 5), ENG = r(4.2, 1, 1, 7), WB1 = r(3.2, .7, 1, 5), WB2 = r(3.4, .7, 1, 5), SEX = sample(1:2, n, TRUE),
      CONTR = sample(1:2, n, TRUE))
    labels <- list(JS = "Job satisfaction", WL = "Workload", AUT = "Job autonomy", TI = "Turnover intention", TEN = "Years with the company", DEPT = "Department",
      ARR = "Work arrangement", LQ = "Leadership quality", ENG = "Work engagement (UWES-9)", WB1 = "Well-being at time 1", WB2 = "Well-being at time 2",
      SEX = "Sex", CONTR = "Contract type")
    levels <- list(DEPT = c(`1` = "Sales", `2` = "IT", `3` = "Human resources"), ARR = c(`1` = "Remote", `2` = "On-site"), SEX = c(`1` = "Male", `2` = "Female"),
      CONTR = c(`1` = "Permanent", `2` = "Temporary"))
    H <- list(
      list("Workload is negatively related to job satisfaction.", "correlation", c("WL", "JS"), dir = "negative"),
      list("Remote employees report higher job satisfaction than on-site employees.", "group_diff", c("JS", "ARR"), higher = "1"),
      list("Job autonomy moderates the relationship between workload and work engagement.", "moderation", c("AUT", "WL", "ENG")),
      list("Engagement mediates the effect of leadership quality on turnover intention.", "mediation", c("LQ", "ENG", "TI")),
      list("Turnover intention differs across departments.", "group_diff", c("TI", "DEPT")),
      list("Well-being at time 2 is higher than well-being at time 1.", "paired", c("WB1", "WB2"), higher = "WB2"),
      list("Leadership quality, autonomy and workload predict work engagement.", "regression", c("LQ", "AUT", "WL", "ENG")),
      list("Contract type is associated with department.", "categorical", c("CONTR", "DEPT")),
      list("The longer employees stay with the company, the higher their job satisfaction.", "correlation", c("TEN", "JS"), dir = "positive"),
      list("Women report lower workload than men.", "group_diff", c("WL", "SEX"), higher = "1"),
      list("Job satisfaction predicts turnover intention over and above workload.", "regression", c("JS", "TI", "WL")),
      list("Leadership quality has a positive influence on engagement.", c("correlation", "regression"), c("LQ", "ENG"), dir = "positive"),
      list("Well-being improves between time 1 and time 2.", "paired", c("WB1", "WB2"), higher = "WB2"),
      list("The effect of workload on turnover intention is stronger for temporary employees than for permanent employees.", "moderation", c("WL", "TI", "CONTR")))
  }
  hyps <- lapply(H, function(h) list(text = h[[1]], family = h[[2]], vars = h[[3]], dir = h$dir, higher = h$higher))
  list(data = d, labels = labels, levels = levels, hyps = hyps)
}

# Test family of one breakdown part, judged with the catalogue's variable kinds
qc_hyp_family <- function(sp, cat) {
  k <- function(v) { x <- cat$kind[cat$variable %in% v]; if (length(x)) x[1] else NA }
  catk <- c("binary", "categorical")
  switch(sp$goal, mediation = "mediation", moderation = "moderation", prediction = "regression", paired_difference = "paired",
    association = "categorical", reliability = "reliability",
    difference = if (k(sp$outcome) %in% catk) "categorical" else "group_diff",
    relationship = { a <- k(sp$var1) %in% catk; b <- k(sp$var2) %in% catk; if (a && b) "categorical" else if (a || b) "group_diff" else "correlation" },
    "unknown")
}

qc_spec_vars <- function(sp) { v <- unlist(sp[intersect(names(sp), c("var1", "var2", "outcome", "group", "predictors", "mediator", "moderator", "controls"))]); unique(v[!is.na(v)]) }

qc_hyp_corpus_report <- function(studies = c("students", "work", "health")) {
  rows <- list()
  for (st in studies) {
    s <- qc_hyp_study(st); cat <- qc_catalog(s$data, s$labels, s$levels)
    for (i in seq_along(s$hyps)) {
      h <- s$hyps[[i]]
      b <- tryCatch(qc_dissect(sprintf("H%d", i), h$text, s$data, cat), error = function(e) list(parts = list(), type = paste("error:", conditionMessage(e))))
      sp <- if (length(b$parts)) b$parts[[1]]$spec else list(goal = "none")
      fam <- if (length(b$parts)) qc_hyp_family(sp, cat) else "none"
      vv <- qc_spec_vars(sp)
      dir_ok <- if (!is.null(h$dir)) identical(sp[["direction"]], h$dir) || identical(unname(sp$directions[1]), h$dir) else NA
      hi_ok <- if (!is.null(h$higher)) identical(as.character(sp$expect_higher), h$higher) else NA
      rows[[length(rows) + 1]] <- data.frame(study = st, id = i, text = h$text, expected = paste(h$family, collapse = "/"), got = fam,
        family_ok = fam %in% h$family, vars_ok = setequal(vv, h$vars), direction_ok = dir_ok, higher_ok = hi_ok,
        got_vars = paste(vv, collapse = ","), want_vars = paste(h$vars, collapse = ","), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows); out$all_ok <- out$family_ok & out$vars_ok & out$direction_ok %in% c(TRUE, NA) & out$higher_ok %in% c(TRUE, NA); out
}
