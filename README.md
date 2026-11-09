# QuantCompass Local

**Click-only, academically sound statistics for students. Upload your survey data and codebook, paste your hypotheses as written in your thesis, and get thesis-ready results, charts, a methods section and the exact R script. No coding.**

For each hypothesis QuantCompass **screens the assumptions first**, **chooses the parametric or non-parametric test by fixed, published rules**, runs it, draws the charts, and writes a **Word + HTML report** explaining every decision, with a paragraph you can paste straight into your thesis.

> **Two versions, one engine**
> - **QuantCompass Local** (this repo): runs on your computer. Your data never leaves it.
> - **[QuantCompass Web](https://github.com/AEMSabbirRifat/QuantCompass-Web)**: the same app in the browser, no installation (in development).

**Status: v0.4 pilot.** Feedback welcome via Issues.

---

## Start in 5 steps (Windows, macOS, Linux)

1. **Install R** (free): [cran.r-project.org](https://cran.r-project.org/) - "Download R for Windows" - "base" - run the installer.
2. **Install RStudio Desktop** (free): [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/).
3. **Download QuantCompass**: green **Code** button above - **Download ZIP** - right-click the ZIP - **Extract All**.
4. **Open `START-QuantCompass.R`** in RStudio (inside the extracted folder; if there is a folder inside a folder, open the inner one).
5. Press **Ctrl + Shift + S** (Mac: Cmd + Shift + S) or click **Source** (top right of the script window). Do not use "Run App". The first start installs what it needs (a few minutes, once). The app then opens in your browser.

That's all. Everything else is clicking.

## How it works (in the app)

Six steps, in the order a study is analysed. The AI assistant (optional, your own key) helps on every step once it is connected; QuantCompass's own rules work without it.

| Step | What you do | What QuantCompass (and the AI assistant) does |
|---|---|---|
| **1. Data & codebook** | Choose your data file and (recommended) your codebook; connect the AI assistant if you like | Reads any format: CSV/TSV/TXT in any encoding (incl. SoSci Survey UTF-16 exports), Excel, OpenDocument, SPSS, Stata, SAS. Reads codebooks in **any layout**: PDF (tables rebuilt from the page), Word, Excel/CSV, TXT/HTML/Markdown, SPSS syntax, Qualtrics (.qsf), LimeSurvey (.lss) or the labels inside an SPSS/Stata file. For each variable the reading that fits your data best is kept; filter rules and forgotten missing-value codes are found and shown. What the codebook writes down in words is used too: its **scale definitions** ("Financial strain (FC_MEAN): recode FC03 (6 - x); mean of FC01-FC05") and its **screening table** ("S1 Incomplete: FINISHED = 1 - 8 removed"). Check table: ✓ matches, ⚠ needs your decision, ✗ not in the codebook. With the AI connected, variables the codebook leaves undescribed are described by the AI (and checked against your data). |
| **2. Hypotheses & variables** | Paste your hypotheses exactly as written (or upload a sheet with ID + Hypothesis); click the recommended variables or choose your own | Your hypotheses are counted as you wrote them: null hypotheses next to them (H1₀, H0₁, "Null: ...") are kept as their H0, never as extra hypotheses, and a sentence with two claims stays one hypothesis (its claims are tested separately and shown together). Each hypothesis is broken down whatever the wording: type (relationship, group difference, before/after, association, prediction, mediation, moderation, reliability), direction, H1 and H0. Then, for every role - **dependent variable, independent variable, groups, mediator, moderator, controls** - QuantCompass recommends what measures it: a single column, or a **multi-item scale** - defined in your codebook, or found in your data (items under the same heading or name stem with the same answer scale; reversed items are found from the answers). Each scale shows **how it is measured**: its scoring (mean, sum, WHO-5 index), every item with its question wording and whether it is reversed, and Cronbach's alpha - and you can drop, add or reverse items there. The AI assistant reads every hypothesis too: badges show whether it agrees (**App**, **✦ AI**), and where QuantCompass is unsure the card starts from the AI's reading. Click a recommendation, an alternative, or **Choose my own** (any column, or your own set of items as a new scale). Nothing is used until you confirm. |
| **3. Prepare data** | Switch steps on or off, decide, confirm | **A** case exclusions: your codebook's own screening steps in their order (each shows how many cases the codebook removes there, so you can see the numbers agree), otherwise the survey's rules (unfinished, screened out, failed attention check; speeders optional). **B** skip-logic and rule-break check with Keep/Exclude and a short "Why?". **C** exclusions by case ID. **D** the scales your hypotheses use, already built (mean, sum, WHO-5 index, reversed items) - check or change them. **E** groups from plain conditions such as `ES01 is 1,2 and ES02 is 1`. The final N with one line per filtering step (e.g. 200 → 188 → 180 → 168). **Confirm data & continue** locks the data and saves the final Excel file, the codebook as applied, a preparation script and a project file. |
| **4. Describe & reliability** | Arrive; optionally change the Table 1 variables | Runs by itself: tables and charts of all variables, a ready-to-paste summary, Table 1 (sample characteristics), and **Cronbach's alpha for every scale** (item-total correlations, alpha if item deleted, Word report per scale). |
| **5. Assumption checks** | Read; switch to the alternative test if you prefer | For every hypothesis the assumptions of its test are checked on the final data (normality, equal variances, outliers, linearity, multicollinearity ...) with charts and plain-language verdicts. The test whose conditions are met is chosen, with the reason; a robust or rank-based alternative is offered. **Run** tests all hypotheses. |
| **6. Results** | Read, download | Per hypothesis: test, decision (H0 rejected or not, direction checked), charts, Word + HTML report and R script. A summary report with an auto-written **methods section** (sample flow, measures, analysis rules, AI use), Table 1, the breakdowns and all decisions. Optional: compare with numbers you already reported. |

## What each hypothesis report contains

1. **Data used**: file, variables, cases analysed and excluded
2. **Assumption screening**: each check, its result and what it means in plain words
3. **Assumption charts**: histograms with normal curves, Q-Q plots, boxplots, scatterplots, residual plots, VIF
4. **Decision**: parametric or non-parametric, the rule that decided it, and the references
5. **Result (ready to paste)**: an APA-style paragraph plus APA tables
6. **Result charts**: means with 95% CI, boxplots, scatterplots, coefficient and odds-ratio plots
7. **Plain-language explanation** of the process and the result
8. **Appendix**: the exact R script that produced every number (rerun it to reproduce them)
9. **Key terms**: plain-language definitions with examples of every statistical term used. In Word, the first mention of each term carries a comment with its definition; in the HTML report, hover over underlined terms.

A **summary report** combines all hypotheses and includes a software/AI disclosure paragraph you can adapt for your thesis declaration.

## Decision rules (fixed in advance)

| Design | Parametric (if assumptions hold) | Non-parametric (if not) |
|---|---|---|
| 2 independent groups | Welch's t-test | Mann-Whitney U |
| 3+ groups | Welch's ANOVA + Games-Howell | Kruskal-Wallis + Holm-corrected pairs |
| Same cases measured twice | Paired t-test | Wilcoxon signed-rank |
| Two numeric variables | Pearson r | Spearman rho |
| Two categorical variables | Chi-square | Fisher's exact test |
| Predict a numeric outcome | Linear regression | HC3 robust standard errors if variance is not constant |
| Predict a yes/no outcome | Logistic regression | Warnings for few events per predictor or separation |
| Scale | Cronbach's alpha + item analysis | - |
| Hierarchical regression (controls first, then predictors) | Change in R-squared with F-change test; each predictor's direction checked | Robust (HC3) Wald test if variance is not constant |
| Ordered groups ("rises from low to high") | Group comparison plus one-sided trend test (Spearman, group order vs outcome) | Both must be significant |
| Moderation (the effect of X on Y depends on W) | Regression with the X × W interaction (mean-centred), F-change test, simple slopes at -1 SD / mean / +1 SD or per group, expected pattern checked | HC3 robust tests if variance is not constant |
| Mediation (X → M → Y) | Regression paths + percentile bootstrap CI of the indirect effect (5,000 resamples) | HC3 robust path tests if variance is not constant |

Normality is judged by sample size following Kim (2013): Shapiro-Wilk below n = 50, skewness/kurtosis z-scores for n = 50-300, absolute skewness/kurtosis above 300. Mean-based tests are kept when normality fails but every group has n >= 30 and no extreme outliers (central limit theorem). Rating-scale items always use rank-based tests. You can override the automatic choice; the report then records it as your decision.

Directional hypotheses are tested two-sided by default and count as supported only if the effect is significant **and** in the stated direction. One-sided tests are available when the direction was planned in advance; confidence intervals stay two-sided.

## Validation

`Rscript validation/run_validation.R` checks every analysis against reference results from the R documentation, every decision rule, and that every report, chart and script is produced. It runs automatically on GitHub after every change.

It also scores QuantCompass against a **test library** (`validation/corpus/`):
- 9 data formats (plain, semicolon + decimal comma + Latin-1, SoSci UTF-16, Qualtrics with 3 header rows, LimeSurvey, Excel with notes rows, SPSS, Stata, 99 as missing) - all read 100% correctly
- 17 codebook layouts - variables, labels, answer codes and missing codes 100%, filter rules found in every file that has them
- 90 hypotheses from three invented studies, written in many styles - type, variables and direction must be right for at least 95% (currently 100%; the third study was written after the rules and scored 88% before any tuning)
- Variable recommendations: multi-item scales and their reversed items are found in every data format of the library, scale scores match a hand calculation, and a construct is always measured by its whole scale
- Codebook definitions, hypothesis count and AI chat (`validation/test_v06.R`): a codebook's scale table and screening table are read and reproduce its documented case counts; codes from a wrapped line never land on another question; pasted null hypotheses never count as extra hypotheses; every change the AI proposes is checked against the data, and single answers are never shown to it in summaries-only mode

## Working through the app

- Six numbered steps in the top bar: a green ✓ means the step is done, a dashed number means it unlocks after you confirm your final data in step 3.
- Every step has two panes that scroll on their own, and a **Back / Continue** bar at the bottom. The bar says what is still to do in the current step; Continue stays grey until the step's required part is done. In step 3 the button confirms your data, in step 5 it runs your hypotheses.
- The **Your selections** panel on the right lists everything chosen so far (files, hypotheses and the variables/scales they use, exclusions, final cases, reliability, results, AI). Click a step there to jump to it; hide it with the **Summary** button.

## AI assistant (your own key)

QuantCompass is made for people new to statistics, so the AI assistant is offered on the first page: choose a provider - Google Gemini, Mistral, Anthropic Claude, any OpenAI-compatible service, the GWDG Academic Cloud (Chat AI / SAIA, available to many German universities) or Ollama on your own computer - and paste your own key. You can still work without it; QuantCompass's own rules then make every recommendation.

**On every page, by itself:** it describes codebook entries the codebook leaves unclear and reads every hypothesis, recommending the dependent and independent variables and the columns or scales that measure them.

**The AI chat (✦ Ask AI, top bar - or the side panel's "AI chat" tab):** ask anything, anywhere - "explain my codebook check", "what is in my data?", "break down H2", "which test fits H3 and why?", "what does p = .03 mean?", "explain my results". It sees your whole project (data, codebook, exclusions, scales, hypotheses, assumption checks, results) and looks numbers up in your data when it needs them. **Explain** buttons next to codebook checks, scales, hypotheses, assumption checks and results ask it directly, and "describe it in your own words" boxes (variables of a hypothesis, exclusion rules) let it set things up from your description.

**It can also do the work:** tell it what to change - fix a codebook entry, set missing codes, build or change a scale, add an exclusion rule or exclude cases, set up the variables of a hypothesis, confirm hypotheses or the data, run the tests. Every change is first checked against your data, then:
- **Ask before every change** (default): each change shows Allow / Deny.
- **Ask once per task**: one Allow for all changes of a request.
- **Change without asking**: changes are made at once.

Every change is listed in the chat with **Undo**. Your original data file is never changed; changes go into the app's working data, the final data file and the preparation script. The AI never adds, splits or removes hypotheses unless you ask it to.

**What it sees** (setting in the chat): *Everything, incl. single answers* (default - summaries of every variable, and single answers when it looks something up) or *Summaries only* (counts, means, results; never single answers - use this when your data protection rules do not allow sending raw answers to an AI service).

- Every AI suggestion is checked against your variables and data before you see it; impossible suggestions are dropped.
- All statistics are computed in R, never by the AI. Reports and the methods section state where an AI suggestion was used.
- Your key is used for the session only. To keep it, copy `.env.example` to `.env` in the QuantCompass folder and fill it in; `.env` is never uploaded (it is in `.gitignore`).

## For advanced users

The engine can be used from R directly:

```r
options(qc.root = normalizePath(".")); source("engine/load.R")
ctx <- list(path = "my_data.csv", sheet = NULL, fixes = character(), labels = list(), level_labels = list(), out_dir = "results/my_run")
qc_run_hypothesis(list(id = "H1", text = "Satisfaction differs by gender",
                       spec = list(goal = "difference", outcome = "satisfaction", group = "gender")), ctx)
```

See `examples/run_example.R`. Layout: `app/` (click app), `engine/` (import, codebook, preparation, hypothesis breakdown, profiling, assumption screening, rules, charts, reports), `templates/` (one vetted R script per analysis), `validation/`, `examples/`, `workflows/` (step procedures).

## Academic use

- QuantCompass computes results; it cannot judge whether your study design fits your research question. Check with your supervisor.
- Mention QuantCompass and R in your methods section and include the generated scripts in your appendix.
- No AI computes any number. The AI assistant (your own key) only suggests codebook entries, hypothesis breakdowns and the variables that measure them; you confirm every suggestion, and its use is disclosed in the reports.

## License

Code: GPL-3.0. Made by [Sabbir Rifat](https://github.com/AEMSabbirRifat).
