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

**Part A - Your data**

| Step | What you do | What QuantCompass does |
|---|---|---|
| **1. Data & codebook** | Choose your data file and (recommended) your codebook | Reads any format: CSV/TSV/TXT in any encoding (incl. SoSci Survey UTF-16 exports), Excel, OpenDocument, SPSS, Stata, SAS. Reads codebooks in **any layout**: PDF (tables are rebuilt from the page, text layouts read line by line), Word (tables or paragraphs), Excel/CSV (wide, long or dictionary style), TXT/HTML/Markdown, SPSS syntax, Qualtrics (.qsf), LimeSurvey (.lss) or the labels inside an SPSS/Stata file. Your data's column names anchor everything, several readers run side by side, and for each variable the reading that fits your actual data best is kept. Filter rules are found in tables, sentences, SoSci PHP, Qualtrics display logic and LimeSurvey relevance, and a rule that most answers break is reported instead of used. A missing-value code the codebook forgets (e.g. -9 in several questions) is detected and shown for you to check. Shows a check table: ✓ matches, ⚠ needs your decision, ✗ not in the codebook. Nothing marked ⚠ is applied unless you tick it. Scanned PDFs are reported, never guessed. |
| **2. Prepare** | Switch steps on or off, decide, build | **A** case exclusions suggested from the survey's own rules (unfinished, screened out, failed attention check; speeders optional). **B** skip-logic and rule-break check using only rules defined in your survey, with Keep/Exclude and a short "Why?" (or "No rule breaks, continue"). **C** exclusions by case ID with a reason. **D** scales (mean, sum, WHO-5 index, reverse coding, Cronbach's alpha). **E** groups from plain conditions such as `ES01 is 1,2 and ES02 is 1`. |

**Part B - Final data gate**

| Step | What you do | What QuantCompass does |
|---|---|---|
| **3. Final data** | Press **OK, use this data** or **Needs editing** | Shows the final N, variables, scales (alpha) and groups, and one line per filtering step in order (e.g. 269 → 250 → 203 → 170 → 169). OK locks the data and saves the final Excel file, the codebook as applied, a preparation script and a project file. Nothing is analysed before OK. |

**Part C - Analysis**

| Step | What you do | What QuantCompass does |
|---|---|---|
| **4. Describe** | One click; choose variables for Table 1 | Tables and charts of all variables, a ready-to-paste summary, Table 1 (sample characteristics), and **Cronbach's alpha for every scale** (item-total correlations, alpha if item deleted, Word report per scale). |
| **5. Hypotheses** | Paste your hypotheses exactly as written (or upload a sheet with ID + Hypothesis) | Breaks each one down, whatever the wording ("X and Y are correlated", "older students sleep less", "students with debt have lower ...", "anxiety decreases from before to after the workshop", "social support buffers the effect of stress on ..."): type (relationship, group difference, before/after, association of two categories, prediction, mediation, **moderation**), direction, compound parts (one sentence with two claims becomes H1b(i) and H1b(ii)), variables and groups matched from your labels and question texts, the expected outcome, H1 and H0, and open questions where something cannot be matched. Each breakdown shows how sure QuantCompass is (✓ Sure, ⚠ Check, ? Unsure); a sentence that matches no pattern is still broken down from the variables it names, and marked for checking. Coded categories without an order (gender 1/2/3, region 1-4) are treated as categories, never as numbers. Gives 2-3 test options with reasons and assumptions. You choose **Use recommended**, **Choose manually** or **Decide for me**, and confirm. Also understands prediction ("A, B and C predict Y") and reliability ("the items of X form a reliable scale"). **Plan a multiple regression**: pick the outcome and QuantCompass marks every other variable as a suitable (✓), questionable (⚠) or unsuitable (✗) predictor with the reason (items of the outcome scale, filtered questions, unordered codes, little variation), and checks sample size (N ≥ 50 + 8m; Green, 1991) and multicollinearity (VIF, r > .80). |
| **6. Results** | Click **Run all confirmed hypotheses** | Per hypothesis: assumption screening, test, decision, charts, Word + HTML report and R script. A summary report with an auto-written **methods section** (sample flow, measures, analysis rules), Table 1, the breakdowns and all decisions. Optional: compare with numbers you already reported. |

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

## Optional AI help (your own key)

QuantCompass works fully without AI. In the **AI help (optional)** tab you can switch on help from Google Gemini, Mistral, Anthropic Claude, any OpenAI-compatible service, the GWDG Academic Cloud (Chat AI / SAIA, available to many German universities) or Ollama on your own computer. It is only asked when QuantCompass is unsure: codebook entries it could not read, and hypotheses it could not break down with confidence.

- Sent: variable names, codebook labels and texts, your hypotheses. **Never your data values.**
- Every AI suggestion is checked against your variables and data before you see it; impossible suggestions are dropped. You decide whether to use it.
- All statistics are computed in R. Reports and the methods section state where an AI suggestion was used.
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
- No AI computes any number. Optional AI help (your own key) only suggests codebook entries and hypothesis breakdowns, and is disclosed in the reports when used.

## License

Code: GPL-3.0. Made by [Sabbir Rifat](https://github.com/AEMSabbirRifat).
