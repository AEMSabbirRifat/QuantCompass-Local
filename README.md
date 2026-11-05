# QuantCompass Local

**Click-only, academically sound statistics for students. Upload your data, describe your hypotheses in plain words, and get thesis-ready results, charts and the exact R script. No coding.**

For each hypothesis QuantCompass **screens the assumptions first**, **chooses the parametric or non-parametric test by fixed, published rules**, runs it, draws the charts, and writes a **Word + HTML report** explaining every decision, with a paragraph you can paste straight into your thesis.

> **Two versions, one engine**
> - **QuantCompass Local** (this repo): runs on your computer. Your data never leaves it.
> - **[QuantCompass Web](https://github.com/AEMSabbirRifat/QuantCompass-Web)**: the same app in the browser, no installation (in development).

**Status: v0.2 pilot.** Feedback welcome via Issues.

---

## Start in 5 steps (Windows, macOS, Linux)

1. **Install R** (free): [cran.r-project.org](https://cran.r-project.org/) - "Download R for Windows" - "base" - run the installer.
2. **Install RStudio Desktop** (free): [posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop/).
3. **Download QuantCompass**: green **Code** button above - **Download ZIP** - right-click the ZIP - **Extract All**.
4. **Open `START-QuantCompass.R`** in RStudio (inside the extracted folder; if there is a folder inside a folder, open the inner one).
5. Press **Ctrl + Shift + S** (Mac: Cmd + Shift + S) or click **Source** (top right of the script window). Do not use "Run App". The first start installs what it needs (a few minutes, once). The app then opens in your browser.

That's all. Everything else is clicking.

## How it works (in the app)

| Step | What you do | What QuantCompass does |
|---|---|---|
| **1. Your data** | Choose your CSV or Excel file, pick the sheet | Checks the data (missing-value codes like 99, decimal commas, text in number columns) and offers one-click fixes. Reads friendly names from a "Codebook" sheet if you have one. |
| **2. Describe dataset** | One click (optional: split by group) | Tables of all variables, histograms, bar charts, correlation heatmap, missing data, and a ready-to-paste summary paragraph. |
| **3. Hypotheses** | **Option A**: choose "Compare groups / Before vs after / Relationship / Association / Predict / Scale reliability" and pick columns from lists. **Option B**: fill the Excel hypothesis template (one row per hypothesis) and upload it. | Shows each hypothesis as a plain sentence, e.g. *"We will test whether house value differs between homes near the river and homes not near the river."* You confirm with **Yes, add it**. |
| **4. Results** | Click **Run all hypotheses** | For each hypothesis: assumption screening, test choice, analysis, charts, report and script. Everything is saved in the `results` folder. |

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

Normality is judged by sample size following Kim (2013): Shapiro-Wilk below n = 50, skewness/kurtosis z-scores for n = 50-300, absolute skewness/kurtosis above 300. Mean-based tests are kept when normality fails but every group has n >= 30 and no extreme outliers (central limit theorem). Rating-scale items always use rank-based tests. You can override the automatic choice; the report then records it as your decision.

## Validation

`Rscript validation/run_validation.R` checks every analysis against reference results from the R documentation, every decision rule, and that every report, chart and script is produced. It runs automatically on GitHub after every change.

## For advanced users

The engine can be used from R directly:

```r
options(qc.root = normalizePath(".")); source("engine/load.R")
ctx <- list(path = "my_data.csv", sheet = NULL, fixes = character(), labels = list(), level_labels = list(), out_dir = "results/my_run")
qc_run_hypothesis(list(id = "H1", text = "Satisfaction differs by gender",
                       spec = list(goal = "difference", outcome = "satisfaction", group = "gender")), ctx)
```

See `examples/run_example.R`. Layout: `app/` (click app), `engine/` (reading, profiling, assumption screening, rules, charts, reports), `templates/` (one vetted R script per analysis), `validation/`, `examples/`, `workflows/` (step procedures).

## Academic use

- QuantCompass computes results; it cannot judge whether your study design fits your research question. Check with your supervisor.
- Mention QuantCompass and R in your methods section and include the generated scripts in your appendix.
- No AI computes any number. Optional AI explanations (bring your own key) are planned.

## License

Code: GPL-3.0. Made by [Sabbir Rifat](https://github.com/AEMSabbirRifat).
