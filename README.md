# QuantCompass Local

**Guided, academically sound statistics for students: from your hypotheses to thesis-ready results, run on your own computer with R.**

QuantCompass is built for students and early researchers who have collected data but are not sure which test to run. You bring the data and your hypotheses; QuantCompass checks the data, suggests the tests that fit your design, tells you when an analysis is *not* possible (and what to do instead), runs vetted R code, and hands you the exact script that produced your results for your thesis appendix.

> **Two versions, one engine**
> - **QuantCompass Local** (this repo): runs on your computer with R. Most robust; your data never leaves your machine.
> - **[QuantCompass Web](https://github.com/AEMSabbirRifat/QuantCompass-Web)**: will run in the browser with no installation (R via WebAssembly).

**Status: v0.1, Stage 1 (statistical core).** The engine, 13 analysis templates and the validation suite are working. The guided interface and AI explanations come next; see [ROADMAP.md](ROADMAP.md).

---

## What makes it different

| | QuantCompass |
|---|---|
| **Starts from your hypotheses** | Maps each hypothesis to the right sheet, columns and tests, instead of making you pick from a menu. |
| **Says "not possible" and explains why** | Blocks or warns on infeasible designs: too few cases, too many predictors, categorical outcomes in mean tests, small expected counts. |
| **Same code, same result** | The script you receive is the file that ran. Re-running it reproduces your numbers exactly. |
| **Validated** | Every template is tested against published R reference results ([validation/](validation/)). |
| **Statistics without AI** | All numbers come from R. AI (optional, your own key) only phrases questions and explanations. |
| **Thesis-ready** | APA-style result sentences, effect sizes with confidence intervals, assumption checks, appendix code. |

## Analyses in v0.1

| Goal | Recommended test | Alternative (chosen automatically when assumptions fail) |
|---|---|---|
| Describe variables | Descriptive statistics | - |
| Compare 2 groups | Welch's t-test + Cohen's d | Mann-Whitney U + rank-biserial r |
| Compare 3+ groups | Welch's ANOVA + Games-Howell | Kruskal-Wallis + Holm-corrected pairs |
| Compare 2 measurements of the same people | Paired t-test + d_z | Wilcoxon signed-rank |
| Relationship of 2 numeric variables | Pearson r | Spearman rho |
| Relationship of 2 categorical variables | Chi-square + Cramer's V | Fisher's exact test (small counts) |
| Predict a numeric outcome | Multiple linear regression (VIF, Breusch-Pagan) | - |
| Predict a yes/no outcome | Binary logistic regression (odds ratios) | - |
| Scale reliability | Cronbach's alpha + item analysis (reverse-coding) | - |

## Quick start

1. Install [R](https://cran.r-project.org/) (4.1 or newer). RStudio is optional.
2. Install the two required packages: `install.packages(c("readxl", "testthat"))`
3. Download this repo (green **Code** button, then **Download ZIP**) and unzip it.
4. Open R in the unzipped folder and run:

```r
source("examples/run_example.R")
```

This walks through the example workbook (`examples/example_survey.xlsx`, synthetic data with 3 sheets): sheet selection, data check, three hypotheses, test suggestions, results, and the reproducible scripts saved in `output/`.

### Use it on your own data

```r
options(qc.root = normalizePath(".")); source("engine/load.R")
qc_sheets("my_data.xlsx")                       # 1. which sheet?
data <- qc_read("my_data.xlsx", "Sheet1")
qc_profile(data)$issues                         # 2. problems to fix first
opts <- qc_suggest(data, list(goal = "difference", outcome = "satisfaction", group = "gender"))
opts[[1]]$why; opts[[1]]$messages               # 3. why this test, any warnings
out <- qc_analyse("my_data.xlsx", "Sheet1", opts[[1]], "H1: Satisfaction differs by gender")
out$result$apa                                  # 4. APA-style result
writeLines(out$script, "H1_script.R")           # 5. exact script for your appendix
```

Goals: `describe`, `difference`, `paired_difference`, `relationship`, `association`, `prediction`, `reliability`.

## Validation

```bash
Rscript validation/run_validation.R
```

Checks every template against reference values from the R documentation (e.g. `?t.test` sleep data: t = -1.8608, df = 17.776, p = .079), plus identity checks against R's own implementations, the rules engine and the profiler. It runs automatically on every change (GitHub Actions).

## Repository layout (WAT: Workflows - Agents - Tools)

```
workflows/    step-by-step procedures (SOPs) the app follows
engine/       tools: file reading, profiler, rules engine, render & run
templates/    tools: one vetted R script per analysis
validation/   reference tests every release must pass
examples/     synthetic example workbook + end-to-end script
ai/           (Stage 5) optional AI provider adapters - bring your own key
```

## Using AI (optional, coming in Stage 5)

QuantCompass works fully without AI. When AI features arrive, you can connect **your own** API key (Gemini, Mistral, Claude, OpenAI-compatible, or a free local model via Ollama). Copy `.env.example` to `.env` and add your key. **Never commit `.env`.** Only summaries such as variable names and results are sent to the AI, never your raw data.

## Academic use

- Results are computed by R; QuantCompass does not guarantee that your study design is appropriate. Check with your supervisor.
- If you use QuantCompass in a thesis, say so in your methods section and include the generated script in the appendix.
- References for each test are listed in the rules engine (`engine/rules.R`) and in each suggestion's `references` field.

## License

Code: GPL-3.0. Explanatory content (from Stage 4): CC BY-SA 4.0, building on OpenIntro Statistics (CC BY-SA).

Made by [Sabbir Rifat](https://github.com/AEMSabbirRifat) - feedback and contributions welcome via Issues.
