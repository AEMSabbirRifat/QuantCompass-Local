# QuantCompass - version history

What was built in each version, newest first. Every version passed the full validation suite (`Rscript validation/run_validation.R`), which also runs on GitHub after every change.

## v0.6 (9 Nov 2026) - AI chat, codebook definitions, hypotheses counted as written
- **AI chat on every page**: explains anything (data, codebook check, hypotheses, assumption checks, results), looks numbers up in the data, and makes changes on request (codebook entries, missing codes, scales, exclusions, case IDs, the variables of a hypothesis, confirming, running). Every change is checked against the data, needs the user's permission (ask every time / once per task / without asking) and can be undone. Data access setting: everything, or summaries only.
- **Explain** buttons next to codebook checks, scales, hypotheses, assumption checks and results; "describe it in your own words" boxes for variables and exclusion rules.
- **Codebook definitions**: scale definitions (items, reversed items, mean / sum / WHO-5 index) and screening tables (with the documented number of cases removed at each step) are read from the codebook text and used directly.
- **More robust codebook reading**: answer codes that fit none of a column's values are rejected; labels cut at a column border are completed; section headings end table rows.
- **Hypotheses counted as written**: null hypotheses pasted next to them are kept as their H0, wrapped lines are joined, and a sentence with two claims stays one hypothesis (its claims are tested separately); each claim keeps its own direction.
- **How a construct is measured** is shown for every scale: scoring, each item with its wording, reversed or not, Cronbach's alpha - editable.
- Tests: `validation/test_v06.R`.

## v0.5 (9 Nov 2026) - hypotheses first, variables recommended
- New order: data and codebook, hypotheses and variables, data preparation, description and reliability, assumption checks, results.
- For every role of a hypothesis (dependent / independent variable, groups, mediator, moderator, controls) QuantCompass recommends a column or a multi-item scale; the user clicks a recommendation, an alternative, or chooses their own.
- Multi-item scales found from codebook headings or item names; reversed items found from the answers (`engine/measures.R`).
- The AI assistant works on every page once a key is connected.
- Tests: `validation/test_measures.R`.

## v0.4.1 (9 Nov 2026) - clearer interface
- Numbered step bar with status, Back / Continue bar that says what is still to do, two independently scrolling panes, a "Your selections" summary panel.

## v0.4 (7 Nov 2026) - any data, any codebook, any wording
- Any data layout (notes rows, Qualtrics headers, decimal commas, file type by content) and any codebook layout (PDF tables rebuilt from the page, Word, Excel, text, SPSS syntax, Qualtrics, LimeSurvey).
- Hypotheses in any wording (moderation, mediation, comparisons, before/after, with/without groups), with a confidence mark.
- Moderation analysis (centred interaction, simple slopes, expected pattern checked); before/after comparisons.
- Optional AI layer with the user's own key (Gemini, Mistral, Claude, OpenAI-compatible, GWDG Academic Cloud, Ollama); every suggestion validated against the data.
- Test library: 9 data formats, 17 codebook layouts, 90 hypotheses (`validation/corpus/`).

## v0.3 (6 Nov 2026) - survey intake and hypothesis breakdown
- Codebook check, exclusions from the survey's own rules, skip-logic check, scales, groups, a final data gate that saves the prepared data and a preparation script.
- Hypothesis breakdown from plain sentences; mediation; one-sided tests; direction check; methods section and Table 1.
- Hierarchical regression, trend test for ordered groups, regression planner, Cronbach's alpha card.

## v0.1 - v0.2 (5 Nov 2026) - engine and click app
- 13 vetted R templates, rules engine, assumption screening with the parametric / non-parametric decision, validation suite and GitHub CI.
- Click app, Word + HTML reports with APA paragraphs, charts, plain-language explanations and the exact R script.

## Open points
- Test the AI assistant with more providers and models in real use.
- Pilot with students; collect feedback on clarity and correctness.
- Web version (see [ROADMAP.md](ROADMAP.md)).
