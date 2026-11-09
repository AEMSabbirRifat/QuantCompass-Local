# QuantCompass v0.4 - progress note (updated 7 Nov 2026)

## Done (built and tested)
- **Import any data format** (`engine/import.R`): CSV/TSV/TXT in any encoding incl. SoSci Survey UTF-16 exports (label row detected), Excel, ODS, SPSS, Stata, SAS. The load code is written into every script.
- **Codebook** (`engine/codebook.R`): PDF, Word, Excel/CSV (wide and long), TXT/HTML, Qualtrics .qsf, LimeSurvey .lss, SPSS/Stata labels. Shows a ✓/⚠/✗ check table. "Not applicable" codes are only treated as missing if ticked. SoSci PHP skip rules are translated; scanned PDFs are reported, never guessed.
- **Preparation** (`engine/prepare.R`):
  - exclusions (unfinished, screened out, attention check, optional speeders)
  - skip-logic and invalid-code check with Keep/Exclude and "Why?", plus a "No rule breaks, continue" button
  - pattern hints (kept by default), manual exclusions by ID
  - scales (mean/sum/WHO-5 index, reverse coding, alpha) and groups from plain conditions
  - final data gate (OK / Needs editing) that saves Excel, the applied codebook, a preparation script and a project file
- **Analysis**:
  - per-hypothesis subsets and compare-only-these-groups
  - one-sided tests (CIs stay two-sided)
  - direction check (a significant effect in the wrong direction is reported as "not supported")
  - mediation template (bootstrap, 5,000 resamples, seed 2026) with a path diagram
  - new glossary terms
- **Hypothesis breakdown** (`engine/dissect.R`): rule-based reading of type, direction, compound parts and groups; variable matching using labels, question texts and synonyms; 2–3 test options; open questions instead of guesses.
- **Summary report**: auto-written methods section (sample flow, measures with alpha, analysis rules), Table 1, and the breakdown list.
- **App** (`app/app.R`): 6 steps (Data & codebook → Prepare → Final data → Describe → Hypotheses → Results). Analysis is locked until the final data is confirmed. Includes an optional check against already-reported numbers.
- **Validation**: `validation/test_v03.R` added, and all suites pass. Browser test on the thesis data: 269 → 250 → 203 → 170 → 169 (CASE 624), with 10 hypotheses split into 11 parts, all run.

## Done in the second session (6 Nov, afternoon)
- Example workbooks with a "Codebook" sheet are read automatically; codebooks with word codes ("yes = remote workers; no = on-site workers") now work.
- Example without a separate codebook re-tested in the browser: "Remote workers have higher well-being than on-site workers" is matched to remote_work (yes vs no).
- README.md (6-step flow, formats, mediation, one-sided/direction rule) and ROADMAP.md updated; engine version 0.3.0.
- Full validation passes; browser test on the thesis data passes again (169 cases, 11 parts, all reports).

## Done in the third session (6 Nov, evening)
- Multiple regression planner (outcome suggestions, predictor checks with reasons, Green 1991 sample size, VIF/high correlations) in step 5; prediction sentences recognised.
- Cronbach's alpha: reliability card in step 4 (all scales) and a reliability hypothesis type; reliability sentences recognised.
- Validation and browser tests pass.

## Published (6 Nov 2026, evening)
- Desktop folder updated (validate.yml is protected locally; it is on GitHub).
- GitHub QuantCompass-Local: 8 commits; validation run #27 passed.
- Next ideas: QuantCompass-Web (Shinylive) with the v0.3 engine.

## Notes
- WHO-5 identical-answer cases among the 170 eligible: **21** (earlier estimate said 22). They are kept.
- Thesis test settings used:
  - fin_stress = mean of FC01_03, FC02_01, FC03_01, FC04_01, FC05_02, FC06_01
  - WHO5_index from WH01_01–05
  - wsb_conflict = mean of BL02
  - acad_perf = AC02 reversed, with 6 set to missing
  - groups job_type, work_stress_group (not working and fin_stress < 5.5 vs working) and region (EU vs non-EU)

## v0.4 (7 Nov 2026): reads any data/codebook, understands any hypothesis wording, moderation, optional AI
- **Any codebook layout** (`engine/codebook_general.R`): files become lines + real tables (PDF tables rebuilt from word positions); readers for tables, variable mentions in text, SPSS syntax, SoSci, .qsf, .lss; per variable the reading that best fits the data wins; rules from tables, sentences, PHP, display logic, relevance; rules most answers break are dropped and reported; free-text "please specify" fields recognised; filter conditions never read as codes; undocumented missing codes (e.g. -9 in several coded questions) inferred and reported.
- **Any data layout** (`engine/import.R`): notes rows above the header, Qualtrics 3 header rows, decimal comma per column, file type by content.
- **Hypotheses in any wording** (`engine/hypothesis_general.R`): normalisation + new patterns (moderation, mediation "through", comparatives, "has an effect on", "more likely to", with/without groups, men/women), before/after pairs found from labels, fallback from variable mentions, confidence badge (Sure / Check / Unsure). Unordered coded categories (gender, region) are categories everywhere and enter models as dummies (factor line in the script).
- **Moderation analysis** (`templates/moderation.R`): centred interaction, F-change or robust Wald, simple slopes (-1 SD/mean/+1 SD or per group), expected pattern checked in the decision, simple-slopes chart, glossary, methods text.
- **Before/after** form in the app with expected direction and one-sided option; direction checked in the decision.
- **Optional AI help** (`engine/ai.R`, "✦ AI help" side panel): Gemini, Mistral, Claude, OpenAI-compatible (university gateways), Ollama; key per session or local `.env`; only names/labels/codebook text/hypotheses sent; every suggestion validated against the data; AI use disclosed in reports and methods.
- **Test library** (`validation/corpus/`): 9 data formats, 17 codebook layouts, 90 hypotheses (3 studies; the held-out third scored 88% before tuning, 100% after general fixes). New suites: `test_corpus.R`, `test_v04.R`, `test_ai.R`. Full validation passes.
- Browser tests: new study (moderation, before/after, dummies, chi-square, fallback) all run; N=200 walkthrough H1-H8 unchanged.
- Real codebooks: thesis 43 vars / 3 rules (ES03 code 4 correctly flagged: not in the codebook); N=200 65 vars / 3 rules, only AC02 code 6 left for the user.

## Published (7 Nov 2026, night)
- Desktop folder updated (58 files, checksums verified; your own extra files and results untouched).
- GitHub QuantCompass-Local: 11 commits; validation run #44 passed. Runs #35-#43 failed only because they ran on half-uploaded states, and one real issue: the newest officer package reads Word tables differently on GitHub; Word codebooks are now read straight from the document XML, independent of the officer version.

## v0.4.1 (9 Nov 2026): clearer, more professional interface
- Numbered step bar with status (✓ done, ● to do, ○ optional, dashed = locked until the data is confirmed).
- Every step: a short header saying what to do, two panes that scroll independently, and a fixed Back / Continue bar that says what is still missing. Continue is grey until the required part is done; in step 3 it confirms the data, in step 5 it runs the confirmed hypotheses.
- "Your selections" panel on the right: files, codebook check, exclusions, scales, groups, final cases, hypotheses (✓ / needs a look), results, AI status; click a step to jump there; can be hidden.
- AI help on demand: no more automatic AI calls. A ✦ AI help button in the top bar opens the settings with "when to use it"; "Ask AI" buttons on each hypothesis card, on a bar listing the unsure ones, and in the codebook step. Without a key the buttons open the settings.
- Calmer styling (solid buttons, no glow animations), laptop-sized top bar (1024-1440 px tested), single-column fallback on small screens.
- Tested: full browser run of all six steps using only the Back / Continue bar (1440x900 and 1280x720), AI drawer and Ask AI with a missing and an invalid key; full validation passes.

## v0.5 (9 Nov 2026): hypotheses first, variables recommended, AI assistant on every page
- New order: 1 Data & codebook -> 2 Hypotheses & variables -> 3 Prepare data (confirm) -> 4 Describe & reliability -> 5 Assumption checks -> 6 Results.
- Step 2: for every role (dependent / independent variable, groups, mediator, moderator, controls) QuantCompass recommends a column or a multi-item scale; click the recommendation, an alternative, or "Choose my own" (any column or your own items as a new scale). Badges show where the app and the AI agree.
- `engine/measures.R`: finds multi-item scales from codebook headings / column stems with the same answer scale, finds reversed items from the answers, builds them with the same code as the final data, and replaces a single item by its whole scale.
- AI assistant: connected on step 1 (card) or in the top bar; once connected it runs by itself (codebook gaps, every hypothesis), sees the found scales, and its suggestions are validated. Where QuantCompass is unsure, the card starts from the AI's reading.
- Step 3 builds the scales the hypotheses use; step 4 runs the description and Cronbach's alpha by itself; step 5 (new) shows the assumption checks and the chosen test per hypothesis before running.
- Tests: new suite `validation/test_measures.R` (57 checks); full validation passes. Browser runs of all six steps on the N=200 data (5 hypotheses, 168 cases, all results) and with a stand-in AI service (agree / differ / take over when unsure).

## Still open
- validate.yml on GitHub: optionally add "curl" to the package list (tests do not need it).
- The AI assistant was tested offline only (stand-in service); test once with a real key on the laptop (step 1 AI card > Test the connection).
- QuantCompass-Web still on the old engine.
