# Workflow

The fixed pipeline the app follows. The order is enforced in code, not decided by an AI. The AI assistant (optional, the user's own key) may suggest and, with the user's permission, make checked changes; the engine has the final say.

| # | Step | Engine | User checkpoint | AI role (optional) |
|---|---|---|---|---|
| 1 | **Data & codebook**: read any data and codebook format; check every variable against the codebook; read scale definitions and screening rules | `qc_read`, `qc_read_any_codebook`, `qc_codebook_check` | Decide the ⚠ items | Describes variables the codebook leaves unclear (checked against the data) |
| 2 | **Hypotheses & variables**: read the hypotheses as written; type, direction, H1/H0; recommend the column or scale for each role | `qc_read_hyp_text`, `qc_dissect`, `qc_measure_candidates`, `qc_recommend_roles` | Confirm each hypothesis | Reads each hypothesis and recommends variables; never adds or removes hypotheses unless asked |
| 3 | **Prepare data**: exclusions, skip-logic check, case IDs, scales, groups; final data gate | `qc_exclusion_suggestions`, `qc_apply_prep` | Confirm the final data | Sets up exclusions, scales or groups on request |
| 4 | **Describe & reliability**: descriptives, Table 1, Cronbach's alpha | templates `descriptives`, `reliability_alpha` | - | Explains |
| 5 | **Assumption checks**: screening and the test chosen by the rules | `qc_screen` (assumptions), `qc_suggest` (rules) | Keep or switch to the alternative test | Explains |
| 6 | **Run & report**: fill the vetted template, run that exact file, write the reports and the methods section | `qc_run_hypothesis` | Download | Explains the results |

Rules for every step: R computes every number; every AI suggestion is checked against the data before the user sees it; every change is recorded in the preparation script; what the AI may see is the user's setting.
