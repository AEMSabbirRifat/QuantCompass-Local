# Workflows (SOPs)

The fixed pipeline the app follows. Order is enforced in code (a state machine), not decided by an AI.
AI may only act where marked **[AI optional]**; the engine tools have the final say.

| # | Step | Tool (engine) | User checkpoint | AI role |
|---|---|---|---|---|
| 01 | **Profile**: list sheets, user picks one; detect types, missing codes, decimal commas, item batteries; verdict ready / needs fixing / blocked | `qc_sheets`, `qc_read`, `qc_profile` | Pick sheet | - |
| 02 | **Intake**: purpose, research question, hypotheses (3-5 short questions) | - | Answer questions | [AI optional] rephrase questions, parse hypotheses into goals |
| 03 | **Map**: each hypothesis -> goal -> columns; confirm variable types | `qc_vartype` | Confirm types + mapping | [AI optional] suggest column matches |
| 04 | **Feasibility + suggestions**: valid tests, recommended vs alternative, warnings or block with "do this instead" | `qc_suggest` | Choose a test | [AI optional] phrase the explanation |
| 05 | **Run**: fill vetted template, save script, run that exact file | `qc_render`, `qc_run` | - | - |
| 06 | **Report**: APA sentence, tables, charts, appendix script, disclosure statement | template `result` | Download | - |
| 07 | **Explain**: plain language -> formula with the user's numbers -> real-life example, cited | open-textbook index | Ask | [AI optional] write explanation from retrieved passages only |

Rules for every step: never send raw data to an AI; check every number an AI writes against the R result; log every change made to the data.
