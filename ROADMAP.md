# Roadmap

| Stage | Content | Status |
|---|---|---|
| 1 | Statistical core: vetted R templates, rules engine, assumption screening, validation suite + CI | Done (v0.1-v0.2) |
| 2 | Click app: plain-language hypotheses, Word + HTML reports, APA tables, charts, exact R script | Done (v0.2) |
| 3 | Survey intake: any data and codebook format, codebook check, exclusions, skip-logic check, scales, groups, final data gate; methods section + Table 1 | Done (v0.3-v0.4) |
| 4 | Hypotheses in any wording; mediation, moderation, hierarchical regression, before/after, trend tests | Done (v0.3-v0.4) |
| 5 | AI assistant with the user's own key: reads codebooks and hypotheses, recommends variables and scales, chat that explains and makes checked changes with permission and undo | Done (v0.4-v0.6) |
| 6 | Guided interface: hypotheses first, variables and scales recommended, step bar, Back / Continue, summary panel | Done (v0.4.1-v0.6) |
| 7 | **Web version**: the same app in the browser (R running in the browser via webR / Shinylive), no installation, data stays on the device | Next |
| 8 | **Pilot**: a methods course and thesis supervision; feedback on clarity and correctness | Next |
| 9 | More analyses: repeated-measures and mixed ANOVA, multilevel models, factor analysis, SEM (lavaan) for latent variables and multiple mediators | Planned |
| 10 | German interface; report checker (written results vs computed numbers, APA checklist) | Planned |

## Design rules

1. R computes every number; the AI never writes or changes analysis code.
2. The script given to the user is the file that ran.
3. The rules engine has the final say on which tests are valid.
4. Every template must pass validation before release.
5. Nothing is sent to an AI service without the user's own key and consent; what it may see is the user's setting.
6. Every change to the data is recorded in the preparation script; the original data file is never changed.
