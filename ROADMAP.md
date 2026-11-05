# Roadmap

| Stage | Content | Status |
|---|---|---|
| 0 | Repos, licence, structure, key safety (`.env`, `.gitignore`) | Done |
| 1 | Statistical core: 13 vetted R templates, rules engine, feasibility rules, profiler, validation suite + CI | Done (v0.1) |
| 2 | Data intake: sheet picker with previews, readiness verdict, one-click fixes recorded in the script, codebook names | **Done (v0.2)** |
| 3 | Click app: plain-language hypotheses (Option A), Excel hypothesis sheet (Option B), sentence confirmation, assumption screening -> parametric/non-parametric decision, override | **Done (v0.2)** |
| 4 | Outputs: 8-section Word + HTML reports, APA tables, 300-dpi charts, appendix code, disclosure paragraph, whole-dataset description | **Done (v0.2)**; textbook-cited "Explain this" with formulas planned |
| 5 | Optional AI layer, bring your own key (Gemini, Mistral, Claude, OpenAI-compatible, Ollama); summaries only, never raw data; every AI-written number checked against R output | Planned |
| 6 | Web version (same app via Shinylive on GitHub Pages); privacy note; pilot with volunteer students | Next |
| 7 | Thesis-grade mode: report critic (APA checklist + number matching), independent verifier rules, AI voting only when rules are uncertain | Planned |
| 8 | Multi-hypothesis orchestration; MCP server so AI assistants can call the validated engine | Planned |

## Design rules

1. R computes every number; AI never writes or changes analysis code.
2. The script given to the user is the file that ran.
3. The rules engine has the final say on which tests are valid.
4. Every template must pass validation before release.
5. Nothing is sent anywhere without the user's own key and consent.
