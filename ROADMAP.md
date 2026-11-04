# Roadmap

| Stage | Content | Status |
|---|---|---|
| 0 | Repos, licence, structure, key safety (`.env`, `.gitignore`) | Done |
| 1 | Statistical core: 13 vetted R templates, rules engine, feasibility rules, profiler, validation suite + CI | **Done (v0.1)** |
| 2 | Data intake: sheet picker with previews, readiness verdict, one-click fixes with a changelog | Engine done; UI next |
| 3 | Guided flow: questionnaire (purpose, research question, hypotheses), routing, hypothesis -> columns -> tests, 2-3 confirmation checkpoints, parallel checks | Next |
| 4 | Outputs: APA tables, 300-dpi charts, appendix code, AI-use disclosure statement, "Explain this" (plain language, formula, real-life example) from open textbooks with citations | Planned |
| 5 | Optional AI layer, bring your own key (Gemini, Mistral, Claude, OpenAI-compatible, Ollama); summaries only, never raw data; every AI-written number checked against R output | Planned |
| 6 | Privacy note, AI disclosure, accessibility; pilot with volunteer students | Planned |
| 7 | Thesis-grade mode: report critic (APA checklist + number matching), independent verifier rules, AI voting only when rules are uncertain | Planned |
| 8 | Multi-hypothesis orchestration; MCP server so AI assistants can call the validated engine | Planned |

## Design rules

1. R computes every number; AI never writes or changes analysis code.
2. The script given to the user is the file that ran.
3. The rules engine has the final say on which tests are valid.
4. Every template must pass validation before release.
5. Nothing is sent anywhere without the user's own key and consent.
