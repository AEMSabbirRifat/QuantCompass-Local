# AI adapters (Stage 5 - planned)

Optional. QuantCompass computes every result in R without AI.
Planned: one small adapter per provider (Gemini, Mistral, Claude, OpenAI-compatible, Ollama), selected via `.env`.

Rules the adapters must follow:
- Send only summaries (variable names, types, test results), never raw rows.
- Show "You are interacting with an AI system" at the start of each session (EU AI Act, Art. 50).
- Every number in AI-written text is checked against the R `result`; mismatches are rejected.
- Users bring their own key; keys stay on their machine.
