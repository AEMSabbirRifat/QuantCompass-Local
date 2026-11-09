# AI assistant - implemented in `engine/ai.R`

QuantCompass computes every result in R, never with AI. Once the user connects a key, the AI assistant works on every page by itself and **suggests**:
1. codebook entries the readers could not describe (label, answer codes, missing codes), and
2. for every hypothesis: its type and direction, and the dependent / independent variables (mediator, moderator, groups) with the column or multi-item scale that measures each. It sees the scales QuantCompass found (`engine/measures.R`) with their items, and is told to prefer whole scales.

The **AI chat** (`engine/assistant.R`) answers any question about the project and, when the user asks, proposes changes:
- It receives the project state (`qc_assist_context`): variables with labels, codes, codebook check and summaries of the answers; exclusions; scales with items, reversed items and alpha; hypotheses as the user wrote them; assumption checks and results.
- It may request read-only lookups (`qc_assist_lookup`): value counts, descriptives, cross-tables, correlations, codebook search, and single rows - rows only when the user chose "Everything, incl. single answers".
- Its actions (`qc_assist_action_types`) are checked by `qc_assist_check_action` against the data before the user sees them; the app applies them only with the permission the user chose (ask every time / once per task / without asking) and keeps an Undo snapshot.
- The system prompt forbids adding, splitting or removing hypotheses unless the user asks, and inventing numbers.

Providers: Google Gemini, Mistral, Anthropic Claude, OpenAI or any OpenAI-compatible endpoint, GWDG Academic Cloud (Chat AI / SAIA: https://chat-ai.academiccloud.de/v1, model list in the app), Ollama (local).
Settings: the AI card on step 1 or the "✦ AI assistant" button in the top bar; the chat opens with "✦ Ask AI", or a local `.env` file (copy `.env.example`). Keys are never stored by the app and `.env` is git-ignored.

Rules the code follows (tested in `validation/test_ai.R` with a fake transport):
- Hypothesis and codebook readings send only variable names, codebook labels/texts and hypotheses. The chat also sends summaries of the data and - only with the user's "Everything" setting - single rows it looks up.
- "You are interacting with an AI system" is shown when the AI assistant is on (EU AI Act, Art. 50).
- Every suggestion is validated: variables must exist, types must fit the analysis, the analysis must be possible with the data; AI answer codes must cover every value in the data, otherwise only the label is kept.
- The user decides; reports and the methods section disclose where an AI suggestion was used.
