# AI assistant - implemented in `engine/ai.R`

QuantCompass computes every result in R, never with AI. Once the user connects a key, the AI assistant works on every page by itself and **suggests**:
1. codebook entries the readers could not describe (label, answer codes, missing codes), and
2. for every hypothesis: its type and direction, and the dependent / independent variables (mediator, moderator, groups) with the column or multi-item scale that measures each. It sees the scales QuantCompass found (`engine/measures.R`) with their items, and is told to prefer whole scales.

Providers: Google Gemini, Mistral, Anthropic Claude, OpenAI or any OpenAI-compatible endpoint, GWDG Academic Cloud (Chat AI / SAIA: https://chat-ai.academiccloud.de/v1, model list in the app), Ollama (local).
Settings: the AI card on step 1 or the "✦ AI assistant" button in the top bar, or a local `.env` file (copy `.env.example`). Keys are never stored by the app and `.env` is git-ignored.

Rules the code follows (tested in `validation/test_ai.R` with a fake transport):
- Only variable names, codebook labels/texts and hypotheses are sent; category names only when they come from the codebook. Never data values or rows.
- "You are interacting with an AI system" is shown when the AI assistant is on (EU AI Act, Art. 50).
- Every suggestion is validated: variables must exist, types must fit the analysis, the analysis must be possible with the data; AI answer codes must cover every value in the data, otherwise only the label is kept.
- The user decides; reports and the methods section disclose where an AI suggestion was used.
