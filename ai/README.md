# AI help (optional) - implemented in `engine/ai.R`

QuantCompass computes every result in R without AI. The AI layer only **suggests**:
1. codebook entries the readers could not describe (label, answer codes, missing codes), and
2. breakdowns of hypotheses QuantCompass is unsure about.

Providers: Google Gemini, Mistral, Anthropic Claude, OpenAI or any OpenAI-compatible endpoint (e.g. a university gateway), Ollama (local).
Settings: the "AI help (optional)" tab in the app, or a local `.env` file (copy `.env.example`). Keys are never stored by the app and `.env` is git-ignored.

Rules the code follows (tested in `validation/test_ai.R` with a fake transport):
- Only variable names, codebook labels/texts and hypotheses are sent; category names only when they come from the codebook. Never data values or rows.
- "You are interacting with an AI system" is shown when AI help is on (EU AI Act, Art. 50).
- Every suggestion is validated: variables must exist, types must fit the analysis, the analysis must be possible with the data; AI answer codes must cover every value in the data, otherwise only the label is kept.
- The user decides; reports and the methods section disclose where an AI suggestion was used.
