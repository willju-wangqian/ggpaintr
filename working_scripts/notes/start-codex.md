# Codex Starting Prompt

Use this prompt at the start of a fresh Codex or ChatGPT session for this repo.

```text
You are helping with development in the `ggpaintr` repository.

Before doing anything else, rebuild context from the repo itself.

Read these files first, in this order:

1. working_scripts/notes/knowledge-schema.md
2. working_scripts/notes/current-status.md
3. working_scripts/notes/project-overview.md
4. working_scripts/notes/testing-strategy.md
5. working_scripts/notes/next-steps.md

Then read the implementation files referenced by those notes, especially:

- R/paintr2_func.R
- R/ui_function.R
- working_scripts/paintr_distribute.R
- DESCRIPTION
- tests/testthat/
- tests/manual/manual_test.Rmd
- tests/manual/manual-checklist-paintr2.md

Important project context:

- This repo contains an older package-level `ggpaintr` path and a newer
  `paintr2` formula-driven workflow.
- The main current development focus is the `paintr2` path.
- `paintr2` uses formula strings with placeholders:
  - `var`
  - `text`
  - `num`
  - `expr`
  - `upload`
- `upload` currently supports `.csv` and `.rds`.
- The notes in `working_scripts/notes/` are the note-level starting point, but
  source code is the final authority.

When the user says:

`update knowledge based on the schema defined at working_scripts/notes/knowledge-schema.md`

follow the maintenance rules in that schema file.

Working rules:

- Do not rely on memory from earlier sessions.
- Treat repo notes as the starting point, but treat current source code and
  tests as the final authority.
- When explaining behavior, cite file paths and line numbers used.
- Prefer preserving the current `paintr2` direction and the current `testthat`
  structure.
- If behavior changes, update tests when needed.
- If export behavior is touched, read `tests/testthat/test-export-shiny.R`.
- If manual interaction behavior is touched, read
  `tests/manual/manual_test.Rmd` and
  `tests/manual/manual-checklist-paintr2.md`.
- Do not create extra ad hoc markdown notes unless the schema allows it.
- Do not update `working_scripts/notes/developer-notes.md` unless the user
  explicitly asks for edits to that file.
- Be concise but concrete.
```
