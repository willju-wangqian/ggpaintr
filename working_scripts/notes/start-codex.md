You are helping with development in the `ggpaintr` repository.

Before doing anything else, rebuild context from the repo itself.

Read these files first, in this order:

1. `working_scripts/notes/knowledge-schema.md`
2. `working_scripts/notes/current-status.md`
3. `working_scripts/notes/project-overview.md`
4. `working_scripts/notes/testing-strategy.md`
5. `working_scripts/notes/next-steps.md`

Then read the active package implementation files, especially:

- `R/paintr-app.R`
- `R/paintr-copy.R`
- `R/paintr-export.R`
- `R/paintr-parse.R`
- `R/paintr-runtime.R`
- `R/paintr-ui.R`
- `R/paintr-upload.R`
- `R/paintr-utils.R`
- `DESCRIPTION`
- `NAMESPACE`
- `tests/testthat/test-copy-rules.R`
- `tests/testthat/test-export-shiny.R`
- `tests/testthat/`
- `tests/manual/manual-test-ggpaintr.Rmd`
- `tests/manual/manual-checklist-ggpaintr.md`

Important project context:

- The repo contains archived legacy package content under
  `archive/legacy-package/`.
- The maintained development focus is the `ggpaintr` package surface.
- `ggpaintr` uses formula strings with placeholders:
  - `var`
  - `text`
  - `num`
  - `expr`
  - `upload`
- `upload` currently supports `.csv` and `.rds`.
- `ggpaintr_app()`, `ggpaintr_server()`, and `generate_shiny()` now support
  runtime `copy_rules` customization, with default and override logic
  centralized in `R/paintr-copy.R`.
- Repo notes are the starting point, but current source code and tests are the
  final authority.

When the user says:

`update knowledge based on the schema defined at working_scripts/notes/knowledge-schema.md`

follow the maintenance rules in that schema file.

Working rules:

- Do not rely on memory from earlier sessions.
- Treat repo notes as the starting point, but treat current source code and
  tests as the final authority.
- When explaining behavior, cite file paths and line numbers used.
- Prefer preserving the current `ggpaintr` direction and the current `testthat`
  structure.
- If behavior changes, update tests when needed.
- If copy behavior or user-facing prompt text is touched, read
  `R/paintr-copy.R`, `tests/testthat/test-copy-rules.R`, and the custom-copy
  example in `README.md`.
- If export behavior is touched, read `tests/testthat/test-export-shiny.R`.
- If manual interaction behavior is touched, read
  `tests/manual/manual-test-ggpaintr.Rmd` and
  `tests/manual/manual-checklist-ggpaintr.md`.
- Do not create extra ad hoc markdown notes unless the schema allows it.
- Do not update `working_scripts/notes/developer-notes.md` unless the user
  explicitly asks for edits to that file.
- Be concise but concrete.
