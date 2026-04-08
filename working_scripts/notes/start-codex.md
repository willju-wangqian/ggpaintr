You are helping with development in the `ggpaintr` repository.

Before doing anything else, rebuild context from the repo itself.

Normal startup read:

1. `working_scripts/notes/index.md`
2. `working_scripts/notes/current-status.md`
3. the smallest relevant source files, tests, and docs for the user's task

Load additional notes only when needed:

- `working_scripts/notes/knowledge-schema.md` when the user asks to update
  knowledge, clean up the repo based on the schema, or when editing the note
  system itself
- `working_scripts/notes/project-overview.md` for architecture, public API, or
  support-boundary questions
- `working_scripts/notes/testing-strategy.md` for tests, verification,
  README/pkgdown/manual-sync work, or acceptance expectations
- `working_scripts/notes/next-steps.md` for planning, prioritization, or
  "what next" questions
- do not auto-read `working_scripts/notes/developer-notes.md`; only read it if
  the user explicitly asks

If the user explicitly asks to rebuild full context, read the maintained note
set plus the source files and tests that match the task.

Then inspect the current source and tests relevant to the user's task. Treat
current source code and tests as the final authority. Use
`working_scripts/notes/index.md` to choose the smallest relevant source, test,
and doc set for the task.

Working reminders:

- The repo contains archived legacy package content under
  `archive/legacy-package/`.
- The maintained development focus is the `ggpaintr` package surface.
- `README.Rmd` is the editable README source of truth. `README.md` is generated
  from it.
- Repo notes are the starting point. Current source code and tests are the
  final authority.
- Keep startup light. Do not preload optional notes unless the triggers above
  apply.

When the user says:

`update knowledge based on the schema defined at working_scripts/notes/knowledge-schema.md`

follow the maintenance rules in `working_scripts/notes/knowledge-schema.md`.

When the user says either:

- `clean up project repo`
- `clean up project repo based on the schema defined at working_scripts/notes/knowledge-schema.md`

follow the cleanup workflow in `working_scripts/notes/knowledge-schema.md`.

Working rules:

- Do not rely on memory from earlier sessions.
- Treat repo notes as the starting point, but treat current source code and
  tests as the final authority.
- When explaining behavior, cite file paths and line numbers used.
- Prefer preserving the current `ggpaintr` direction and the current `testthat`
  structure.
- If behavior changes, update tests when needed.
- If low-level runtime input discovery or advanced examples are touched, read
  `R/paintr-parse.R`, `R/paintr-runtime.R`, and the relevant README/vignette
  sections.
- If placeholder-registry behavior is touched, read `R/paintr-placeholders.R`,
  `R/paintr-parse.R`, `R/paintr-runtime.R`,
  `tests/testthat/test-placeholder-registry.R`, and the placeholder-registry
  vignette.
- If copy behavior or user-facing prompt text is touched, read
  `R/paintr-copy.R`, `tests/testthat/test-copy-rules.R`, and the relevant
  examples in `README.Rmd`.
- If export behavior is touched, read `R/paintr-export.R` and
  `tests/testthat/test-export-shiny.R`.
- If manual interaction behavior is touched, read
  `tests/manual/manual-test-ggpaintr.Rmd` and
  `tests/manual/manual-checklist-ggpaintr.md`.
- Do not create extra ad hoc markdown notes unless the schema allows it.
- Do not update `working_scripts/notes/developer-notes.md` unless the user
  explicitly asks for edits to that file.
- Be concise but concrete.
- Trigger the notify hook when you finish or before asking my input.
