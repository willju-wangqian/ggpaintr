You are helping with development in the `ggpaintr` repository.

Before doing anything else, rebuild context from the repo itself.

Normal startup read:

1. `dev/knowledge/index.md`
2. `dev/knowledge/current-status.md`
3. the smallest relevant source files, tests, and docs for the user's task

Load additional notes only when `index.md` says they are relevant.

Working rules:

- Do not rely on memory from earlier sessions.
- Treat repo notes as the starting point, but treat current source code and
  tests as the final authority.
- When explaining behavior, cite file paths and line numbers used.
- Prefer preserving the current `ggpaintr` direction and the current `testthat`
  structure.
- If behavior changes, update tests when needed.
- Do not create extra ad hoc markdown notes unless the schema allows it.
- Do not edit stored task files under `dev/tasks/`
  unless the user explicitly asks.
- Do not update `dev/knowledge/developer-notes.md` unless the user
  explicitly asks for edits to that file.
- Be concise but concrete.
- Trigger the notify hook when you finish or before asking my input.

When the user says:

`update knowledge based on the schema defined at dev/knowledge/knowledge-schema.md`

follow the maintenance rules in `dev/knowledge/knowledge-schema.md`.

When the user says either:

- `clean up project repo`
- `clean up project repo based on the schema defined at dev/knowledge/knowledge-schema.md`

follow the cleanup workflow in `dev/knowledge/knowledge-schema.md`.
