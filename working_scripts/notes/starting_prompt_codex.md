# Codex / ChatGPT Starting Prompt

Use this prompt at the start of a fresh ChatGPT/Codex session for this repo.

```text
You are helping with development in the `ggpaintr` repository.

Before doing anything else, rebuild context from the repo itself.

Read these files first, in this order:

1. working_scripts/notes/current-status.md
2. working_scripts/notes/paintr2-overview.md

Then read the implementation files referenced by those notes, especially:

- R/paintr2_func.R
- R/ui_function.R
- working_scripts/paintr_distribute.R
- DESCRIPTION
- tests/testthat/

Important project context:

- This repo contains an older package-level `ggpaintr` path and a newer `paintr2` formula-driven workflow.
- The main current development focus is the `paintr2` path.
- `paintr2` uses formula strings with placeholders:
  - `var`
  - `text`
  - `num`
  - `expr`
  - `upload`
- `upload` is currently implemented for `.csv` and `.rds`.
- There is now a package-standard automated test setup under `tests/testthat/`.
- The notes in `working_scripts/notes/` contain code references with file paths and line numbers. Use them.

After reading the files above:

1. Summarize your understanding of the current repo status.
2. Summarize the current `paintr2` architecture.
3. Summarize the current placeholder behavior.
4. Identify any relevant testing/dev workflow for the task.
5. Then help with this task:

[PASTE TASK HERE]

Working rules:

- Do not rely on memory from earlier sessions.
- Treat the repo notes as the starting point, but treat the current source code as the final authority.
- When explaining behavior, cite the file paths and line numbers you used.
- Prefer preserving the current `paintr2` direction and the current `testthat` structure.
- If you change behavior, update tests when needed.
- If you discover important new context, update the notes in `working_scripts/notes/` so a future fresh session can recover quickly.
- Be concise but concrete.
```
