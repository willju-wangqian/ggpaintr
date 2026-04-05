You are helping develop the `ggpaintr` repo.

Before doing anything else, read these files in this order:

1. working_scripts/notes/current-status.md
2. working_scripts/notes/paintr2-overview.md

Then read the current implementation files that those notes reference, especially:
- R/paintr2_func.R
- R/ui_function.R
- working_scripts/paintr_distribute.R
- DESCRIPTION
- tests/testthat/

Important context:
- The repo contains an older package-level `ggpaintr` path and a newer `paintr2` formula-driven workflow.
- The main current development focus is the `paintr2` path.
- `paintr2` uses formula strings with placeholders:
  - var
  - text
  - num
  - expr
  - upload
- `upload` is currently implemented for `.csv` and `.rds`.
- There is now a package-standard automated test setup under `tests/testthat/` and CI config in `.github/workflows/R-CMD-check.yaml`.
- The notes contain code references with line numbers. Use them.

After reading those files:
1. Summarize your understanding of the current project status.
2. Summarize the current `paintr2` architecture and placeholder behavior.
3. Identify any uncommitted work or current test/dev setup relevant to the task.
4. Then help with the following development task:

[PASTE TASK HERE]

Requirements:
- Do not assume old context outside the repo notes and current source code.
- Use the notes as the starting point, but trust the source code as the final authority.
- When explaining behavior, cite the file paths and line numbers you used.
- If you make code changes, preserve the current `paintr2` direction and test structure.
- If relevant, update tests and notes so future sessions can reload context quickly.
