# Notes Index

## Purpose

This file is the lightweight router for the maintained `ggpaintr` note system.

- Read this first in a normal coding session.
- Use it to decide which other note files are worth loading.
- Keep it short; do not duplicate durable architecture or session status here.

## Default session load

For ordinary development work, read only:

1. `dev/knowledge/current-status.md`
2. the source files, tests, and docs directly relevant to the user's task

Only load the other note files when the triggers below apply.

## Note map

`dev/knowledge/current-status.md`
- latest verified repo state
- recent completed work
- current focus and blockers
- quick re-entry pointers

`dev/knowledge/knowledge-schema.md`
- markdown governance for the note system itself
- read only when updating repo knowledge, cleaning up the repo, or changing the
  note system

`dev/knowledge/project-overview.md`
- durable architecture
- supported public API and boundary notes
- read for architecture, API-surface, or support-boundary questions

`dev/knowledge/testing-strategy.md`
- durable testing policy
- verification commands and manual coverage expectations
- read for testing, verification, README/pkgdown/manual-sync, or acceptance work

`dev/knowledge/next-steps.md`
- prioritized roadmap and exit conditions
- read for planning, prioritization, or "what next" discussions

`dev/knowledge/start-codex.md`
- copy-paste session-start prompt
- not the main durable memory file once a session is running

`dev/tasks/publication-readiness-cran-prep-prompt.md`
- stored strict review prompt for future publication/community-readiness
  assessment
- read only when the user explicitly asks for that prompt or asks for a strict
  publication-readiness / CRAN-prep style package review
- do not edit unless the user explicitly asks

`dev/knowledge/developer-notes.md`
- developer-maintained scratchpad
- not part of the canonical AI memory system
- do not auto-read

## Task routing

Start with the smallest relevant code/doc set:

- general app/runtime flow:
  `R/paintr-app.R`, `R/paintr-runtime.R`, `R/paintr-ui.R`,
  `R/paintr-parse.R`
- placeholder-registry behavior:
  `R/paintr-placeholders.R`, `R/paintr-parse.R`, `R/paintr-runtime.R`,
  `tests/testthat/test-placeholder-registry.R`,
  `vignettes/ggpaintr-placeholder-registry.Rmd`
- copy behavior or user-facing prompt text:
  `R/paintr-copy.R`, `tests/testthat/test-copy-rules.R`, `README.Rmd`
- export behavior:
  `R/paintr-export.R`, `tests/testthat/test-export-shiny.R`
- upload behavior:
  `R/paintr-upload.R`, `tests/testthat/test-upload.R`
- Shiny integration helpers:
  `R/paintr-app.R`, `tests/testthat/test-extensibility.R`,
  `vignettes/ggpaintr-extensibility.Rmd`
- manual interaction behavior:
  `tests/manual/manual-test-ggpaintr.Rmd`,
  `tests/manual/manual-checklist-ggpaintr.md`
- package surface or docs:
  `DESCRIPTION`, `NAMESPACE`, `README.Rmd`, `_pkgdown.yml`

## Authority and scope

- Treat current source code and tests as the final authority over notes.
- The maintained development focus is the `ggpaintr` package surface.
- Legacy package content under `archive/legacy-package/` is archived context, not
  the maintained path.
