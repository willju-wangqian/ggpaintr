# Knowledge Schema

## Purpose

This file defines the markdown maintenance contract for the `ggpaintr`
repository.

Its job is to keep repo knowledge easy to recover in a fresh session while
preventing note sprawl and duplicated, stale markdown files.

This is a governance document, not a session log.

- Update this file only when the markdown system itself changes.
- Do not store project status, implementation progress, or feature notes here.
- Treat source code as the final authority over all notes.

## Core Files

The active operational note set for this repo is:

1. `working_scripts/notes/index.md`
2. `working_scripts/notes/current-status.md`
3. `working_scripts/notes/project-overview.md`
4. `working_scripts/notes/testing-strategy.md`
5. `working_scripts/notes/start-codex.md`
6. `working_scripts/notes/next-steps.md`

These files have distinct responsibilities and should not substantially
duplicate one another.

`working_scripts/notes/knowledge-schema.md` governs the system, but it is not
part of the default lightweight startup load for ordinary coding tasks.

### Session-maintained files

These are the default files to review and update before a session ends:

- `working_scripts/notes/current-status.md`
- `working_scripts/notes/next-steps.md`

### Mostly stable files

These should be updated only when their own scope changes:

- `working_scripts/notes/index.md`
- `working_scripts/notes/project-overview.md`
- `working_scripts/notes/testing-strategy.md`
- `working_scripts/notes/start-codex.md`
- `working_scripts/notes/knowledge-schema.md`

### Non-core markdown files

These are not part of the operational note system:

- `README.md`
- `tests/manual/manual-checklist-ggpaintr.md`
- `tests/manual/manual-test-ggpaintr.Rmd`
- `working_scripts/notes/developer-notes.md`
- app/package content docs under `inst/`
- older note files that are kept only for history or compatibility

## Maintenance Rules

### File responsibilities

`index.md`
- lightweight router for the note system
- first note read in a normal coding session
- default versus conditional note-loading rules
- canonical file-routing pointers
- update only when the note-loading strategy or canonical routing changes

`current-status.md`
- latest project state
- recent completed work
- current focus
- current risks or blockers
- quick re-entry reading order
- should stay focused on volatile state rather than re-explaining durable
  architecture already covered elsewhere
- update whenever repo truth changed during the session

`project-overview.md`
- durable architecture and mental model
- key workflows and supported boundaries
- update only when architecture or support boundaries meaningfully change

`testing-strategy.md`
- durable testing policy
- automated/manual split
- fixture and acceptance strategy
- update only when testing policy or structure changes

`start-codex.md`
- strict reusable session-start prompt
- copy-paste entry prompt for new sessions
- lightweight default read order and working rules
- update only when startup instructions or canonical paths change

`next-steps.md`
- prioritized upcoming work
- dependencies and exit conditions
- update whenever priorities materially change

### Authoring rules

- Keep notes concise and high-signal.
- Prefer bullets and short sections over narrative logs.
- Cite source files and line numbers when describing implementation behavior.
- Keep the default startup path lightweight: `index.md`, `current-status.md`,
  then task-relevant source files and tests.
- Do not create new ad hoc markdown files in the active notes root for one-off
  feature work.
- Temporary deep-dive notes should go to an archive area, not the active root.
- Completed work belongs in `current-status.md`, not in `next-steps.md`.
- Stable policy belongs in `testing-strategy.md`, not in `current-status.md`.
- Durable routing rules belong in `index.md`, not duplicated across every note.
- Starter instructions belong only in `start-codex.md`.
- `developer-notes.md` is developer-maintained only and should not be updated by
  AI tools unless the user explicitly requests edits to that file.

## Read Order

For a fresh development session:

1. use `working_scripts/notes/start-codex.md` as the copy-paste session-start
   prompt
2. in a normal coding session, read `working_scripts/notes/index.md`
3. read `working_scripts/notes/current-status.md`
4. load additional note files only when `index.md` says they are relevant
5. then read the implementation files and tests directly relevant to the task

Read `working_scripts/notes/knowledge-schema.md` at the start only when:

- the user asks to update knowledge
- the user asks to clean up the repo based on the schema
- the note system itself is being changed

## Archive Rules

- Feature-specific temporary notes should not be added to the active notes root.
- Historical planning docs, superseded prompts, and one-off implementation notes
  should be treated as archived or legacy material.
- Archived notes should stay untouched unless there is a specific historical
  reason to revise them.
- When there is overlap between an archived note and a core file, the core file
  is the maintained source of note-level truth.

## Prompt Contract

When the user says:

`update knowledge based on the schema defined at working_scripts/notes/knowledge-schema.md`

Codex should:

1. reread `working_scripts/notes/knowledge-schema.md`
2. inspect the current repo state relevant to recent work
3. update only the files whose responsibilities in this schema require updating
4. avoid touching stable or archived docs unless their own contract says they
   should change
5. keep source code and current tests as the final authority over notes

Default review targets for that instruction:

- always review `working_scripts/notes/current-status.md`
- always review `working_scripts/notes/next-steps.md`
- review `working_scripts/notes/index.md` only if routing rules, canonical note
  paths, or the default startup load changed
- review `working_scripts/notes/start-codex.md` only if session-start prompt or
  canonical startup instructions changed
- review `working_scripts/notes/project-overview.md` only if architecture or
  supported behavior boundaries changed
- review `working_scripts/notes/testing-strategy.md` only if testing strategy,
  structure, or acceptance policy changed
- do not update `working_scripts/notes/knowledge-schema.md` unless the markdown
  maintenance system itself changed

When the user says:

`clean up project repo based on the schema defined at working_scripts/notes/knowledge-schema.md`

Codex should:

1. run the full `testthat` suite for the repo
2. run `R CMD check` in the current CRAN-submission style used by the project
3. run `pkgdown::build_site_github_pages()` to refresh the GitHub Pages site
4. inspect `README.Rmd` as the editable README source of truth
5. knit or render `README.Rmd` to regenerate `README.md`
6. finally apply the `update knowledge based on the schema defined at working_scripts/notes/knowledge-schema.md` workflow

Default expectations for that instruction:

- use the current maintained repo commands for `testthat`, package check, and
  pkgdown build
- treat `README.Rmd` as the source of truth and `README.md` as generated output
- if `README.Rmd` is already current, still regenerate `README.md` so the repo
  stays clean and synchronized
- report any failures encountered during tests, checks, pkgdown build, or README
  rendering before updating knowledge notes
