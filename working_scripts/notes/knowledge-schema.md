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

1. `working_scripts/notes/current-status.md`
2. `working_scripts/notes/project-overview.md`
3. `working_scripts/notes/testing-strategy.md`
4. `working_scripts/notes/start-codex.md`
5. `working_scripts/notes/next-steps.md`

These files have distinct responsibilities and should not substantially
duplicate one another.

### Session-maintained files

These are the default files to review and update before a session ends:

- `working_scripts/notes/current-status.md`
- `working_scripts/notes/next-steps.md`

### Mostly stable files

These should be updated only when their own scope changes:

- `working_scripts/notes/project-overview.md`
- `working_scripts/notes/testing-strategy.md`
- `working_scripts/notes/start-codex.md`
- `working_scripts/notes/knowledge-schema.md`

### Non-core markdown files

These are not part of the five-file operational note system:

- `README.md`
- `tests/manual/manual-checklist-ggpaintr.md`
- `tests/manual/manual-test-ggpaintr.Rmd`
- `working_scripts/notes/developer-notes.md`
- app/package content docs under `inst/`
- older note files that are kept only for history or compatibility

## Maintenance Rules

### File responsibilities

`current-status.md`
- latest project state
- recent completed work
- current focus
- current risks or blockers
- quick re-entry reading order
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
- exact read order and working rules
- update only when startup instructions or canonical paths change

`next-steps.md`
- prioritized upcoming work
- dependencies and exit conditions
- update whenever priorities materially change

### Authoring rules

- Keep notes concise and high-signal.
- Prefer bullets and short sections over narrative logs.
- Cite source files and line numbers when describing implementation behavior.
- Do not create new ad hoc markdown files in the active notes root for one-off
  feature work.
- Temporary deep-dive notes should go to an archive area, not the active root.
- Completed work belongs in `current-status.md`, not in `next-steps.md`.
- Stable policy belongs in `testing-strategy.md`, not in `current-status.md`.
- Starter instructions belong only in `start-codex.md`.
- `developer-notes.md` is developer-maintained only and should not be updated by
  AI tools unless the user explicitly requests edits to that file.

## Read Order

For a fresh development session, read notes in this order:

1. `working_scripts/notes/knowledge-schema.md`
2. `working_scripts/notes/start-codex.md`
3. `working_scripts/notes/current-status.md`
4. `working_scripts/notes/project-overview.md`
5. `working_scripts/notes/testing-strategy.md`
6. `working_scripts/notes/next-steps.md`

Then read the implementation files and tests referenced by those notes.

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
- review `working_scripts/notes/start-codex.md` only if session-start
  instructions or canonical note paths changed
- review `working_scripts/notes/project-overview.md` only if architecture or
  supported behavior boundaries changed
- review `working_scripts/notes/testing-strategy.md` only if testing strategy,
  structure, or acceptance policy changed
- do not update `working_scripts/notes/knowledge-schema.md` unless the markdown
  maintenance system itself changed
