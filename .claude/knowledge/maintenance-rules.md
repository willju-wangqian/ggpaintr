# Maintenance Rules

## Purpose

This file defines the maintenance contract for project knowledge and documentation.
It governs how notes, memory, and knowledge files are maintained — not how the R package code works.

Source code is the final authority. These rules are governance, not a session log.

## File Responsibilities

### Auto-loaded (every session)

`CLAUDE.md` (project root)
- Commands, architecture summary, conventions, governance rules, context routing, prompt contracts
- Update when architecture, commands, conventions, or governance rules change

### On-demand knowledge (`.claude/knowledge/`)

`project-overview.md`
- Durable architecture, public API surface, supported boundaries, export design, placeholder/copy model
- Update only when architecture or support boundaries meaningfully change

`testing-strategy.md`
- Automated/manual test policy, required test layers, fixture strategy, acceptance criteria
- Update only when testing policy or structure changes

`maintenance-rules.md` (this file)
- File responsibilities, maintenance workflow, archive rules
- Update only when the maintenance system itself changes

### Memory files (auto-loaded, volatile)

`current-status` (memory, type: project)
- Latest verification results, recently completed work, current focus, risks/blockers
- Update whenever repo truth changes during a session

`next-steps` (memory, type: project)
- Prioritized upcoming work, dependencies, exit conditions
- Update whenever priorities materially change

### Not maintained by Claude

`dev/developer-notes.md` — human scratchpad, do not edit unless user explicitly asks
`dev/tasks/*.md` — stored prompts, do not edit unless user explicitly asks

## Authoring Rules

- Keep notes concise and high-signal. Prefer bullets over narrative.
- Cite source files and line numbers when describing implementation behavior.
- Do not create ad-hoc markdown files in `.claude/knowledge/`.
- Completed work belongs in current-status memory, not next-steps.
- Stable policy belongs in testing-strategy, not current-status.
- Temporary deep-dive notes go to `dev/`, not `.claude/knowledge/`.

## Archive Rules

- Historical planning docs, superseded prompts, and one-off notes are archived/legacy material.
- Archived content stays untouched unless there is a specific historical reason to revise.
- When overlap exists between archived and maintained content, maintained files are authoritative.
- `archive/legacy-package/` contains legacy package content — not part of maintained surface.
- `preconsideration/` contains tracked historical exploratory material — excluded from package/build paths.
