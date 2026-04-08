---
name: publication-loop
description: Run an autonomous publication-readiness / CRAN-prep improvement loop for the ggpaintr repository. Use when the user wants Codex to run the stored assessment prompt in working_scripts/notes/prompts/publication-readiness-cran-prep-prompt.md, turn findings into concrete plans, implement the fixes, delegate independent batches to subagents, and rerun the assessment after major updates until substantive requirements are addressed.
---

# Publication Loop

Use this skill for unattended publication-readiness work on `ggpaintr`.

## Startup

Rebuild context from the repository before judging or changing anything.

Read these first:

1. `working_scripts/notes/start-codex.md`
2. `working_scripts/notes/index.md`
3. `working_scripts/notes/current-status.md`
4. `working_scripts/notes/prompts/publication-readiness-cran-prep-prompt.md`

Then read only the smallest relevant code, tests, docs, and metadata needed for
the current assessment findings and planned batch. Treat current source code and
tests as the final authority.

Read `working_scripts/notes/testing-strategy.md` whenever the batch changes
behavior, tests, README/pkgdown/manual-sync expectations, or verification
workflow.

## Autonomous loop

Follow this loop without waiting for user confirmation unless you hit a true
external blocker or an irreversible high-risk choice that cannot be resolved
from repo context.

1. Run the strict publication-readiness assessment using the stored prompt's
   standards and output structure.
2. Convert findings into an implementation backlog, separating substantive
   blockers from minor polish.
3. Choose the highest-leverage batch that can be completed safely now.
4. If the user has allowed multiple agents, delegate independent sidecar tasks
   to subagents with disjoint file ownership. Keep urgent blocking work local.
5. Implement the batch, keeping docs, tests, and generated artifacts aligned
   with behavior changes.
6. Run the most relevant verification for the files you changed.
7. After a major update batch, rerun the strict assessment from the current repo
   state.
8. Continue until the reassessment shows no substantive unaddressed
   publication-readiness requirements, or the remaining items are blocked by
   external decisions, unavailable resources, or non-code constraints.

## Working rules

- Make reasonable conservative assumptions instead of asking for preferences.
- State assumptions in the final response after the work is done.
- Do not edit stored prompt-library files under `working_scripts/notes/prompts/`
  unless the user explicitly asks.
- When behavior changes, update tests and user-facing docs as needed.
- Prefer focused batches that leave the repo in a coherent state after each
  reassessment.
- Keep findings evidence-based and cite file paths and line numbers when
  reporting assessment results.

## Final response

Return a concise findings-first closeout that makes clear:

- the current overall assessment
- what was completed in this pass
- what requirements remain, if any
- whether another autonomous pass should continue
