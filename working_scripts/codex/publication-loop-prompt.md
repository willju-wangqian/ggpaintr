Use the repo-local `publication-loop` skill from `.agents/skills/publication-loop/SKILL.md`.

You are running pass `__PASS__` of an unattended publication-readiness loop for
the `ggpaintr` repository.

Requirements:

- run the strict assessment based on
  `working_scripts/notes/prompts/publication-readiness-cran-prep-prompt.md`
- turn the assessment findings into concrete plans
- implement the plan instead of stopping at analysis
- after each major update batch, rerun the assessment from the current repo
  state before deciding what to do next
- continue until substantive requirements raised by the reassessment are
  addressed, or until you hit a true external blocker
- do not ask for my opinion during the process; make reasonable conservative
  assumptions and proceed
- you may use up to `__AGENT_COUNT__` subagents in parallel when helpful, with
  disjoint ownership and no duplicate work
- do not edit stored prompt-library files under `working_scripts/notes/prompts/`
- keep tests, README/vignettes/manual docs, and generated artifacts in sync when
  behavior changes
- if `devtools` is unavailable in the runtime environment, use equivalent
  commands built from `roxygen2`, `pkgload`, and `rcmdcheck` rather than
  treating that as a blocker by itself

Execution guidance:

- Do as much useful work as possible in this pass before returning.
- You may perform multiple assess -> plan -> implement -> reassess cycles within
  this single pass.
- If the repository is not yet meaningfully closer to publication readiness,
  keep working rather than returning early.
- Only return `status: "continue"` if another unattended wrapper pass should run
  after this one because more substantive work remains that is better handled in
  a fresh pass.
- Return `status: "completed"` only when the latest reassessment shows no
  substantive unaddressed publication-readiness requirements that you can act on
  now.
- Return `status: "blocked"` only for a real external blocker that cannot be
  responsibly resolved from repo context.

Your final message must be valid JSON matching the provided output schema.
