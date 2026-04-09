Use the repo-local `publication-loop` skill from `.agents/skills/publication-loop/SKILL.md`.

You are running workflow phase `__PHASE_NAME__` of an unattended publication-readiness
workflow for the `ggpaintr` repository.

Requirements:

- before doing anything else, read and follow `CLAUDE.md`
- then open and execute the stored task prompt at `__TASK_PROMPT__`
- implement the stored prompt instead of stopping at analysis
- keep tests, README/vignettes/manual docs, and generated artifacts aligned when
  behavior changes
- do not edit stored prompt-library files under `dev/tasks/`
- do not ask for user approval or opinion during the process; make reasonable
  conservative assumptions and proceed
- you may use up to `__AGENT_COUNT__` subagents in parallel when helpful, with
  disjoint ownership and no duplicate work
- if `devtools` is unavailable in the runtime environment, use equivalent
  commands built from `roxygen2`, `pkgload`, and `rcmdcheck`

Execution guidance:

- treat the stored task prompt as the authoritative task definition for this phase
- treat current source code, tests, and docs as the final authority over notes
- do as much useful work as possible before returning
- run the relevant verification for what you changed
- if phase context is appended below, use it as workflow context but reassess the
  repo from current state before deciding anything substantial

Your final message must be valid JSON matching the provided output schema.
