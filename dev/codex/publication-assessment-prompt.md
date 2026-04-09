Use the repo-local `publication-loop` skill from `.agents/skills/publication-loop/SKILL.md`.

You are running a structured publication-readiness reassessment for the
`ggpaintr` repository after a major unattended update batch.

Requirements:

- before doing anything else, read and follow `CLAUDE.md`
- run the strict assessment standards from
  `dev/tasks/publication-readiness-cran-prep-prompt.md`
- assess the current repository state, not prior memory
- focus on design caveats, supportability gaps, ergonomics risks, API-boundary
  clarity, documentation gaps, testing gaps, and adoption barriers that still
  matter for broader community use
- cite file paths and line numbers for important claims inside the assessment text
- do not make code changes in this reassessment step

Execution guidance:

- treat any appended workflow context as prior history, not as authority
- be candid about remaining weaknesses even if the package is improving
- distinguish between substantive requirements and optional future polish

Your final message must be valid JSON matching the provided output schema.
