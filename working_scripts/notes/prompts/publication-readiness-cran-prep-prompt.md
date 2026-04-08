# Publication Readiness / CRAN-Prep Assessment Prompt

Use this prompt when you want a stricter publication-readiness review of the
`ggpaintr` package.

Keep this file unchanged unless the user explicitly asks to edit this stored
prompt.

```text
You are reviewing the `ggpaintr` R package as a future community-facing package
that is intended to be publishable and broadly usable.

Your job is to perform a strict publication-readiness / CRAN-prep style
assessment based on the current repository state.

Instructions:
- Rebuild context from the repo itself before judging anything.
- Start with:
  1. `working_scripts/notes/index.md`
  2. `working_scripts/notes/current-status.md`
- Then inspect the current source code, tests, README, vignettes, manual docs,
  package metadata, and generated docs that are relevant to a publication
  assessment.
- Treat current source code and tests as the final authority.
- Do not rely on memory from earlier sessions.
- Do not treat encouragement as the goal; the goal is an honest readiness
  review.

Evaluation goals:
1. List the package’s major strengths.
2. List the package’s major weaknesses or risks.
3. List concrete improvements needed before future publication and broader
   community adoption.
4. Assess whether the package is currently teachable, supportable, and
   maintainable for outside users.
5. Assess whether the package has a sufficiently clear supported API and
   stability boundary.
6. Assess whether the documentation is accurate, complete, and community-ready.
7. Assess whether the test suite and verification workflow are strong enough to
   support future publication.

Important evaluation criteria:
- package design coherence
- user-facing ergonomics
- API clarity and support boundary
- documentation completeness and accuracy
- examples and vignette quality
- testing coverage and verification discipline
- Shiny integration quality
- extensibility model
- export behavior
- error handling
- long-term maintainability
- likely adoption barriers

Output requirements:
- Organize the answer into these sections:
  - Overall Assessment
  - Pros
  - Cons
  - Potential Improvements
  - Publication and Community Readiness
  - Recommended Next Priorities
- Be concrete, not generic.
- Cite file paths and line numbers for important claims.
- Distinguish clearly between:
  - current strengths
  - current risks
  - future improvements
- If something is promising but not yet publication-ready, say so plainly.
- If documentation and implementation differ, treat that as a problem and point
  it out.
- Do not just summarize features; evaluate whether the package is genuinely
  usable, teachable, supportable, and maintainable for outside users.
- Prefer a review mindset: findings first, evidence-based reasoning, concise
  conclusions.
```
