# Publication Improvements Batch 2-3 Prompt

Use this prompt when you want the next session to continue publication-readiness
work after the first boundary/docs/export-API cleanup batch for the maintained
`ggpaintr` package.

Keep this file unchanged unless the user explicitly asks to edit this stored
prompt.

```text
You are helping with development in the `ggpaintr` repository.

`working_scripts/notes/start-codex.md` has already been injected for this session.
Follow it exactly, and rebuild context from the repo itself before making any changes.

This task continues publication-readiness improvement work after the first focused
boundary/docs/export-API cleanup batch.

Recommended order:
1. Batch 2: extensibility and exportability hardening
2. Batch 3: advanced integration ergonomics and community-polish follow-up

If only one batch is tackled in this session, do Batch 2 first.

Do not treat older notes or memory as authority. Treat current source code, tests,
README/vignettes, generated docs, package metadata, and maintained notes as the
final authority.

Before editing anything, rebuild context from:
1. `working_scripts/notes/index.md`
2. `working_scripts/notes/current-status.md`

Then read the smallest additional set needed for this task:
- `working_scripts/notes/project-overview.md`
- `working_scripts/notes/testing-strategy.md`
- `working_scripts/notes/next-steps.md`
- `README.Rmd`
- `R/paintr-app.R`
- `R/paintr-export.R`
- `R/paintr-placeholders.R`
- `R/paintr-parse.R`
- `R/paintr-runtime.R`
- `tests/testthat/test-export-shiny.R`
- `tests/testthat/test-extensibility.R`
- `tests/testthat/test-placeholder-registry.R`
- `tests/testthat/test-runtime-input-spec.R`
- `vignettes/ggpaintr-extensibility.Rmd`
- `vignettes/ggpaintr-placeholder-registry.Rmd`
- `tests/manual/manual-test-ggpaintr.Rmd`
- `tests/manual/manual-checklist-ggpaintr.md`

Batch 2 goal:
Improve the custom-placeholder extensibility path so it is more teachable,
supportable, and publication-ready without replacing the current registry-based
architecture.

Batch 2 primary problems to address:
1. Exported custom placeholders still depend on inline hook definitions, which is
   a real support and usability constraint.
2. The current placeholder-registry layer is powerful, but still feels closer to
   maintainer tooling than a polished contributor-facing extension surface.
3. The docs and tests should make the exportable-placeholder contract more explicit
   and easier for outside users to follow correctly.

Batch 2 required outcomes:
1. Make the custom-placeholder export contract clearer and safer.
2. Improve contributor ergonomics for authoring exportable custom placeholders
   without broadening the architecture into a new DSL.
3. Keep built-in and custom placeholders on the same registry/runtime/export path.
4. Keep README, vignettes, manual docs, and tests aligned with the hardened
   contract.

Batch 2 implementation expectations:
- Preserve the current `ggpaintr_placeholder()` / `ggpaintr_effective_placeholders()`
  registry model.
- Do not replace the formula-string authoring model.
- Prefer hardening and clarifying the current exportability contract over
  introducing a broad new abstraction layer.
- If you add helper affordances, keep them small, well-documented, and clearly
  within the current supported surface.
- If a deeper redesign becomes tempting, stop and keep this batch on the smaller,
  publication-readiness path instead.

Batch 2 specific decisions to implement:
- Keep the current registry contract as the supported extensibility seam.
- Make exportability expectations more explicit in source/docs/tests/manual guidance.
- Prefer stronger validation, clearer errors, and clearer authoring guidance before
  adding new public extension primitives.
- Do not attempt a full redesign of custom-placeholder serialization in this batch
  unless a minimal, low-risk improvement is clearly available.

Batch 3 goal:
Improve the advanced Shiny integration and community-facing support story after
Batch 2 is stable.

Batch 3 primary problems to address:
1. The advanced integration layer is capable, but still feels closer to package
   developer tooling than a polished external-user workflow.
2. The package needs a clearer “how to grow from simple to advanced usage” story
   for outside users.
3. Manual docs, examples, and support-facing guidance should better reinforce the
   intended supported path for embedded usage.

Batch 3 required outcomes:
1. Make the advanced integration path easier to discover and teach.
2. Decide whether to add a small higher-level integration affordance on top of the
   current id-based helpers, without replacing them.
3. Improve the progression from `ggpaintr_app()` to embedded integrations in docs
   and examples.
4. Keep the maintained package surface coherent and avoid broadening support for
   deeper `paintr_*` internals.

Batch 3 implementation expectations:
- Keep `ggpaintr_*` as the user-facing supported surface.
- If a Shiny module wrapper is added, layer it on top of the current helpers rather
  than replacing them.
- If no module wrapper is added, improve docs/examples/manual coverage enough that
  the current supported integration path is more community-ready.
- Keep support boundaries explicit and conservative.

Batch 3 specific decisions to implement:
- Prefer additive ergonomics on top of the current integration helpers rather than
  a redesign.
- Keep custom top-level ids, bind helpers, and pure-value helpers as the stable
  integration foundation.
- Do not expand the public promise around arbitrary internal placeholder ids or
  deeper `paintr_*` mechanics.
- If repo-noise cleanup such as `preconsideration/` archival is touched, keep it
  clearly separate from the maintained package path and do not let it derail the
  integration/usability focus.

Verification expectations for either batch:
- add or update focused `testthat` coverage for changed behavior
- update README/vignettes/manual docs when user-facing or support-facing behavior changes
- run `Rscript -e 'testthat::test_dir("tests/testthat")'`
- run `Rscript -e 'devtools::document()'` if roxygen/public docs change
- run `Rscript -e 'devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())'` if README source changes
- run `Rscript -e 'pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)'` if pkgdown or vignette-facing outputs change
- run `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'` before finishing

Acceptance criteria for Batch 2:
- the placeholder-registry/exportability story is easier to teach and harder to misuse
- the exportability contract is clearer in source/docs/tests than before
- the package remains on the same registry/runtime/export architecture
- tests and `R CMD check` pass after the changes

Acceptance criteria for Batch 3:
- the advanced integration path is easier to discover and explain
- the supported `ggpaintr_*` progression from simple wrapper to embedded usage is clearer
- any new integration affordance stays layered on top of the current supported helpers
- tests and `R CMD check` pass after the changes

Working rules:
- cite file paths and line numbers when explaining behavior
- keep startup light and only load additional notes if the task truly needs them
- update tests when behavior changes
- keep manual docs in sync when interaction or support workflows change
- do not edit stored prompt-library files unless necessary for this task
- trigger the notify hook when you finish or before asking for user input
```
