# Publication Improvements Batch 1 Prompt

Use this prompt when you want the next session to execute the first focused
publication-readiness improvement batch for the maintained `ggpaintr` package.

Keep this file unchanged unless the user explicitly asks to edit this stored
prompt.

```text
You are helping with development in the `ggpaintr` repository.

`working_scripts/notes/start-codex.md` has already been injected for this session.
Follow it exactly, and rebuild context from the repo itself before making any changes.

This task is the first focused publication-readiness improvement batch, based on a prior strict CRAN-prep style assessment.

Goal:
Improve community-facing publication readiness by tightening the supported API/story,
reducing internal-doc leakage, and cleaning up the public export API surface.

Do not treat older notes or memory as authority. Treat current source code, tests,
README/vignettes, generated docs, and package metadata as the final authority.

Before editing anything, rebuild context from:
1. `working_scripts/notes/index.md`
2. `working_scripts/notes/current-status.md`

Then read the smallest additional set needed for this task:
- `working_scripts/notes/project-overview.md`
- `working_scripts/notes/testing-strategy.md`
- `DESCRIPTION`
- `NAMESPACE`
- `_pkgdown.yml`
- `README.Rmd`
- `R/paintr-export.R`
- `R/paintr-app.R`
- `R/paintr-parse.R`
- `R/paintr-runtime.R`
- `tests/testthat/test-export-shiny.R`
- `tests/testthat/test-extensibility.R`
- `tests/testthat/test-runtime-input-spec.R`
- `docs/news/index.md`
- the generated docs/reference or pkgdown configuration files needed to verify whether internal helpers are being surfaced

Primary problems to address in this batch:
1. The public API/support boundary is conceptually documented, but generated docs currently surface many internal helpers and muddy the community-facing story.
2. `generate_shiny()` still exposes a required deprecated legacy `var_ui` argument, which is awkward in the public API and in examples.
3. The changelog/public-facing docs should accurately describe the currently supported surface after this cleanup.

Required outcomes:
1. Make the user-facing/publicly documented API boundary clearer and more supportable for outside users.
2. Prevent internal helper pages from being treated as part of the public documentation surface.
3. Improve `generate_shiny()` so its public call shape is community-ready while preserving backward compatibility where reasonable.
4. Keep README, pkgdown/reference organization, generated docs, and tests aligned with the resulting supported surface.
5. Preserve the current `ggpaintr` direction and current `testthat` structure.

Implementation expectations:
- Prefer a minimally disruptive cleanup, not a broad redesign.
- Keep the beginner path centered on `ggpaintr_app()`, `ggpaintr_server()`, `ggpaintr_server_state()`, the documented integration helpers, the placeholder registry helpers, the runtime helpers that are intentionally exported, and `generate_shiny()`.
- Do not silently broaden the supported API.
- If you change public behavior or signatures, update tests and docs in the same change.
- If generated docs or pkgdown behavior depend on roxygen/documentation structure, make the smallest change that yields a cleaner public surface.
- Keep explicit evidence-based reasoning tied to current code and docs.

Specific decisions to implement in this batch:
- Treat API-boundary/docs cleanup and `generate_shiny()` signature cleanup as one combined publication-readiness pass.
- Prefer making `var_ui` optional/defaulted and clearly deprecated rather than keeping it as a required public argument.
- Align the changelog/public docs with the actual maintained public surface after the cleanup.
- Do not take on deeper placeholder-export redesign in this batch unless required to avoid breakage.

Verification expectations:
- add or update focused tests for any changed public signature or documentation-sensitive export behavior
- run `Rscript -e 'testthat::test_dir("tests/testthat")'`
- run `Rscript -e 'devtools::document()'` if roxygen/public docs are changed
- run `Rscript -e 'devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())'` if README source changes
- run `Rscript -e 'pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)'` if pkgdown/reference surface changes
- run `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'` before finishing

Acceptance criteria:
- the supported/public API story is clearer in source/docs than before
- internal helpers are no longer presented to outside users as part of the main public documentation surface
- `generate_shiny()` is easier to call and document as a public function
- README/pkgdown/changelog/generated docs are consistent with the current maintained surface
- tests and `R CMD check` pass after the changes

Working rules:
- cite file paths and line numbers when explaining behavior
- keep startup light and only load additional notes if the task truly needs them
- update tests when behavior changes
- do not edit stored prompt-library files unless necessary for this task
- trigger the notify hook when you finish or before asking for user input
```
