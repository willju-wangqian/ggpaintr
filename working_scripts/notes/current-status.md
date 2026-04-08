# Current Status

## Snapshot

- `ggpaintr` is maintained as a single package surface. Archived legacy package
  content remains under `archive/legacy-package/`.  
  Reference: `archive/legacy-package/`
- The public surface still centers on three groups: wrapper/runtime helpers,
  phase-1 Shiny integration helpers, and phase-2 placeholder extensibility
  helpers.  
  Reference: `R/paintr-app.R:21-567`, `R/paintr-parse.R:20-77`,
  `R/paintr-runtime.R:389-393`, `R/paintr-export.R:229-252`,
  `R/paintr-placeholders.R:44-131`, `NAMESPACE:3-26`
- Durable architecture, supported boundaries, and export design live in
  `working_scripts/notes/project-overview.md`. Use
  `working_scripts/notes/index.md` for the normal note-routing entry point.

## Current behavior boundaries

- Built-in placeholders remain `var`, `text`, `num`, `expr`, and `upload`.
  Custom placeholders are registered per app through
  `ggpaintr_effective_placeholders()` and now share the same parse, UI, runtime,
  copy-rule, and export path as built-ins.  
  Reference: `R/paintr-placeholders.R:69-149`, `R/paintr-parse.R:20-77`,
  `R/paintr-ui.R:210-253`, `R/paintr-runtime.R:180-214`,
  `R/paintr-copy.R:12-18`, `R/paintr-export.R:95-128`
- `upload` supports `.csv` and `.rds` and still derives a default object name
  from the uploaded filename when the dataset-name field is blank.  
  Reference: `R/paintr-upload.R:17-27`, `R/paintr-upload.R:35-95`
- `ggpaintr_app()`, `ggpaintr_server()`, `ggpaintr_server_state()`, and
  `generate_shiny()` still support runtime `copy_rules`, with default and
  override logic centralized in `R/paintr-copy.R`.  
  Reference: `R/paintr-app.R:191-215`, `R/paintr-app.R:513-567`,
  `R/paintr-copy.R:12-146`, `R/paintr-export.R:229-252`
- `ggpaintr_server()` remains a thin wrapper over shared state plus the standard
  bind helpers.  
  Reference: `R/paintr-app.R:546-567`
- Runtime failures still flow through the shared completion, plot-build, and
  render-validation pipeline with `Input error:` versus `Plot error:` labeling.  
  Reference: `R/paintr-runtime.R:251-393`
- Formulas with `var` and no data source still fail during UI preparation, but
  missing local data objects are deferred to draw-time plot errors.  
  Reference: `R/paintr-placeholders.R:663-777`,
  `tests/testthat/test-unsupported-use-cases.R:7-21`,
  `tests/testthat/test-runtime-feedback.R:58-77`
- Phase-1 integration still exposes only six configurable top-level ids.
  Internal placeholder ids and dynamic `var-*` outputs remain package-owned.  
  Reference: `R/paintr-app.R:21-118`, `R/paintr-app.R:231-447`
- Exported apps still keep explicit `ui <- fluidPage(...)`, explicit
  `server <- function(...)`, default `copy_rules <- NULL`, and compact
  reconstruction of custom copy rules and custom placeholders when needed.
  Exported custom placeholders still need inline hook definitions to stay
  standalone.  
  Reference: `R/paintr-export.R:64-128`, `R/paintr-export.R:137-201`,
  `R/paintr-placeholders.R:457-509`,
  `tests/testthat/test-export-shiny.R:1-336`

## Latest verification

- `Rscript -e 'testthat::test_dir("tests/testthat")'` passes with 294 tests in
  the latest recorded session run.
- `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'`
  completed cleanly with 0 errors, 0 warnings, and 1 standard timestamp note
  (`unable to verify current time`) in the latest recorded session run.
- `Rscript -e 'pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)'`
  completed cleanly in the latest recorded session run.
- `Rscript -e 'devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())'`
  completed cleanly in the latest recorded session run and regenerated
  `README.md`.
- `cran-comments.md` still needs to be refreshed to match the latest clean
  `--as-cran --no-manual` result.

## Completed recently

- implemented phase-2 extensibility through the per-app placeholder registry,
  including `ggpaintr_placeholder()`,
  `ggpaintr_effective_placeholders()`, and `ggpaintr_missing_expr()`  
  Reference: `R/paintr-placeholders.R:44-131`, `NAMESPACE:13-18`
- refactored parsing, UI dispatch, runtime completion, upload eval-env
  preparation, and copy-rule validation so built-in and custom placeholders
  share the same registry path  
  Reference: `R/paintr-parse.R:20-77`, `R/paintr-ui.R:210-253`,
  `R/paintr-runtime.R:180-214`, `R/paintr-upload.R:121-140`,
  `R/paintr-copy.R:12-146`
- extended export generation so custom-placeholder apps serialize only compact
  `custom_placeholders` definitions and rebuild the effective registry in the
  generated app  
  Reference: `R/paintr-export.R:95-128`,
  `tests/testthat/test-export-shiny.R:258-336`
- expanded README, pkgdown, manual docs, and vignettes to cover custom
  placeholders and the newer Shiny integration workflows  
  Reference: `README.Rmd:169-326`, `_pkgdown.yml:7-48`,
  `tests/manual/manual-test-ggpaintr.Rmd:602-841`,
  `vignettes/ggpaintr-placeholder-registry.Rmd:1-347`

## Current focus

- improve `var` handling for spaces and other non-syntactic column names
- decide whether phase 3 should focus on module-layer composition, deeper
  placeholder exportability guarantees, or developer ergonomics for common
  custom-placeholder patterns
- refresh `cran-comments.md` with the latest clean `--as-cran --no-manual`
  result

## Current risks or blockers

- `var` replacement still depends on `rlang::parse_expr(input_item)`, so spaced
  or otherwise non-syntactic column names remain a likely rough edge.  
  Reference: `R/paintr-placeholders.R:562-569`
- exported custom placeholders must currently define hook functions inline
  inside `ggpaintr_placeholder()` calls to remain exportable as standalone apps.  
  Reference: `R/paintr-placeholders.R:457-485`

## Quick re-entry points

Default startup path:

1. `working_scripts/notes/index.md`
2. `working_scripts/notes/current-status.md`
3. task-relevant source files, tests, and docs only

Load additional note files only when needed:

- `working_scripts/notes/knowledge-schema.md` for note maintenance, repo
  cleanup, or note-system edits
- `working_scripts/notes/project-overview.md` for architecture, public API, or
  support-boundary work
- `working_scripts/notes/testing-strategy.md` for testing, verification,
  README/pkgdown/manual-sync, or acceptance questions
- `working_scripts/notes/next-steps.md` for planning and prioritization

Task-specific source routes:

- placeholder registry:
  `R/paintr-placeholders.R`, `R/paintr-parse.R`, `R/paintr-runtime.R`,
  `tests/testthat/test-placeholder-registry.R`,
  `vignettes/ggpaintr-placeholder-registry.Rmd`
- copy behavior or prompt text:
  `R/paintr-copy.R`, `tests/testthat/test-copy-rules.R`, `README.Rmd`
- export behavior:
  `R/paintr-export.R`, `tests/testthat/test-export-shiny.R`
- Shiny integration helpers:
  `R/paintr-app.R`, `tests/testthat/test-extensibility.R`,
  `vignettes/ggpaintr-extensibility.Rmd`
- manual interaction behavior:
  `tests/manual/manual-test-ggpaintr.Rmd`,
  `tests/manual/manual-checklist-ggpaintr.md`
