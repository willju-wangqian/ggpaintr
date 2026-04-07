# Current Status

## Project state

`ggpaintr` is maintained as a single package surface.

Public entry points now fall into three groups:

- default wrapper and runtime surface:
  `ggpaintr_app()`, `ggpaintr_server()`, `paintr_formula()`,
  `paintr_build_runtime()`, `paintr_get_plot()`, and `generate_shiny()`  
  Reference: `R/paintr-app.R:513-567`, `R/paintr-parse.R:20-77`,
  `R/paintr-runtime.R:232-240`, `R/paintr-export.R:229-252`,
  `NAMESPACE:3-26`
- phase-1 Shiny integration surface:
  `ggpaintr_ids()`, `ggpaintr_server_state()`,
  `ggpaintr_bind_control_panel()`, `ggpaintr_bind_draw()`,
  `ggpaintr_bind_export()`, `ggpaintr_bind_plot()`,
  `ggpaintr_bind_error()`, `ggpaintr_bind_code()`,
  `ggpaintr_plot_value()`, `ggpaintr_error_value()`,
  `ggpaintr_code_value()`, `ggpaintr_controls_ui()`, and
  `ggpaintr_outputs_ui()`  
  Reference: `R/paintr-app.R:21-493`, `NAMESPACE:4-21`
- phase-2 placeholder extensibility surface:
  `ggpaintr_placeholder()`, `ggpaintr_effective_placeholders()`, and
  `ggpaintr_missing_expr()`  
  Reference: `R/paintr-placeholders.R:44-131`, `NAMESPACE:13-18`

Active implementation modules are:

- `R/paintr-app.R`
- `R/paintr-copy.R`
- `R/paintr-export.R`
- `R/paintr-parse.R`
- `R/paintr-placeholders.R`
- `R/paintr-runtime.R`
- `R/paintr-ui.R`
- `R/paintr-upload.R`
- `R/paintr-utils.R`

Legacy package content remains archived under:

- `archive/legacy-package/`

## Current behavior boundaries

Built-in placeholders remain:

- `var`
- `text`
- `num`
- `expr`
- `upload`

Custom placeholders are now registered per app through
`ggpaintr_effective_placeholders()`, with parsed occurrences stored in
`paintr_obj$placeholder_map` metadata and dispatched through placeholder hooks.  
Reference: `R/paintr-placeholders.R:69-149`, `R/paintr-parse.R:20-77`,
`R/paintr-parse.R:90-124`

Current boundary summary:

- `upload` supports `.csv` and `.rds` and still derives a default object name
  from the uploaded filename when the dataset-name field is blank.  
  Reference: `R/paintr-upload.R:17-27`, `R/paintr-upload.R:35-95`
- `var` still accepts expression-like input through `rlang::parse_expr()`,
  now via the built-in placeholder resolver path.  
  Reference: `R/paintr-placeholders.R:562-569`
- formulas with `var` and no data source still fail during UI preparation, but
  missing local data objects are deferred to draw-time plot errors.  
  Reference: `R/paintr-placeholders.R:663-777`, `R/paintr-runtime.R:316-377`
- runtime failures still flow through the shared completion, plot-build, and
  render-validation pipeline with `Input error:` versus `Plot error:` labeling.  
  Reference: `R/paintr-runtime.R:251-402`
- phase-1 integration still makes only six top-level app ids configurable:
  control panel, draw button, export button, plot output, error output, and
  code output. Internal placeholder ids and dynamic `var-*` outputs remain
  package-owned.  
  Reference: `R/paintr-app.R:21-37`, `R/paintr-app.R:231-447`
- advanced plot customization should use
  `ggpaintr_plot_value(runtime_result)` inside a user-defined `renderPlot()`
  rather than changing the default plot binder.  
  Reference: `R/paintr-app.R:322-403`
- exported custom placeholders must currently be created with
  `ggpaintr_placeholder()` and define hook functions inline so standalone
  exported apps can serialize them safely.  
  Reference: `R/paintr-placeholders.R:451-509`

## Copy-rule and export status

The maintained app, integration layer, placeholder registry, and export path
use a shared copy-rule system.

- `ggpaintr_app()`, `ggpaintr_server()`, `ggpaintr_server_state()`, and
  `generate_shiny()` preserve the optional `copy_rules = NULL` path.  
  Reference: `R/paintr-app.R:191-215`, `R/paintr-app.R:513-567`,
  `R/paintr-export.R:229-252`
- copy defaults and validation now resolve against the effective placeholder
  registry, so custom placeholder keywords participate in
  `copy_rules$defaults`, `copy_rules$params`, and `copy_rules$layers`.  
  Reference: `R/paintr-copy.R:12-18`, `R/paintr-copy.R:144-146`,
  `R/paintr-copy.R:258-457`, `tests/testthat/test-placeholder-registry.R:64-100`
- exported apps keep the explicit editable `ui <- fluidPage(...)` and
  `server <- function(...) { paintr_state <- ggpaintr_server(...) }` template.  
  Reference: `R/paintr-export.R:137-201`
- exported apps now write `placeholders <- NULL` for the default case or
  compact `custom_placeholders <- ...` plus
  `placeholders <- ggpaintr_effective_placeholders(custom_placeholders)` for
  custom-placeholder exports.  
  Reference: `R/paintr-export.R:95-128`, `tests/testthat/test-export-shiny.R:258-336`

## Testing and verification

Automated tests remain under:

- `tests/testthat/`

Manual interaction coverage remains under:

- `tests/manual/manual-test-ggpaintr.Rmd`
- `tests/manual/manual-checklist-ggpaintr.md`

Latest coverage highlights:

- `tests/testthat/test-extensibility.R` covers id validation, optional UI
  helpers, pure value helpers, custom-id binders, and export visibility.  
  Reference: `tests/testthat/test-extensibility.R:5-237`
- `tests/testthat/test-placeholder-registry.R` covers placeholder constructor
  validation, registry pass-through, parsed metadata, runtime replacement,
  copy-rule integration, and wrapper compatibility.  
  Reference: `tests/testthat/test-placeholder-registry.R:5-140`
- `tests/testthat/test-export-shiny.R` now covers serialized
  `custom_placeholders`, placeholder-registry reconstruction, and
  non-exportable placeholder failures in addition to copy/export parity.  
  Reference: `tests/testthat/test-export-shiny.R:1-420`
- manual coverage now includes embedded custom-id apps, custom plot rendering
  with `ggpaintr_plot_value()`, live custom placeholders, and exported
  custom-placeholder checks.  
  Reference: `tests/manual/manual-test-ggpaintr.Rmd:602-841`,
  `tests/manual/manual-checklist-ggpaintr.md:58-79`

Latest local verification status:

- `Rscript -e 'testthat::test_dir("tests/testthat")'` passes with 294 tests in
  the latest session run
- `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'`
  completed cleanly with 0 errors, 0 warnings, and 1 standard timestamp note
  (`unable to verify current time`) in the latest session run
- `Rscript -e 'pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)'`
  completed cleanly in the latest session run; the `install = TRUE` path is the
  current reliable local pkgdown command for the new placeholder-registry
  vignette
- `Rscript -e 'devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())'`
  completed cleanly in the latest session run and regenerated `README.md`
- `cran-comments.md` still needs to be refreshed to match the latest clean
  `--as-cran --no-manual` result

## Completed recently

- implemented phase-2 extensibility through the per-app placeholder registry,
  including `ggpaintr_placeholder()`,
  `ggpaintr_effective_placeholders()`, and `ggpaintr_missing_expr()`  
  Reference: `R/paintr-placeholders.R:44-131`, `NAMESPACE:13-18`
- refactored parsing, UI dispatch, runtime completion, eval-env preparation,
  and copy-rule validation so built-in and custom placeholders follow the same
  registry path  
  Reference: `R/paintr-parse.R:20-77`, `R/paintr-ui.R:210-277`,
  `R/paintr-runtime.R:180-214`, `R/paintr-upload.R:121-140`,
  `R/paintr-copy.R:12-146`
- extended export generation so custom-placeholder apps serialize only
  `custom_placeholders <- ...` and rebuild the effective placeholder registry
  inside the generated app  
  Reference: `R/paintr-export.R:95-128`, `R/paintr-export.R:137-252`,
  `tests/testthat/test-export-shiny.R:258-336`
- expanded README, pkgdown, the manual workbook/checklist, and vignettes to
  cover custom placeholders plus the newer Shiny integration workflows  
  Reference: `README.Rmd:143-326`, `_pkgdown.yml:42-48`,
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

- `var` replacement still depends on `rlang::parse_expr(input_item)`, so
  spaced or otherwise non-syntactic column names remain a likely rough edge.  
  Reference: `R/paintr-placeholders.R:562-569`
- exported custom placeholders must currently define hook functions inline
  inside `ggpaintr_placeholder()` calls to remain exportable as standalone apps.  
  Reference: `R/paintr-placeholders.R:457-485`

## Quick re-entry points

Read in this order:

1. `working_scripts/notes/knowledge-schema.md`
2. `working_scripts/notes/start-codex.md`
3. `working_scripts/notes/current-status.md`
4. `working_scripts/notes/project-overview.md`
5. `working_scripts/notes/testing-strategy.md`
6. `working_scripts/notes/next-steps.md`
7. `R/paintr-app.R`
8. `R/paintr-placeholders.R`
9. `tests/testthat/test-placeholder-registry.R`
10. `README.Rmd`
11. `vignettes/ggpaintr-placeholder-registry.Rmd`
12. `vignettes/ggpaintr-extensibility.Rmd`
13. `R/paintr-export.R`
14. `R/paintr-runtime.R`
15. `tests/testthat/test-export-shiny.R`
16. `_pkgdown.yml`
