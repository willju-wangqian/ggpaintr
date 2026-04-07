# Current Status

## Project state

`ggpaintr` is maintained as a single package surface.

Public entry points now fall into two groups:

- default wrapper and runtime surface:
  `ggpaintr_app()`, `ggpaintr_server()`, `paintr_formula()`,
  `paintr_build_runtime()`, `paintr_get_plot()`, and `generate_shiny()`  
  Reference: `R/paintr-app.R:493-535`, `R/paintr-parse.R:17-67`,
  `R/paintr-runtime.R:242-250`, `R/paintr-runtime.R:399-402`,
  `R/paintr-export.R:180-193`, `NAMESPACE:3-23`
- phase-1 extensibility surface for custom Shiny integrations:
  `ggpaintr_ids()`, `ggpaintr_server_state()`,
  `ggpaintr_bind_control_panel()`, `ggpaintr_bind_draw()`,
  `ggpaintr_bind_export()`, `ggpaintr_bind_plot()`,
  `ggpaintr_bind_error()`, `ggpaintr_bind_code()`,
  `ggpaintr_plot_value()`, `ggpaintr_error_value()`,
  `ggpaintr_code_value()`, `ggpaintr_controls_ui()`, and
  `ggpaintr_outputs_ui()`  
  Reference: `R/paintr-app.R:21-475`, `NAMESPACE:3-23`

Active implementation modules are:

- `R/paintr-app.R`
- `R/paintr-copy.R`
- `R/paintr-export.R`
- `R/paintr-parse.R`
- `R/paintr-runtime.R`
- `R/paintr-ui.R`
- `R/paintr-upload.R`
- `R/paintr-utils.R`

Legacy package content remains archived under:

- `archive/legacy-package/`

## Current behavior boundaries

Supported placeholders in the maintained path remain:

- `var`
- `text`
- `num`
- `expr`
- `upload`  
  Reference: `R/paintr-parse.R:3-6`

Current boundary summary:

- `upload` supports `.csv` and `.rds` and still derives a default object name
  from the uploaded filename when the dataset-name field is blank.  
  Reference: `R/paintr-upload.R:17-27`, `R/paintr-upload.R:35-49`,
  `R/paintr-upload.R:60-95`
- `var` still accepts expression-like input through `rlang::parse_expr()`.  
  Reference: `R/paintr-runtime.R:9-17`
- formulas with `var` and no data source still fail during UI preparation, but
  missing local data objects are deferred to draw-time plot errors.  
  Reference: `R/paintr-ui.R:250-320`, `R/paintr-runtime.R:326-377`
- runtime failures still flow through the shared completion, plot-build, and
  render-validation pipeline with `Input error:` versus `Plot error:` labeling.  
  Reference: `R/paintr-runtime.R:261-279`, `R/paintr-runtime.R:289-402`
- phase-1 extensibility currently makes only six top-level app ids
  configurable: control panel, draw button, export button, plot output, error
  output, and code output. Internal placeholder ids and dynamic `var-*`
  outputs remain package-owned.  
  Reference: `R/paintr-app.R:21-37`, `R/paintr-app.R:214-244`,
  `R/paintr-app.R:446-475`
- `ggpaintr_server()` is now a thin wrapper over shared state plus bind
  helpers, and advanced plot customization should use
  `ggpaintr_plot_value(runtime_result)` inside a user-defined `renderPlot()`
  rather than changing the default binder.  
  Reference: `R/paintr-app.R:181-199`, `R/paintr-app.R:319-429`,
  `R/paintr-app.R:516-535`

## Copy-rule status

The maintained app, extension layer, and export path use a shared copy-rule
system.

- `ggpaintr_app()`, `ggpaintr_server()`, `ggpaintr_server_state()`, and
  `generate_shiny()` all preserve the optional `copy_rules = NULL` path.  
  Reference: `R/paintr-app.R:181-199`, `R/paintr-app.R:493-535`,
  `R/paintr-export.R:180-193`
- default copy, validation, alias normalization, recursive merges, and final
  resolution live in `R/paintr-copy.R`. The current system supports validated
  top-level sections, `colour -> color` alias normalization, the internal
  `__unnamed__` key for positional arguments, public
  `paintr_effective_copy_rules()`, and export-side compaction back to minimal
  custom overrides.  
  Reference: `R/paintr-copy.R:507-643`, `NAMESPACE:19-23`
- `ggpaintr_bind_control_panel()` still routes generated control labels through
  `register_var_ui_outputs()` and `paintr_get_tab_ui()`, while
  `ggpaintr_controls_ui()` resolves shell button copy through
  `paintr_resolve_shell_copy()`.  
  Reference: `R/paintr-app.R:214-244`, `R/paintr-app.R:446-455`,
  `R/paintr-ui.R:250-426`, `R/paintr-copy.R:582-643`
- exported apps still keep an explicit editable `ui <- fluidPage(...)` and
  `server <- function(...) { paintr_state <- ggpaintr_server(...) }` template,
  resolve shell labels through exported `paintr_resolve_copy()`, write
  `copy_rules <- NULL` for the default case, and write compact
  `custom_copy_rules <- ...` plus
  `copy_rules <- paintr_effective_copy_rules(custom_copy_rules)` only for
  non-default customized exports.  
  Reference: `R/paintr-copy.R:507-580`, `R/paintr-export.R:64-155`,
  `tests/testthat/test-export-shiny.R:135-302`

## Testing and verification

Automated tests remain under:

- `tests/testthat/`

Manual interaction coverage remains under:

- `tests/manual/manual-test-ggpaintr.Rmd`
- `tests/manual/manual-checklist-ggpaintr.md`

Latest coverage highlights:

- dedicated automated extensibility coverage now lives in
  `tests/testthat/test-extensibility.R`. It covers id validation, optional UI
  helpers, pure value helpers, custom-id binders, and export visibility.  
  Reference: `tests/testthat/test-extensibility.R:5-237`
- dedicated automated copy-rule coverage remains in
  `tests/testthat/test-copy-rules.R`.  
  Reference: `tests/testthat/test-copy-rules.R:5-185`
- exported-app parity coverage for shell copy and serialized `copy_rules`
  remains in `tests/testthat/test-export-shiny.R`.  
  Reference: `tests/testthat/test-export-shiny.R:1-410`
- manual coverage still includes default copy, custom `copy_rules`, and
  exported custom-copy checks.  
  Reference: `tests/manual/manual-test-ggpaintr.Rmd:385-561`,
  `tests/manual/manual-checklist-ggpaintr.md:30-56`

Latest local verification status:

- `devtools::document()` completed cleanly in the latest session run
- `testthat::test_dir("tests/testthat")` passes with 255 tests in the latest
  session run
- `pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)`
  completed cleanly in the latest session run, including the new extensibility
  guide and reference grouping
- `devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran",
  "--no-manual"))` completed cleanly with 0 errors, 0 warnings, and 0 notes in
  the latest session run
- `cran-comments.md` still needs to be refreshed to match the latest clean
  result

## Completed recently

- implemented phase-1 extensibility for custom Shiny wrappers through exported
  ids, shared state, bind helpers, pure value helpers, and optional UI helpers  
  Reference: `R/paintr-app.R:21-475`, `NAMESPACE:3-23`
- refactored `ggpaintr_server()` into a thin wrapper over
  `ggpaintr_server_state()` plus the standard bind helpers while preserving the
  default wrapper behavior  
  Reference: `R/paintr-app.R:181-199`, `R/paintr-app.R:516-535`
- added a README integration example, a dedicated extensibility vignette, and a
  pkgdown reference grouping for the new `ggpaintr_*` integration surface  
  Reference: `README.Rmd:83-161`, `_pkgdown.yml:7-42`
- kept the exported app template on the established `ggpaintr_server()` path
  while preserving raw `copy_rules` for export and the explicit editable
  template shape  
  Reference: `R/paintr-export.R:99-193`, `tests/testthat/test-export-shiny.R:1-410`

## Current focus

- define phase-2 extensibility around contributor-facing extension points,
  clearer public/internal docs boundaries, and a possible module layer over the
  new id-based integration helpers
- improve `var` handling for spaces and other non-syntactic column names
- refresh `cran-comments.md` with the latest clean `--as-cran --no-manual`
  result

## Current risks or blockers

- phase-1 extensibility is currently limited to the id-based integration layer.
  A module layer and a supported contributor path for adding new
  placeholder/widget types are not implemented yet.  
  Reference: `R/paintr-app.R:21-475`, `_pkgdown.yml:7-42`
- `var` replacement still depends on `rlang::parse_expr(input_item)`, so
  spaced or otherwise non-syntactic column names remain a likely rough edge.  
  Reference: `R/paintr-runtime.R:9-17`

## Quick re-entry points

Read in this order:

1. `working_scripts/notes/knowledge-schema.md`
2. `working_scripts/notes/start-codex.md`
3. `working_scripts/notes/current-status.md`
4. `working_scripts/notes/project-overview.md`
5. `working_scripts/notes/testing-strategy.md`
6. `working_scripts/notes/next-steps.md`
7. `R/paintr-app.R`
8. `tests/testthat/test-extensibility.R`
9. `README.Rmd`
10. `vignettes/ggpaintr-extensibility.Rmd`
11. `R/paintr-export.R`
12. `R/paintr-runtime.R`
13. `tests/testthat/test-export-shiny.R`
14. `_pkgdown.yml`
