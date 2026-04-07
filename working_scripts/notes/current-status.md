# Current Status

## Project state

`ggpaintr` is maintained as a single package surface.

Public entry points are:

- `ggpaintr_app()`  
  Reference: `R/paintr-app.R:17-19`
- `ggpaintr_server()`  
  Reference: `R/paintr-app.R:40-114`
- `paintr_formula()`  
  Reference: `R/paintr-parse.R:17-67`
- `paintr_build_runtime()`  
  Reference: `R/paintr-runtime.R:399-402`
- `paintr_get_plot()`  
  Reference: `R/paintr-runtime.R:242-250`
- `generate_shiny()`  
  Reference: `R/paintr-export.R:108-120`

Active implementation modules are now:

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
  Reference: `R/paintr-ui.R:285-320`, `R/paintr-runtime.R:326-377`
- runtime failures still flow through the shared completion, plot-build, and
  render-validation pipeline with `Input error:` versus `Plot error:` labeling.  
  Reference: `R/paintr-runtime.R:261-279`, `R/paintr-runtime.R:289-402`

## Copy-rule status

The maintained app and export path now use a shared copy-rule system.

- `ggpaintr_app()`, `ggpaintr_server()`, and `generate_shiny()` all accept
  optional `copy_rules = NULL` inputs.  
  Reference: `R/paintr-app.R:17-19`, `R/paintr-app.R:40-47`,
  `R/paintr-export.R:158-179`
- default copy, validation, alias normalization, recursive merges, and final
  resolution now live in `R/paintr-copy.R`. The current system supports
  validated top-level sections, `colour -> color` alias normalization, the
  internal `__unnamed__` key for positional arguments, public
  `paintr_effective_copy_rules()`, and export-side compaction back to minimal
  custom overrides.  
  Reference: `R/paintr-copy.R:9-125`, `R/paintr-copy.R:252-580`,
  `R/paintr-copy.R:507-580`, `NAMESPACE:3-10`
- UI builders now route app-shell labels, upload labels, control labels/help,
  and layer checkbox text through the same resolver.  
  Reference: `R/paintr-app.R:123-186`, `R/paintr-ui.R:23-168`,
  `R/paintr-ui.R:379-426`
- exported apps now keep an explicit editable `ui <- fluidPage(...)` and
  `server <- function(...) { paintr_state <- ggpaintr_server(...) }` template,
  resolve shell labels through exported `paintr_resolve_copy()`, write
  `copy_rules <- NULL` for the default case, and write compact
  `custom_copy_rules <- ...` plus
  `copy_rules <- paintr_effective_copy_rules(custom_copy_rules)` only for
  non-default customized exports.  
  Reference: `R/paintr-app.R:45-77`, `R/paintr-copy.R:507-580`,
  `R/paintr-export.R:64-155`, `tests/testthat/test-export-shiny.R:135-302`
- exported multiline formulas are now written as readable multiline source in
  the generated app instead of a single string containing literal `\n`.  
  Reference: `R/paintr-export.R:34-56`,
  `tests/testthat/test-export-shiny.R:60-124`

## Testing and verification

Automated tests remain under:

- `tests/testthat/`

Manual interaction coverage remains under:

- `tests/manual/manual-test-ggpaintr.Rmd`
- `tests/manual/manual-checklist-ggpaintr.md`

Latest coverage highlights:

- dedicated automated copy-rule coverage now lives in
  `tests/testthat/test-copy-rules.R`.  
  Reference: `tests/testthat/test-copy-rules.R:5-134`
- exported-app parity coverage for shell copy and serialized `copy_rules`
  remains in `tests/testthat/test-export-shiny.R`.  
  Reference: `tests/testthat/test-export-shiny.R:1-340`
- manual coverage now includes default copy, custom `copy_rules`, and exported
  custom-copy checks.  
  Reference: `tests/manual/manual-test-ggpaintr.Rmd:382-558`,
  `tests/manual/manual-checklist-ggpaintr.md:30-56`

Latest local verification status:

- `devtools::document()` completed cleanly in the latest session run
- `testthat::test_dir("tests/testthat")` passes with 207 tests in the latest
  session run
- `pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)`
  was last confirmed cleanly in the previous verification run
- `devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran",
  "--no-manual"))` completed with only the environment
  `unable to verify current time` NOTE in the latest session run
- `cran-comments.md` still needs to be refreshed to match the latest one-note
  result

## Completed recently

- added the internal copy-rule registry and resolver under `R/paintr-copy.R`  
  Reference: `R/paintr-copy.R:9-585`
- threaded `copy_rules` through the live app, reusable server, exported
  template, and public documentation example  
  Reference: `R/paintr-app.R:17-18`, `R/paintr-app.R:40-78`,
  `R/paintr-export.R:18-120`, `README.md:71-121`
- added manual workbook and checklist coverage for default, custom, and
  exported copy rules  
  Reference: `tests/manual/manual-test-ggpaintr.Rmd:382-558`,
  `tests/manual/manual-checklist-ggpaintr.md:30-56`
- changed the export path so `ggpaintr_server()` preserves raw user
  `copy_rules` for export, default exports expose `copy_rules <- NULL`, custom
  exports write compact `custom_copy_rules <- ...` and rebuild
  `copy_rules <- paintr_effective_copy_rules(custom_copy_rules)`, and
  multiline `input_formula` values stay readable in the generated source.  
  Reference: `R/paintr-app.R:45-77`, `R/paintr-copy.R:507-580`,
  `R/paintr-export.R:34-155`, `tests/testthat/test-export-shiny.R:135-302`
- removed the unused package `stringr` import and test helper load, and updated
  export serialization to use `utils::capture.output()` so the local CRAN-style
  check is back down to the environment-only note  
  Reference: `DESCRIPTION:39-45`, `R/paintr-export.R:1-9`,
  `tests/testthat/helper-fixtures.R:1-7`

## Current focus

- review the public/internal docs boundary for copy helpers now that both
  `paintr_resolve_copy()` and `paintr_effective_copy_rules()` are exported
- improve `var` handling for spaces and other non-syntactic column names
- refresh `cran-comments.md` with the latest `--as-cran --no-manual` result

## Current risks or blockers

- `var` replacement still depends on `rlang::parse_expr(input_item)`, so
  spaced or otherwise non-syntactic column names remain a likely rough edge.  
  Reference: `R/paintr-runtime.R:9-17`
- local CRAN-style verification still ends with the environment-only
  `unable to verify current time` NOTE

## Quick re-entry points

Read in this order:

1. `working_scripts/notes/knowledge-schema.md`
2. `working_scripts/notes/start-codex.md`
3. `working_scripts/notes/current-status.md`
4. `working_scripts/notes/project-overview.md`
5. `working_scripts/notes/testing-strategy.md`
6. `working_scripts/notes/next-steps.md`
7. `R/paintr-copy.R`
8. `R/paintr-app.R`
9. `R/paintr-ui.R`
10. `R/paintr-export.R`
11. `R/paintr-runtime.R`
12. `tests/testthat/test-copy-rules.R`
13. `tests/testthat/test-export-shiny.R`
14. `tests/manual/manual-test-ggpaintr.Rmd`
