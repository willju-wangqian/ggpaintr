# Current Status

## Project state

`ggpaintr` is now maintained as a single `ggpaintr` package.

Active package entry points are:

- `ggpaintr_app()`  
  Reference: `R/paintr-app.R:15-17`
- `ggpaintr_server()`  
  Reference: `R/paintr-app.R:36-93`
- `paintr_formula()`  
  Reference: `R/paintr-parse.R:17-67`
- `paintr_build_runtime()`  
  Reference: `R/paintr-runtime.R:399-402`
- `paintr_get_plot()`  
  Reference: `R/paintr-runtime.R:242-250`
- `generate_shiny()`  
  Reference: `R/paintr-export.R:38-55`

The runtime is now organized into package modules:

- `R/paintr-app.R`
- `R/paintr-export.R`
- `R/paintr-parse.R`
- `R/paintr-runtime.R`
- `R/paintr-ui.R`
- `R/paintr-upload.R`
- `R/paintr-utils.R`

Legacy package content has been moved under:

- `archive/legacy-package/`

## Current placeholder support

Supported placeholders in the maintained path:

- `var`
- `text`
- `num`
- `expr`
- `upload`

Current boundary summary:

- `upload` supports `.csv` and `.rds`
- `var` still accepts expression-like input through `parse_expr()`  
  Reference: `R/paintr-runtime.R:9-17`
- structural formula failures still happen at parse time
- structural formula parsing and placeholder detection are still built from one formula string into a `paintr_obj`  
  Reference: `R/paintr-parse.R:17-67`
- `var` with no data source still fails during UI preparation
- missing local data objects are deferred to draw-time plot errors  
  Reference: `R/paintr-app.R:67-95`, `R/paintr-runtime.R:326-377`
- render-time ggplot failures use the same inline runtime error channel
- dynamic `var` UI registration is handled by `register_var_ui_outputs()`  
  Reference: `R/paintr-ui.R:152-273`

## Package surface status

Current exported API is:

- `ggpaintr_app()`
- `ggpaintr_server()`
- `paintr_formula()`
- `paintr_build_runtime()`
- `paintr_get_plot()`
- `generate_shiny()`

Current generated package artifacts are expected to be derived from source:

- `NAMESPACE`
- `man/`
- `docs/`

Current naming status:

- active package/docs/notes now use `ggpaintr` as the maintained workflow name
- active manual assets now live at `tests/manual/manual-test-ggpaintr.Rmd` and `tests/manual/manual-checklist-ggpaintr.md`
- the active vignette now lives at `vignettes/ggpaintr-workflow.Rmd`

## Testing status

Automated tests remain under:

- `tests/testthat/`

Manual interaction coverage remains under:

- `tests/manual/manual-test-ggpaintr.Rmd`
- `tests/manual/manual-checklist-ggpaintr.md`

Latest verification status:

- `testthat::test_dir("tests/testthat")` passes with 129 tests in the latest session run  
  Reference: `tests/testthat/test-runtime-feedback.R:1-178`, `tests/testthat/test-export-shiny.R:1-140`
- `generate_shiny()` now writes an explicit `ui <- ...`, `server <- function(...)`, `shinyApp(ui, server)` template that calls `ggpaintr_server(...)`  
  Reference: `R/paintr-export.R:5-84`, `tests/testthat/test-export-shiny.R:1-33`
- `ggpaintr_server()` reusable runtime state is covered for both success and failure draws  
  Reference: `R/paintr-app.R:36-93`, `tests/testthat/test-export-shiny.R:76-140`
- `devtools::check()` completed cleanly in the latest session run except for the environment time-verification NOTE
- `pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)` completed cleanly in the latest session run
- `cran-comments.md` still records the earlier one-note result and should be refreshed before submission  
  Reference: `cran-comments.md:5-20`

## Session progress

Completed in this session:

- archived the legacy package implementation under `archive/legacy-package/`
- replaced the active package API with a focused `ggpaintr` exported surface
- split active runtime code into focused `R/` modules
- added roxygen2 comments across the active implementation
- regenerated `NAMESPACE` and `man/`
- rewrote `DESCRIPTION`, `README`, vignette, and pkgdown configuration around `ggpaintr`
- added `NEWS.md` and `cran-comments.md`
- renamed the active workflow/docs/manual assets from `paintr2` to `ggpaintr`
- rebuilt the canonical manual workbook at `tests/manual/manual-test-ggpaintr.Rmd`
- reran generated outputs and package verification after the rename sweep
- added exported `ggpaintr_server()` so custom or exported apps can reuse the standard server wiring and latest runtime state
- changed `generate_shiny()` to emit a thin editable template with explicit `ui` and `server`
- renamed the internal `var` UI helper from `output_embed_var()` to `register_var_ui_outputs()`

## Quick re-entry points

Read in this order:

1. `working_scripts/notes/knowledge-schema.md`
2. `working_scripts/notes/start-codex.md`
3. `working_scripts/notes/current-status.md`
4. `working_scripts/notes/project-overview.md`
5. `working_scripts/notes/testing-strategy.md`
6. `working_scripts/notes/next-steps.md`
7. `R/paintr-app.R`
8. `R/paintr-parse.R`
9. `R/paintr-export.R`
10. `R/paintr-runtime.R`
11. `tests/testthat/test-export-shiny.R`
12. `tests/testthat/test-runtime-feedback.R`
13. `tests/manual/manual-test-ggpaintr.Rmd`
