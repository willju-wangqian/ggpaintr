# Current Status

## Project state

`ggpaintr` currently has two tracks in the repo:

1. the older package-level `ggpaintr` path
2. the newer `paintr2` formula-driven workflow

Current development focus is the `paintr2` path centered on:

- `R/paintr2_func.R`
- `R/ui_function.R`
- `working_scripts/paintr_distribute.R`

Core runtime flow:

- `paintr_formula()` parses one ggplot-like formula string and builds a `paintr_obj`  
  Reference: `R/paintr2_func.R:511-562`
- `output_embed_var()` resolves dynamic `var` controls from static or uploaded data  
  Reference: `R/ui_function.R:113-260`
- `paintr_complete_expr()` substitutes current input values and builds `code_text`  
  Reference: `R/paintr2_func.R:613-653`
- `paintr_get_plot()` evaluates the completed expression list into a plot  
  Reference: `R/paintr2_func.R:655-665`
- `paintr_build_runtime()` now wraps completion plus plot construction into one structured result for the app UI and exported apps, preserving generated code on plot-stage failures  
  Reference: `R/paintr2_func.R`
- `generate_shiny()` exports a standalone app script using the same runtime helpers  
  Reference: `R/paintr2_func.R:778-796`

## Current placeholder support

Supported placeholders in `paintr2`:

- `var`
- `text`
- `num`
- `expr`
- `upload`

Current behavior summary:

- `var` is rendered dynamically from available column names, but on substitution it is still parsed with `parse_expr()`, so expression-like inputs such as `log(mpg)` are allowed  
  Reference: `R/ui_function.R:22-27`, `R/ui_function.R:65-85`, `R/paintr2_func.R:254-265`
- `text` uses `textInput()` and blank values are removed from the completed expression  
  Reference: `R/ui_function.R:29-39`, `R/paintr2_func.R:277-286`
- `num` uses `numericInput()` and missing values are removed from the completed expression  
  Reference: `R/ui_function.R:41-51`, `R/paintr2_func.R:267-275`
- `expr` uses `textInput()`, is parsed with `parse_expr()`, and invalid syntax still errors during completion  
  Reference: `R/ui_function.R:53-63`, `R/paintr2_func.R:288-295`
- `upload` supports `.csv` and `.rds`, creates a file input plus dataset-name input, and injects uploaded data into the evaluation environment  
  Reference: `R/ui_function.R:1-20`, `R/paintr2_func.R:167-250`

Upload naming status:

- blank upload names still default from the uploaded filename  
  Reference: `R/paintr2_func.R:156-165`, `R/paintr2_func.R:199-200`
- custom upload names now normalize whitespace to underscores before `make.names()`  
  Example: `"custom dataset"` becomes `"custom_dataset"`  
  Reference: `R/paintr2_func.R:197-204`

## Testing status

Automated testing is in package-standard `testthat` layout:

- `tests/testthat/`
- `tests/testthat/fixtures/`
- `tests/manual/manual-checklist-paintr2.md`

Reference:

- `DESCRIPTION:26-41`
- `tests/testthat.R`

Current automated coverage includes:

- formula parsing and placeholder detection
- placeholder substitution
- upload helper behavior
- upload-backed `var` handling
- expression completion and plot construction
- supported and unsupported formula cases
- Shiny app export generation
- exported `input_formula` preservation in generated apps  
  Reference: `tests/testthat/test-export-shiny.R:1-58`

Manual testing status:

- one round of manual `paintr2` interaction testing has been completed in this session
- a runnable manual workbook now exists at `tests/manual/manual_test.Rmd`
- the manual checklist remains in `tests/manual/manual-checklist-paintr2.md`

## Session progress

Completed in this session:

- added `tests/manual/manual_test.Rmd` with `ggpaintr_basic2()` manual test formulas and suggested inputs
- finished one manual test round against the current `paintr2` interaction path
- updated upload custom-name handling so whitespace becomes underscores  
  Reference: `R/paintr2_func.R:197-204`
- fixed `generate_shiny()` formula-text export handling so the exported app preserves the formula string correctly  
  Reference: `R/paintr2_func.R:778-789`
- added export test coverage for `input_formula` preservation  
  Reference: `tests/testthat/test-export-shiny.R:31-58`
- added structured draw-time error handling for completion and plot failures, with inline Shiny error output and preserved code text on plot-stage failures
- added automated test coverage for malformed `expr`, invalid uploads, and plot-stage missing-object errors
- updated manual docs to cover inline error feedback flows

## Quick re-entry points

If starting fresh next time, read in this order:

1. `working_scripts/notes/knowledge-schema.md`
2. `working_scripts/notes/start-codex.md`
3. `working_scripts/notes/current-status.md`
4. `working_scripts/notes/project-overview.md`
5. `working_scripts/notes/testing-strategy.md`
6. `working_scripts/notes/next-steps.md`
7. `R/paintr2_func.R`
8. `R/ui_function.R`
9. `tests/testthat/test-export-shiny.R`
10. `tests/manual/manual_test.Rmd`
