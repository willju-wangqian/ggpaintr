# Plan: Expert Test Strategy for `ggpaintr` / `paintr2`

## Summary

Set this up as a package-standard test suite, not a custom top-level `testing/` folder.

Recommended structure:
- Put automated tests in `tests/testthat/`
- Put reusable test fixtures in `tests/testthat/fixtures/`
- Keep only exploratory or one-off demos in `working_scripts/`
- Add a short manual test checklist in `tests/manual/` or `working_scripts/notes/`
- Automate core logic and export generation; keep live Shiny interaction checks manual for now

Chosen defaults:
- Testing posture: package-standard CI
- Shiny/browser automation: automate logic, keep UI interaction manual
- Upload fixture formats: `csv` and `rds`
- Main target under test: `paintr2` flow (`paintr_formula()`, `output_embed_var()`, `paintr_complete_expr()`, `paintr_get_plot()`, `generate_shiny()`)

## Best-Practice Test Setup

### Test layout

Use this structure:
- `tests/testthat/test-parse-formula.R`
- `tests/testthat/test-placeholders.R`
- `tests/testthat/test-upload.R`
- `tests/testthat/test-complete-expr.R`
- `tests/testthat/test-plot-build.R`
- `tests/testthat/test-export-shiny.R`
- `tests/testthat/test-supported-use-cases.R`
- `tests/testthat/test-unsupported-use-cases.R`
- `tests/testthat/helper-fixtures.R`
- `tests/testthat/fixtures/`
- `tests/manual/manual-checklist-paintr2.md`

Do not create a generic `testing/` folder as the main solution. An expert would prefer `testthat` because:
- it integrates with `R CMD check`
- it is the standard package testing layout
- it is easier to run in CI
- failures are easier to localize and maintain

### Fixture strategy

Under `tests/testthat/fixtures/`, keep small deterministic datasets:
- `simple_numeric.csv`
- `simple_numeric.rds`
- `simple_categorical.csv`
- `simple_spaces_in_names.csv`
- `single_row.csv`
- `bad_extension.txt`
- optionally `non_tabular.rds`

Use tiny datasets with clear column names and expected behaviors. Prefer committed fixtures over generator scripts for core tests because:
- tests stay deterministic
- failures are easier to inspect
- CI does not depend on fixture generation order

If you want a generator, add one helper script only for refreshing fixtures, not for every test run:
- `tests/testthat/fixtures/make_fixtures.R`

### Package setup

Add standard test infrastructure:
- `Suggests: testthat`
- optionally `withr`
- optionally `callr`
- `Config/testthat/edition: 3`
- `tests/testthat.R`

Do not add `shinytest2` in the first pass unless later needed. For now, keep exported-app and interactive behavior at the logic/file level plus manual checks.

## Functionalities to Test

### 1. Formula parsing and metadata construction

Test `paintr_formula()` for:
- valid formula string parses successfully
- top-level `+` splitting works
- duplicate layer names are handled
- placeholder detection works for `var`, `text`, `num`, `expr`, `upload`
- `data = ...` arguments are discovered correctly
- returned `paintr_obj` contains expected fields
- generated IDs are stable and predictable enough for downstream use

### 2. Placeholder-specific behavior

Test each placeholder separately.

`var`
- placeholder is detected
- corresponding UI placeholder is generated
- selected variable is inserted into final expression
- `var` works in `aes()`
- `var` works with uploaded data
- `var` accepts expression-like input if that is still intended behavior

`text`
- text input becomes quoted text in final expression
- blank text removes optional argument cleanly
- text works in `labs()` and `theme(legend.position = ...)`

`num`
- numeric input is inserted correctly
- missing numeric input removes optional argument cleanly
- numeric values work in `size`, `alpha`, `hjust`, etc.

`expr`
- expression text is parsed and inserted correctly
- blank expr removes optional argument cleanly
- valid expression works in `facet_wrap()`, `facet_grid()`, summary functions, labeller-like contexts
- invalid expression fails in a controlled way

`upload`
- upload UI bundle is generated
- blank name defaults to normalized file name
- custom name overrides default
- `.csv` loads correctly
- `.rds` loads correctly
- unsupported extension errors clearly
- uploaded dataset columns feed `var` choices
- global `ggplot(data = upload, ...)` works
- layer-specific `geom_*(data = upload, ...)` works

### 3. Dynamic UI behavior

Test `output_embed_var()` for:
- `var` controls appear when static data exists
- `var` controls stay absent before upload-backed data exists
- `var` controls appear after upload-backed data exists
- layer-specific data overrides global data correctly
- formulas with no data but `var` fail in the expected way

### 4. Expression completion and code output

Test `paintr_complete_expr()` for:
- placeholders are replaced correctly
- blank optional inputs are removed
- empty calls are pruned as expected
- checkbox-disabled layers are removed
- returned `code_text` matches the final expression structure
- upload-backed expressions substitute the resolved dataset object name
- returned evaluation environment contains uploaded datasets under expected names

### 5. Plot construction

Test `paintr_get_plot()` for:
- final object is a `ggplot`
- multi-layer plots reconstruct correctly
- upload-backed plots build successfully
- layer removal still yields a valid plot when possible
- unsupported formulas fail at plot-build time in expected ways

### 6. Exported app generation

Test `generate_shiny()` for:
- output file is written
- generated file contains the formula text
- generated file contains calls to the expected runtime helpers
- generated app text includes upload-related logic when formula contains `upload`
- generated app text does not regress non-upload formulas
- exported script is syntactically valid R

### 7. Regression coverage

Test that older non-upload behaviors still work:
- formulas using only static data
- formulas with `text`, `num`, `expr` but no `upload`
- checkbox behavior still works
- code output still renders
- exported app generation still works for non-upload formulas

## Recommended Automated Test Cases

### Convertible `ggplot2` use cases that should work

Use these as positive fixtures.

Basic scatter:
- `ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()`

Scatter with text and num:
- `ggplot(data = iris, aes(x = var, y = var)) + geom_point(size = num) + labs(title = text, x = text, y = text)`

Facet with expr:
- `ggplot(data = iris, aes(x = var, y = var)) + geom_point() + facet_wrap(expr)`

Theme/labs options:
- `ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + theme(legend.position = text) + labs(title = text)`

Upload global data:
- `ggplot(data = upload, aes(x = var, y = var)) + geom_point()`

Upload layer data:
- `ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(data = upload, aes(x = var, y = var))`

Bar/column:
- `ggplot(data = df, aes(x = var, y = var)) + geom_col()`

Facet grid:
- `ggplot(data = df2, aes(x = var, y = var)) + geom_point() + facet_grid(var ~ var, labeller = expr)`

Map-like layer composition:
- a global data layer plus another layer with separate static data

Conditional layer removal:
- formula with two optional `(expr)` or layer checkboxes where disabling a layer still leaves a valid plot

### `ggplot2` use cases that should NOT be considered supported

Use these as negative/guardrail cases.

Not a single valid expression:
- multiple expressions separated by `;`

No data context for `var`:
- `ggplot(aes(x = var, y = var)) + geom_point()`

Non-ggplot formula:
- `lm(y ~ x, data = df)`

Upload in unsupported file type:
- upload `.txt`, `.xlsx`

Formulas relying on objects unavailable at runtime:
- `ggplot(data = unknown_object, aes(x = var, y = var)) + geom_point()`

Expressions that need NSE or environments `paintr2` does not preserve:
- highly custom quosure-driven helper code
- formulas depending on ad hoc local functions not present in export/runtime context

Cases with unsupported placeholder semantics:
- quoted placeholders like `"var"` instead of bare symbols
- malformed `expr` input that does not parse

Cases that produce plots only with manual preprocessing outside `paintr2`:
- formulas requiring complex precomputed objects not created in the app/exported script

## Automated Workflow

### Local developer workflow

Support these commands:
- run all tests: `devtools::test()`
- run one file: `testthat::test_file("tests/testthat/test-upload.R")`
- run package checks: `devtools::check()`

### CI workflow

Set up a basic GitHub Actions workflow that:
- installs R
- installs package dependencies
- runs `rcmdcheck` or `devtools::check()`
- fails on test failures

Scope for CI v1:
- package unit tests only
- no browser automation
- exported app verification only at the generated-file and syntax level

### What to automate first

Priority order:
1. `paintr_formula()`
2. placeholder substitution
3. upload helpers
4. `paintr_complete_expr()`
5. `paintr_get_plot()`
6. `generate_shiny()`
7. representative supported/unsupported formulas

## Manual Testing Checklist

Keep a short human-run checklist for real Shiny interaction.

Manual app checks should cover:
- upload `.csv` and `.rds` through the actual app
- leave dataset name blank and confirm default naming
- enter custom dataset name and confirm it is used
- confirm `var` selectors appear only after upload
- confirm plot updates after clicking `draw`
- confirm `outputCode` matches the selected inputs
- confirm layer checkbox removal behaves correctly
- export a Shiny app, run it, and repeat one upload flow in the exported app
- try one invalid upload and confirm the app fails visibly but not catastrophically

## Assumptions and Defaults

- Use `tests/testthat`, not a repo-root `testing/` folder, as the main testing home.
- Small committed fixtures are preferred over generating datasets during every test run.
- Manual Shiny checks remain necessary because the app depends on real human interaction.
- Browser-level Shiny automation is deferred unless the manual workflow becomes too costly.
- “Can be converted” means supported by current `paintr2` parsing/substitution/runtime semantics, not arbitrary `ggplot2`.
