# paintr2 Overview

## What `ggpaintr` does

`ggpaintr` is an R package for building modular Shiny apps with `ggplot2`
plotting functionality. It reduces the amount of manual UI and server code
needed to expose plotting controls to users.

In the `paintr2` workflow, a plot specification is written as a string
containing a valid `ggplot2` expression template. Special placeholder tokens in
that template are detected and converted into Shiny controls automatically. User
input is then substituted back into the template to generate:

- a plot
- the corresponding code

Relevant files:

- `R/paintr2_func.R`
- `R/ui_function.R`
- `working_scripts/paintr_distribute.R`

Reference:

- `R/paintr2_func.R:511-791`
- `R/ui_function.R:1-260`
- `working_scripts/paintr_distribute.R:96-180`

## What `ggpaintr_basic2()` does

`ggpaintr_basic2()` is defined in `working_scripts/paintr_distribute.R`. It
takes a single formula string and turns it into a Shiny app.

High-level flow:

1. Parse the formula string with `paintr_formula()`.
2. Build a tabbed control panel from detected placeholders.
3. Dynamically populate `var` inputs from the available dataset columns.
4. On `draw`, replace placeholders with current Shiny input values using
   `paintr_complete_expr()`.
5. Evaluate the resulting `ggplot2` expression pieces with `paintr_get_plot()`.
6. Show both the rendered plot and generated code.
7. Optionally export a standalone Shiny app with `generate_shiny()`.

Reference:

- `working_scripts/paintr_distribute.R:96-180`

This makes `ggpaintr_basic2()` a generic formula-driven plotting app generator.

## How `paintr_formula()` works

`paintr_formula()` expects a string and starts by calling
`rlang::parse_expr(formula)`.

It then:

1. Splits a `ggplot2` expression on top-level `+` operators with `break_sum()`.
2. Names the resulting expression pieces by function name, such as `ggplot`,
   `geom_point`, `labs`, and so on.
3. Searches those expressions for special placeholder tokens and `data`
   arguments.
4. Creates input IDs and metadata describing where each placeholder occurs.
5. Builds matching UI components using functions from `R/ui_function.R`.

The return value is a `paintr_obj` containing:

- `formula_text`
- `param_list`
- `keywords_list`
- `index_path_list`
- `id_list`
- `expr_list`
- `ui_list`

Reference:

- `R/paintr2_func.R:511-562`

## Placeholder tokens

The current supported placeholders are bare symbols:

- `var`
- `text`
- `num`
- `expr`
- `upload`

These are detected in `detect_keywords()`.

Reference:

- `R/paintr2_func.R:126-149`

Their intended meaning in the current code:

- `var`: a variable-like expression chosen by the user
- `text`: character input
- `num`: numeric input
- `expr`: arbitrary R expression entered as text and parsed
- `upload`: dataset upload placeholder with file and dataset-name inputs

Reference:

- `R/ui_function.R:1-110`
- `R/paintr2_func.R:254-304`

## Requirements for the formula string

The `formula` supplied to `paintr_formula()` should follow these rules.

### 1. It must be a single valid R expression string

Because `paintr_formula()` uses `parse_expr()`, the input must be parsable as
one R expression.

Reference:

- `R/paintr2_func.R:514`

Good:

```r
"ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
```

Bad:

```r
"ggplot(data = mtcars, aes(x = var, y = var)); geom_point()"
```

### 2. It should be a `ggplot2` expression built with `+`

The parser logic assumes a top-level plot specification that can be split into
pieces and re-added later.

Reference:

- `R/paintr2_func.R:26-40`
- `R/paintr2_func.R:514-518`

Typical shape:

```r
"ggplot(data = mtcars, aes(x = var, y = var)) +
  geom_point() +
  labs(title = text)"
```

### 3. Placeholder tokens must be bare symbols, not strings

Good:

```r
aes(x = var, y = var)
labs(title = text)
facet_wrap(expr)
geom_point(size = num)
```

Bad:

```r
aes(x = \"var\", y = \"var\")
labs(title = \"text\")
```

### 4. If `var` is used, data must be available

`var` controls need access to column names. Data can come from:

- a global `ggplot(data = ...)`
- a layer-specific `data = ...`

If no data is available for a `var` placeholder, the UI-building path can fail
with `data is not provided!`.

Reference:

- `R/ui_function.R:113-260`

### 5. Data objects must exist in the evaluation environment

Examples in the repo use datasets such as:

- `mtcars`
- `iris`
- `some.eu.maps`
- `region.lab.data`

If the formula references a data object, that object must exist when the app
runs.

Reference:

- `R/ui_function.R:141-145`
- `R/ui_function.R:172-174`
- `R/paintr2_func.R:655-665`

### 6. `expr` inputs must be valid R code when entered

User-provided `expr` values are parsed with `parse_expr()`. This allows flexible
injection, but invalid syntax will fail.

Reference:

- `R/paintr2_func.R:288-295`

Examples:

```r
facet_wrap(expr)
labs(subtitle = expr)
```

### 7. `var` is not restricted to simple column names

In the current code, `var` input is parsed with `parse_expr(input_item)`, not
forced into `.data[[...]]`. That means users can potentially enter expressions
such as:

```r
log(disp)
wt + 1
```

This is flexible, but it also means `var` behaves more like a plot expression
input than a strict column-name selector.

Reference:

- `R/paintr2_func.R:254-265`

### 8. Blank optional inputs are removed from the expression

Missing or blank `text`, `num`, and `expr` values are converted to
`_NULL_PLACEHOLDER`, then removed later. Empty calls are also pruned when
possible.

This allows optional components like:

```r
labs(title = text)
facet_wrap(expr)
theme(legend.position = text)
```

to disappear cleanly if left blank.

Reference:

- `R/paintr2_func.R:267-295`
- `R/paintr2_func.R:395-465`

### 9. `upload` currently supports local `.csv` and `.rds` files

When `data = upload` is present in the formula:

- the UI includes a `fileInput()` for the dataset
- the UI includes a companion `textInput()` for the dataset object name
- if the dataset name is blank, a default object name is derived from the file name
- uploaded data is read on the server side and assigned into the plotting evaluation environment
- uploaded data can drive `var` choices
- both global `ggplot(data = upload, ...)` and layer-specific `data = upload` are supported

Unsupported upload file types currently error.

Reference:

- `R/ui_function.R:1-20`
- `R/ui_function.R:113-260`
- `R/paintr2_func.R:152-251`
- `R/paintr2_func.R:613-653`

## Notes about execution flow

`paintr_complete_expr()` performs the placeholder substitution at draw time.

It:

1. Replaces each placeholder with the current input value.
2. Removes null placeholders.
3. Removes empty calls where possible.
4. Applies layer checkbox settings.
5. Produces both:
   - `complete_expr_list`
   - `code_text`
   - `eval_env`

`paintr_get_plot()` then evaluates each expression piece in the provided
environment and rebuilds the final plot by adding the components back together.

For upload-backed formulas, `eval_env` contains the uploaded datasets under
their resolved object names.

Reference:

- `R/paintr2_func.R:613-665`

## Practical authoring advice

When writing a `paintr2` formula template, use this checklist:

- Start with `ggplot(data = ..., aes(...))`
- Use top-level `+` between plot components
- Use bare placeholder symbols, not quoted names
- Ensure referenced datasets exist in the app environment
- Use `var` only where a data context exists
- Use `expr` only where arbitrary parsed code is acceptable
- Use `upload` only for `.csv` or `.rds` datasets
- Remember that exported apps currently reuse the `paintr2` runtime through `ggpaintr:::...` calls

Reference:

- `R/paintr2_func.R:705-791`

## Testing status

The repo now includes package-standard automated testing for the `paintr2` path.

Current coverage includes:

- formula parsing
- placeholder detection and substitution
- upload helper behavior
- upload-backed `var` handling
- expression completion
- plot construction
- supported and unsupported use cases
- Shiny app export generation

Current testing locations:

- `tests/testthat/`
- `tests/testthat/fixtures/`
- `tests/manual/manual-checklist-paintr2.md`
- `.github/workflows/R-CMD-check.yaml`

Reference:

- `DESCRIPTION:26-41`
- `tests/testthat.R`
- `.github/workflows/R-CMD-check.yaml`

## Suggested future cleanup

Current code observations worth revisiting later:

- `var` currently accepts parsed expressions, not only selected column names.
- There is at least one `browser()` call still present in
  `expr_remove_emptycall()`, though that function does not appear to be the main
  path used by `paintr_complete_expr()`.

Reference:

- `R/paintr2_func.R:254-265`
- `R/paintr2_func.R:410-423`
