# Construct a Custom ggpaintr Placeholder

Build one placeholder specification for use with
[`ptr_merge_placeholders()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_placeholders.md).
Custom placeholders can define their own UI control, runtime expression
replacement, deferred UI binding, and evaluation-environment
preparation.

## Usage

``` r
ptr_define_placeholder(
  keyword,
  build_ui,
  resolve_expr,
  resolve_input = NULL,
  bind_ui = NULL,
  prepare_eval_env = NULL,
  copy_defaults = list(label = "Enter a value for {param}"),
  source_file = NULL,
  source_package = NULL,
  source_function = NULL,
  on_missing = "warn"
)
```

## Arguments

- keyword:

  A single syntactic placeholder name used inside the formula.

- build_ui:

  Function with signature `(id, copy, meta, context)` returning a Shiny
  UI control or placeholder.

- resolve_expr:

  Function with signature `(value, meta, context)` returning an R
  expression or
  [`ptr_missing_expr()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_missing_expr.md).

- resolve_input:

  Optional function with signature `(input, id, meta, context)`
  returning the raw value to hand to `resolve_expr()`. Defaults to
  `input[[id]]`.

- bind_ui:

  Optional function with signature `(input, output, metas, context)` for
  registering deferred UI such as the built-in `var` placeholder.

- prepare_eval_env:

  Optional function with signature `(input, metas, eval_env, context)`
  returning an updated evaluation environment.

- copy_defaults:

  Optional named list with `label`, `help`, `placeholder`, and
  `empty_text`. Defaults to `list(label = "Enter a value for {param}")`.

- source_file:

  Character string (or named list with `.default` and hook-name keys)
  giving the path to an R file that defines the hook functions. The
  exported app will [`source()`](https://rdrr.io/r/base/source.html)
  this file. Provide the file alongside the exported `app.R`. Applies to
  all hooks unless overridden per-hook with a named list.

- source_package:

  Character string (or named list with `.default` and hook-name keys)
  naming an R package that exports the hook functions. The exported app
  will call [`library()`](https://rdrr.io/r/base/library.html) and
  install the package if missing.

- source_function:

  Named list mapping hook names to the actual function objects (e.g.
  `list(build_ui = my_build_ui)`). The exported app will contain the
  deparsed function definitions before the placeholder call.

- on_missing:

  One of `"warn"` (default) or `"error"`. Controls the exported app's
  behaviour when a `source_file` file or `source_package` package is
  unavailable at runtime.

## Value

An object of class `ptr_define_placeholder`.

## Note

**Export of non-inline hooks:** Hook functions can be made exportable
through
[`ptr_generate_shiny()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_generate_shiny.md)
in three ways: define them **inline** inside the
`ptr_define_placeholder()` call (serialized directly), supply them via
`source_function` (deparsed into the generated app), reference them via
`source_file` (a [`source()`](https://rdrr.io/r/base/source.html) call
is emitted), or reference them via `source_package` (a
[`library()`](https://rdrr.io/r/base/library.html) call is emitted).
Non-inline hooks with no source strategy will abort at export time.

## Examples

``` r
date_placeholder <- ptr_define_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::dateInput(id, copy$label)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }

    rlang::expr(as.Date(!!value))
  },
  copy_defaults = list(label = "Choose a date for {param}")
)
date_placeholder$keyword
#> [1] "date"
```
