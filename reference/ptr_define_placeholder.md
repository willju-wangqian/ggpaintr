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
  copy_defaults = list(label = "Enter a value for {param}")
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

## Value

An object of class `ptr_define_placeholder`.

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
