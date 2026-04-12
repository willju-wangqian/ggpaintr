# Build the Full Runtime Result for a Paintr App

Build the Full Runtime Result for a Paintr App

## Usage

``` r
ptr_exec(ptr_obj, input, envir = parent.frame(), expr_check = TRUE)
```

## Arguments

- ptr_obj:

  A `ptr_obj`.

- input:

  A Shiny input-like object.

- envir:

  The environment used to resolve local data objects.

- expr_check:

  Controls `expr` placeholder validation at runtime. `TRUE` (default)
  applies the built-in denylist of dangerous functions. `FALSE` disables
  all checking. A named list with `deny_list` and/or `allow_list`
  character vectors supplies a custom check; when both are given, denied
  entries are removed from the allowlist. Note: the formula template
  itself is separately validated at parse time via `formula_check` in
  [`ptr_parse_formula`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_parse_formula.md).
  Disabling `expr_check` here does not affect that earlier check, and
  vice versa.

## Value

A runtime result list containing `ok`, `stage`, `message`, `code_text`,
`complete_expr_list`, `eval_env`, `condition`, and `plot`.
Completion-stage validation failures return `stage = "complete"`;
plot-construction or render failures return `stage = "plot"`.

## Examples

``` r
library(ggplot2)

obj <- ptr_parse_formula(
  "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
)
spec <- ptr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "Sepal.Length"
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "Sepal.Width"
runtime <- ptr_exec(
  obj,
  inputs
)
isTRUE(runtime$ok)
#> [1] TRUE
```
