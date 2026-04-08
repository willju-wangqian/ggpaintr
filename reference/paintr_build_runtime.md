# Build the Full Runtime Result for a Paintr App

Build the Full Runtime Result for a Paintr App

## Usage

``` r
paintr_build_runtime(paintr_obj, input, envir = parent.frame())
```

## Arguments

- paintr_obj:

  A `paintr_obj`.

- input:

  A Shiny input-like object.

- envir:

  The environment used to resolve local data objects.

## Value

A runtime result list containing `ok`, `stage`, `message`, `code_text`,
`complete_expr_list`, `eval_env`, `condition`, and `plot`.
Completion-stage validation failures return `stage = "complete"`;
plot-construction or render failures return `stage = "plot"`.

## Examples

``` r
library(ggplot2)

obj <- paintr_formula(
  "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
)
spec <- ggpaintr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "Sepal.Length"
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "Sepal.Width"
runtime <- paintr_build_runtime(
  obj,
  inputs
)
isTRUE(runtime$ok)
#> [1] TRUE
```
