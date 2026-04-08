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
runtime <- paintr_build_runtime(
  obj,
  list("ggplot+3+2" = "Sepal.Length", "ggplot+3+3" = "Sepal.Width", "geom_point+checkbox" = TRUE)
)
isTRUE(runtime$ok)
#> [1] TRUE
```
