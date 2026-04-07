# Build a Plot from Completed Layer Expressions

Build a Plot from Completed Layer Expressions

## Usage

``` r
paintr_get_plot(plot_expr_list, envir = parent.frame())
```

## Arguments

- plot_expr_list:

  A list of completed plot layer expressions.

- envir:

  The evaluation environment.

## Value

A `ggplot` object.

## Examples

``` r
library(ggplot2)

obj <- paintr_formula(
  "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
)
runtime <- paintr_build_runtime(obj, list("geom_point+checkbox" = TRUE))
plot_obj <- paintr_get_plot(runtime$complete_expr_list, runtime$eval_env)
inherits(plot_obj, "ggplot")
#> [1] TRUE
```
