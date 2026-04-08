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

A `ggplot` object assembled from the retained layer expressions. Errors
when no plot expressions remain after runtime processing.

## Examples

``` r
library(ggplot2)

obj <- paintr_formula(
  "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
)
spec <- ggpaintr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
runtime <- paintr_build_runtime(obj, inputs)
plot_obj <- paintr_get_plot(runtime$complete_expr_list, runtime$eval_env)
inherits(plot_obj, "ggplot")
#> [1] TRUE
```
