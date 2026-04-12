# Build a Plot from Completed Layer Expressions

Build a Plot from Completed Layer Expressions

## Usage

``` r
ptr_assemble_plot(plot_expr_list, envir = parent.frame(), expr_check = TRUE)
```

## Arguments

- plot_expr_list:

  A list of completed plot layer expressions.

- envir:

  The evaluation environment. Defaults to
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html), which is
  the caller's frame. When called from `ptr_exec`, a cloned runtime
  environment is passed explicitly. Direct callers should provide an
  explicit `envir` to avoid evaluating expressions in an unintended
  scope.

- expr_check:

  Logical or list controlling safety validation of the assembled
  expressions. Forwarded to `validate_expr_safety`. Defaults to `TRUE`.

## Value

A `ggplot` object assembled from the retained layer expressions. Errors
when no plot expressions remain after runtime processing.

## Examples

``` r
library(ggplot2)

obj <- ptr_parse_formula(
  "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
)
spec <- ptr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
runtime <- ptr_exec(obj, inputs)
plot_obj <- ptr_assemble_plot(runtime$complete_expr_list, runtime$eval_env)
inherits(plot_obj, "ggplot")
#> [1] TRUE
```
