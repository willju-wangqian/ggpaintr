# Return the Built Plot from a Runtime Result

Return the Built Plot from a Runtime Result

## Usage

``` r
ggpaintr_plot_value(runtime_result)
```

## Arguments

- runtime_result:

  A runtime result list returned by
  [`paintr_build_runtime()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr_build_runtime.md).

## Value

A `ggplot` object or `NULL`.

## Examples

``` r
obj <- paintr_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
spec <- ggpaintr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
runtime <- paintr_build_runtime(
  obj,
  inputs
)
#> The function geom_point() is removed.
inherits(ggpaintr_plot_value(runtime), "ggplot")
#> [1] FALSE
```
