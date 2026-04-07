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
runtime <- paintr_build_runtime(
  obj,
  list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp", "geom_point+checkbox" = TRUE)
)
#> The function geom_point() is removed.
inherits(ggpaintr_plot_value(runtime), "ggplot")
#> [1] FALSE
```
