# Return the Built Plot from a Runtime Result

Return the Built Plot from a Runtime Result

## Usage

``` r
ptr_extract_plot(runtime_result)
```

## Arguments

- runtime_result:

  A runtime result list returned by
  [`ptr_exec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_exec.md).

## Value

A `ggplot` object or `NULL`.

## Examples

``` r
obj <- ptr_parse_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
spec <- ptr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
runtime <- ptr_exec(
  obj,
  inputs
)
inherits(ptr_extract_plot(runtime), "ggplot")
#> [1] TRUE
```
