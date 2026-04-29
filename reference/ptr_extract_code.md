# Return Generated Code Text from a Runtime Result

Return Generated Code Text from a Runtime Result

## Usage

``` r
ptr_extract_code(runtime_result, extras = NULL)
```

## Arguments

- runtime_result:

  A runtime result list returned by
  [`ptr_exec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_exec.md).

- extras:

  Optional list of quosures captured by
  [`ptr_gg_extra()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_gg_extra.md),
  appended to the runtime code text when the runtime succeeded. Extras
  are suppressed when `runtime_result$ok` is not `TRUE`, so stale extras
  from a prior successful draw never surface during a failed draw.

## Value

A character string or `NULL`.

## Examples

``` r
if (interactive()) {
obj <- ptr_parse_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
spec <- ptr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
runtime <- ptr_exec(obj, inputs)
ptr_extract_code(runtime)
}
```
