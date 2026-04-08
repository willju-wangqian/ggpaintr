# Describe the Runtime Inputs for a Parsed Formula

Return the input ids consumed by
[`paintr_build_runtime()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr_build_runtime.md)
in a stable, documented data frame. This is the supported low-level
discovery helper for tests, tooling, and advanced package authors who
need to construct an input list programmatically without hard-coding
internal ids in documentation or downstream code.

## Usage

``` r
ggpaintr_runtime_input_spec(paintr_obj)
```

## Arguments

- paintr_obj:

  A `paintr_obj`.

## Value

A base `data.frame` with columns `input_id`, `role`, `layer_name`,
`keyword`, `param_key`, and `source_id`.

## Details

The returned rows follow the current UI/runtime order:

- placeholder inputs in parsed layer order

- derived upload dataset-name inputs immediately after each `upload`
  placeholder row

- layer checkbox inputs for every non-`ggplot` layer, appended in layer
  order

Raw ids such as `"ggplot+3+2"` still appear in the returned data, but
those ids remain implementation details. Call this helper instead of
relying on the exact encoding scheme directly.

## Examples

``` r
obj <- paintr_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + labs(title = text)"
)
ggpaintr_runtime_input_spec(obj)
#>              input_id           role layer_name keyword param_key  source_id
#> 1          ggplot+3+2    placeholder     ggplot     var         x ggplot+3+2
#> 2          ggplot+3+3    placeholder     ggplot     var         y ggplot+3+3
#> 3              labs+2    placeholder       labs    text     title     labs+2
#> 4 geom_point+checkbox layer_checkbox geom_point    <NA>      <NA>       <NA>
#> 5       labs+checkbox layer_checkbox       labs    <NA>      <NA>       <NA>
```
