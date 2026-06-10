# Remove user-registered placeholders

Unregisters placeholders added with
[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md),
[`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md),
or
[`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md).
The five built-in placeholders (`ppVar`, `ppText`, `ppNum`, `ppExpr`,
`ppUpload`) are never removed.

## Usage

``` r
ptr_clear_placeholder(keyword = NULL)
```

## Arguments

- keyword:

  Optional single string. When supplied, only that placeholder is
  removed. When omitted (the default), every user-registered placeholder
  is removed.

## Value

The character vector of keywords that were removed, invisibly.

## Examples

``` r
ptr_define_placeholder_value(
  "demo_kw",
  build_ui = function(node, ...) shiny::textInput(node$id, "demo"),
  resolve_expr = function(value, node, ...) value
)
#> function (x, ...) 
#> x
#> <bytecode: 0x555b02c21d00>
#> <environment: 0x555afefb5960>
ptr_clear_placeholder("demo_kw")
#> ✔ Cleared placeholder: "demo_kw".
```
