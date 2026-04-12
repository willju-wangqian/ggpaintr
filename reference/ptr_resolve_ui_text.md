# Resolve Copy for One Control or App Element

Resolve Copy for One Control or App Element

## Usage

``` r
ptr_resolve_ui_text(
  component,
  keyword = NULL,
  layer_name = NULL,
  param = NULL,
  ui_text = NULL,
  placeholders = NULL
)
```

## Arguments

- component:

  One of `title`, `draw_button`, `upload_file`, `upload_name`,
  `layer_checkbox`, or `control`.

- keyword:

  Optional placeholder keyword.

- layer_name:

  Optional layer name.

- param:

  Optional parameter name.

- ui_text:

  Effective or user-supplied copy rules.

- placeholders:

  Optional custom placeholder definitions or an effective placeholder
  registry.

## Value

A named list with `label`, `help`, `placeholder`, and `empty_text`.

## Examples

``` r
# Resolve copy for the title element
ptr_resolve_ui_text("title")
#> $label
#> [1] "ggpaintr Plot Builder"
#> 

# Resolve copy for a var control on the x-axis
ptr_resolve_ui_text("control", keyword = "var", param = "x")
#> $label
#> [1] "Choose the x-axis column"
#> 
#> $empty_text
#> [1] "Choose one column"
#> 
```
