# Build ggpaintr UI for a Shiny Module

Use this helper when you prefer Shiny modules for Level 2 integration.
It is also a compact template for custom module wrappers built from
[`ptr_input_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_input_ui.md)
and
[`ptr_output_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_output_ui.md).

## Usage

``` r
ptr_module_ui(id, ui_text = NULL)
```

## Arguments

- id:

  Module id.

- ui_text:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

## Value

A Shiny UI object.

## Examples

``` r
paintr_ui <- ptr_module_ui("plot1")
inherits(paintr_ui, "shiny.tag.list")
#> [1] TRUE
```
