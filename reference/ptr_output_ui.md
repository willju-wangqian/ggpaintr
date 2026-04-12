# Build Default ggpaintr Output Widgets

Build Default ggpaintr Output Widgets

## Usage

``` r
ptr_output_ui(ids = ptr_build_ids())
```

## Arguments

- ids:

  A `ptr_build_ids` object describing the top-level Shiny ids used by
  the integration helpers.

## Value

A Shiny UI object.

## Examples

``` r
ui <- ptr_output_ui(ptr_build_ids(plot_output = "main_plot"))
inherits(ui, "shiny.tag.list")
#> [1] TRUE
```
