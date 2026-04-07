# Build Default ggpaintr Output Widgets

Build Default ggpaintr Output Widgets

## Usage

``` r
ggpaintr_outputs_ui(ids = ggpaintr_ids())
```

## Arguments

- ids:

  A `ggpaintr_ids` object describing the top-level Shiny ids used by the
  integration helpers.

## Value

A Shiny UI object.

## Examples

``` r
ui <- ggpaintr_outputs_ui(ggpaintr_ids(plot_output = "main_plot"))
inherits(ui, "shiny.tag.list")
#> [1] TRUE
```
