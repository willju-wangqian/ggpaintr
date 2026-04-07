# Build Default ggpaintr Control Widgets

Build Default ggpaintr Control Widgets

## Usage

``` r
ggpaintr_controls_ui(ids = ggpaintr_ids(), copy_rules = NULL)
```

## Arguments

- ids:

  A `ggpaintr_ids` object describing the top-level Shiny ids used by the
  integration helpers.

- copy_rules:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

## Value

A Shiny UI object.

## Examples

``` r
ui <- ggpaintr_controls_ui(
  ggpaintr_ids(draw_button = "render_plot"),
  copy_rules = list(shell = list(draw_button = list(label = "Render plot")))
)
inherits(ui, "shiny.tag.list")
#> [1] TRUE
```
