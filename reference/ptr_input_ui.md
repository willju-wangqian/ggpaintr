# Build Default ggpaintr Control Widgets

Build Default ggpaintr Control Widgets

## Usage

``` r
ptr_input_ui(ids = ptr_build_ids(), ui_text = NULL)
```

## Arguments

- ids:

  A `ptr_build_ids` object describing the top-level Shiny ids used by
  the integration helpers.

- ui_text:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

## Value

A Shiny UI object.

## Examples

``` r
ui <- ptr_input_ui(
  ptr_build_ids(draw_button = "render_plot"),
  ui_text = list(shell = list(draw_button = list(label = "Render plot")))
)
inherits(ui, "shiny.tag.list")
#> [1] TRUE
```
