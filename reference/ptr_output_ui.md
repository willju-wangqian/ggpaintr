# Build Default ggpaintr Output Widgets

Build Default ggpaintr Output Widgets

## Usage

``` r
ptr_output_ui(ids = ptr_build_ids(), ns = shiny::NS(NULL))
```

## Arguments

- ids:

  A `ptr_build_ids` object describing the top-level Shiny ids used by
  the integration helpers.

- ns:

  An optional namespace function (`character -> character`). Must match
  the `ns` passed to
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md).
  Defaults to `shiny::NS(NULL)` (no prefixing).

## Value

A Shiny UI object.

## Examples

``` r
ui <- ptr_output_ui(ptr_build_ids(plot_output = "main_plot"))
inherits(ui, "shiny.tag.list")
#> [1] TRUE
```
