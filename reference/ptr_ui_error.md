# Inline Error Pane Piece for a `ggpaintr` Formula

The inline error slot on its own: a
[`shiny::uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html)
bound to the `ptr_error` id the server writes parse/runtime error alerts
to (see
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)).
One of the single-piece UI builders for the L3 "own every UI piece"
workflow.

## Usage

``` r
ptr_ui_error(id = NULL)
```

## Arguments

- id:

  Optional module id; the namespace prefix for the output. Defaults to
  `NULL` (identity namespace). When set, must match the `id` passed to
  the other piece functions and the server wiring.

## Value

A [shiny::tag](https://rdrr.io/pkg/shiny/man/reexports.html).

## See also

[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md),
[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)

## Examples

``` r
ptr_ui_error("myplot")
#> <div id="myplot-ptr_error" class="shiny-html-output"></div>
```
