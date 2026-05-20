# Plot Pane Piece for a `ggpaintr` Formula

The plot card on its own: a
[`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html)
bound to the `ptr_plot` id the server writes to (see
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)).
One of the single-piece UI builders for the L3 "own every UI piece"
workflow; place it anywhere in your own layout and wire the server with
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).

## Usage

``` r
ptr_ui_plot(id = NULL)
```

## Arguments

- id:

  Optional module id; the namespace prefix for the output. Defaults to
  `NULL` (identity namespace) for the single-embedding case. When set,
  must match the `id` passed to the other piece functions and to the
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
  wrapping
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  (or to
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)).

## Value

A [shiny::tag](https://rdrr.io/pkg/shiny/man/reexports.html).

## Details

The piece is **truly bare**: just the plot card, with no error slot and
no show-code button. Behaviour is added compositionally by the
combinators
[`ptr_ui_inline_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_inline_error.md)
(nests an error slot in the card body) and
[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md)
(adds the `</>` toggle + slide-out code window) — not by flags on this
function.

## See also

[`ptr_ui_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_error.md),
[`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md),
[`ptr_ui_inline_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_inline_error.md),
[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md),
[`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)

## Examples

``` r
ptr_ui_plot("myplot")
#> <div class="ptr-card ptr-card--plot">
#>   <div class="ptr-card__head">
#>     <h3 class="ptr-card__title">Plot</h3>
#>   </div>
#>   <div class="ptr-card__body">
#>     <div class="shiny-plot-output html-fill-item" id="myplot-ptr_plot" style="width:100%;height:400px;"></div>
#>   </div>
#> </div>
```
