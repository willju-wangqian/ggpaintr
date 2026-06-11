# Build a plotly Widget From a `ggpaintr` Instance, Wired for Linked Selection

`ptr_ggplotly()` is the state-first counterpart to
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html) for
the supported L3 custom-render pattern (see
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md),
section "Custom render (L3)"). Call it from inside
[`plotly::renderPlotly()`](https://rdrr.io/pkg/plotly/man/plotly-shiny.html)
on the live instance returned by
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md):
it reads the drawn plot off `state$runtime()$plot`, mints a per-draw row
key (the plotly `key` aesthetic, on a widget-only copy of the data),
sets `dragmode = "select"`, registers the `plotly_selected` /
`plotly_deselect` events, and returns a plain plotly object so your own
plotly verbs stay composable after it.

## Usage

``` r
ptr_ggplotly(state, ..., source = NULL)
```

## Arguments

- state:

  A `ptr_state` handle as returned by
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).

- ...:

  Forwarded verbatim to
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  (e.g. `tooltip = "all"`).

- source:

  Character scalar for plotly's `source =` channel, or `NULL` (default)
  to derive a distinct string from the instance namespace.

## Value

A plain plotly htmlwidget object (inherits class `"plotly"`), with
`dragmode = "select"` set and the `plotly_selected` / `plotly_deselect`
events registered.

## Details

The helper owns the
[`shiny::req()`](https://rdrr.io/pkg/shiny/man/req.html) pre-draw guard
(it raises Shiny's silent pre-draw condition before the first draw,
exactly like a bare `req(p)`) and derives plotly's `source =` channel
from the instance namespace, so the companion selection reader
coordinates without extra wiring. The user's drawn data is never
mutated: keys are minted per draw on the widget copy only.

plotly is an optional dependency (in `Suggests`). When it is not
installed, this function aborts, mirroring
[`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md)'s
bslib guard.

## See also

[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
for the L3 custom-render contract;
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html).

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny)
library(plotly)
f <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()
)
ui <- ptr_ui_page(
  ptr_ui_controls(formula = f, id = "p"),
  plotly::plotlyOutput("plt")
)
server <- function(input, output, session) {
  state <- ptr_server(f, "p")
  output$plt <- plotly::renderPlotly({
    # state-first: req() guard + key minting + select wiring are internal
    ptr_ggplotly(state, tooltip = "all") |>
      plotly::layout(legend = list(orientation = "h"))
  })
}
shinyApp(ui, server)
} # }
```
