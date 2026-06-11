# Read a `ptr_ggplotly()` Linked Selection As Rows or a Flag Column

`ptr_plotly_selection()` is the companion reader for
[`ptr_ggplotly()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ggplotly.md):
it returns a Shiny reactive carrying the current brush/lasso selection
of the plotly widget, projected one of two ways. Call it once in your
server (not inside a render) on the live instance returned by
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
and feed the returned reactive to a table, a second formula, or any
downstream consumer.

## Usage

``` r
ptr_plotly_selection(state, mode = c("rows", "flag"), source = NULL)
```

## Arguments

- state:

  A `ptr_state` handle as returned by
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).

- mode:

  `"rows"` (default) or `"flag"` — which projection of the one selection
  to return. Any other value aborts (the bare-indices projection is
  rejected by design).

- source:

  Character scalar for plotly's `source =` channel, or `NULL` (default)
  to derive the same distinct string
  [`ptr_ggplotly()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ggplotly.md)
  derives from the instance namespace.

## Value

A Shiny reactive. Its value is the rows projection (a data frame, the
selected slice; zero rows, same columns, when empty) or the flag
projection (the full drawn data plus a logical `.ptr_selected`), per
`mode`.

## Details

A selection names rows of the **drawn data** —
`state$runtime()$plot$data` of the draw that produced the widget — never
"the original object". Keys are minted per draw and are meaningless
across draws, so the selection **resets to empty on every draw** (a
redraw after a
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html)/[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
pipeline-head change or an upload swap starts the selection over). A
`plotly_deselect` event clears it likewise. Two projections of the one
selection:

- `mode = "rows"` — the selected slice; a **zero-row data frame with the
  drawn data's columns** when nothing is selected (so a
  [`shiny::renderTable()`](https://rdrr.io/pkg/shiny/man/renderTable.html)
  consumer needs no [`req()`](https://rdrr.io/pkg/shiny/man/req.html)
  dance).

- `mode = "flag"` — the **full** drawn data plus a logical
  `.ptr_selected` column, `TRUE` exactly at the selected rows (all
  `FALSE` when empty).

`.ptr_selected` is a reserved name: a pre-existing `.ptr_selected` on
the drawn data is silently overwritten (the overwrite keeps chained
selection-fed instances correct). The internal `.ptr_row` key never
appears in either projection.

**Pre-draw window:** before the first draw there is no snapshot yet, so
the returned reactive raises Shiny's silent pre-draw condition (via
[`shiny::req()`](https://rdrr.io/pkg/shiny/man/req.html)); under live
mode a selection-fed instance shows its inline pre-draw state for one
flush. **Live-mode key reset:** changing a placeholder widget re-draws
the source plot, which re-mints the keys, so the selection resets to
empty on that picker change.

plotly is an optional dependency (in `Suggests`); this reader is only
meaningful alongside
[`ptr_ggplotly()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ggplotly.md),
which guards on plotly being installed.

## See also

[`ptr_ggplotly()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ggplotly.md)
for the widget side;
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
for the L3 custom-render contract.

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
  plotly::plotlyOutput("plt"),
  tableOutput("sel")
)
server <- function(input, output, session) {
  state <- ptr_server(f, "p")
  output$plt <- plotly::renderPlotly({
    ptr_ggplotly(state, tooltip = "all")
  })
  # Full loop: brush the plot -> the selected rows appear in the table.
  sel <- ptr_plotly_selection(state, mode = "rows")
  output$sel <- renderTable(sel())
}
shinyApp(ui, server)
} # }
```
