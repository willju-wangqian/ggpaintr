# Level 3 — round-trip host layers into the code pane with `ptr_gg_extra()`

Use when: a host-added layer / theme / scale must appear in the **generated-code** output as well as the plot, so a reader can reproduce the exact plot they see. (Plain `+ theme` in a custom renderer shows in the plot but never in the code pane — that gap is what `ptr_gg_extra()` closes.)

## Pattern

`ptr_gg_extra(state, ...)` evaluates one or more `ggplot2` expressions and attaches the results as "extras" on the state. The runtime folds them into the rendered plot during the next cycle, and `state$runtime()$code_text` (plus `ptr_extract_code(state)`) include them in the printable code whenever the runtime succeeds. The bundled plot pane picks the extras up automatically — the plot **and** the code pane stay in sync without writing a custom renderer.

```r
library(shiny); library(ggpaintr); library(ggplot2)

formula <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"

ui <- fluidPage(
  actionButton("add_log", "Toggle log-scale"),
  ptr_ui(formula, "p")
)
server <- function(input, output, session) {
  state <- ptr_server(formula, "p")
  shiny::observeEvent(input$add_log, {
    ptr_gg_extra(state, ggplot2::scale_x_log10())
  })
}

shinyApp(ui, server)
```

`ptr_server()` returns `state` (the `ptr_state` from `ptr_init_state()`); the bundled `ptr_plot` pane and the `ptr_code` pane both reflect the extras after the next runtime cycle.

## Contract — memorise these four points

1. **Replace, not append.** Each call overwrites the previously captured extras. Pass every component you want layered on in a single call.
2. **Atomic update.** Eval errors from the captured expressions leave the existing extras untouched.
3. **Suppressed on failure.** When the runtime reports `ok = FALSE`, the code binder ignores extras — stale values from a prior successful draw never leak into a failed-draw code pane.
4. **Works with the bundled UI.** You do *not* need a custom `renderPlot()` — `ptr_ui()` / `ptr_server()` (or `ptr_app()`) honor extras through the standard plot pane and code pane.

## With a custom renderer

If you also own the plot renderer (Plotly, ggiraph, …), the same `ptr_gg_extra()` call updates `state$runtime()$code_text` *and* `state$runtime()$plot`. Read `state$runtime()` inside your renderer — the extras are already folded in:

```r
output[[shiny::NS("p")("custom_plot")]] <- plotly::renderPlotly({
  res <- state$runtime()
  shiny::req(isTRUE(res$ok), res$plot)
  plotly::ggplotly(res$plot)
})
```

For the full custom-render scaffold (`state <- ptr_server(formula, id)` + `shiny::NS(id)` in the UI), see `level3_custom_render`.

## Custom code pane

If you write your own code-output binder, read from `state$runtime()` inside a reactive context or use the accessor outside one:

```r
output$my_code <- shiny::renderText({
  state$runtime()$code_text %||% ""
})

# Or, outside reactive contexts (download handler, snapshot):
ptr_extract_code(state)
```

`ptr_extract_code(state)` already appends the captured extras when the runtime succeeded — no extra threading needed.
