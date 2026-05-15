# Level 3 — round-trip host layers into the code pane with `ptr_gg_extra()`

Use when: same as `level3_custom_render`, but the host-added theme /
layer / scale must ALSO appear in the generated-code output so a reader
can reproduce the exact plot they see.

## Pattern

`ptr_gg_extra(state, ...)` evaluates one or more `ggplot2` expressions
and attaches the results as "extras" on the state. The runtime folds
them into the rendered plot during the next cycle, and `state$runtime()$code_text`
(plus `ptr_extract_code(state)`) include them in the printable code
whenever the runtime succeeds.

```r
library(shiny); library(ggpaintr); library(ggplot2)

formula <- "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"

ui <- fluidPage(
  checkboxInput("with_smooth", "Add geom_smooth()", FALSE),
  ptr_module_ui("p", formula)
)

server <- function(input, output, session) {
  state <- ptr_module_server("p", formula)

  # Recompute extras whenever the host toggle flips. ptr_gg_extra() is
  # replace-semantics: one call, every extra you want.
  shiny::observe({
    if (isTRUE(input$with_smooth)) {
      ptr_gg_extra(
        state,
        theme_minimal(base_size = 16),
        geom_smooth(method = "lm", se = FALSE)
      )
    } else {
      ptr_gg_extra(state, theme_minimal(base_size = 16))
    }
  })
}

shinyApp(ui, server)
```

The bundled plot pane (`ptr_plot`, written by `ptr_register_plot()` inside
`ptr_module_server()`) picks up the extras automatically — the plot
**and** the code pane stay in sync without writing a custom renderer.

## Contract — memorise these four points

1. **Replace, not append.** Each call overwrites the previously captured
   extras. Pass every component you want in a single call.
2. **Suppressed on failure.** When the runtime reports `ok = FALSE`, the
   code binder ignores extras — stale values from a prior successful
   draw do not leak into a failed-draw code pane.
3. **Plain list return.** `ptr_gg_extra()` returns the state invisibly;
   the captured extras are stored on `state$extras` and folded into the
   plot during the next runtime cycle (via `ggplot2::ggplot_add.list`).
4. **Works with the bundled UI.** Unlike the previous API, you do *not*
   need to write a custom `renderPlot()` — `ptr_module_ui()` /
   `ptr_module_server()` (or `ptr_app()`) honor extras through the
   standard plot pane.

## Custom plot renderer

If you also own the plot renderer (Plotly, ggiraph, …), the same
`ptr_gg_extra()` call updates `state$runtime()$code_text` *and*
`state$runtime()$plot`. Read `state$runtime()` inside your renderer —
the extras are already folded in:

```r
output$custom_plot <- plotly::renderPlotly({
  res <- state$runtime()
  shiny::req(isTRUE(res$ok), res$plot)
  plotly::ggplotly(res$plot)
})
```

## Custom code pane

If you write your own code-output binder, read from `state$runtime()`
or call the accessor:

```r
output$my_code <- shiny::renderText({
  state$runtime()$code_text %||% ""
})

# Or, outside reactive contexts:
ptr_extract_code(state)
```

`ptr_extract_code(state)` already appends the captured extras when the
runtime succeeded — no extra threading needed.
