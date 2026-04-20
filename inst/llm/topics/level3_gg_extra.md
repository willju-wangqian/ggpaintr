# Level 3 — round-trip host layers into the code pane with `ptr_gg_extra()`

Use when: same as `level3_custom_render`, but the host-added theme /
layer / scale must ALSO appear in the generated-code output so a reader
can reproduce the exact plot they see.

## Pattern

`ptr_gg_extra(ptr_state, ...)` captures ggplot components added outside
the runtime and stores them on `ptr_state$extras`. The default
`ptr_register_code()` picks them up and appends them to the code text
whenever the runtime succeeds.

```r
library(shiny); library(ggpaintr); library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ptr_input_ui(),
      checkboxInput("with_smooth", "Add geom_smooth()", FALSE)
    ),
    mainPanel(ptr_output_ui())
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)

  output$outputPlot <- renderPlot({
    plot_obj <- ptr_extract_plot(ptr_state$runtime())
    if (is.null(plot_obj)) {
      graphics::plot.new()
      return(invisible(NULL))
    }

    # Replace-semantics: one call, every extra you want.
    extras <- if (isTRUE(input$with_smooth)) {
      ptr_gg_extra(
        ptr_state,
        theme_minimal(base_size = 16),
        geom_smooth(method = "lm", se = FALSE)
      )
    } else {
      ptr_gg_extra(ptr_state, theme_minimal(base_size = 16))
    }

    plot_obj + extras
  })
}

shinyApp(ui, server)
```

## Contract — memorise these four points

1. **Replace, not append.** Each call overwrites the previously captured
   extras. Pass every component you want in a single call.
2. **Suppressed on failure.** When the runtime reports `ok = FALSE`, the
   code binder ignores extras — stale values from a prior successful
   draw do not leak into a failed-draw code pane.
3. **Plain list return.** Dispatched through `ggplot2::ggplot_add.list`,
   so `plot_obj + ptr_gg_extra(...)` behaves exactly like
   `plot_obj + list(...)`.
4. **Not wired into `ptr_app()`.** Only meaningful when you own a
   `renderPlot({...})` built on `ptr_server_state()` +
   `ptr_register_*()` binders.

## Custom code binder

If you write your own code-output binder (instead of
`ptr_register_code()`), call:

```r
ptr_extract_code(ptr_state$runtime(), extras = ptr_state$extras())
```

That is exactly what `ptr_register_code()` passes internally.
