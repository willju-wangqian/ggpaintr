# Level 3 — own `renderPlot()` with host post-processing

Use when: ggpaintr's parsed formula drives the plot, but your own Shiny
code adds a theme / scale / layer that depends on host state (a theme
picker, a user toggle) before rendering.

## Pattern

Write your own `renderPlot({...})` and call `ptr_extract_plot()` on
`ptr_state$runtime()`. Use every `ptr_register_*()` binder EXCEPT
`ptr_register_plot` (you own the plot output).

```r
library(shiny); library(ggpaintr); library(ggplot2)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ptr_input_ui(),
      selectInput("theme_pick", "Theme",
                  choices = c("minimal", "bw", "classic"))
    ),
    mainPanel(
      plotOutput("outputPlot"),
      verbatimTextOutput("outputCode"),
      uiOutput("outputError")
    )
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
  # NOTE: no ptr_register_plot() — we define outputPlot ourselves.

  output$outputPlot <- renderPlot({
    plot_obj <- ptr_extract_plot(ptr_state$runtime())
    if (is.null(plot_obj)) {
      graphics::plot.new()
      return(invisible(NULL))
    }

    chosen <- switch(input$theme_pick,
      minimal = theme_minimal(),
      bw      = theme_bw(),
      classic = theme_classic()
    )
    plot_obj + chosen
  })
}

shinyApp(ui, server)
```

## Contract

- `ptr_state$runtime()` is **reactive** — read it inside the render
  function; do not cache it.
- `ptr_extract_plot()` returns `NULL` on the first tick (before the
  draw button fires) and on runtime failure. Handle both.
- A plain `+ chosen` renders correctly, but the code pane does **NOT**
  reflect it (because the addition happens outside `ptr_exec()`).
  If the code pane must match, use `ptr_gg_extra()` — see
  `level3_gg_extra`.
