# Level 3 — own `renderPlot()` with host post-processing

Use when: ggpaintr's parsed formula drives the runtime, but your own Shiny
code adds a theme / scale / layer that depends on host state (a theme
picker, a user toggle) before rendering.

## Pattern

`ptr_module_server()` returns the underlying `ptr_state`. Read
`state$runtime()$plot` inside your own `renderPlot()` (or
`plotly::renderPlotly()`, `ggiraph::renderGirafe()`, …) and add your
host-side components on top.

The error and code panes that `ptr_module_ui()` ships with — those
write to the `ptr_error` / `ptr_code` outputs and `ptr_module_server()`
binds them automatically — keep working as-is. Skip `ptr_module_ui()`
if you don't want those panes; the auto-registered outputs stay inert
when no UI consumer exists.

```r
library(shiny); library(ggpaintr); library(ggplot2)

formula <- "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ptr_controls_ui("p", formula),
      selectInput("theme_pick", "Theme",
                  choices = c("minimal", "bw", "classic"))
    ),
    mainPanel(
      plotOutput("my_plot"),                       # host-owned plot
      verbatimTextOutput("p-ptr_code"),            # ggpaintr's code pane
      uiOutput("p-ptr_error")                      # ggpaintr's error pane
    )
  )
)

server <- function(input, output, session) {
  state <- shiny::moduleServer("p", function(input, output, session) {
    ptr_server(input, output, session, formula)
  })

  output$my_plot <- renderPlot({
    res <- state$runtime()
    req(isTRUE(res$ok), res$plot)
    chosen <- switch(input$theme_pick,
      minimal = theme_minimal(),
      bw      = theme_bw(),
      classic = theme_classic()
    )
    res$plot + chosen
  })
}

shinyApp(ui, server)
```

## Module-id form (compact)

The same pattern collapses neatly when the controls + ggpaintr-owned
outputs live in one module rectangle:

```r
ui <- fluidPage(
  ptr_module_ui("p", formula),
  plotOutput("my_plot")                # host-owned, outside the module
)

server <- function(input, output, session) {
  state <- ptr_module_server("p", formula)
  output$my_plot <- renderPlot({
    res <- state$runtime()
    req(isTRUE(res$ok), res$plot)
    res$plot + theme_minimal()
  })
}
```

`ptr_module_server()` returns `state` (the same `ptr_state` from
`ptr_init_state()`), so the host renderer can read `state$runtime()`
just like the previous example.

## Plotly / ggiraph swap

Same pattern, different render function — `state$runtime()` is renderer-
agnostic:

```r
output$custom_plot <- plotly::renderPlotly({
  res <- state$runtime()
  req(isTRUE(res$ok), res$plot)
  plotly::ggplotly(res$plot)
})
```

## Contract

- `state$runtime()` is **reactive** — read it inside the render
  function; do not cache it.
- `res$ok` is `FALSE` on the first tick (before the draw button fires)
  and on runtime failure. `req(isTRUE(res$ok), res$plot)` guards both.
- A plain `+ chosen` renders correctly, but the code pane does **NOT**
  reflect it (because the addition happens outside the runtime's
  `code_text`). If the code pane must match what the user sees, use
  `ptr_gg_extra(state, ...)` — see `level3_gg_extra`.
- For non-reactive contexts (download handler, snapshot test), use
  `ptr_extract_plot(state)`. Calling it **inside** a `renderPlot({...})`
  silently breaks reactivity (the isolate wrapper skips the dependency
  on `state$runtime()`).
