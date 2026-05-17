# Level 3 — own the render path off `state`

Use when: ggpaintr's parsed formula should drive the runtime, but you render the plot yourself — Plotly, ggiraph, a custom output — or post-process the ggplot before drawing.

Both `ptr_server()` and `ptr_module_server()` **return** the `ptr_state`, so you swap ggpaintr's renderer for your own. The runtime stays inside ggpaintr; you replace only the *output*. Owning the render path is definitionally L3: the *enabling* return value (`state`) lives at the L2 server boundary, but using it to replace a pane is owning the render path.

There is **no headless / non-Shiny path**, and `testServer()` is **not** a feature surface. L3 custom render is ordinary Shiny rendering off the returned `state`.

## The canonical pattern

Nest `ptr_server()` inside **your own** `shiny::moduleServer(id, …)`, and place your output widget at `shiny::NS(id)(…)` in the UI. Namespacing is then automatic and plain-Shiny-idiomatic — you never touch any internal namespace plumbing (`state$server_ns_fn` is internal, not a user tool).

```r
# UI:     plotly::plotlyOutput(shiny::NS("plot1")("my_plot"))   + ptr_ui_controls("plot1", formula)
# server: shiny::moduleServer("plot1", function(input, output, session) {
#           state <- ptr_server(input, output, session, formula)
#           output$my_plot <- plotly::renderPlotly({ ... state$runtime() ... })
#         })
```

## Two reading paths off a `ptr_state`

| Inside a `renderX({...})` / `reactive({...})` / `observeEvent({...})` | Outside reactive contexts (download handlers, snapshots) |
|----|----|
| Read `state$runtime()` and unpack its slots (`$ok`, `$plot`, `$code_text`, `$error`). This takes the reactive dependency that wires re-renders to *Update plot* clicks. | Call `ptr_extract_plot(state)` / `ptr_extract_code(state)` / `ptr_extract_error(state)`. These wrap `shiny::isolate()` — current value, no reactive dependency. |

Mixing the two — calling `ptr_extract_plot(state)` inside `renderPlotly({...})` — silently breaks reactivity (the block fires once on mount and never again). Stick to `state$runtime()` inside reactive blocks.

## Custom plot renderer — `state$runtime()$plot` inside `renderPlotly()`

Render `ptr_ui_controls(id, formula)` for the widgets, place your own output container at `shiny::NS(id)(...)`, and **never** place `ptr_ui_plot(id)` — with no UI slot bound to it the built-in `ptr_plot` output stays inert, so there is no bundled pane at all.

```r
formula <- "ggplot(iris, aes(x = var, y = var, color = var)) + geom_point()"

ui <- ptr_ui_page(
  shiny::fluidRow(
    shiny::column(5, ptr_ui_controls("plot1", formula)),                   # widgets only
    shiny::column(7, plotly::plotlyOutput(shiny::NS("plot1")("custom_plot"),
                                          height = "500px"))               # your own output
  )
)
server <- function(input, output, session) {
  shiny::moduleServer("plot1", function(input, output, session) {
    state <- ptr_server(input, output, session, formula)   # runtime wired; ptr_plot unbound = inert
    output$custom_plot <- plotly::renderPlotly({
      res <- state$runtime()
      shiny::req(isTRUE(res$ok), res$plot)
      plotly::ggplotly(res$plot)
    })
  })
}

shiny::shinyApp(ui, server)
```

`req(isTRUE(res$ok), res$plot)` keeps the panel quiet between draws and on transient error states (`res$ok` is `FALSE` on the first tick before the draw button fires, and on runtime failure). The same pattern, different render function — `ggiraph::renderGirafe()`, `shiny::renderPlot()` + host post-processing — works because `state$runtime()` is renderer-agnostic.

If you would rather keep ggpaintr's chrome *and* add a second custom view, capture the module's state instead — `ptr_module_ui("p", formula)` in the UI plus `state <- ptr_module_server("p", formula)` in the server — but `ptr_module_ui()` always renders the bundled plot pane alongside your custom one. Use the bare-piece composition above when you want exactly one (custom) plot.

## Custom code pane

Bind to `state$runtime()` inside `renderText({...})`, inside the same `moduleServer(id)` as the `state <-` assignment:

```r
output$my_code <- shiny::renderText({
  state$runtime()$code_text %||% ""
})
```

For non-reactive contexts (a download handler, a snapshot test), use `ptr_extract_code(state)`. It returns a single string and includes any `ptr_gg_extra()` expressions captured on the state.

## Custom error UI

`state$runtime()$error` is the latest runtime error string (or `NULL` when the last cycle succeeded):

```r
output$my_status <- shiny::renderUI({
  msg <- state$runtime()$error
  if (is.null(msg)) shiny::tags$div(class = "status-ok", "Plot is up to date")
  else              shiny::tags$div(class = "status-error", msg)
})
```

`ptr_extract_error(state)` is the non-reactive counterpart.

## Shared widgets driving custom renderers

Combine the multi-instance shared coordinator with custom outputs: one `ptr_shared_server(obj)` bundle feeds every module's `ptr_server()`, and each module's returned `state` feeds its own renderer. Each plot gets its own `moduleServer(id)` so the per-placeholder ids never collide; the cross-formula key lives in the standalone `ptr_ui_shared_panel(obj)` (its keys are excluded from each `ptr_ui_controls(id, formula, shared = obj)`).

```r
plots <- list(
  "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Length, fill = Species)) + geom_boxplot()",
  "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Width,  fill = Species)) + geom_violin()"
)

obj <- ptr_shared(formulas = plots,
                   shared_ui = list(metric = function(id) shiny::selectInput(id, "Metric", names(iris))))

ui <- ptr_ui_page(
  ptr_ui_shared_panel(obj),                                  # cross-formula panel (bare, L3)
  shiny::fluidRow(
    shiny::column(3, ptr_ui_controls("plot_1", plots[[1]], shared = obj)),
    shiny::column(9, plotly::plotlyOutput(shiny::NS("plot_1")("custom")))
  ),
  shiny::fluidRow(
    shiny::column(3, ptr_ui_controls("plot_2", plots[[2]], shared = obj)),
    shiny::column(9, plotly::plotlyOutput(shiny::NS("plot_2")("custom")))
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)                               # top level, once
  shiny::moduleServer("plot_1", function(input, output, session) {
    state <- ptr_server(input, output, session, plots[[1]], shared_state = sh)
    output$custom <- plotly::renderPlotly({
      res <- state$runtime(); shiny::req(isTRUE(res$ok), res$plot)
      plotly::ggplotly(res$plot)
    })
  })
  shiny::moduleServer("plot_2", function(input, output, session) {
    state <- ptr_server(input, output, session, plots[[2]], shared_state = sh)
    output$custom <- plotly::renderPlotly({
      res <- state$runtime(); shiny::req(isTRUE(res$ok), res$plot)
      plotly::ggplotly(res$plot)
    })
  })
}

shiny::shinyApp(ui, server)
```

The coordinator feeds the modules; the modules feed your custom renderers. For the bare-piece (default-pane) layout, see `level3_layout`; for keeping the host-added theme/scale **in the code pane** as well as the plot, see `level3_gg_extra`.

## Contract

- `state$runtime()` is **reactive** — read it inside the render function; do not cache it.
- `res$ok` is `FALSE` on the first tick (before the draw button fires) and on runtime failure. `req(isTRUE(res$ok), res$plot)` guards both.
- A plain `+ theme/scale` renders correctly, but the code pane does **NOT** reflect it. If the code pane must match what the user sees, use `ptr_gg_extra(state, ...)` — see `level3_gg_extra`.
