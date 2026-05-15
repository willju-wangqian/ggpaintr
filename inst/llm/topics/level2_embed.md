# Level 2 — embed ggpaintr in your own Shiny app

Use when: the user already has a Shiny app and wants one tab / panel to
hold a ggpaintr-driven plot without giving up control of layout or chrome.

Two paths cover the common cases:

- **Module path** (compact): one `id`, controls + outputs as a single
  rectangle inside the host UI.
- **Split path**: place `ptr_controls_ui()` and `ptr_outputs_ui()`
  anywhere on the page; wire the server with `ptr_server()`.

## Module path — `ptr_module_ui()` + `ptr_module_server()`

```r
library(shiny); library(ggpaintr)

formula <- "ggplot(data = mtcars, aes(x = var, y = var)) +
              geom_point() +
              labs(title = text)"

ui <- fluidPage(
  titlePanel("Embedded ggpaintr"),
  ptr_module_ui("p", formula)
)

server <- function(input, output, session) {
  ptr_module_server("p", formula)
}

shinyApp(ui, server)
```

`ptr_module_server()` returns the underlying `ptr_state` (from
`ptr_init_state()`), so an embedder can capture it to call
`ptr_gg_extra()` or read `state$runtime()$plot` — see `level3_gg_extra`
and `level3_custom_render`. `...` is forwarded to `ptr_init_state()`,
which is where `ui_text =`, `checkbox_defaults =`, `expr_check =`,
`safe_to_remove =` are accepted.

## Split path — `ptr_controls_ui()` + `ptr_outputs_ui()` + `ptr_server()`

When the controls and outputs need to live in different parts of the
page:

```r
library(shiny); library(ggpaintr)

formula <- "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(ptr_controls_ui("p", formula)),  # widgets only
    mainPanel(ptr_outputs_ui("p"))                # plot / error / code
  )
)

server <- function(input, output, session) {
  shiny::moduleServer("p", function(input, output, session) {
    ptr_server(input, output, session, formula)
  })
}

shinyApp(ui, server)
```

`ptr_controls_ui("p", formula)`, `ptr_outputs_ui("p")`, and the
`moduleServer("p", ...)` wrapper **must share the same `id`**. The
controls UI builds widgets under that namespace; the outputs UI reserves
the three canonical output slots (`ptr_plot`, `ptr_error`, `ptr_code`)
that `ptr_server()` writes to via `ptr_register_plot()` /
`ptr_register_error()` / `ptr_register_code()`.

## What each call does

- `ptr_module_ui(id, formula, ui_text, checkbox_defaults, expr_check, css)` —
  composed sidebar+main panel; an `id`-keyed wrapper around
  `ptr_controls_ui()` + `ptr_outputs_ui()`.
- `ptr_controls_ui(id, formula, ...)` — `tagList` of layer picker,
  per-layer parameter panels, draw button.
- `ptr_outputs_ui(id, css)` — plot pane + inline error pane + code pane.
- `ptr_server(input, output, session, formula, ...)` — wires
  `ptr_init_state()` and registers all three output binders. Returns the
  state object.
- `ptr_module_server(id, formula, ..., shared_state = NULL)` — wraps
  `ptr_server()` inside a `moduleServer(id, ...)`. Returns the state.
- `ptr_init_state(formula, ...)` — state-only constructor; use directly
  when composing custom output bundles (see `level3_custom_render`).

## Splitting plot / code / error into separate panels

`ptr_outputs_ui("p")` ships the three outputs as a single block. If you
want them in different containers (different tabs, separate columns),
skip `ptr_outputs_ui()` and place the underlying outputs yourself —
the output ids inside the `"p"` namespace are `"ptr_plot"`, `"ptr_error"`,
`"ptr_code"`:

```r
ns <- shiny::NS("p")
mainPanel(
  tabsetPanel(
    tabPanel("Plot",   plotOutput(ns("ptr_plot"))),
    tabPanel("Code",   verbatimTextOutput(ns("ptr_code"))),
    tabPanel("Status", uiOutput(ns("ptr_error")))
  )
)
```

Pair with the same `ptr_server()` setup above — its
`ptr_register_*()` registrations write to those namespaced ids.

## Output ids are package-owned

The `ptr_plot` / `ptr_error` / `ptr_code` / `ptr_update_plot` ids (plus
internal `ptr_layer_select` / `ptr_layer_tabset` controls) are fixed
under the module namespace; treat the `ptr_` prefix as reserved. If
you need different output names — your own `plotOutput("my_chart")` —
that is Level 3: read `state$runtime()$plot` (reactive) or
`ptr_extract_plot(state)` (isolated) inside your own `renderPlot()`.
