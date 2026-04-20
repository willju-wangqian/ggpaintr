# Level 2 — embed ggpaintr in your own Shiny app

Use when: the user already has a Shiny app and wants one tab / panel to
hold a ggpaintr-driven plot without giving up control of layout or chrome.

## Minimal embed — 6 public calls

```r
library(shiny); library(ggpaintr)

ui <- fluidPage(
  titlePanel("Embedded ggpaintr"),
  sidebarLayout(
    sidebarPanel(ptr_input_ui()),
    mainPanel(ptr_output_ui())
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) +
       geom_point() +
       labs(title = text)"
  )

  ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_plot(output, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)
}

shinyApp(ui, server)
```

## What each call does

- `ptr_input_ui()` — `uiOutput()` slot for generated controls + draw
  button.
- `ptr_output_ui()` — `plotOutput()` + error `uiOutput()` +
  `verbatimTextOutput()` for generated code.
- `ptr_server_state(formula, ...)` — parses the formula and returns a
  `ptr_state` object with reactives: `obj`, `runtime`, `extras`,
  `var_ui_list`, plus metadata slots. Pass this around to every binder.
- `ptr_setup_controls()` — observes dynamic `var` controls and renders
  the tabbed control panel into `ids$control_panel`.
- `ptr_register_draw()` — observes the draw button and updates
  `ptr_state$runtime()` with the latest `ptr_exec()` result.
- `ptr_register_plot / _error / _code` — each writes its output to a
  single id. Drop any binder you don't need, or replace with your own.

## One-shot shortcut

```r
server <- function(input, output, session) {
  ptr_server(input, output, session, formula = "...")
}
```

`ptr_server()` does `ptr_server_state()` + `ptr_setup_controls()` + all
four `ptr_register_*()` helpers in one call. Use it when you don't need
to customize binders. Use the decomposed form above when you need to
skip or replace binders (e.g. own `renderPlot()` — see
`level3_custom_render`).

## Splitting plot / code / error into separate panels

Because each binder writes to one output id, put the `plotOutput`,
`verbatimTextOutput`, and `uiOutput` anywhere you want:

```r
mainPanel(
  tabsetPanel(
    tabPanel("Plot",   plotOutput("outputPlot")),
    tabPanel("Code",   verbatimTextOutput("outputCode")),
    tabPanel("Status", uiOutput("outputError"))
  )
)
```

Default ids (`outputPlot`, `outputCode`, `outputError`, `controlPanel`,
`draw`) match `ptr_build_ids()` defaults. To rename ids, see
`level2_custom_ids`.
