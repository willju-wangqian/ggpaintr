# Boot scaffolding: load the in-development package into this child app
# process so the e2e test exercises dev source, not a stale system install.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(ggplot2)

# ADR 0028 baseline fixture (default gate_draw = TRUE): a reactive pipeline
# head (`rx()`) with `draw_trigger = rx` — a reactive carrying a DATA FRAME,
# not a click counter. Pins two as-is runtime contracts:
#   1. a reactive pipeline head boots, seeds the ppVar pickers, and draws;
#   2. draw_trigger's clicked() gate (numeric >= 1) means a non-counter value
#      never triggers a redraw — clicking "shrink" changes rx() but the plot
#      must NOT re-render (no Update click happened).
f <- rlang::expr(
  rx() |> ggplot(aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()
)

ui <- ptr_ui_page(
  actionButton("shrink", "Shrink data"),
  sidebarLayout(
    sidebarPanel(ptr_ui_controls(formula = f, id = "p")),
    mainPanel(ptr_ui_plot("p"), ptr_ui_error("p"))
  )
)

server <- function(input, output, session) {
  rx <- reactiveVal(mtcars)
  observeEvent(input$shrink, rx(mtcars[1:5, ]))
  ptr_server(f, "p", draw_trigger = rx)
}

shinyApp(ui, server)
