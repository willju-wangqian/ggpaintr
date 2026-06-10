# Boot scaffolding: load the in-development package into this child app
# process so the e2e test exercises dev source, not a stale system install.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(ggplot2)

# ADR 0028 baseline fixture (live mode): `ptr_options(gate_draw = FALSE)` runs
# the runtime body un-isolated, so the formula's reactive pipeline head
# (`rx()`) is an ordinary reactive dependency — clicking "shrink" changes the
# data and the plot MUST re-render with no Update click (there is no Update
# button in live mode). This is also the positive control proving the
# "plot html changed" detector used by the sibling gate-counter fixture.
ptr_options(gate_draw = FALSE)

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
  ptr_server(f, "p")
}

shinyApp(ui, server)
