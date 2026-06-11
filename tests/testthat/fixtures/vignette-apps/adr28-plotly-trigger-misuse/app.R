# Boot scaffolding: load the in-development package into this child app
# process so the e2e test exercises dev source, not a stale system install.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(ggplot2)
library(plotly)

# >>> ADR 0028 rejected-by-design #3 (PLAN-04). DEFAULT gate (gate_draw = TRUE):
# instance 2 only re-renders on its Update/Draw click, because `draw_trigger`'s
# clicked() gate (R/paintr-server.R) accepts a numeric click counter (>= 1)
# only. Here `draw_trigger = sel` hands it the SELECTION REACTIVE — a value
# carrying a data frame, NOT a click counter — so a brush that updates sel()
# can never fire the gate, and instance 2 silently never redraws. This is the
# discriminating partner of adr28-plotly-linked's live-mode redraw: same
# injection, but no live render body and a non-counter trigger => no redraw.
#
# Instance 1 is wired exactly as in adr28-plotly-linked (explicit
# source = "ptr_e2e", same widget id "main_plotly") so PLAN-04 can inject the
# identical `plotly_selected-ptr_e2e` payload; the only differences are the
# default gate and the draw_trigger = sel misuse.

# Instance 1: a ggpaintr formula over mtcars (the plotly-rendered source).
f1 <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()
)

# Instance 2: an ordinary ggpaintr formula whose pipeline head is sel().
f2 <- rlang::expr(
  sel() |>
    ggplot(aes(x = ppVar(hp), y = ppVar(qsec), color = .ptr_selected)) +
    geom_point()
)

ui <- ptr_ui_page(
  sidebarLayout(
    sidebarPanel(
      ptr_ui_controls(formula = f1, id = "p1"),
      ptr_ui_controls(formula = f2, id = "p2")
    ),
    mainPanel(
      plotly::plotlyOutput("main_plotly"),
      ptr_ui_error("p1"),
      ptr_ui_plot("p2"),
      ptr_ui_error("p2")
    )
  )
)

server <- function(input, output, session) {
  state1 <- ptr_server(f1, "p1")

  # Explicit source so PLAN-04's injected event ids are deterministic.
  output$main_plotly <- plotly::renderPlotly(
    ptr_ggplotly(state1, source = "ptr_e2e")
  )

  # The selection reactive (flag projection), as in adr28-plotly-linked.
  sel <- ptr_plotly_selection(state1, mode = "flag", source = "ptr_e2e")

  # ADR rejected-by-design #3: wiring the selection reactive itself as
  # draw_trigger. clicked() expects a numeric click counter, so this never
  # fires a redraw of instance 2 — the brush is silently ignored.
  ptr_server(f2, "p2", draw_trigger = sel)
}
# <<<

shinyApp(ui, server)
