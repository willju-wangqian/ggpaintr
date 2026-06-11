# Boot scaffolding: load the in-development package into this child app
# process so the e2e test exercises dev source, not a stale system install.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
library(ggplot2)
library(plotly)

# >>> ADR 0028 headline composition (Decision 4 / worked example "becomes
# possible #1/#2"), live mode. The second plot is an ORDINARY ggpaintr formula
# whose pipeline head is the selection reactive `sel()`; it redraws per brush
# purely because live mode runs the render body un-isolated, so reading `sel()`
# is an ordinary reactive dependency. No trigger machinery, no adapter, no
# special-cased selection plumbing in core (cf. dev/scripts/plotly_linked_select.R,
# which is the pre-ADR hand-wired shape — deliberately NOT mirrored here).
ptr_options(gate_draw = FALSE)

# Instance 1: a ggpaintr formula over mtcars. Its drawn-data snapshot
# (ggplot's `$data` = full mtcars) is what the flag/rows projections read, so
# the selection-fed instance 2 can map hp/qsec off it.
f1 <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()
)

# Instance 2: an ordinary ggpaintr formula whose pipeline head is sel(). The
# flag projection appends a logical `.ptr_selected`; color = .ptr_selected.
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
      ptr_ui_error("p2"),
      tableOutput("sel_table")
    )
  )
)

server <- function(input, output, session) {
  state1 <- ptr_server(f1, "p1")

  # Explicit source so PLAN-04's injected event ids are deterministic.
  output$main_plotly <- plotly::renderPlotly(
    ptr_ggplotly(state1, source = "ptr_e2e")
  )

  # The selection, projected two ways off the same instance-1 widget.
  sel <- ptr_plotly_selection(state1, mode = "flag", source = "ptr_e2e")
  sel_rows <- ptr_plotly_selection(state1, mode = "rows", source = "ptr_e2e")

  # Instance 2: ordinary ggpaintr formula, pipeline head sel().
  ptr_server(f2, "p2")

  # Worked example "becomes possible #2": the zero-row-same-columns rows
  # projection means the consumer renders unconditionally — no req() dance,
  # no error placeholder; an empty selection is a zero-row table, not blank.
  output$sel_table <- renderTable(sel_rows())
}
# <<<

shinyApp(ui, server)
