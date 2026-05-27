# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- ADR 0025 worked example #2 / PLAN-04.
# Two embedded plot modules on one page, each with its own pipeline-head
# `ppUpload()`. Each module has its own code panel; the prologue line
# in each is independent (one upload binds in its module's state, the
# other binds in the sibling module's state). This is the architecturally
# clean form of "two coordinators on one page each with their own upload"
# -- the literal "shared = 'ds'" key form is infeasible today (see
# DRIFT NOTE in the test file: panel-source build_ui hardcodes the
# inner DOM widget id, colliding when two panels both render `shared_ds`).

formula <- "ppUpload() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"

ui <- shiny::fluidPage(
  shiny::div(id = "left-block",
             shiny::h3("Left module"),
             ptr_ui(formula, "left_p1")),
  shiny::div(id = "right-block",
             shiny::h3("Right module"),
             ptr_ui(formula, "right_p1"))
)

server <- function(input, output, session) {
  ptr_server(formula, "left_p1")
  ptr_server(formula, "right_p1")
}

shiny::shinyApp(ui, server)
