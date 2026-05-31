# J11 journey scaffolding (NOT a vignette chunk): smallest fixture that
# exercises ptr_ui / ptr_server explicit-id namespacing. Absorbs the
# unit-level id-alignment assertion previously pinned at
# test-rewrite-app.R:205 (deleted as part of the J11 browser-faithfulness
# merge -- see dev/audit/audit-test-fidelity-v7-j11-browser-faithfulness-
# 2026-05-27-2330.html).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

formula <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"

ui <- shiny::fluidPage(
  ptr_ui(formula, "p1")
)
server <- function(input, output, session) {
  ptr_server(formula, "p1")
}

shiny::shinyApp(ui, server)
