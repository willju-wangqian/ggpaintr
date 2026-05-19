# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `l3-gg-extra` >>>
formula <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"

ui <- shiny::fluidPage(
  shiny::actionButton("add_log", "Toggle log-scale"),
  ptr_module_ui(formula, "p")
)
server <- function(input, output, session) {
  state <- ptr_module_server(formula, "p")
  shiny::observeEvent(input$add_log, {
    ptr_gg_extra(state, ggplot2::scale_x_log10())
  })
}

shiny::shinyApp(ui, server)
# <<<
