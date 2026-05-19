# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `module-app` >>>
formula <- "ggplot(iris, aes(var, var, color = var)) + geom_point()"

ui <- shiny::fluidPage(
  shiny::titlePanel("My host app"),
  ptr_ui(formula)
)
server <- function(input, output, session) {
  ptr_server(formula)
}

shiny::shinyApp(ui, server)
# <<<
