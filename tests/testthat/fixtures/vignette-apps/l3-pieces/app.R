# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `l3-pieces` >>>
formula <- "ggplot(iris, aes(x = var, y = var, color = var)) + geom_point()"

ui <- ptr_ui_page(                          # Bootstrap page + single .ptr-app + assets
  ptr_ui_header("Iris explorer"),
  shiny::fluidRow(
    shiny::column(4, ptr_ui_controls(formula = formula)),
    shiny::column(8, ptr_ui_plot())         # bare plot card; error placed below
  ),
  ptr_ui_error(),                           # error banner in its own row
  ptr_ui_code()                             # plain, always-visible code card
)
server <- function(input, output, session) {
  ptr_server(input, output, session, formula)   # binds ptr_plot / ptr_error / ptr_code
}

shiny::shinyApp(ui, server)
# <<<
