# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `l2-shared` >>>
plots <- list(
  "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Length, fill = Species)) + geom_boxplot()",
  "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Width,  fill = Species)) + geom_violin()"
)

obj <- ptr_shared(formulas = plots,
                   shared_ui = list(metric = function(id) shiny::selectInput(id, "Metric", names(iris))))

ui <- shiny::fluidPage(
  shiny::titlePanel("My host app"),
  ptr_shared_panel(obj),
  shiny::fluidRow(
    shiny::column(6, ptr_ui(plots[[1]], "plot_1", shared = obj)),
    shiny::column(6, ptr_ui(plots[[2]], "plot_2", shared = obj))
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(plots[[1]], "plot_1", shared_state = sh)
  ptr_server(plots[[2]], "plot_2", shared_state = sh)
}

shiny::shinyApp(ui, server)
# <<<
