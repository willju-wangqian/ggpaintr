# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-gallery.Rmd chunk `ggiraph-paintr` >>>
library(ggiraph)
library(colourpicker)

ptr_define_placeholder_value(
  keyword       = "color",
  build_ui      = function(node, label = NULL, ...) {
    colourpicker::colourInput(node$id, label = label %||% "Highlight colour",
                              value = "#e63946")
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value) || identical(value, "")) return(NULL)
    as.character(value)
  },
  copy_defaults = list(label = "Highlight colour for {param}")
)

formula <- "ggplot(data = mpg, aes(x = var, y = var)) +
              geom_point_interactive(aes(tooltip = var),
                                     size = num, color = color)"

ui <- fluidPage(
  fluidRow(
    column(5, ptr_module_ui(formula, "ggiraph_demo")),
    column(7, ggiraph::girafeOutput("interactive_plot", height = "500px"))
  )
)

server <- function(input, output, session) {
  state <- ptr_module_server(formula, "ggiraph_demo")
  output$interactive_plot <- ggiraph::renderGirafe({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    ggiraph::girafe(code = print(res$plot))
  })
}

shinyApp(ui, server)
# <<<
