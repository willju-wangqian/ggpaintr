# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> lifted: vignettes/ggpaintr-use-cases.Rmd chunk `l3-pieces-toggle` (UI)
#     paired with the chunk `l3-pieces` formula + documented server
#     ("# server unchanged"). Verbatim-equivalent. >>>
formula <- "ggplot(iris, aes(x = var, y = var, color = var)) + geom_point()"

ui <- ptr_ui_page(
  ptr_ui_header("Iris explorer"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(ptr_ui_controls(formula = formula)),
    shiny::mainPanel(
      ptr_ui_toggle_code(                                  # </> slide-out toggle ...
        ptr_ui_inline_error(ptr_ui_plot(), ptr_ui_error()),# ... around plot + inline error
        ptr_ui_code()                                      # ... wrapped as the slide-out window
      )
    )
  )
)
# server unchanged
server <- function(input, output, session) {
  ptr_server(formula)
}

shiny::shinyApp(ui, server)
# <<<
