# Boot scaffolding (NOT vignette code). ADR 0023 worked example #3:
# Mixed scope -- the source `ds` is panel-owned (>=2 formulas), while the
# consumers `colA` / `colB` are formula-local (each appears in exactly one
# formula). The formula-local consumer pickers MUST read from the
# panel-resolved data frame; their DOM ids are per-instance-namespaced
# (`p1-shared_colA`, `p2-shared_colB`), and no panel-scope `shared_colA`
# / `shared_colB` exists.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

plots <- c(
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'colA'), y = body_mass_g)) + geom_point()",
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'colB'), y = flipper_length_mm))  + geom_point()"
)

panel <- ptr_shared(plots)

ui <- shiny::fluidPage(
  ptr_shared_panel(panel),
  shiny::fluidRow(
    shiny::column(6, ptr_ui(panel$formulas[[1L]], "p1", shared = panel)),
    shiny::column(6, ptr_ui(panel$formulas[[2L]], "p2", shared = panel))
  )
)
server <- function(input, output, session) {
  s <- ptr_shared_server(panel)
  ptr_server(panel$formulas[[1L]], "p1", shared_state = s)
  ptr_server(panel$formulas[[2L]], "p2", shared_state = s)
}

shiny::shinyApp(ui, server)
