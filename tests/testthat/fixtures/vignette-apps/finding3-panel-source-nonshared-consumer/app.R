# Boot scaffolding (NOT vignette code). FINDING #3 composite:
# panel-owned ppUpload (shared = 'ds' across BOTH formulas) feeding
# *non-shared* ppVar consumers (different bare-name var per formula:
# 'xa' / 'xb'). The closest prior fixture is shared-source-panel-multi-mixed,
# which uses ppVar(shared='colA'/'colB') (shared-section, key in one formula);
# this fixture is strictly different -- the consumers have no `shared=` arg,
# so they live under each per-instance ns at id
# `<inst>-ggplot_1_1_ppVar_NA` (confirmed via ptr_id_table()).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

plots <- c(
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar('xa'), y = body_mass_g)) + geom_point()",
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar('xb'), y = flipper_length_mm)) + geom_point()"
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
