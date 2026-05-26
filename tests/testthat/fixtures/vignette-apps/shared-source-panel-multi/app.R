# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the ADR 0023 §1 "Context" worked example #1 (the textbook Bug 1 reproducer:
# two plots sharing one uploaded dataset via a panel-owned ppUpload + a
# panel-owned ppVar). Reviewable by diff against the ADR.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: dev/adr/0023-panel-owned-shared-sources-end-to-end.html
#     §1 worked example #1 — two plots sharing one uploaded dataset >>>
plots <- c(
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = body_mass_g)) + geom_point()",
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = flipper_length_mm))  + geom_point()"
)

panel <- ptr_shared(plots)

ui <- shiny::fluidPage(
  ptr_shared_panel(panel),                                       # shared_ds + shared_col live here
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
# <<<
