# Boot scaffolding (NOT vignette code): load the in-development package so the
# e2e test exercises dev source, not a stale system install.
#
# ADR 0025 item #7 FOLLOW-UP fixture -- the shared-coordinator HOST source
# path (R/paintr-shared-ui.R `ptr_setup_panel_sources`), the parallel of the
# single-instance `ptr_setup_source_uis()` that #7 landed. Two plots share a
# panel-owned `ppUpload(shared='ds')` + `ppVar(shared='col')`.
#
# `typed_ds` is an env frame so a typed shortcut resolves through the env-load
# gate (ptr_shared_server runs envir = parent.frame()). Its columns OVERLAP
# penguins.csv on `body_mass_g` / `flipper_length_mm` -- a load-bearing
# proxy-trap guard: the consumer-clear assertion must select an overlapping
# column, because `selectInput` silently drops a column that is unique to the
# old dataset, which would yield a FALSE GREEN. `grp` (typed_ds-only) and
# `species` (penguins-only) make the active source identity observable.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE,
                  attach_testthat = FALSE)
library(shiny)

typed_ds <- data.frame(
  grp               = rep(c("a", "b"), 10),
  body_mass_g       = rnorm(20, 4000, 500),
  flipper_length_mm = rnorm(20, 200, 10)
)

plots <- c(
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = body_mass_g)) + geom_point()",
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = flipper_length_mm)) + geom_point()"
)
panel <- ptr_shared(plots)

ui <- fluidPage(
  ptr_shared_panel(panel),
  fluidRow(
    column(6, ptr_ui(panel$formulas[[1L]], "p1", shared = panel)),
    column(6, ptr_ui(panel$formulas[[2L]], "p2", shared = panel))
  )
)
server <- function(input, output, session) {
  s <- ptr_shared_server(panel)
  ptr_server(panel$formulas[[1L]], "p1", shared_state = s)
  ptr_server(panel$formulas[[2L]], "p2", shared_state = s)
}
shinyApp(ui, server)
