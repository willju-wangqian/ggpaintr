# Boot scaffolding (NOT vignette code). ADR 0023 worked example #2:
# `default_arg` primes panel_sources at boot. `df_main` in script scope is
# captured by `ppUpload(df_main, shared='ds')` as the placeholder's default;
# `try_bind_source_default()` resolves it through eval_env at boot, before
# any upload click, so the downstream `ppVar(shared='col')` picker shows
# `names(mtcars)` immediately.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

df_main <- mtcars

plots <- c(
  "ppUpload(df_main, shared = 'ds') |> ggplot(aes(x = ppVar(shared = 'col'), y = mpg)) + geom_point()",
  "ppUpload(df_main, shared = 'ds') |> ggplot(aes(x = ppVar(shared = 'col'), y = hp))  + geom_col()"
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
  s <- ptr_shared_server(panel, envir = environment())
  ptr_server(panel$formulas[[1L]], "p1", shared_state = s)
  ptr_server(panel$formulas[[2L]], "p2", shared_state = s)
}

shiny::shinyApp(ui, server)
