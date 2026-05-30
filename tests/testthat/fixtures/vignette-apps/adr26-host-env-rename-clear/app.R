# Boot scaffolding (NOT vignette code): load the in-development package so the
# e2e test exercises dev source, not a stale system install.
#
# ADR 0026 fixture -- the host-scope shared ENV->ENV shortcut rename. The
# parallel adr25-host-shared-source-reset fixture covers the host UPLOAD clear
# (ADR 0025 item #7); this one isolates the path #7 did NOT cover: a typed env
# shortcut renamed to a DIFFERENT env frame, with NO file upload at either end.
# At host scope `ptr_bind_shared_consumer_uis` runs with state = NULL, so the
# consumer-clear identity is bound-name-blind (`bn` always "") AND has no
# upload datapath (`dp` always ""), leaving the identity constant across the
# rename -> the §3b new-source clear never fires and a stale column pick rides
# onto the new frame.
#
# TWO env frames are required to express a rename. Both carry `body_mass_g`
# (the OVERLAPPING column) -- a load-bearing proxy-trap guard: the clear
# assertion must select an overlapping column, because `selectInput` silently
# drops a selection unique to the old frame, which would yield a FALSE GREEN.
# `grp` (chickish-only) and `trt` (plantish-only) make the active frame's
# identity observable so the test can prove the rename actually loaded.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE,
                  attach_testthat = FALSE)
library(shiny)

chickish <- data.frame(
  grp         = rep(c("a", "b"), 10),
  body_mass_g = rnorm(20, 4000, 500)
)
plantish <- data.frame(
  trt         = rep(c("ctrl", "trt"), 10),
  body_mass_g = rnorm(20, 300, 50)
)

plots <- c(
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = body_mass_g)) + geom_point()",
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = body_mass_g)) + geom_point()"
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
