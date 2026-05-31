# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. This fixture is NOT vignette-paired — it exists to e2e-cover
# the ADR-0009 code-mode toggle (final vs preserve), which has no vignette
# chunk and is otherwise unit-tested at the renderer level only
# (test-render-dual-mode.R). The wiring from the UI radio to ptr_register_code
# was broken between PLAN-08 merge and commit 2c504da; an e2e differential
# assertion (toggle the radio, assert the rendered code text changes shape)
# is the cheapest regression net.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
)
