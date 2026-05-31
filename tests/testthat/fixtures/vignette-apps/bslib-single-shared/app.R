# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install.
#
# Behavior fixture (no vignette-chunk equivalent): a single-formula
# `ppVar(shared = "v")` used in TWO aes slots (alpha + size). It exists to
# prove that ptr_app_bslib() self-binds/coordinates the shared key -- one
# inline widget drives BOTH consumers -- which the prior unit assertion
# `expect_silent(testServer(...flushReact()))` never checked (it only
# proved the server booted without warning). See test-app-bslib.R.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app_bslib(
  paste0(
    "ggplot(data = mtcars, aes(x = mpg, y = hp, alpha = ppVar(shared = \"v\"))) + ",
    "geom_point(aes(size = ppVar(shared = \"v\")))"
  )
)
