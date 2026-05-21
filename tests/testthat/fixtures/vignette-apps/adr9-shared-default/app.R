# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. This fixture is NOT vignette-paired — it covers the
# ADR-0009 first-occurrence-wins rule for the shared widget's default
# value (PLAN-07's shared_widget_default helper): two ppVar(shared='col',
# default=...) occurrences with DIFFERENT defaults; the shared widget must
# seed from the FIRST occurrence and silently ignore the second.
# Browser-only because the assertion observes the shared widget's initial
# value before any user interaction.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  "ggplot(mtcars, aes(x = mpg)) +
     geom_point(aes(y  = ppVar(hp, shared = 'col'))) +
     geom_smooth(aes(y = ppVar(wt, shared = 'col')))"
)
