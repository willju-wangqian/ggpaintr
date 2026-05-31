# Boot scaffolding (NOT vignette code). Companion fixture for ADR 0015 PLAN-01
# SC-9: with NO source placeholder anywhere in the upstream, the consumer
# picker's renderUI must still pre-warm at boot (entry_reactive's req() guard
# is no-op because source_ready is NULL on this path).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(ggplot(mtcars, aes(x = ppVar(mpg))))
