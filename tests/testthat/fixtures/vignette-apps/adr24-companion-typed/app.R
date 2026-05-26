# Boot scaffolding: see .claude/rules/testing.md "Browser e2e (shinytest2)".
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: ADR 0024 — companion as data-loading entry point. No `default=`,
# no `spec=`. The companion textInput starts blank; the test drives both
# the happy path (type "mtcars" → ppVar populates) and the error path
# (type "fooberry" → inline error "not found"). `mtcars` is in scope so
# the get(name, envir, inherits=TRUE) lookup resolves.
ptr_app(
  "ppUpload() |> ggplot(aes(x = ppVar('col'))) + geom_bar()"
)
