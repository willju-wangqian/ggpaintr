# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- this fixture exists for ADR 0025 §2 / PLAN-03
# (UI mutex). It drives ppUpload's fileInput + shortcut textInput sibling
# pair, exercises the typing-resets-file branch of the mutex
# (`ptr_bind_source_mutex()`), and asserts that the file pill disappears
# the moment the user types into the shortcut textbox.
ptr_app(
  "ppUpload |> ggplot(aes(ppVar, ppVar)) + geom_point()"
)
