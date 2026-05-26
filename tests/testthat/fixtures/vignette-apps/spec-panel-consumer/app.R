# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` and project memory
# `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the FINDING #1
# regression from dev/notes/placeholder-role-coverage2.html. A panel-shared
# `ppVar(shared = "col")` (key appears in 2 formulas → panel scope) must
# respect spec=list(shared_col = "mpg") at boot.
#
# Pre-fix bug: spec keys at the host's un-namespaced root (`shared_col`)
# are dropped by every per-instance apply_spec_at_boot (prefix filter at
# R/paintr-server.R:533) and there is no host-scope apply_spec_at_boot;
# meanwhile ptr_bind_shared_consumer_uis at R/paintr-server.R:2470-2473
# gates its seed-read on `!is.null(state)` and ptr_shared_server passes no
# state, so the renderUI seed-branch returns NULL and the picker falls
# back to shared_widget_default() (first-occurrence default = "cyl").
ptr_app_grid(
  plots = c(
    "ggplot(mtcars, aes(x = ppVar('cyl', shared = 'col'), y = mpg)) + geom_point()",
    "ggplot(mtcars, aes(x = ppVar('cyl', shared = 'col'), y = hp))  + geom_col()"
  ),
  spec = list(shared_col = "mpg")
)
