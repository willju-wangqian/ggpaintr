# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` and project memory
# `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the FINDING #7
# regression from dev/notes/placeholder-role-coverage2.html. A panel-shared
# `ppNum(shared = "lw")` (key appears in 2 formulas → panel scope) must
# respect spec=list(shared_lw = 2) at boot.
#
# Pre-fix bug: build_ui_for.ptr_ph_value emits a `uiOutput("shared_lw_ui")`
# placeholder at the host's un-namespaced root (R/paintr-build-ui.R:46-63),
# but no host-scope server-side renderUI registers `output$shared_lw_ui`.
# The per-instance shared-value loop in ptr_setup_value_uis registers at
# the per-instance namespaced output id (`<plot_module_id>-shared_lw_ui`),
# so its `state$spec_seed[[raw_id]]` read at R/paintr-server.R:1881 never
# drives the panel widget. Net: the empty `<div id="shared_lw_ui">` is
# never populated.
ptr_app_grid(
  plots = c(
    "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_line(linewidth = ppNum(1, shared = 'lw'))",
    "ggplot(mtcars, aes(x = wt, y = hp))  + geom_line(linewidth = ppNum(1, shared = 'lw'))"
  ),
  spec = list(shared_lw = 2)
)
