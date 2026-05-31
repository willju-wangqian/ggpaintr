# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- this fixture exists for ADR 0025 worked
# example #1 (custom source opts in to env-shortcut), updated for item #7.
# It registers a custom `ppDataset` source with `shortcut = TRUE`. Under
# item #7 the shortcut textInput is FRAMEWORK-OWNED (emitted by
# `build_ui_for`), so the author's `build_ui` no longer renders it -- an
# env-shortcut-only source has no data widget of its own, so `build_ui`
# contributes nothing. `mtcars` is exposed in caller env so that typing
# "mtcars" into the framework shortcut textbox binds the caller-env frame
# and the downstream ppVar picker populates with names(mtcars).
mtcars <- datasets::mtcars
.caller_env <- environment()
ptr_define_placeholder_source(
  keyword       = "ppDataset",
  shortcut      = TRUE,
  build_ui      = function(node, label = NULL, ...) {
    # ADR 0025 item #7: env-shortcut-only source. The shortcut textInput is
    # now framework-owned (build_ui_for), so this source contributes no
    # widget of its own -- the framework textbox is the sole entry point.
    NULL
  },
  resolve_data  = function(value, node, ...) {
    nm <- if (is.character(value) && length(value) == 1L && nzchar(value)) {
      value
    } else NULL
    if (is.null(nm)) return(NULL)
    tryCatch(get(nm, envir = .caller_env, inherits = TRUE),
             error = function(e) NULL)
  },
  resolve_expr  = function(value, node, ...) rlang::sym(value)
)
ptr_app("ppDataset() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()")
