# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- this fixture exists for ADR 0025 worked
# example #1 (custom source opts in to env-shortcut). It registers a
# custom `ppDataset` source with `shortcut = TRUE`, renders both
# selectInput(node$id) and textInput(node$shortcut_id) per the ADR's
# code listing, and exposes `mtcars` in caller env so that typing
# "mtcars" into the shortcut textbox binds the caller-env frame and the
# downstream ppVar picker populates with names(mtcars).
mtcars <- datasets::mtcars
.caller_env <- environment()
ptr_define_placeholder_source(
  keyword       = "ppDataset",
  shortcut      = TRUE,
  build_ui      = function(node, label = NULL, ...) {
    # ADR 0025 worked example #1: env-shortcut-only source. The textInput
    # is the sole entry point; user types a caller-env frame name to bind
    # the source. No selectInput here -- a non-NULL selectInput value
    # would short-circuit the try_bind_source_default path and the
    # shortcut would never be consulted.
    shiny::textInput(
      node$shortcut_id,
      label = label %||% "Env frame name",
      value = ""
    )
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
