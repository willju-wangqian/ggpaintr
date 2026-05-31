# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- this fixture exists for J2 stage 2
# (resolve-upload-source-helper.R:152 absorbed). It registers a custom
# source whose `resolve_data` deterministically throws `"boom"`, so the
# upload observer's tryCatch routes the error into
# `state$resolve_errors[[key]]` and the `#ptr_error` renderUI surfaces
# the literal message verbatim. The build_ui renders the source's own
# fileInput (node$id) so the journey can drive a real `<input type="file">`
# upload via `app$upload_file()`; under ADR 0025 item #7 the shortcut
# textInput is framework-owned (build_ui_for), so build_ui no longer emits
# it (doing so would double-bind node$shortcut_id).
ptr_define_placeholder_source(
  keyword       = "ppFailingSource",
  shortcut      = TRUE,
  build_ui      = function(node, label = NULL, ...) {
    shiny::fileInput(
      inputId = node$id,
      label   = label %||% "Failing data file",
      accept  = NULL
    )
  },
  resolve_data  = function(value, node, ...) {
    if (is.null(value)) return(NULL)
    stop("boom")
  },
  resolve_expr  = function(value, node, ...) rlang::sym(node$auto_name)
)
ptr_app("ppFailingSource() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()")
