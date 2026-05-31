# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- ADR 0025 §4 / PLAN-04 (updated for item #7).
# A custom source registered with `shortcut = TRUE` but NO `prologue_emit_fn`
# hook -- the code panel must NOT emit a prologue line for it (the
# custom-source hook is explicitly deferred per ADR scope cuts). Under
# item #7 the shortcut textInput is framework-owned, so `build_ui` no longer
# renders it; the shortcut is seeded to "mtcars" via `spec=` (applied to the
# framework textbox at boot by apply_spec_at_boot -> updateTextInput),
# binding `mtcars` into eval_env at boot.
mtcars <- datasets::mtcars
.caller_env <- environment()
ptr_define_placeholder_source(
  keyword       = "ppDataset",
  shortcut      = TRUE,
  build_ui      = function(node, label = NULL, ...) {
    # ADR 0025 item #7: shortcut textInput is framework-owned; env-only
    # source contributes no widget of its own.
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
ptr_app(
  "ppDataset() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()",
  spec = list(ggplot_0_ppDataset_NA_shortcut = "mtcars")
)
