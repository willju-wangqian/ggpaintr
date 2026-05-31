# J12 journey scaffolding (NOT a vignette chunk): custom-keyword spec at
# boot. Absorbs the testServer-level assertion previously pinned at
# test-adr12b-spec-custom-keyword.R:33 (deleted as part of the J12
# browser-faithfulness merge — see dev/audit/audit-test-fidelity-v8-j12-
# browser-faithfulness-2026-05-27-2337.html).
#
# Registers a custom value placeholder `ppCustomChoice` with a `selected`
# formal so it honors `extra$selected` from invoke_build_ui (the ADR 0012
# PLAN-01 spec-seed pipeline). The browser journey reads the rendered
# selectInput HTML at `#ggplot_1_1_ppCustomChoice_NA` and asserts the
# seeded option carries the `selected` attribute (regex-tolerant per v8
# decision §7.3: `value="b"[^>]*selected`).
#
# Pre-fix bug (Bug B in ADR 0012 §PLAN-01): apply_spec_entry() dispatched
# on placeholder keyword and fell through to FALSE for any custom keyword
# without a built-in updateXyz partner, silently aggregating custom
# entries into the "skipped invalid value" warning. Post-fix the value
# reaches the widget via state$spec_seed -> invoke_build_ui's
# extra$selected -> the hook's `selected` formal.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_define_placeholder_value(
  keyword = "ppCustomChoice",
  build_ui = function(node, label = NULL, selected = NULL, ...) {
    shiny::selectInput(
      inputId  = node$id,
      label    = label %||% "Choice",
      choices  = c("a", "b", "c"),
      selected = selected
    )
  },
  resolve_expr = function(value, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    value
  }
)

ptr_app(
  "ggplot(mtcars, aes(ppCustomChoice, mpg)) + geom_point()",
  spec = list(ggplot_1_1_ppCustomChoice_NA = "b")
)
