# Regression tests for the `extra$selected` contract in `invoke_build_ui()`
# (R/paintr-build-ui.R:805). Pins the boot-seed precedence and the deselect
# bug fix (consumer renderUI emitting `selected = character(0)` after a
# user empties a `pickerInput(multiple = TRUE)` must NOT snap back to
# `node$default`).
#
# Documented contract reference: R/paintr-registry.R:372-414 (Widget-seeding
# contract on ?ptr_define_placeholder_value) — the framework
# delivers `selected` on every renderUI fire (including the boot fire,
# which carries `node$default` when a positional default is present).
#
# These run in-process against an internal helper. They sit at the unit
# layer so the per-call-site shinytest2 tests (`ptr_setup_value_uis()`,
# `ptr_setup_consumer_uis()`, `ptr_bind_shared_consumer_uis()`) only need
# to confirm wiring, not re-prove the contract per hook signature.

# Each test registers a recording placeholder (build_ui captures the args
# it was called with), invokes `invoke_build_ui()`, and inspects the
# captured `selected`. Cleanup uses `withr::defer()` so the registry is
# restored on test exit.

local_recorder <- function(keyword,
                           hook = c("explicit_selected", "dots_only"),
                           default_arg = NULL,
                           envir = parent.frame()) {
  hook <- match.arg(hook)
  sink <- new.env(parent = emptyenv())
  sink$selected_was_passed <- FALSE
  sink$selected <- NULL
  # Sentinel used by hooks with an explicit `selected` formal default.
  # Lets U2 prove the framework OMITS `selected` (rather than passing
  # NULL or character(0)) when there's no node$default and no caller-
  # supplied value -- the hook's own formal default kicks in.
  sink$sentinel <- list("HOOK_FORMAL_DEFAULT")

  build_ui <- switch(
    hook,
    explicit_selected = function(node, label = NULL, selected = sink$sentinel, ...) {
      sink$selected_was_passed <- !identical(selected, sink$sentinel)
      sink$selected <- selected
      shiny::tags$span()
    },
    dots_only = function(node, label = NULL, ...) {
      dots <- list(...)
      sink$selected_was_passed <- "selected" %in% names(dots)
      sink$selected <- dots$selected
      shiny::tags$span()
    }
  )

  ptr_define_placeholder_value(
    keyword     = keyword,
    build_ui    = build_ui,
    resolve_expr = function(value, ...) value,
    default_arg = default_arg
  )
  withr::defer(suppressMessages(ptr_clear_placeholder(keyword)), envir = envir)
  sink
}

mk_value_node <- function(keyword, default = NULL, id = "n1") {
  ptr_ph_value(
    id = id, keyword = keyword, param = "x",
    expr = NULL, default = default, named_args = list()
  )
}

call_ibu <- function(node, extra = list()) {
  invoke_build_ui(
    node       = node,
    ui_text    = list(),
    layer_name = NULL,
    ns_fn      = identity,
    extra      = extra
  )
}

# ---- U1 -------------------------------------------------------------------
test_that("U1: omitted `selected` + positional default seeds the default at boot", {
  sink <- local_recorder("ppRecU1")
  node <- mk_value_node("ppRecU1", default = "mpg")
  call_ibu(node, extra = list())
  expect_true(sink$selected_was_passed)
  expect_identical(sink$selected, "mpg")
})

# ---- U2 -------------------------------------------------------------------
test_that("U2: omitted `selected` + NULL default => framework omits, hook formal default applies", {
  # When neither the caller (call site) nor the node carries a value to
  # pass to `selected`, the framework MUST NOT inject character(0) or NULL
  # explicitly -- it must omit the arg entirely so the hook's own formal
  # default kicks in. This preserves the legacy contract relied on by
  # custom value placeholders like super-4's ppColor, which declares
  # `selected = NULL` and body `selected %||% "#3366FF"` — a framework
  # that passed character(0) instead of omitting would break that
  # fallback chain (`character(0) %||% "..."` is character(0), not the
  # fallback string).
  sink <- local_recorder("ppRecU2")
  node <- mk_value_node("ppRecU2", default = NULL)
  call_ibu(node, extra = list())
  expect_false(sink$selected_was_passed,
               label = "framework should omit `selected` when no default + no caller value")
  # The recorder's `selected = sink$sentinel` formal default fired,
  # proving the framework didn't pass `selected`.
  expect_identical(sink$selected, sink$sentinel)
})

# ---- U3 -------------------------------------------------------------------
test_that("U3: explicit character(0) is preserved verbatim — the deselect bug regression", {
  # The reported defect: after the user deselects a `pickerInput(multiple =
  # TRUE)`, `input[[id]]` is character(0). The consumer renderUI forwards
  # it as `extra$selected`. The old `length(extra$selected) == 0L` branch
  # in `invoke_build_ui` would clobber it back to `node$default`. Fix:
  # respect verbatim; the call site is responsible for distinguishing
  # NULL (input not bound) from character(0) (user emptied it).
  sink <- local_recorder("ppRecU3")
  node <- mk_value_node("ppRecU3", default = "mpg")
  call_ibu(node, extra = list(selected = character(0)))
  expect_true(sink$selected_was_passed)
  expect_identical(sink$selected, character(0))
})

# ---- U4 -------------------------------------------------------------------
test_that("U4: explicit selected value is passed through unchanged", {
  sink <- local_recorder("ppRecU4")
  node <- mk_value_node("ppRecU4", default = "mpg")
  call_ibu(node, extra = list(selected = "gear"))
  expect_identical(sink$selected, "gear")
})

# ---- U5 -------------------------------------------------------------------
test_that("U5: language `node$default` is deparsed before reaching the hook", {
  # `ptr_arg_expression()` stores the expression as a language object on
  # `node$default`; invoke_build_ui must deparse before binding it, or
  # `do.call` would evaluate the call when binding to the formal.
  sink <- local_recorder("ppRecU5")
  node <- mk_value_node("ppRecU5", default = quote(x + 1))
  call_ibu(node, extra = list())
  expect_identical(sink$selected, "x + 1")
})

# ---- U6 -------------------------------------------------------------------
test_that("U6: hook with `...` only (no explicit `selected` formal) still receives default", {
  # The docs allow `...` instead of an explicit `selected` formal. The
  # `hook_accepts_dots` branch in invoke_build_ui must cover this case.
  sink <- local_recorder("ppRecU6", hook = "dots_only")
  node <- mk_value_node("ppRecU6", default = "mpg")
  call_ibu(node, extra = list())
  expect_true(sink$selected_was_passed)
  expect_identical(sink$selected, "mpg")
})

# ---- U7 -------------------------------------------------------------------
test_that("U7: caller-supplied selected overrides node$default (spec-seed precedence)", {
  # Spec seed flows in as `extra$selected` from the consumer renderUI's
  # `seed %||% current` chain. Must win over node$default.
  sink <- local_recorder("ppRecU7")
  node <- mk_value_node("ppRecU7", default = "mpg")
  call_ibu(node, extra = list(selected = "hp"))
  expect_identical(sink$selected, "hp")
})

# ---- U8 -------------------------------------------------------------------
test_that("U8: NA_real_ is a real value (not 'empty') and survives untouched", {
  # `numericInput` emits NA_real_ when the user clears the field. NA has
  # length 1, so the old `length == 0L` check did not fire — but pin it
  # explicitly to lock the boundary.
  sink <- local_recorder("ppRecU8")
  node <- mk_value_node("ppRecU8", default = 2)
  call_ibu(node, extra = list(selected = NA_real_))
  expect_identical(sink$selected, NA_real_)
})
