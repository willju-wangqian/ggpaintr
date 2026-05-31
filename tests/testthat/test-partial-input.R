# Partial-input signaling contract (ppExpr mid-typing + author-facing helper).
#
# Behavior under test:
#   * `ptr_signal_partial()` raises an `rlang` error with class
#     `ptr_partial_input` (the marker class that live picker / preview
#     reactives catch and silently cancel).
#   * The built-in `expr` placeholder's `resolve_expr` hook now signals
#     `ptr_partial_input` -- not a bare `rlang::abort()` -- for empty,
#     unparseable, and multi-expression input. This is the load-bearing
#     change for "stop spamming stderr while the user is mid-typing an
#     expression upstream of a `var` picker" (see R/paintr-builtins.R
#     and R/paintr-server.R entry_reactive partial-input catch).
#
# What this file deliberately does NOT cover:
#   * The shinytest2 picker-stability check (kept out of the unit suite;
#     the catch site is exercised by the kitchen-sink super-app fixture
#     and any browser fixture that pairs a `ppExpr` mutate with a
#     downstream `ppVar` picker).
#   * The plot path's loud surfacing -- the catch only lives inside the
#     live `entry_reactive`, so `ptr_resolve_upstream()` called from the
#     gated observe still re-throws the condition for the error pane.

test_that("ptr_signal_partial raises an rlang error with class ptr_partial_input", {
  err <- expect_error(
    ptr_signal_partial("mid-typing"),
    class = "ptr_partial_input"
  )
  expect_true(inherits(err, "ptr_partial_input"))
  expect_true(inherits(err, "rlang_error"))
  expect_equal(conditionMessage(err), "mid-typing")
})

test_that("ppExpr resolve_expr signals ptr_partial_input for empty input", {
  for (val in list("", NULL, character(0), NA_character_, character(2))) {
    expect_error(
      ptr_builtin_expr_resolve_expr(val, node = list(id = "x")),
      class = "ptr_partial_input"
    )
  }
})

test_that("ppExpr resolve_expr signals ptr_partial_input for unparseable text", {
  # The motivating user report: "mpg / " in a textAreaInput.
  expect_error(
    ptr_builtin_expr_resolve_expr("mpg / ", node = list(id = "x")),
    class = "ptr_partial_input"
  )
  expect_error(
    ptr_builtin_expr_resolve_expr("foo(", node = list(id = "x")),
    class = "ptr_partial_input"
  )
})

test_that("ppExpr resolve_expr signals ptr_partial_input for multi-expression input", {
  expect_error(
    ptr_builtin_expr_resolve_expr("1; 2", node = list(id = "x")),
    class = "ptr_partial_input"
  )
})

test_that("ppExpr resolve_expr still returns a parsed expression for valid input", {
  out <- ptr_builtin_expr_resolve_expr("factor(cyl)", node = list(id = "x"))
  expect_true(is.call(out))
  expect_identical(out, quote(factor(cyl)))
})

# Gated plot-build path: a partial value that survives until the user clicks
# Update / Draw must not crash the Shiny session. `ptr_complete_expr_safe()`'s
# existing `tryCatch(error=)` converts the `ptr_partial_input` condition into
# an `ok = FALSE` runtime result so the error pane shows the diagnostic.
# Regression for the user-reported "app aborts directly" failure (2026-05-25).
test_that("partial input at Update/Draw surfaces as ok=FALSE, not session abort", {
  fml <- 'ggtitle(label = ppExpr())'
  # Look up the placeholder's input id from the parsed tree -- robust to
  # id-scheme tweaks. ppExpr is the only placeholder in this fragment.
  tree <- ptr_translate(fml)
  phs <- find_nodes(tree, is_ptr_placeholder)
  expr_id <- phs[[1L]]$id

  res <- ptr_run_formula(
    fml,
    inputs = setNames(list("mpg /"), expr_id),
    envir = list2env(list(mtcars = mtcars), parent = globalenv())
  )
  expect_false(isTRUE(res$ok))
  expect_equal(res$stage, "complete")
  expect_match(res$error, "could not parse input as R expression")
})
