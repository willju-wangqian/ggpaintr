# Regression tests for ADR-0014 — placeholder-registry init ordering.
#
# Defect: the three public placeholder constructors
# (`ptr_define_placeholder_value/_consumer/_source`) bypassed
# `ensure_registry_initialized()` and went straight to
# `ptr_registry_register(entry)`. When a user (or fixture) called any of them
# BEFORE the first read of the registry, init's
# `length(ls(.ptr_registry)) > 0L` short-circuit fired and the five built-ins
# (ppVar/ppText/ppNum/ppExpr/ppUpload) were never registered.
#
# Fix (Shape A): each constructor now calls `ensure_registry_initialized()`
# as its first statement. These tests pin the contract for each constructor
# plus an idempotency invariant.

builtin_keywords <- c("ppVar", "ppText", "ppNum", "ppExpr", "ppUpload")

test_that("ptr_define_placeholder_value on a cleared registry preserves builtins", {
  ptr_registry_clear()
  withr::defer(suppressWarnings(ptr_register_builtins()))

  ptr_define_placeholder_value(
    keyword = "ppFooVal",
    build_ui = function(node, ...) shiny::textInput(node$id, "x"),
    resolve_expr = function(value, ...) value
  )

  kws <- ptr_registry_keywords()
  expect_true(all(builtin_keywords %in% kws))
  expect_true("ppFooVal" %in% kws)
})

test_that("ptr_define_placeholder_consumer on a cleared registry preserves builtins", {
  ptr_registry_clear()
  withr::defer(suppressWarnings(ptr_register_builtins()))

  ptr_define_placeholder_consumer(
    keyword = "ppFooCons",
    build_ui = function(node, cols = character(), ...) {
      shiny::selectInput(node$id, "x", choices = cols)
    },
    resolve_expr = function(value, ...) rlang::sym(value %||% "x")
  )

  kws <- ptr_registry_keywords()
  expect_true(all(builtin_keywords %in% kws))
  expect_true("ppFooCons" %in% kws)
})

test_that("ptr_define_placeholder_source on a cleared registry preserves builtins", {
  ptr_registry_clear()
  withr::defer(suppressWarnings(ptr_register_builtins()))

  ptr_define_placeholder_source(
    keyword = "ppFooSrc",
    build_ui = function(node, ...) shiny::textInput(node$id, "x"),
    resolve_data = function(value, ...) NULL
  )

  kws <- ptr_registry_keywords()
  expect_true(all(builtin_keywords %in% kws))
  expect_true("ppFooSrc" %in% kws)
})

test_that("ptr_register_builtins() on a cleared registry emits no overwrite warnings", {
  # Recursion-net for ADR-0014 follow-up: after the constructors call
  # `ensure_registry_initialized()` (Shape A), a cleared-then-rebuilt
  # registry used to re-enter `ptr_register_builtins()` from inside the
  # first constructor, leading to 5 "Overwriting placeholder registry
  # entry" warnings (one per built-in) on every clear-then-rebuild.
  # The 1-line `done <- TRUE` at the top of `ptr_register_builtins()` is
  # the regression guard this test pins.
  ptr_registry_clear()
  withr::defer(suppressWarnings(ptr_register_builtins()))

  expect_no_warning(ptr_register_builtins())

  kws <- ptr_registry_keywords()
  expect_true(all(builtin_keywords %in% kws))
})

test_that("registering two custom placeholders does not double-register builtins", {
  ptr_registry_clear()
  withr::defer(suppressWarnings(ptr_register_builtins()))

  ptr_define_placeholder_value(
    keyword = "ppDupA",
    build_ui = function(node, ...) shiny::textInput(node$id, "x"),
    resolve_expr = function(value, ...) value
  )
  ptr_define_placeholder_value(
    keyword = "ppDupB",
    build_ui = function(node, ...) shiny::textInput(node$id, "x"),
    resolve_expr = function(value, ...) value
  )

  kws <- ptr_registry_keywords()
  # No keyword appears twice (idempotency of the init short-circuit).
  expect_equal(sum(kws == "ppVar"), 1L)
  expect_equal(sum(kws == "ppText"), 1L)
  expect_equal(sum(kws == "ppNum"), 1L)
  expect_equal(sum(kws == "ppExpr"), 1L)
  expect_equal(sum(kws == "ppUpload"), 1L)
  expect_true(all(c("ppDupA", "ppDupB") %in% kws))
})
