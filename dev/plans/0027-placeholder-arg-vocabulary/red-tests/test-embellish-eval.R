# Red-first tests for PLAN-01 (ADR 0027 D4): embellish_eval built-in helpers.
# Derived from the plan's Interface Contract + BDD ONLY (implementation-blind:
# embellish_identity()/embellish_symbol_to_string() were not read from source —
# they do not exist yet). RED until R/paintr-embellish.R ships them; the DoD
# (devtools::test) turns them green.

test_that("embellish_identity() returns the identity function [PLAN-01 IC: embellish_identity worked example]", {
  f <- embellish_identity()
  expect_identical(f(5L), 5L)
  expect_identical(f(c("a", "b")), c("a", "b"))
})

test_that("embellish_symbol_to_string() makes a tidyselect consumer work in plain R [PLAN-01 BDD: becomes-possible #1]", {
  kw <- "colvars_p2t_pos"
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  colvars <- ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = function(node, cols, data, ...)
      shiny::selectInput(node$id, "Columns", choices = cols, multiple = TRUE),
    resolve_expr = function(value, node, ...) rlang::call2("c", !!!rlang::syms(value)),
    runtime = embellish_symbol_to_string()
  )
  # naked, no app: the consumer's runtime callable is what plain-R eval invokes.
  d <- mtcars |> dplyr::select(colvars(c(mpg, hp)))
  expect_identical(names(d), c("mpg", "hp"))
})

test_that("the identity default breaks the naked tidyselect path [PLAN-01 BDD: rejected-by-design #2]", {
  kw <- "colvars_p2t_bad"
  withr::defer(try(ptr_clear_placeholder(kw), silent = TRUE))
  colvars_bad <- ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = function(node, cols, data, ...)
      shiny::selectInput(node$id, "Columns", choices = cols, multiple = TRUE),
    resolve_expr = function(value, node, ...) rlang::call2("c", !!!rlang::syms(value)),
    runtime = embellish_identity()
  )
  expect_error(mtcars |> dplyr::select(colvars_bad(c(mpg, hp))), "not found")
})
