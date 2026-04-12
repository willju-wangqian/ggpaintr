# Tests for:
#   W1 – ptr_complete_expr / ptr_bind_placeholder_ui accept pre-computed
#          eval_env and var_column_map (caching refactor)

# =============================================================================
# Helpers shared across W1 tests
# =============================================================================

make_basic_obj <- function() {
  ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
}

make_basic_input <- function() {
  list(
    "ggplot+3+2"        = "mpg",
    "ggplot+3+3"        = "disp",
    "geom_point+checkbox" = TRUE
  )
}

# =============================================================================
# W1: ptr_complete_expr – eval_env / var_column_map optional params
# =============================================================================

test_that("ptr_complete_expr succeeds with both eval_env and var_column_map NULL (backward compat)", {
  obj   <- make_basic_obj()
  input <- make_basic_input()

  result <- ptr_complete_expr(obj, input, envir = globalenv())

  expect_type(result, "list")
  expect_named(result, c("complete_expr_list", "code_text", "eval_env"), ignore.order = TRUE)
  expect_match(result$code_text, "mpg")
  expect_match(result$code_text, "disp")
})

test_that("ptr_complete_expr uses a pre-computed eval_env when supplied", {
  obj   <- make_basic_obj()
  input <- make_basic_input()

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())

  # Wrap in a counted environment so we can verify ptr_prepare_eval_env is NOT
  # called a second time — achieved by checking the returned eval_env is the
  # identical object we passed in.
  result <- ptr_complete_expr(obj, input, envir = globalenv(), eval_env = pre_env)

  expect_true(identical(result$eval_env, pre_env))
})

test_that("ptr_complete_expr uses a pre-computed var_column_map when supplied", {
  obj   <- make_basic_obj()
  input <- make_basic_input()

  pre_env  <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context  <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input    <- input
  context$eval_env <- pre_env
  pre_map <- ptr_build_var_column_map(obj, input, context, pre_env)

  result <- ptr_complete_expr(
    obj, input,
    envir          = globalenv(),
    eval_env       = pre_env,
    var_column_map = pre_map
  )

  # Result must still be valid – columns from mtcars resolved correctly
  expect_match(result$code_text, "mpg")
  expect_match(result$code_text, "disp")
  expect_true(identical(result$eval_env, pre_env))
})

test_that("ptr_complete_expr with eval_env supplied but var_column_map NULL still works", {
  obj   <- make_basic_obj()
  input <- make_basic_input()

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())

  # var_column_map = NULL → should be computed fresh internally
  result <- ptr_complete_expr(
    obj, input,
    envir    = globalenv(),
    eval_env = pre_env,
    var_column_map = NULL
  )

  expect_match(result$code_text, "mpg")
  expect_true(identical(result$eval_env, pre_env))
})

test_that("ptr_complete_expr with var_column_map supplied but eval_env NULL still works", {
  obj   <- make_basic_obj()
  input <- make_basic_input()

  # Build map using a freshly computed env (NULL path inside ptr_complete_expr
  # will compute eval_env; we just supply a pre-built map).
  pre_env  <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context  <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input    <- input
  context$eval_env <- pre_env
  pre_map <- ptr_build_var_column_map(obj, input, context, pre_env)

  result <- ptr_complete_expr(
    obj, input,
    envir          = globalenv(),
    eval_env       = NULL,
    var_column_map = pre_map
  )

  expect_match(result$code_text, "mpg")
})

test_that("ptr_complete_expr returns eval_env in result list", {
  obj   <- make_basic_obj()
  input <- make_basic_input()

  result <- ptr_complete_expr(obj, input, envir = globalenv())

  expect_true(is.environment(result$eval_env))
})

# =============================================================================
# W1: ptr_bind_placeholder_ui – eval_env / var_column_map optional params
# =============================================================================

test_that("ptr_bind_placeholder_ui succeeds with both params NULL (backward compat)", {
  obj    <- make_basic_obj()
  input  <- list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  expect_no_error(
    result <- ptr_bind_placeholder_ui(
      input, output, obj, envir = globalenv()
    )
  )
  expect_type(result, "list")
})

test_that("ptr_bind_placeholder_ui uses pre-computed eval_env when supplied", {
  obj    <- make_basic_obj()
  input  <- list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())

  expect_no_error(
    ptr_bind_placeholder_ui(
      input, output, obj,
      envir    = globalenv(),
      eval_env = pre_env
    )
  )
})

test_that("ptr_bind_placeholder_ui uses pre-computed var_column_map when supplied", {
  obj    <- make_basic_obj()
  input  <- list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context  <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input    <- input
  context$eval_env <- pre_env
  pre_map <- ptr_build_var_column_map(obj, input, context, pre_env)

  expect_no_error(
    ptr_bind_placeholder_ui(
      input, output, obj,
      envir          = globalenv(),
      eval_env       = pre_env,
      var_column_map = pre_map
    )
  )
})

test_that("ptr_bind_placeholder_ui with eval_env supplied but var_column_map NULL works", {
  obj    <- make_basic_obj()
  input  <- list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())

  expect_no_error(
    ptr_bind_placeholder_ui(
      input, output, obj,
      envir          = globalenv(),
      eval_env       = pre_env,
      var_column_map = NULL
    )
  )
})

# =============================================================================
# W1: ptr_bind_var_ui_impl – caching via context fields
# =============================================================================

test_that("ptr_bind_var_ui_impl uses context eval_env when pre-set", {
  obj    <- make_basic_obj()
  input  <- list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input    <- input
  context$eval_env <- pre_env

  metas <- ptr_flatten_placeholder_map(obj, keyword = "var")

  expect_no_error(
    ptr_bind_var_ui_impl(input, output, metas, context)
  )
})

test_that("ptr_bind_var_ui_impl uses context var_column_map when pre-set", {
  obj    <- make_basic_obj()
  input  <- list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input    <- input
  context$eval_env <- pre_env
  context$var_column_map <- ptr_build_var_column_map(obj, input, context, pre_env)

  metas <- ptr_flatten_placeholder_map(obj, keyword = "var")

  # Should not error; the pre-set map means no internal recomputation needed
  expect_no_error(
    result <- ptr_bind_var_ui_impl(input, output, metas, context)
  )
  expect_type(result, "list")
})

test_that("ptr_bind_var_ui_impl with NULL context eval_env and var_column_map still works", {
  obj    <- make_basic_obj()
  input  <- list("ggplot+3+2" = "mpg", "ggplot+3+3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  context <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input <- input
  # eval_env and var_column_map deliberately left NULL

  metas <- ptr_flatten_placeholder_map(obj, keyword = "var")

  expect_no_error(
    ptr_bind_var_ui_impl(input, output, metas, context)
  )
})

# =============================================================================
# W1: pre-computed cache produces same output as no-cache path
# =============================================================================

test_that("ptr_complete_expr gives identical code_text whether or not cache is supplied", {
  obj   <- make_basic_obj()
  input <- make_basic_input()

  result_no_cache <- ptr_complete_expr(obj, input, envir = globalenv())

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context  <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input    <- input
  context$eval_env <- pre_env
  pre_map <- ptr_build_var_column_map(obj, input, context, pre_env)

  result_with_cache <- ptr_complete_expr(
    obj, input,
    envir          = globalenv(),
    eval_env       = pre_env,
    var_column_map = pre_map
  )

  expect_equal(result_no_cache$code_text, result_with_cache$code_text)
})

