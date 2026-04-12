# Tests for improvements W1-W6 introduced in:
#   R/paintr-placeholders.R, R/paintr-utils.R, R/paintr-export.R, R/paintr-app.R

# =============================================================================
# W1: ptr_resolve_num_expr - NULL/NA guard order
# =============================================================================

test_that("ptr_resolve_num_expr returns ptr_missing_expr for NULL without warning", {
  expect_no_warning(
    result <- ptr_resolve_num_expr(NULL, meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for NA", {
  result <- ptr_resolve_num_expr(NA_real_, meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for NA integer", {
  result <- ptr_resolve_num_expr(NA_integer_, meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns a numeric expression for a valid number", {
  result <- ptr_resolve_num_expr(3.14, meta = list(), context = list())
  expect_true(is.numeric(eval(result)))
  expect_equal(eval(result), 3.14)
})

test_that("ptr_resolve_num_expr NULL check does not invoke is.na on NULL", {
  # Before W1, is.na(NULL) returns logical(0) which is falsy, making the guard
  # fail silently. This test confirms NULL short-circuits before is.na.
  expect_no_error(
    ptr_resolve_num_expr(NULL, meta = list(), context = list())
  )
})

# =============================================================================
# W2: ptr_bind_var_ui_impl - silent skip when has_data is FALSE
# =============================================================================

test_that("formulas with var and no data silently skip during UI preparation", {
  obj <- ptr_parse_formula(unsupported_use_cases$no_data_for_var$formula)
  output <- list2env(list(), parent = emptyenv())
  # Must not error or abort — previously aborted inside a tryCatch
  expect_no_error(
    register_var_ui_outputs(list(), output, obj)
  )
})

test_that("var UI skips silently for layer with no data object", {
  # A layer-level var with no data= argument provides no column info
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length)) + geom_point(aes(colour = var))"
  )
  output <- list2env(list(), parent = emptyenv())
  expect_no_error(
    register_var_ui_outputs(list(), output, obj, envir = globalenv())
  )
})

# =============================================================================
# W3: ptr_normalize_placeholders - seen_keywords initialized as character(0)
# =============================================================================

test_that("ptr_normalize_placeholders with a single valid placeholder has no empty-string contamination", {
  ph <- make_test_date_placeholder()
  result <- ptr_normalize_placeholders(list(date = ph))
  expect_length(result, 1)
  expect_named(result, "date")
  # No empty strings should appear as names
  expect_false("" %in% names(result))
})

test_that("ptr_normalize_placeholders with multiple distinct placeholders stores all", {
  ph_date <- make_test_date_placeholder()
  ph_text <- ptr_define_placeholder(
    keyword = "text",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) {
      if (is.null(value) || identical(value, "")) return(ptr_missing_expr())
      rlang::expr(!!value)
    }
  )
  result <- ptr_normalize_placeholders(list(date = ph_date, text = ph_text))
  expect_length(result, 2)
  expect_named(result, c("date", "text"))
  expect_false(any(names(result) == ""))
})

test_that("ptr_normalize_placeholders detects duplicate keywords without false positives", {
  ph_date1 <- make_test_date_placeholder()
  ph_date2 <- make_test_date_placeholder()
  expect_error(
    ptr_normalize_placeholders(list(date = ph_date1, date = ph_date2)),
    "duplicated keywords"
  )
})

test_that("ptr_normalize_placeholders returns empty list for NULL input", {
  expect_equal(ptr_normalize_placeholders(NULL), list())
})

# =============================================================================
# W5: handle_duplicate_names - O(n) environment-based deduplication
# =============================================================================

test_that("handle_duplicate_names leaves a vector with no duplicates unchanged", {
  x <- c("a", "b", "c")
  expect_equal(handle_duplicate_names(x), c("a", "b", "c"))
})

test_that("handle_duplicate_names keeps first occurrence, suffixes second with -2", {
  result <- handle_duplicate_names(c("a", "b", "a"))
  expect_equal(result, c("a", "b", "a-2"))
})

test_that("handle_duplicate_names suffixes three identical names correctly", {
  result <- handle_duplicate_names(c("x", "x", "x"))
  expect_equal(result, c("x", "x-2", "x-3"))
})

test_that("handle_duplicate_names handles multiple independent duplicated groups", {
  result <- handle_duplicate_names(c("a", "b", "a", "b", "a"))
  expect_equal(result, c("a", "b", "a-2", "b-2", "a-3"))
})

test_that("handle_duplicate_names returns a single-element vector unchanged", {
  expect_equal(handle_duplicate_names(c("z")), c("z"))
})

test_that("handle_duplicate_names returns empty character vector unchanged", {
  expect_equal(handle_duplicate_names(character(0)), character(0))
})

test_that("handle_duplicate_names does not modify the first occurrence's name", {
  result <- handle_duplicate_names(c("geom_point", "geom_point"))
  expect_equal(result[[1]], "geom_point")
  expect_equal(result[[2]], "geom_point-2")
})

# =============================================================================
# W6: ids param in ptr_server
# =============================================================================

test_that("ptr_server accepts ids param without error", {
  server_wrapper <- function(input, output, session) {
    custom_ids <- ptr_build_ids(
      control_panel = "myPanel",
      draw_button = "myDraw",
      plot_output = "myPlot",
      error_output = "myError",
      code_output = "myCode"
    )
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      ids = custom_ids
    )
  }

  shiny::testServer(server_wrapper, {
    expect_type(session$userData$ptr_state, "list")
    expect_s3_class(session$userData$ptr_state, "ptr_state")
    expect_equal(session$userData$ptr_state$ids$control_panel, "myPanel")
    expect_equal(session$userData$ptr_state$ids$draw_button, "myDraw")
  })
})

test_that("ptr_server with default ids has standard id values in state", {
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
    )
  }

  shiny::testServer(server_wrapper, {
    expect_equal(session$userData$ptr_state$ids$control_panel, "controlPanel")
    expect_equal(session$userData$ptr_state$ids$draw_button, "draw")
  })
})

# =============================================================================
# Fix A: numeric(0) / length-0 guard in ptr_resolve_num_expr
# =============================================================================

test_that("ptr_resolve_num_expr returns ptr_missing_expr for numeric(0) without warning", {
  expect_no_warning(
    result <- ptr_resolve_num_expr(numeric(0), meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for integer(0) without warning", {
  expect_no_warning(
    result <- ptr_resolve_num_expr(integer(0), meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for character(0) without error", {
  expect_no_error(
    result <- ptr_resolve_num_expr(character(0), meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})
