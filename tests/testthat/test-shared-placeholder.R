test_that("call-form num placeholder parses with shared metadata", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  num_metas <- Filter(function(m) m$keyword == "num", metas)
  expect_length(num_metas, 1L)
  expect_identical(num_metas[[1]]$shared, "size_filter")
})

test_that("bare-symbol placeholder still parses with NULL shared", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num)"
  )
  metas <- ptr_flatten_placeholder_map(obj)
  num_metas <- Filter(function(m) m$keyword == "num", metas)
  expect_length(num_metas, 1L)
  expect_null(num_metas[[1]]$shared)
})

test_that("call-form var and text placeholders parse with shared metadata", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = var(shared = "x_axis"), y = mpg)) + labs(title = text(shared = "plot_title"))'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  shared_names <- vapply(metas, function(m) m$shared %||% NA_character_, character(1))
  keywords <- vapply(metas, function(m) m$keyword, character(1))
  expect_true("x_axis" %in% shared_names)
  expect_true("plot_title" %in% shared_names)
  expect_identical(unname(shared_names[keywords == "var"])[1], "x_axis")
  expect_identical(unname(shared_names[keywords == "text"])[1], "plot_title")
})

test_that("call-form placeholder with no args is equivalent to bare symbol", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num())"
  )
  metas <- ptr_flatten_placeholder_map(obj)
  num_metas <- Filter(function(m) m$keyword == "num", metas)
  expect_length(num_metas, 1L)
  expect_null(num_metas[[1]]$shared)
})

test_that("positional arg in call-form placeholder is rejected", {
  expect_error(
    ptr_parse_formula(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num("size_filter"))'
    ),
    "Positional arguments are not allowed"
  )
})

test_that("unknown arg in call-form placeholder is rejected", {
  expect_error(
    ptr_parse_formula(
      "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(min = 0))"
    ),
    "unknown argument"
  )
})

test_that("non-string shared value is rejected", {
  expect_error(
    ptr_parse_formula(
      "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = 42))"
    ),
    "must be a single non-empty string"
  )
})

test_that("empty shared string is rejected", {
  expect_error(
    ptr_parse_formula(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = ""))'
    ),
    "must be a single non-empty string"
  )
})

test_that("call-form placeholder substitutes correctly at runtime", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  spec <- ptr_runtime_input_spec(obj)
  num_id <- spec$input_id[spec$keyword == "num" & !is.na(spec$keyword)][1]
  input <- list()
  input[["geom_point_checkbox"]] <- TRUE
  input[[num_id]] <- 3
  res <- ptr_complete_expr(obj, input)
  expect_true(grepl("size = 3", res$code_text, fixed = TRUE))
  expect_false(grepl("shared", res$code_text, fixed = TRUE))
})

test_that("call-form expr placeholder is exempt from safety walker", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + expr(shared = "extras")'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  expr_metas <- Filter(function(m) m$keyword == "expr", metas)
  expect_length(expr_metas, 1L)
  expect_identical(expr_metas[[1]]$shared, "extras")
})

test_that("multiple placeholders can share the same name within one formula", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = var(shared = "axis"), y = var(shared = "axis"))) + geom_point()'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  shared_axis <- Filter(function(m) identical(m$shared, "axis"), metas)
  expect_length(shared_axis, 2L)
})
