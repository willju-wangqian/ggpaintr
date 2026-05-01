make_eval_env <- function() {
  env <- new.env(parent = globalenv())
  env$mtcars <- datasets::mtcars
  env$iris <- datasets::iris
  env
}

test_that("var resolves columns when pipe sits at the head of the formula", {
  skip_if_not_installed("dplyr")

  obj <- ptr_parse_formula(
    "mtcars |> dplyr::filter(mpg > 20) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  res <- ptr_exec(
    obj,
    list("ggplot_3_2" = "wt", "ggplot_3_3" = "mpg", "geom_point_checkbox" = TRUE),
    envir = make_eval_env()
  )

  expect_true(isTRUE(res$ok))
  expect_s3_class(res$plot, "ggplot")
})

test_that("var resolves columns when named data argument contains a pipe chain", {
  skip_if_not_installed("dplyr")

  obj <- ptr_parse_formula(
    "ggplot(data = mtcars |> dplyr::filter(mpg > 20), aes(x = var, y = var)) + geom_point()"
  )
  res <- ptr_exec(
    obj,
    list("ggplot_3_2" = "wt", "ggplot_3_3" = "mpg", "geom_point_checkbox" = TRUE),
    envir = make_eval_env()
  )

  expect_true(isTRUE(res$ok))
  expect_s3_class(res$plot, "ggplot")
})

test_that("var resolves columns when named data argument is a non-pipe call", {
  obj <- ptr_parse_formula(
    "ggplot(data = subset(mtcars, mpg > 20), aes(x = var)) + geom_histogram()"
  )
  res <- ptr_exec(
    obj,
    list("ggplot_3_2" = "wt", "geom_histogram_checkbox" = TRUE),
    envir = make_eval_env()
  )

  expect_true(isTRUE(res$ok))
  expect_s3_class(res$plot, "ggplot")
})

test_that("var still resolves for the plain-symbol data form (regression)", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  res <- ptr_exec(
    obj,
    list("ggplot_3_2" = "wt", "ggplot_3_3" = "mpg", "geom_point_checkbox" = TRUE),
    envir = make_eval_env()
  )

  expect_true(isTRUE(res$ok))
  expect_s3_class(res$plot, "ggplot")
})

test_that("piped data with a missing object degrades to a user-friendly error", {
  skip_if_not_installed("dplyr")

  obj <- ptr_parse_formula(
    "nonexistent |> dplyr::filter(mpg > 20) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  res <- ptr_exec(
    obj,
    list("ggplot_3_2" = "wt", "ggplot_3_3" = "mpg", "geom_point_checkbox" = TRUE),
    envir = make_eval_env()
  )

  expect_false(isTRUE(res$ok))
  expect_type(res$message, "character")
  expect_true(nzchar(res$message))
})

test_that("piped data still validates the column list against placeholder input", {
  skip_if_not_installed("dplyr")

  obj <- ptr_parse_formula(
    "mtcars |> dplyr::filter(mpg > 20) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  res <- ptr_exec(
    obj,
    list("ggplot_3_2" = "not_a_column", "ggplot_3_3" = "mpg", "geom_point_checkbox" = TRUE),
    envir = make_eval_env()
  )

  expect_false(isTRUE(res$ok))
  expect_match(
    res$message,
    "must match one available column name for layer 'ggplot'",
    fixed = TRUE
  )
})
