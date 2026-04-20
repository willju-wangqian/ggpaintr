test_that("ptr_complete_expr removes unchecked layers and prepares eval env", {
  obj <- ptr_parse_formula(
    paste(
      "ggplot(data = upload, aes(x = var, y = var)) +",
      "geom_point() +",
      "labs(title = text)"
    )
  )

  input <- list(
    "ggplot_2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple_numeric.csv"),
    "ggplot_2_name" = "uploaded_data",
    "ggplot_3_2" = "x",
    "ggplot_3_3" = "y",
    "labs_2" = "Uploaded scatter",
    "geom_point_checkbox" = TRUE,
    "labs_checkbox" = FALSE
  )

  result <- ptr_complete_expr(obj, input)

  expect_false(grepl("labs\\(", result$code_text))
  expect_match(result$code_text, "ggplot\\(data = uploaded_data")
  expect_true(exists("uploaded_data", envir = result$eval_env, inherits = FALSE))
})

test_that("ptr_complete_expr errors on malformed expr input", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  input <- list("facet_wrap_2" = "~", "facet_wrap_checkbox" = TRUE)

  expect_error(
    ptr_complete_expr(obj, input)
  )
})

test_that("ptr_complete_expr requires explicit layer checkbox inputs", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot_3_2" = "mpg",
    "ggplot_3_3" = "disp"
  )

  expect_error(
    ptr_complete_expr(obj, input),
    "geom_point_checkbox"
  )
})

test_that("ptr_complete_expr rejects invalid layer checkbox inputs", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot_3_2" = "mpg",
    "ggplot_3_3" = "disp",
    "geom_point_checkbox" = "yes"
  )

  expect_error(
    ptr_complete_expr(obj, input),
    "single TRUE/FALSE value"
  )
})
