test_that("ggpaintr_complete_expr removes unchecked layers and prepares eval env", {
  obj <- ggpaintr_formula(
    paste(
      "ggplot(data = upload, aes(x = var, y = var)) +",
      "geom_point() +",
      "labs(title = text)"
    )
  )

  input <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple_numeric.csv"),
    "ggplot+2+name" = "uploaded_data",
    "ggplot+3+2" = "x",
    "ggplot+3+3" = "y",
    "labs+2" = "Uploaded scatter",
    "geom_point+checkbox" = TRUE,
    "labs+checkbox" = FALSE
  )

  result <- ggpaintr_complete_expr(obj, input)

  expect_false(grepl("labs\\(", result$code_text))
  expect_match(result$code_text, "ggplot\\(data = uploaded_data")
  expect_true(exists("uploaded_data", envir = result$eval_env, inherits = FALSE))
})

test_that("ggpaintr_complete_expr errors on malformed expr input", {
  obj <- ggpaintr_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  input <- list("facet_wrap+2" = "~", "facet_wrap+checkbox" = TRUE)

  expect_error(
    ggpaintr_complete_expr(obj, input)
  )
})

test_that("ggpaintr_complete_expr requires explicit layer checkbox inputs", {
  obj <- ggpaintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot+3+2" = "mpg",
    "ggplot+3+3" = "disp"
  )

  expect_error(
    ggpaintr_complete_expr(obj, input),
    "geom_point\\+checkbox"
  )
})

test_that("ggpaintr_complete_expr rejects invalid layer checkbox inputs", {
  obj <- ggpaintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot+3+2" = "mpg",
    "ggplot+3+3" = "disp",
    "geom_point+checkbox" = "yes"
  )

  expect_error(
    ggpaintr_complete_expr(obj, input),
    "single TRUE/FALSE value"
  )
})
