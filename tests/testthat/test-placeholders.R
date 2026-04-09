test_that("var placeholders replace selected columns inside formula-level transforms", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var + 1, y = log(var))) + geom_point()"
  )

  input <- list(
    "ggplot+3+2+2" = "mpg",
    "ggplot+3+3+2" = "disp",
    "geom_point+checkbox" = TRUE
  )

  result <- ptr_complete_expr(obj, input)

  expect_match(result$code_text, "x = mpg \\+ 1")
  expect_match(result$code_text, "y = log\\(disp\\)")
})

test_that("var placeholders reject direct runtime expressions", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot+3+2" = "log(mpg)",
    "ggplot+3+3" = "disp",
    "geom_point+checkbox" = TRUE
  )

  expect_error(
    ptr_complete_expr(obj, input),
    "must match one available column name"
  )
})

test_that("var placeholders reject unknown column names", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot+3+2" = "not_a_column",
    "ggplot+3+3" = "disp",
    "geom_point+checkbox" = TRUE
  )

  expect_error(
    ptr_complete_expr(obj, input),
    "must match one available column name"
  )
})

test_that("var placeholders reject multiple selected columns", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot+3+2" = c("mpg", "disp"),
    "ggplot+3+3" = "disp",
    "geom_point+checkbox" = TRUE
  )

  expect_error(
    ptr_complete_expr(obj, input),
    "must select exactly one column name"
  )
})

test_that("normalized local column names work with var placeholders", {
  spaced <- data.frame(left = 1:3, right = 4:6, check.names = FALSE)
  names(spaced) <- c("first column", "second column")
  spaced <- ptr_normalize_column_names(spaced)

  obj <- ptr_parse_formula(
    "ggplot(data = spaced, aes(x = var, y = var)) + geom_point()"
  )
  output <- list2env(list(), parent = emptyenv())

  expect_no_error(register_var_ui_outputs(list(), output, obj, envir = environment()))

  result <- ptr_exec(
    obj,
    list(
      "ggplot+3+2" = "first_column",
      "ggplot+3+3" = "second_column",
      "geom_point+checkbox" = TRUE
    ),
    envir = environment()
  )

  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
  expect_match(result$code_text, "x = first_column")
  expect_match(result$code_text, "y = second_column")
})

test_that("text and num placeholders are inserted into the final code", {
  obj <- ptr_parse_formula(
    paste(
      "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +",
      "geom_point(size = num) +",
      "labs(title = text)"
    )
  )

  input <- list(
    "geom_point+2" = 3,
    "labs+2" = "Iris plot",
    "geom_point+checkbox" = TRUE,
    "labs+checkbox" = TRUE
  )

  result <- ptr_complete_expr(obj, input)

  expect_match(result$code_text, 'size = 3')
  expect_match(result$code_text, 'title = "Iris plot"')
})

test_that("expr placeholders are parsed into the completed expression", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  input <- list("facet_wrap+2" = "~ Species", "facet_wrap+checkbox" = TRUE)
  result <- ptr_complete_expr(obj, input)

  expect_match(result$code_text, "facet_wrap\\(~Species\\)")
})
