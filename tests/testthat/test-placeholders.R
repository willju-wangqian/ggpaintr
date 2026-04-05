test_that("var placeholders are substituted into aes mappings", {
  obj <- paintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot+3+2" = "log(mpg)",
    "ggplot+3+3" = "disp + 1",
    "geom_point+checkbox" = TRUE
  )

  result <- paintr_complete_expr(obj, input)

  expect_match(result$code_text, "x = log\\(mpg\\)")
  expect_match(result$code_text, "y = disp \\+ 1")
})

test_that("text and num placeholders are inserted into the final code", {
  obj <- paintr_formula(
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

  result <- paintr_complete_expr(obj, input)

  expect_match(result$code_text, 'size = 3')
  expect_match(result$code_text, 'title = "Iris plot"')
})

test_that("expr placeholders are parsed into the completed expression", {
  obj <- paintr_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  input <- list("facet_wrap+2" = "~ Species", "facet_wrap+checkbox" = TRUE)
  result <- paintr_complete_expr(obj, input)

  expect_match(result$code_text, "facet_wrap\\(~Species\\)")
})
