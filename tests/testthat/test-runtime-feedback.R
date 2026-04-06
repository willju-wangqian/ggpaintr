test_that("paintr_complete_expr_safe captures malformed expr input", {
  obj <- paintr_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  input <- list("facet_wrap+2" = "~", "facet_wrap+checkbox" = TRUE)
  result <- paintr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_match(result$message, "^Input error:")
})

test_that("paintr_complete_expr_safe captures invalid uploads", {
  obj <- paintr_formula(
    "ggplot(data = upload, aes(x = x, y = y)) + geom_point()"
  )

  input <- list(
    "ggplot+2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
    "ggplot+2+name" = ""
  )
  result <- paintr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_match(result$message, "Please upload a \\.csv or \\.rds file\\.")
})

test_that("paintr_get_plot_safe captures plot-stage missing object errors and keeps code", {
  obj <- paintr_formula(
    "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
  )

  complete_result <- paintr_complete_expr_safe(
    obj,
    list("geom_point+checkbox" = TRUE)
  )
  plot_result <- paintr_get_plot_safe(complete_result)

  expect_true(complete_result$ok)
  expect_false(plot_result$ok)
  expect_identical(plot_result$stage, "plot")
  expect_match(plot_result$message, "^Plot error:")
  expect_match(plot_result$message, "unknown_object")
  expect_match(plot_result$code_text, "ggplot\\(data = unknown_object")
})

test_that("paintr_build_runtime returns plots on successful inputs", {
  obj <- paintr_formula(
    paste(
      "ggplot(data = iris, aes(x = var, y = var)) +",
      "geom_point() +",
      "facet_wrap(expr)"
    )
  )

  input <- list(
    "ggplot+3+2" = "Sepal.Length",
    "ggplot+3+3" = "Sepal.Width",
    "facet_wrap+2" = "~ Species",
    "geom_point+checkbox" = TRUE,
    "facet_wrap+checkbox" = TRUE
  )
  result <- paintr_build_runtime(obj, input)

  expect_true(result$ok)
  expect_null(result$message)
  expect_s3_class(result$plot, "ggplot")
  expect_match(result$code_text, "facet_wrap\\(~Species\\)")
})
