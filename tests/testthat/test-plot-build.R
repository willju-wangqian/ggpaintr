test_that("paintr_get_plot returns ggplot objects for supported formulas", {
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

  result <- paintr_complete_expr(obj, input)
  plot_obj <- paintr_get_plot(result$complete_expr_list, envir = result$eval_env)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("paintr_get_plot returns a base ggplot when optional layers are unchecked", {
  obj <- paintr_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )

  result <- paintr_complete_expr(
    obj,
    list("geom_point+checkbox" = FALSE)
  )
  plot_obj <- paintr_get_plot(result$complete_expr_list, envir = result$eval_env)

  expect_s3_class(plot_obj, "ggplot")
  expect_equal(length(plot_obj$layers), 0)
  expect_no_match(result$code_text, "geom_point\\(")
})

test_that("paintr_get_plot errors clearly when no plot expressions remain", {
  expect_error(
    paintr_get_plot(NULL),
    "No plot layers remain after processing the selected inputs\\."
  )

  expect_error(
    paintr_get_plot(list()),
    "No plot layers remain after processing the selected inputs\\."
  )
})

test_that("upload-backed formulas can be built into final plots", {
  obj <- paintr_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.rds"), "simple_numeric.rds"),
    "ggplot+2+name" = "",
    "ggplot+3+2" = "x",
    "ggplot+3+3" = "y",
    "geom_point+checkbox" = TRUE
  )

  result <- paintr_complete_expr(obj, input)
  plot_obj <- paintr_get_plot(result$complete_expr_list, envir = result$eval_env)

  expect_s3_class(plot_obj, "ggplot")
})
