test_that("ptr_assemble_plot returns ggplot objects for supported formulas", {
  obj <- ptr_parse_formula(
    paste(
      "ggplot(data = iris, aes(x = var, y = var)) +",
      "geom_point() +",
      "facet_wrap(expr)"
    )
  )

  input <- list(
    "ggplot_3_2" = "Sepal.Length",
    "ggplot_3_3" = "Sepal.Width",
    "facet_wrap_2" = "~ Species",
    "geom_point_checkbox" = TRUE,
    "facet_wrap_checkbox" = TRUE
  )

  result <- ptr_complete_expr(obj, input)
  plot_obj <- ptr_assemble_plot(result$complete_expr_list, envir = result$eval_env)

  expect_s3_class(plot_obj, "ggplot")
})

test_that("ptr_assemble_plot returns a base ggplot when optional layers are unchecked", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )

  result <- ptr_complete_expr(
    obj,
    list("geom_point_checkbox" = FALSE)
  )
  plot_obj <- ptr_assemble_plot(result$complete_expr_list, envir = result$eval_env)

  expect_s3_class(plot_obj, "ggplot")
  expect_equal(length(plot_obj$layers), 0)
  expect_no_match(result$code_text, "geom_point\\(")
})

test_that("ptr_assemble_plot errors clearly when no plot expressions remain", {
  expect_error(
    ptr_assemble_plot(NULL),
    "No plot layers remain after processing the selected inputs\\."
  )

  expect_error(
    ptr_assemble_plot(list()),
    "No plot layers remain after processing the selected inputs\\."
  )
})

test_that("upload-backed formulas can be built into final plots", {
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot_2" = mock_upload_input(fixture_path("simple_numeric.rds"), "simple_numeric.rds"),
    "ggplot_2_name" = "",
    "ggplot_3_2" = "x",
    "ggplot_3_3" = "y",
    "geom_point_checkbox" = TRUE
  )

  result <- ptr_complete_expr(obj, input)
  plot_obj <- ptr_assemble_plot(result$complete_expr_list, envir = result$eval_env)

  expect_s3_class(plot_obj, "ggplot")
})
