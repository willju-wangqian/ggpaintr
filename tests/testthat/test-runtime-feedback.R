test_that("paintr_complete_expr_safe captures malformed expr input", {
  obj <- paintr_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  input <- list("facet_wrap+2" = "~", "facet_wrap+checkbox" = TRUE)
  result <- paintr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_null(result$eval_env)
  expect_null(result$complete_expr_list)
  expect_null(result$plot)
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
  expect_null(result$eval_env)
  expect_null(result$complete_expr_list)
  expect_null(result$plot)
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
  expect_null(plot_result$plot)
  expect_match(plot_result$message, "^Plot error:")
  expect_match(plot_result$message, "unknown_object")
  expect_match(plot_result$code_text, "ggplot\\(data = unknown_object")
})

test_that("paintr_build_runtime defers missing local data objects until draw time", {
  obj <- paintr_formula(
    "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
  )
  output <- list2env(list(), parent = emptyenv())

  expect_no_error(register_var_ui_outputs(list(), output, obj))

  runtime_result <- paintr_build_runtime(
    obj,
    list("geom_point+checkbox" = TRUE)
  )

  expect_false(runtime_result$ok)
  expect_identical(runtime_result$stage, "plot")
  expect_null(runtime_result$plot)
  expect_match(runtime_result$message, "^Plot error:")
  expect_match(runtime_result$message, "unknown_object")
  expect_match(runtime_result$code_text, "ggplot\\(data = unknown_object")
})

test_that("paintr_build_runtime captures render-time faceting errors and keeps code", {
  obj <- paintr_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  complete_result <- paintr_complete_expr_safe(
    obj,
    list("facet_wrap+2" = "~ Speciesasdf", "facet_wrap+checkbox" = TRUE)
  )
  expect_true(complete_result$ok)
  expect_match(complete_result$code_text, "facet_wrap\\(~Speciesasdf\\)")

  runtime_result <- paintr_build_runtime(
    obj,
    list("facet_wrap+2" = "~ Speciesasdf", "facet_wrap+checkbox" = TRUE)
  )

  expect_false(runtime_result$ok)
  expect_identical(runtime_result$stage, "plot")
  expect_null(runtime_result$plot)
  expect_match(runtime_result$message, "^Plot error:")
  expect_match(runtime_result$message, "Speciesasdf")
  expect_match(runtime_result$code_text, "facet_wrap\\(~Speciesasdf\\)")
})

test_that("paintr_format_runtime_message formats stage labels consistently", {
  expect_identical(
    paintr_format_runtime_message("complete", message = "bad expr"),
    "Input error: bad expr"
  )
  expect_identical(
    paintr_format_runtime_message("plot", message = "missing object"),
    "Plot error: missing object"
  )
  expect_identical(
    paintr_format_runtime_message("other", message = "fallback"),
    "Runtime error: fallback"
  )
})

test_that("paintr_error_ui returns NULL for blank input and tags for messages", {
  expect_null(paintr_error_ui(NULL))
  expect_null(paintr_error_ui(""))
  expect_null(paintr_error_ui("   "))

  ui <- paintr_error_ui("Input error: bad expr")

  expect_s3_class(ui, "shiny.tag")
  expect_identical(ui$name, "div")
  expect_match(as.character(ui), "Input error: bad expr", fixed = TRUE)
})

test_that("paintr_validate_plot_render_safe returns plot failures from ggplot_build", {
  bad_plot <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~Speciesasdf)

  runtime_result <- list(
    ok = TRUE,
    stage = "plot",
    message = NULL,
    code_text = "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(~Speciesasdf)",
    complete_expr_list = NULL,
    eval_env = parent.frame(),
    condition = NULL,
    plot = bad_plot
  )

  validated <- paintr_validate_plot_render_safe(runtime_result)

  expect_false(validated$ok)
  expect_identical(validated$stage, "plot")
  expect_null(validated$plot)
  expect_match(validated$message, "^Plot error:")
  expect_match(validated$message, "Speciesasdf")
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
