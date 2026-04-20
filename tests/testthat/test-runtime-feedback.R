test_that("ptr_complete_expr_safe captures malformed expr input", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  input <- list("facet_wrap_2" = "~", "facet_wrap_checkbox" = TRUE)
  result <- ptr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_null(result$eval_env)
  expect_null(result$complete_expr_list)
  expect_null(result$plot)
  expect_match(result$message, "^Input error:")
})

test_that("ptr_complete_expr_safe captures invalid uploads", {
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = x, y = y)) + geom_point()"
  )

  input <- list(
    "ggplot_2" = mock_upload_input(fixture_path("bad_extension.txt"), "bad_extension.txt"),
    "ggplot_2_name" = ""
  )
  result <- ptr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_null(result$eval_env)
  expect_null(result$complete_expr_list)
  expect_null(result$plot)
  expect_match(result$message, "Please upload a \\.csv or \\.rds file\\.")
})

test_that("ptr_complete_expr_safe fails fast when layer checkbox inputs are missing", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot_3_2" = "mpg",
    "ggplot_3_3" = "disp"
  )
  result <- ptr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_null(result$eval_env)
  expect_null(result$complete_expr_list)
  expect_null(result$plot)
  expect_match(result$message, "geom_point_checkbox")
})

test_that("ptr_complete_expr_safe fails fast on invalid layer checkbox inputs", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  input <- list(
    "ggplot_3_2" = "mpg",
    "ggplot_3_3" = "disp",
    "geom_point_checkbox" = "yes"
  )
  result <- ptr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_null(result$eval_env)
  expect_null(result$complete_expr_list)
  expect_null(result$plot)
  expect_match(result$message, "single TRUE/FALSE value")
})

test_that("ptr_complete_expr_safe captures non-tabular rds uploads early", {
  non_coercible_path <- tempfile(fileext = ".rds")
  saveRDS(function(x) x, non_coercible_path)

  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = x, y = y)) + geom_point()"
  )

  input <- list(
    "ggplot_2" = mock_upload_input(non_coercible_path, "non_coercible.rds"),
    "ggplot_2_name" = ""
  )
  result <- ptr_complete_expr_safe(obj, input)

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$code_text)
  expect_null(result$eval_env)
  expect_null(result$complete_expr_list)
  expect_null(result$plot)
  expect_match(
    result$message,
    "Uploaded data is not usable as tabular data for ggpaintr"
  )
})

test_that("ptr_exec fails fast on invalid var selections", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  result <- ptr_exec(
    obj,
    list(
      "ggplot_3_2" = "log(mpg)",
      "ggplot_3_3" = "disp",
      "geom_point_checkbox" = TRUE
    )
  )

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_null(result$plot)
  expect_match(result$message, "must match one available column name")
  expect_null(result$code_text)
})

test_that("ptr_assemble_plot_safe captures plot-stage missing object errors and keeps code", {
  obj <- ptr_parse_formula(
    "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
  )

  complete_result <- ptr_complete_expr_safe(
    obj,
    list("geom_point_checkbox" = TRUE)
  )
  plot_result <- ptr_assemble_plot_safe(complete_result)

  expect_true(complete_result$ok)
  expect_false(plot_result$ok)
  expect_identical(plot_result$stage, "plot")
  expect_null(plot_result$plot)
  expect_match(plot_result$message, "^Plot error:")
  expect_match(plot_result$message, "unknown_object")
  expect_match(plot_result$code_text, "ggplot\\(data = unknown_object")
})

test_that("ptr_exec defers missing local data objects until draw time", {
  obj <- ptr_parse_formula(
    "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
  )
  output <- list2env(list(), parent = emptyenv())

  expect_no_error(register_var_ui_outputs(list(), output, obj))

  runtime_result <- ptr_exec(
    obj,
    list("geom_point_checkbox" = TRUE)
  )

  expect_false(runtime_result$ok)
  expect_identical(runtime_result$stage, "plot")
  expect_null(runtime_result$plot)
  expect_match(runtime_result$message, "^Plot error:")
  expect_match(runtime_result$message, "unknown_object")
  expect_match(runtime_result$code_text, "ggplot\\(data = unknown_object")
})

test_that("ptr_exec captures render-time faceting errors and keeps code", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)"
  )

  complete_result <- ptr_complete_expr_safe(
    obj,
    list("facet_wrap_2" = "~ Speciesasdf", "facet_wrap_checkbox" = TRUE)
  )
  expect_true(complete_result$ok)
  expect_match(complete_result$code_text, "facet_wrap\\(~Speciesasdf\\)")

  runtime_result <- ptr_exec(
    obj,
    list("facet_wrap_2" = "~ Speciesasdf", "facet_wrap_checkbox" = TRUE)
  )

  expect_false(runtime_result$ok)
  expect_identical(runtime_result$stage, "plot")
  expect_null(runtime_result$plot)
  expect_match(runtime_result$message, "^Plot error:")
  expect_match(runtime_result$message, "Speciesasdf")
  expect_match(runtime_result$code_text, "facet_wrap\\(~Speciesasdf\\)")
})

test_that("ptr_format_runtime_message formats stage labels consistently", {
  expect_identical(
    ptr_format_runtime_message("complete", message = "bad expr"),
    "Input error: bad expr"
  )
  expect_identical(
    ptr_format_runtime_message("plot", message = "missing object"),
    "Plot error: missing object"
  )
  expect_identical(
    ptr_format_runtime_message("other", message = "fallback"),
    "Runtime error: fallback"
  )
})

test_that("ptr_format_runtime_message strips ANSI escape codes", {
  ansi_msg <- paste0(
    "\033[38;5;255mMissing `asdf`\n",
    "\033[31m", "x", "\033[38;5;255m Layer 1 is missing `asdf`\033[39m"
  )
  cond <- simpleError(ansi_msg)
  result <- ptr_format_runtime_message("plot", cond)
  expect_false(grepl("\033", result, fixed = TRUE))
  expect_match(result, "Plot error: Missing `asdf`")
})

test_that("ptr_error_ui returns NULL for blank input and tags for messages", {
  expect_null(ptr_error_ui(NULL))
  expect_null(ptr_error_ui(""))
  expect_null(ptr_error_ui("   "))

  ui <- ptr_error_ui("Input error: bad expr")

  expect_s3_class(ui, "shiny.tag")
  expect_identical(ui$name, "div")
  expect_match(as.character(ui), "Input error: bad expr", fixed = TRUE)
})

test_that("ptr_validate_plot_render_safe returns plot failures from ggplot_build", {
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

  validated <- ptr_validate_plot_render_safe(runtime_result)

  expect_false(validated$ok)
  expect_identical(validated$stage, "plot")
  expect_null(validated$plot)
  expect_match(validated$message, "^Plot error:")
  expect_match(validated$message, "Speciesasdf")
})

test_that("ptr_exec returns plots on successful inputs", {
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
  result <- ptr_exec(obj, input)

  expect_true(result$ok)
  expect_null(result$message)
  expect_s3_class(result$plot, "ggplot")
  expect_match(result$code_text, "facet_wrap\\(~Species\\)")
})

test_that("ptr_exec succeeds when only the base ggplot remains", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )

  result <- ptr_exec(
    obj,
    list("geom_point_checkbox" = FALSE)
  )

  expect_true(result$ok)
  expect_null(result$message)
  expect_s3_class(result$plot, "ggplot")
  expect_equal(length(result$plot$layers), 0)
  expect_no_match(result$code_text, "geom_point\\(")
})

test_that("ptr_assemble_plot_safe reports empty plot-expression input as a plot-stage error", {
  runtime_result <- list(
    ok = TRUE,
    stage = "complete",
    message = NULL,
    code_text = "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))",
    complete_expr_list = NULL,
    eval_env = parent.frame(),
    condition = NULL,
    plot = NULL
  )

  result <- ptr_assemble_plot_safe(runtime_result)

  expect_false(result$ok)
  expect_identical(result$stage, "plot")
  expect_null(result$plot)
  expect_match(result$message, "^Plot error:")
  expect_match(
    result$message,
    "No plot layers remain after processing the selected inputs\\."
  )
})

test_that("ptr_exec uses normalized uploaded column names in code and plots", {
  spaced_path <- tempfile(fileext = ".rds")
  spaced_data <- data.frame(left = 1:3, right = 4:6, check.names = FALSE)
  names(spaced_data) <- c("first column", "second column")
  saveRDS(spaced_data, spaced_path)

  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )
  input <- list(
    "ggplot_2" = mock_upload_input(spaced_path, "spaced columns.rds"),
    "ggplot_2_name" = "",
    "ggplot_3_2" = "first_column",
    "ggplot_3_3" = "second_column",
    "geom_point_checkbox" = TRUE
  )

  result <- ptr_exec(obj, input)

  expect_true(result$ok)
  expect_null(result$message)
  expect_s3_class(result$plot, "ggplot")
  expect_match(result$code_text, "x = first_column")
  expect_match(result$code_text, "y = second_column")
})
