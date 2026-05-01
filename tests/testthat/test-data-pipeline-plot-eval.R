make_plot_eval_env <- function() {
  env <- new.env(parent = .GlobalEnv)
  env$mtcars <- datasets::mtcars
  env$iris <- datasets::iris
  env
}

# ---------------------------------------------------------------------------
# Unit-level: resolved_data substitutes the data argument before eval
# ---------------------------------------------------------------------------

test_that("ptr_complete_expr substitutes the cached frame for the data argument", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = mpg, y = disp)) + geom_point()"
  )
  eval_env <- make_plot_eval_env()
  cached_frame <- datasets::mtcars[1:3, ]
  resolved_data <- list(ggplot = shiny::reactiveVal(cached_frame))

  num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
  input <- list("geom_point_checkbox" = TRUE)
  input[[num_id]] <- 7L

  result <- shiny::isolate(
    ptr_complete_expr(obj, input, envir = eval_env, resolved_data = resolved_data)
  )

  ggplot_call <- result$complete_expr_list[["ggplot"]]
  data_idx <- obj$data_pipeline_info[["ggplot"]]$data_arg_index
  data_slot <- ggplot_call[[data_idx]]

  expect_s3_class(data_slot, "data.frame")
  expect_identical(nrow(data_slot), 3L)
  expect_identical(data_slot, cached_frame)
})

test_that("ptr_assemble_plot renders against the cached frame, not the live pipeline", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = mpg, y = disp)) + geom_point()"
  )
  eval_env <- make_plot_eval_env()
  cached_frame <- datasets::mtcars[1:3, ]
  resolved_data <- list(ggplot = shiny::reactiveVal(cached_frame))

  num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
  input <- list("geom_point_checkbox" = TRUE)
  input[[num_id]] <- 7L

  result <- shiny::isolate(
    ptr_exec(obj, input, envir = eval_env, resolved_data = resolved_data)
  )

  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
  expect_identical(nrow(result$plot$data), 3L)
})

test_that("ptr_complete_expr_safe surfaces an error when the cached frame is NULL", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = mpg, y = disp)) + geom_point()"
  )
  eval_env <- make_plot_eval_env()
  resolved_data <- list(ggplot = shiny::reactiveVal(NULL))

  num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
  input <- list("geom_point_checkbox" = TRUE)
  input[[num_id]] <- 3L

  result <- shiny::isolate(
    ptr_complete_expr_safe(obj, input, envir = eval_env, resolved_data = resolved_data)
  )

  expect_false(result$ok)
  expect_identical(result$stage, "complete")
  expect_match(result$message, "Update data", fixed = TRUE)
})

test_that("resolved_data = NULL falls through to live evaluation (regression)", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = mpg, y = disp)) + geom_point()"
  )
  eval_env <- make_plot_eval_env()

  num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
  input <- list("geom_point_checkbox" = TRUE)
  input[[num_id]] <- 4L

  result <- shiny::isolate(
    ptr_complete_expr(obj, input, envir = eval_env, resolved_data = NULL)
  )

  ggplot_call <- result$complete_expr_list[["ggplot"]]
  data_idx <- obj$data_pipeline_info[["ggplot"]]$data_arg_index

  # Without resolved_data, the data argument remains the (substituted) pipeline call.
  expect_true(is.call(ggplot_call[[data_idx]]))
})

test_that("plain pipe without placeholders still renders (no data_pipeline_info)", {
  obj <- ptr_parse_formula(
    "mtcars |> ggplot(aes(x = mpg, y = disp)) + geom_point()"
  )
  expect_length(obj$data_pipeline_info %||% list(), 0L)

  result <- ptr_exec(
    obj,
    input = list("geom_point_checkbox" = TRUE),
    envir = make_plot_eval_env()
  )
  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
})

# ---------------------------------------------------------------------------
# Per-layer substitution: a geom layer with its own pipeline does not affect
# the global ggplot layer, and vice versa.
# ---------------------------------------------------------------------------

test_that("per-layer cached data substitutes only the matching layer", {
  obj <- ptr_parse_formula(
    "ggplot(mtcars, aes(x = mpg, y = disp)) + geom_point(data = iris |> head(num), aes(x = Sepal.Length, y = Sepal.Width))"
  )

  layer_with_pipeline <- names(obj$data_pipeline_info)
  expect_length(layer_with_pipeline, 1L)
  expect_match(layer_with_pipeline, "geom_point")

  cached_iris <- datasets::iris[1:5, ]
  resolved_data <- stats::setNames(
    list(shiny::reactiveVal(cached_iris)),
    layer_with_pipeline
  )

  num_id <- obj$data_pipeline_info[[layer_with_pipeline]]$placeholder_ids[[1]]
  input <- list("geom_point_checkbox" = TRUE)
  input[[num_id]] <- 99L

  result <- shiny::isolate(
    ptr_complete_expr(
      obj, input, envir = make_plot_eval_env(),
      resolved_data = resolved_data
    )
  )

  ggplot_call <- result$complete_expr_list[["ggplot"]]
  geom_call <- result$complete_expr_list[[layer_with_pipeline]]

  # ggplot layer keeps its symbolic mtcars reference (no pipeline -> no substitution).
  expect_identical(ggplot_call[[2]], quote(mtcars))

  # geom_point layer's data arg is the cached iris frame, not the pipeline call.
  data_idx <- obj$data_pipeline_info[[layer_with_pipeline]]$data_arg_index
  expect_s3_class(geom_call[[data_idx]], "data.frame")
  expect_identical(nrow(geom_call[[data_idx]]), 5L)
})

# ---------------------------------------------------------------------------
# Integration: clicking Update plot does not re-run the data pipeline.
# ---------------------------------------------------------------------------

test_that("Update plot uses the cached frame without re-running the pipeline", {
  pipeline_env <- make_plot_eval_env()

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = mpg, y = disp)) + geom_point()",
      envir = pipeline_env
    )
  }

  shiny::testServer(server_wrapper, {
    state <- session$userData$ptr_state
    obj <- shiny::isolate(state$obj())
    num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
    update_data_id <- ptr_update_data_input_id("ggplot")
    draw_id <- state$server_ids$draw_button

    # Click Update data with num = 3, then move num to 7 *without* clicking
    # Update data again. The cache should still hold the 3-row frame.
    args <- list("geom_point_checkbox" = TRUE)
    args[[num_id]] <- 3L
    args[[update_data_id]] <- 1
    do.call(session$setInputs, args)
    cached <- state$resolved_data[["ggplot"]]()
    expect_identical(nrow(cached), 3L)

    args2 <- list()
    args2[[num_id]] <- 7L
    args2[[draw_id]] <- 1
    do.call(session$setInputs, args2)

    runtime_result <- state$runtime()
    expect_true(runtime_result$ok)
    expect_s3_class(runtime_result$plot, "ggplot")
    expect_identical(nrow(runtime_result$plot$data), 3L)
  })
})
