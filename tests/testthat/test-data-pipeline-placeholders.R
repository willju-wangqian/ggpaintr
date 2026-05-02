make_pipeline_eval_env <- function() {
  env <- new.env(parent = .GlobalEnv)
  env$mtcars <- datasets::mtcars
  env$iris <- datasets::iris
  env
}

# ---------------------------------------------------------------------------
# Detection
# ---------------------------------------------------------------------------

test_that("ptr_data_arg_index finds positional first arg by default", {
  e <- quote(ggplot(mtcars, aes(x = var)))
  expect_identical(ptr_data_arg_index(e), 2L)
})

test_that("ptr_data_arg_index finds named `data` regardless of position", {
  e <- quote(ggplot(aes(x = var), data = mtcars))
  expect_identical(ptr_data_arg_index(e), 3L)
})

test_that("ptr_data_arg_index returns NA when no data slot is identifiable", {
  e <- quote(geom_point(color = "red"))
  expect_true(is.na(ptr_data_arg_index(e)))
})

test_that("ptr_compute_data_pipeline_info detects placeholders inside a pipe", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )

  info <- obj$data_pipeline_info[["ggplot"]]

  expect_false(is.null(info))
  expect_identical(info$data_arg_index, 2L)
  expect_length(info$placeholder_ids, 1L)
  expect_match(info$placeholder_ids, "^ggplot_2_")
})

test_that("layers without data placeholders are absent from data_pipeline_info", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  expect_length(obj$data_pipeline_info, 0L)
})

test_that("data placeholder at the data slot itself is not pipeline-detected", {
  # `upload` here IS the data argument, not nested inside a pipeline.
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var)) + geom_point()"
  )
  expect_length(obj$data_pipeline_info, 0L)
})

# ---------------------------------------------------------------------------
# trim-and-eval
# ---------------------------------------------------------------------------

test_that("ptr_trim_and_eval drops a marker arg and keeps the verb", {
  marker <- ptr_unset_data_marker()
  expr <- bquote(head(mtcars, .(marker)))
  res <- ptr_trim_and_eval(expr, make_pipeline_eval_env(), marker)

  expect_true(res$ok)
  expect_s3_class(res$value, "data.frame")
  expect_identical(nrow(res$value), 6L) # head() default
})

test_that("ptr_trim_and_eval falls back to upstream data when verb itself errors", {
  marker <- ptr_unset_data_marker()
  # subset()'s `subset =` is dropped, leaving subset(mtcars). Returns mtcars.
  expr <- bquote(subset(mtcars, .(marker)))
  res <- ptr_trim_and_eval(expr, make_pipeline_eval_env(), marker)

  expect_true(res$ok)
  expect_identical(nrow(res$value), nrow(datasets::mtcars))
})

test_that("ptr_trim_and_eval returns ok=FALSE when the root symbol is unresolvable", {
  marker <- ptr_unset_data_marker()
  expr <- bquote(head(does_not_exist_xyz, .(marker)))
  res <- ptr_trim_and_eval(expr, make_pipeline_eval_env(), marker)

  expect_false(res$ok)
  expect_null(res$value)
})

test_that("ptr_trim_and_eval drops a verb whose marker-trim leaves a 0-column frame", {
  # dplyr::select(mtcars) succeeds and returns a 0-column tibble. Without the
  # scoped fallback, downstream verbs would silently break on that frame.
  skip_if_not_installed("dplyr")
  marker <- ptr_unset_data_marker()
  expr <- bquote(dplyr::select(mtcars, .(marker)))
  res <- ptr_trim_and_eval(expr, make_pipeline_eval_env(), marker)

  expect_true(res$ok)
  expect_identical(res$value, datasets::mtcars)
  expect_identical(res$expr, quote(mtcars))
})

test_that("ptr_trim_and_eval preserves a user-authored 0-column verb when no marker present", {
  # The user wrote select(mtcars, c()) by hand — no placeholder involved.
  # Scope rule: only drop a 0-col result when a marker was actually trimmed.
  skip_if_not_installed("dplyr")
  marker <- ptr_unset_data_marker()
  expr <- quote(dplyr::select(mtcars, c()))
  res <- ptr_trim_and_eval(expr, make_pipeline_eval_env(), marker)

  expect_true(res$ok)
  expect_s3_class(res$value, "data.frame")
  expect_identical(ncol(res$value), 0L)
})

# ---------------------------------------------------------------------------
# ptr_resolve_layer_data integration
# ---------------------------------------------------------------------------

test_that("ptr_resolve_layer_data resolves a placeholder-bearing pipe at initial load", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input = list(),
    context = context,
    eval_env = make_pipeline_eval_env()
  )

  expect_true(result$has_data)
  expect_s3_class(result$data, "data.frame")
  expect_identical(names(result$data), names(datasets::mtcars))
})

test_that("ptr_resolve_layer_data uses the placeholder value when set", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input = stats::setNames(list(3L), num_id),
    context = context,
    eval_env = make_pipeline_eval_env()
  )

  expect_true(result$has_data)
  expect_identical(nrow(result$data), 3L)
})

test_that("ptr_build_var_column_map populates columns for piped data with unset placeholder", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)

  cmap <- ptr_build_var_column_map(
    obj,
    input = list(),
    context = context,
    eval_env = make_pipeline_eval_env()
  )

  expect_true("ggplot" %in% names(cmap))
  expect_true(cmap[["ggplot"]]$has_data)
  expect_setequal(cmap[["ggplot"]]$columns, names(datasets::mtcars))
})

test_that("placeholder inside filter() degrades cleanly when unset", {
  skip_if_not_installed("dplyr")
  obj <- ptr_parse_formula(
    "mtcars |> dplyr::filter(mpg > num) |> ggplot(aes(x = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input = list(),
    context = context,
    eval_env = make_pipeline_eval_env()
  )

  expect_true(result$has_data)
  expect_s3_class(result$data, "data.frame")
  expect_identical(names(result$data), names(datasets::mtcars))
})

test_that("multiple placeholder-bearing verbs all trim down to the root", {
  skip_if_not_installed("dplyr")
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> dplyr::filter(mpg > num) |> ggplot(aes(x = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input = list(),
    context = context,
    eval_env = make_pipeline_eval_env()
  )

  expect_true(result$has_data)
  expect_identical(names(result$data), names(datasets::mtcars))
})

test_that("regression: plain pipe without data placeholders still resolves", {
  skip_if_not_installed("dplyr")
  obj <- ptr_parse_formula(
    "mtcars |> dplyr::filter(mpg > 20) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input = list(),
    context = context,
    eval_env = make_pipeline_eval_env()
  )

  expect_true(result$has_data)
  expect_s3_class(result$data, "data.frame")
})
