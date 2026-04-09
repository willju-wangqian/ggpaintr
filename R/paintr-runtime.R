#' Build the Runtime Checkbox Input Id for One Layer
#'
#' @param layer_name A parsed non-`ggplot` layer name.
#'
#' @return A single input id string.
#' @noRd
ggpaintr_checkbox_input_id <- function(layer_name) {
  paste0(layer_name, "+checkbox")
}

#' Validate One Layer Checkbox Runtime Input
#'
#' @param layer_name A parsed non-`ggplot` layer name.
#' @param input A Shiny input-like object.
#'
#' @return A single logical value.
#' @noRd
ggpaintr_validate_layer_checkbox_input <- function(layer_name, input) {
  checkbox_id <- ggpaintr_checkbox_input_id(layer_name)
  checkbox_value <- input[[checkbox_id]]

  if (is.null(checkbox_value)) {
    rlang::abort(
      paste0(
        "Missing required runtime input '",
        checkbox_id,
        "'. ggpaintr_build_runtime() requires an explicit TRUE/FALSE value for every non-ggplot layer."
      )
    )
  }

  if (!is.logical(checkbox_value) || length(checkbox_value) != 1 || is.na(checkbox_value)) {
    rlang::abort(
      paste0(
        "Runtime input '",
        checkbox_id,
        "' must be a single TRUE/FALSE value."
      )
    )
  }

  checkbox_value
}

#' Validate Checkbox Inputs for Every Optional Layer
#'
#' @param ggpaintr_obj A `ggpaintr_obj`.
#' @param input A Shiny input-like object.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
ggpaintr_validate_layer_checkbox_inputs <- function(ggpaintr_obj, input) {
  layer_names <- setdiff(names(ggpaintr_obj$expr_list), "ggplot")

  for (layer_name in layer_names) {
    ggpaintr_validate_layer_checkbox_input(layer_name, input)
  }

  invisible(NULL)
}

#' Apply a Layer Checkbox Result
#'
#' @param expr A layer expression.
#' @param nn The layer name.
#' @param input A Shiny input-like object.
#'
#' @return The layer expression or `NULL`.
#' @noRd
expr_apply_checkbox_result <- function(expr, nn, input) {
  if (nn == "ggplot") {
    return(expr)
  }

  if (isTRUE(input[[ggpaintr_checkbox_input_id(nn)]])) {
    expr
  } else {
    NULL
  }
}

#' Complete a Parsed Formula with User Inputs
#'
#' @param ggpaintr_obj A `ggpaintr_obj`.
#' @param input A Shiny input-like object.
#' @param envir The environment used to resolve local data objects.
#'
#' @return A named list with `complete_expr_list`, `code_text`, and `eval_env`.
#' @noRd
ggpaintr_complete_expr <- function(ggpaintr_obj, input, envir = parent.frame()) {
  assertthat::assert_that(inherits(ggpaintr_obj, "ggpaintr_obj"))

  ggpaintr_processed_expr_list <- ggpaintr_obj[["expr_list"]]
  eval_env <- ggpaintr_prepare_eval_env(ggpaintr_obj, input, envir = envir)
  context <- ggpaintr_placeholder_context(ggpaintr_obj, copy_rules = NULL, envir = envir)
  context$input <- input
  context$eval_env <- eval_env
  context$var_column_map <- ggpaintr_build_var_column_map(
    ggpaintr_obj,
    input,
    context,
    eval_env
  )
  placeholder_metas <- ggpaintr_flatten_placeholder_map(ggpaintr_obj)

  for (meta in placeholder_metas) {
    spec <- ggpaintr_obj$placeholders[[meta$keyword]]
    input_item <- ggpaintr_resolve_placeholder_input(spec, input, meta, context)
    resolved_expr <- ggpaintr_resolve_placeholder_expr(spec, input_item, meta, context)

    expr_pluck(ggpaintr_processed_expr_list[[meta$layer_name]], meta$index_path) <- resolved_expr
  }

  ggpaintr_processed_expr_list <- lapply(ggpaintr_processed_expr_list, expr_remove_null)
  ggpaintr_processed_expr_list <- lapply(ggpaintr_processed_expr_list, expr_remove_emptycall2)
  ggpaintr_validate_layer_checkbox_inputs(ggpaintr_obj, input)
  ggpaintr_processed_expr_list <- purrr::map2(
    ggpaintr_processed_expr_list,
    names(ggpaintr_processed_expr_list),
    expr_apply_checkbox_result,
    input
  )
  ggpaintr_processed_expr_list <- check_remove_null(ggpaintr_processed_expr_list)

  code_text_list <- lapply(ggpaintr_processed_expr_list, rlang::expr_text)
  code_text <- do.call(paste, c(unname(code_text_list), sep = " +\n  "))

  list(
    complete_expr_list = ggpaintr_processed_expr_list,
    code_text = code_text,
    eval_env = eval_env
  )
}

#' Build a Plot from Completed Layer Expressions
#'
#' @param plot_expr_list A list of completed plot layer expressions.
#' @param envir The evaluation environment.
#'
#' @return A `ggplot` object assembled from the retained layer expressions.
#'   Errors when no plot expressions remain after runtime processing.
#' @examples
#' library(ggplot2)
#'
#' obj <- ggpaintr_formula(
#'   "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
#' )
#' spec <- ggpaintr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' runtime <- ggpaintr_build_runtime(obj, inputs)
#' plot_obj <- ggpaintr_get_plot(runtime$complete_expr_list, runtime$eval_env)
#' inherits(plot_obj, "ggplot")
#' @export
ggpaintr_get_plot <- function(plot_expr_list, envir = parent.frame()) {
  if (is.null(plot_expr_list) || length(plot_expr_list) == 0) {
    rlang::abort("No plot layers remain after processing the selected inputs.")
  }

  plot_list <- lapply(plot_expr_list, eval, envir = envir)
  if (length(plot_list) == 0) {
    rlang::abort("No plot layers remain after processing the selected inputs.")
  }

  p <- plot_list[[1]]
  if (length(plot_list) == 1) {
    return(p)
  }

  for (i in seq.int(2, length(plot_list))) {
    p <- p + plot_list[[i]]
  }

  p
}

#' Format a Runtime Error Message
#'
#' @param stage The failure stage.
#' @param condition An optional condition object.
#' @param message An optional message override.
#'
#' @return A formatted message string.
#' @noRd
ggpaintr_format_runtime_message <- function(stage, condition = NULL, message = NULL) {
  stage_label <- switch(
    stage,
    complete = "Input error",
    plot = "Plot error",
    "Runtime error"
  )

  detail <- message
  if (is.null(detail) || identical(trimws(detail), "")) {
    detail <- if (is.null(condition)) NULL else conditionMessage(condition)
  }

  if (is.null(detail) || identical(trimws(detail), "")) {
    return(stage_label)
  }

  paste0(stage_label, ": ", detail)
}

#' Apply a Structured Runtime Failure to a Result Object
#'
#' @param runtime_result A runtime result list.
#' @param stage The failure stage.
#' @param condition A condition object.
#'
#' @return An updated runtime result.
#' @noRd
ggpaintr_mark_runtime_failure <- function(runtime_result, stage, condition) {
  runtime_result$ok <- FALSE
  runtime_result$stage <- stage
  runtime_result$message <- ggpaintr_format_runtime_message(stage, condition)
  runtime_result$condition <- condition
  runtime_result$plot <- NULL
  runtime_result
}

#' Safely Complete a Parsed Formula
#'
#' @param ggpaintr_obj A `ggpaintr_obj`.
#' @param input A Shiny input-like object.
#' @param envir The evaluation environment.
#'
#' @return A structured runtime result.
#' @noRd
ggpaintr_complete_expr_safe <- function(ggpaintr_obj, input, envir = parent.frame()) {
  tryCatch(
    {
      complete_result <- ggpaintr_complete_expr(ggpaintr_obj, input, envir = envir)
      list(
        ok = TRUE,
        stage = "complete",
        message = NULL,
        code_text = complete_result$code_text,
        complete_expr_list = complete_result$complete_expr_list,
        eval_env = complete_result$eval_env,
        condition = NULL,
        plot = NULL
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        stage = "complete",
        message = ggpaintr_format_runtime_message("complete", e),
        code_text = NULL,
        complete_expr_list = NULL,
        eval_env = NULL,
        condition = e,
        plot = NULL
      )
    }
  )
}

#' Safely Build a Plot from a Runtime Result
#'
#' @param runtime_result A runtime result list.
#' @param envir A fallback evaluation environment.
#'
#' @return An updated runtime result.
#' @noRd
ggpaintr_get_plot_safe <- function(runtime_result, envir = parent.frame()) {
  if (!isTRUE(runtime_result$ok)) {
    return(runtime_result)
  }

  plot_env <- runtime_result$eval_env
  if (is.null(plot_env)) {
    plot_env <- envir
  }

  tryCatch(
    {
      runtime_result$plot <- ggpaintr_get_plot(runtime_result$complete_expr_list, envir = plot_env)
      runtime_result
    },
    error = function(e) ggpaintr_mark_runtime_failure(runtime_result, "plot", e)
  )
}

#' Safely Validate a Plot at Render Time
#'
#' @param runtime_result A runtime result list.
#'
#' @return An updated runtime result.
#' @noRd
ggpaintr_validate_plot_render_safe <- function(runtime_result) {
  if (!isTRUE(runtime_result$ok)) {
    return(runtime_result)
  }

  tryCatch(
    {
      ggplot2::ggplot_build(runtime_result$plot)
      runtime_result
    },
    error = function(e) ggpaintr_mark_runtime_failure(runtime_result, "plot", e)
  )
}

#' Build the Full Runtime Result for a Paintr App
#'
#' @param ggpaintr_obj A `ggpaintr_obj`.
#' @param input A Shiny input-like object.
#' @param envir The environment used to resolve local data objects.
#'
#' @return A runtime result list containing `ok`, `stage`, `message`,
#'   `code_text`, `complete_expr_list`, `eval_env`, `condition`, and `plot`.
#'   Completion-stage validation failures return `stage = "complete"`;
#'   plot-construction or render failures return `stage = "plot"`.
#' @examples
#' library(ggplot2)
#'
#' obj <- ggpaintr_formula(
#'   "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ggpaintr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "Sepal.Length"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "Sepal.Width"
#' runtime <- ggpaintr_build_runtime(
#'   obj,
#'   inputs
#' )
#' isTRUE(runtime$ok)
#' @export
ggpaintr_build_runtime <- function(ggpaintr_obj, input, envir = parent.frame()) {
  runtime_result <- ggpaintr_complete_expr_safe(ggpaintr_obj, input, envir = envir)
  runtime_result <- ggpaintr_get_plot_safe(runtime_result, envir = envir)
  ggpaintr_validate_plot_render_safe(runtime_result)
}

#' Build Inline Error UI for a Paintr App
#'
#' @param message A runtime error message.
#'
#' @return A Shiny tag or `NULL`.
#' @noRd
ggpaintr_error_ui <- function(message) {
  if (is.null(message) || identical(trimws(message), "")) {
    return(NULL)
  }

  shiny::tags$div(
    style = paste(
      "margin-top: 12px;",
      "margin-bottom: 12px;",
      "padding: 12px;",
      "border: 1px solid #c62828;",
      "border-radius: 4px;",
      "background-color: #fff3f3;",
      "color: #7f1d1d;"
    ),
    shiny::tags$strong("Error"),
    shiny::tags$div(
      style = "white-space: pre-wrap; margin-top: 6px;",
      message
    )
  )
}
