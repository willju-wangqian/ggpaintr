#' Build the Runtime Checkbox Input Id for One Layer
#'
#' @param layer_name A parsed non-`ggplot` layer name.
#'
#' @return A single input id string.
#' @noRd
ptr_checkbox_input_id <- function(layer_name) {
  paste0(layer_name, "+checkbox")
}

#' Validate One Layer Checkbox Runtime Input
#'
#' @param layer_name A parsed non-`ggplot` layer name.
#' @param input A Shiny input-like object.
#'
#' @return A single logical value.
#' @noRd
ptr_validate_layer_checkbox_input <- function(layer_name, input) {
  checkbox_id <- ptr_checkbox_input_id(layer_name)
  checkbox_value <- input[[checkbox_id]]

  if (is.null(checkbox_value)) {
    rlang::abort(
      paste0(
        "Missing required runtime input '",
        checkbox_id,
        "'. ptr_exec() requires an explicit TRUE/FALSE value for every non-ggplot layer."
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
#' @param ptr_obj A `ptr_obj`.
#' @param input A Shiny input-like object.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
ptr_validate_layer_checkbox_inputs <- function(ptr_obj, input) {
  layer_names <- setdiff(names(ptr_obj$expr_list), "ggplot")

  for (layer_name in layer_names) {
    ptr_validate_layer_checkbox_input(layer_name, input)
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

  if (isTRUE(input[[ptr_checkbox_input_id(nn)]])) {
    expr
  } else {
    NULL
  }
}

#' Complete a Parsed Formula with User Inputs
#'
#' @param ptr_obj A `ptr_obj`.
#' @param input A Shiny input-like object.
#' @param envir The environment used to resolve local data objects.
#'
#' @return A named list with `complete_expr_list`, `code_text`, and `eval_env`.
#' @noRd
ptr_complete_expr <- function(ptr_obj, input, envir = parent.frame()) {
  assertthat::assert_that(inherits(ptr_obj, "ptr_obj"))

  ptr_processed_expr_list <- ptr_obj[["expr_list"]]
  eval_env <- ptr_prepare_eval_env(ptr_obj, input, envir = envir)
  context <- ptr_define_placeholder_context(ptr_obj, ui_text = NULL, envir = envir)
  context$input <- input
  context$eval_env <- eval_env
  context$var_column_map <- ptr_build_var_column_map(
    ptr_obj,
    input,
    context,
    eval_env
  )
  placeholder_metas <- ptr_flatten_placeholder_map(ptr_obj)

  for (meta in placeholder_metas) {
    spec <- ptr_obj$placeholders[[meta$keyword]]
    input_item <- ptr_resolve_placeholder_input(spec, input, meta, context)
    resolved_expr <- ptr_resolve_placeholder_expr(spec, input_item, meta, context)

    expr_pluck(ptr_processed_expr_list[[meta$layer_name]], meta$index_path) <- resolved_expr
  }

  ptr_processed_expr_list <- lapply(ptr_processed_expr_list, expr_remove_null)
  ptr_processed_expr_list <- lapply(ptr_processed_expr_list, expr_remove_emptycall2)
  ptr_processed_expr_list <- ptr_remove_empty_nonstandalone_layers(ptr_processed_expr_list)
  ptr_validate_layer_checkbox_inputs(ptr_obj, input)
  ptr_processed_expr_list <- purrr::map2(
    ptr_processed_expr_list,
    names(ptr_processed_expr_list),
    expr_apply_checkbox_result,
    input
  )
  ptr_processed_expr_list <- check_remove_null(ptr_processed_expr_list)

  code_text_list <- lapply(ptr_processed_expr_list, rlang::expr_text)
  code_text <- do.call(paste, c(unname(code_text_list), sep = " +\n  "))

  list(
    complete_expr_list = ptr_processed_expr_list,
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
#' obj <- ptr_parse_formula(
#'   "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' runtime <- ptr_exec(obj, inputs)
#' plot_obj <- ptr_assemble_plot(runtime$complete_expr_list, runtime$eval_env)
#' inherits(plot_obj, "ggplot")
#' @export
ptr_assemble_plot <- function(plot_expr_list, envir = parent.frame()) {
  if (is.null(plot_expr_list) || length(plot_expr_list) == 0) {
    rlang::abort("No plot layers remain after processing the selected inputs.")
  }

  plot_list <- lapply(plot_expr_list, eval, envir = envir)

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
ptr_format_runtime_message <- function(stage, condition = NULL, message = NULL) {
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

  detail <- cli::ansi_strip(detail)

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
ptr_mark_runtime_failure <- function(runtime_result, stage, condition) {
  runtime_result$ok <- FALSE
  runtime_result$stage <- stage
  runtime_result$message <- ptr_format_runtime_message(stage, condition)
  runtime_result$condition <- condition
  runtime_result$plot <- NULL
  runtime_result
}

#' Safely Complete a Parsed Formula
#'
#' @param ptr_obj A `ptr_obj`.
#' @param input A Shiny input-like object.
#' @param envir The evaluation environment.
#'
#' @return A structured runtime result.
#' @noRd
ptr_complete_expr_safe <- function(ptr_obj, input, envir = parent.frame()) {
  tryCatch(
    {
      complete_result <- ptr_complete_expr(ptr_obj, input, envir = envir)
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
        message = ptr_format_runtime_message("complete", e),
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
ptr_assemble_plot_safe <- function(runtime_result, envir = parent.frame()) {
  if (!isTRUE(runtime_result$ok)) {
    return(runtime_result)
  }

  plot_env <- runtime_result$eval_env
  if (is.null(plot_env)) {
    plot_env <- envir
  }

  tryCatch(
    {
      runtime_result$plot <- ptr_assemble_plot(runtime_result$complete_expr_list, envir = plot_env)
      runtime_result
    },
    error = function(e) ptr_mark_runtime_failure(runtime_result, "plot", e)
  )
}

#' Safely Validate a Plot at Render Time
#'
#' @param runtime_result A runtime result list.
#'
#' @return An updated runtime result.
#' @noRd
ptr_validate_plot_render_safe <- function(runtime_result) {
  if (!isTRUE(runtime_result$ok)) {
    return(runtime_result)
  }

  tryCatch(
    {
      ggplot2::ggplot_build(runtime_result$plot)
      runtime_result
    },
    error = function(e) ptr_mark_runtime_failure(runtime_result, "plot", e)
  )
}

#' Build the Full Runtime Result for a Paintr App
#'
#' @param ptr_obj A `ptr_obj`.
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
#' obj <- ptr_parse_formula(
#'   "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "Sepal.Length"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "Sepal.Width"
#' runtime <- ptr_exec(
#'   obj,
#'   inputs
#' )
#' isTRUE(runtime$ok)
#' @export
ptr_exec <- function(ptr_obj, input, envir = parent.frame()) {
  runtime_result <- ptr_complete_expr_safe(ptr_obj, input, envir = envir)
  runtime_result <- ptr_assemble_plot_safe(runtime_result, envir = envir)
  ptr_validate_plot_render_safe(runtime_result)
}

#' Build Inline Error UI for a Paintr App
#'
#' @param message A runtime error message.
#'
#' @return A Shiny tag or `NULL`.
#' @noRd
ptr_error_ui <- function(message) {
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
