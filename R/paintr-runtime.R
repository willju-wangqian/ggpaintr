#' Build the Runtime Checkbox Input Id for One Layer
#'
#' @param layer_name A parsed non-`ggplot` layer name.
#'
#' @return A single input id string.
#' @noRd
ptr_checkbox_input_id <- function(layer_name) {
  paste0(layer_name, "_checkbox")
}

#' Validate One Layer Checkbox Runtime Input
#'
#' @param layer_name A parsed non-`ggplot` layer name.
#' @param input A Shiny input-like object.
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return A single logical value.
#' @noRd
ptr_validate_layer_checkbox_input <- function(layer_name, input,
                                              ns_fn = shiny::NS(NULL)) {
  checkbox_id <- ptr_ns_id(ns_fn, ptr_checkbox_input_id(layer_name))
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
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
ptr_validate_layer_checkbox_inputs <- function(ptr_obj, input,
                                               ns_fn = shiny::NS(NULL)) {
  layer_names <- setdiff(names(ptr_obj$expr_list), "ggplot")

  for (layer_name in layer_names) {
    ptr_validate_layer_checkbox_input(layer_name, input, ns_fn = ns_fn)
  }

  invisible(NULL)
}

#' Apply a Layer Checkbox Result
#'
#' @param expr A layer expression.
#' @param nn The layer name.
#' @param input A Shiny input-like object.
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return The layer expression or `NULL`.
#' @noRd
expr_apply_checkbox_result <- function(expr, nn, input,
                                       ns_fn = shiny::NS(NULL)) {
  if (nn == "ggplot") {
    return(expr)
  }

  if (isTRUE(input[[ptr_ns_id(ns_fn, ptr_checkbox_input_id(nn))]])) {
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
#' @param ns_fn A namespace function `character -> character`.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Extends the curated default set: `theme()`, `labs()`,
#'   `xlab()`, `ylab()`, `ggtitle()`, `facet_wrap()`, `facet_grid()`,
#'   `facet_null()`, `xlim()`, `ylim()`, `lims()`, `expand_limits()`,
#'   `guides()`, `annotate()`, `annotation_custom()`, `annotation_map()`,
#'   `annotation_raster()`, `aes()`, `aes_()`, `aes_q()`, `aes_string()`,
#'   `vars()`, `element_text()`, `element_line()`, `element_rect()`,
#'   `element_point()`, `element_polygon()`, `element_geom()`. `geom_*()` /
#'   `stat_*()` standalone layers are always preserved.
#'
#' @return A named list with `complete_expr_list`, `code_text`, and `eval_env`.
#' @noRd
ptr_substitute_cached_data <- function(expr_list, data_pipeline_info, resolved_data) {
  if (is.null(resolved_data) || length(resolved_data) == 0L) return(expr_list)
  if (is.null(data_pipeline_info) || length(data_pipeline_info) == 0L) return(expr_list)

  for (layer_name in names(data_pipeline_info)) {
    if (!layer_name %in% names(expr_list)) next
    cache_react <- resolved_data[[layer_name]]
    if (is.null(cache_react)) next
    cached <- cache_react()
    if (is.null(cached)) {
      rlang::abort(sprintf(
        "No cached data available for layer \"%s\". Click \"Update data\" to refresh.",
        layer_name
      ))
    }
    pipeline <- data_pipeline_info[[layer_name]]
    layer_expr <- expr_list[[layer_name]]
    if (is.call(layer_expr) && pipeline$data_arg_index <= length(layer_expr)) {
      layer_expr[[pipeline$data_arg_index]] <- cached
      expr_list[[layer_name]] <- layer_expr
    }
  }
  expr_list
}

ptr_complete_expr <- function(ptr_obj, input, envir = parent.frame(),
  eval_env = NULL, var_column_map = NULL, expr_check = TRUE,
  ns_fn = shiny::NS(NULL), safe_to_remove = character(),
  shared_bindings = list(), resolved_data = NULL,
  last_click_inputs = NULL) {
  assertthat::assert_that(inherits(ptr_obj, "ptr_obj"))
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)
  remove_set <- unique(c(default_safe_to_remove(), safe_to_remove))

  ptr_processed_expr_list <- ptr_obj[["expr_list"]]
  original_expr_list <- ptr_obj[["expr_list"]]
  if (is.null(eval_env)) {
    eval_env <- ptr_prepare_eval_env(ptr_obj, input, envir = envir, ns_fn = ns_fn)
  }
  context <- ptr_define_placeholder_context(
    ptr_obj, ui_text = NULL, envir = envir, expr_check = expr_check
  )
  context$ns_fn <- ns_fn
  context$input <- input
  context$eval_env <- eval_env
  context$shared_bindings <- shared_bindings
  if (is.null(var_column_map)) {
    var_column_map <- ptr_build_var_column_map(
      ptr_obj,
      input,
      context,
      eval_env,
      resolved_data = resolved_data
    )
  }
  context$var_column_map <- var_column_map
  placeholder_metas <- ptr_flatten_placeholder_map(ptr_obj)

  data_pipeline_info <- ptr_obj[["data_pipeline_info"]] %||% list()
  pipeline_id_to_layer <- character()
  for (ln in names(data_pipeline_info)) {
    ids <- data_pipeline_info[[ln]]$placeholder_ids
    pipeline_id_to_layer[ids] <- ln
  }

  for (meta in placeholder_metas) {
    spec <- ptr_obj$placeholders[[meta$keyword]]
    is_data_placeholder <- meta$id %in% names(pipeline_id_to_layer)
    if (is_data_placeholder && !is.null(last_click_inputs)) {
      snap_layer <- unname(pipeline_id_to_layer[meta$id])
      snap <- last_click_inputs[[snap_layer]]
      input_item <- if (is.list(snap) && meta$id %in% names(snap)) snap[[meta$id]] else NULL
    } else {
      input_item <- ptr_resolve_placeholder_input(spec, input, meta, context)
    }
    if (is.null(input_item)) {
      expr_pluck(ptr_processed_expr_list[[meta$layer_name]], meta$index_path) <- ptr_missing_expr_symbol()
      next
    }
    resolved_expr <- ptr_resolve_placeholder_expr(spec, input_item, meta, context)

    expr_pluck(ptr_processed_expr_list[[meta$layer_name]], meta$index_path) <- resolved_expr
  }

  for (layer_name in names(data_pipeline_info)) {
    if (!layer_name %in% names(ptr_processed_expr_list)) next
    pipeline <- data_pipeline_info[[layer_name]]
    layer_expr <- ptr_processed_expr_list[[layer_name]]
    if (!is.call(layer_expr) ||
        pipeline$data_arg_index > length(layer_expr)) next
    data_expr <- layer_expr[[pipeline$data_arg_index]]
    effective <- ptr_trim_and_eval(
      data_expr, eval_env, ptr_missing_expr_symbol()
    )
    if (isTRUE(effective$ok) && !is.null(effective$expr)) {
      layer_expr[[pipeline$data_arg_index]] <- effective$expr
      ptr_processed_expr_list[[layer_name]] <- layer_expr
    }
  }

  layer_names <- names(ptr_processed_expr_list)
  for (nn in layer_names) {
    ptr_processed_expr_list[[nn]] <- prune_empty_substitution_artifacts(
      ptr_processed_expr_list[[nn]],
      original_expr_list[[nn]],
      remove_set
    )
  }
  ptr_processed_expr_list <- ptr_remove_empty_nonstandalone_layers(
    ptr_processed_expr_list,
    original_expr_list = original_expr_list,
    remove_set = remove_set
  )
  ptr_validate_layer_checkbox_inputs(ptr_obj, input, ns_fn = ns_fn)
  ptr_processed_expr_list <- purrr::map2(
    ptr_processed_expr_list,
    names(ptr_processed_expr_list),
    expr_apply_checkbox_result,
    input,
    ns_fn
  )
  ptr_processed_expr_list <- check_remove_null(ptr_processed_expr_list)

  code_text_list <- lapply(ptr_processed_expr_list, rlang::expr_text)
  pipe_chain_ops <- ptr_obj[["ggplot_pipe_chain_ops"]]
  if (is.null(pipe_chain_ops)) {
    pipe_op <- ptr_obj[["ggplot_pipe_op"]]
    pipe_chain_ops <- if (is.null(pipe_op)) character() else pipe_op
  }
  if (length(pipe_chain_ops) > 0L && "ggplot" %in% names(code_text_list)) {
    code_text_list[["ggplot"]] <- render_ggplot_with_pipe_chain(
      ptr_processed_expr_list[["ggplot"]],
      pipe_chain_ops
    )
  }

  formula_text <- ptr_obj[["formula_text"]] %||% ""
  data_arg_pipe_op <- if (length(pipe_chain_ops) > 0L) {
    pipe_chain_ops[length(pipe_chain_ops)]
  } else if (grepl("|>", formula_text, fixed = TRUE)) {
    "|>"
  } else if (grepl("%>%", formula_text, fixed = TRUE)) {
    "%>%"
  } else {
    NULL
  }
  if (!is.null(data_arg_pipe_op)) {
    for (layer_name in names(data_pipeline_info)) {
      if (layer_name == "ggplot" && length(pipe_chain_ops) > 0L) next
      if (!layer_name %in% names(ptr_processed_expr_list)) next
      pipeline <- data_pipeline_info[[layer_name]]
      layer_expr <- ptr_processed_expr_list[[layer_name]]
      if (is.null(layer_expr) || !is.call(layer_expr) ||
          pipeline$data_arg_index > length(layer_expr)) next
      data_expr <- layer_expr[[pipeline$data_arg_index]]
      depth <- expr_left_spine_depth(data_expr)
      if (depth == 0L) next
      pipe_text <- render_ggplot_with_pipe_chain(
        data_expr, rep(data_arg_pipe_op, depth)
      )
      bait <- "ptrDataArgBait_x7Qk"
      bait_expr <- layer_expr
      bait_expr[[pipeline$data_arg_index]] <- as.symbol(bait)
      layer_text <- rlang::expr_text(bait_expr)
      code_text_list[[layer_name]] <- gsub(
        bait, pipe_text, layer_text, fixed = TRUE
      )
    }
  }
  code_text <- do.call(paste, c(unname(code_text_list), sep = " +\n  "))

  eval_expr_list <- ptr_substitute_cached_data(
    ptr_processed_expr_list,
    ptr_obj[["data_pipeline_info"]],
    resolved_data
  )

  list(
    complete_expr_list = eval_expr_list,
    code_text = code_text,
    eval_env = eval_env
  )
}

#' Build a Plot from Completed Layer Expressions
#'
#' @param plot_expr_list A list of completed plot layer expressions.
#' @param envir The evaluation environment. Defaults to \code{parent.frame()},
#'   which is the caller's frame. When called from \code{ptr_exec}, a cloned
#'   runtime environment is passed explicitly. Direct callers should provide
#'   an explicit \code{envir} to avoid evaluating expressions in an
#'   unintended scope.
#' @param expr_check Logical or list controlling safety validation of the
#'   assembled expressions. Forwarded to \code{validate_expr_safety}.
#'   Defaults to \code{TRUE}.
#'
#' @return A `ggplot` object assembled from the retained layer expressions.
#'   Errors when no plot expressions remain after runtime processing.
#' @examples
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
ptr_assemble_plot <- function(plot_expr_list, envir = parent.frame(), expr_check = TRUE) {
  if (is.null(plot_expr_list) || length(plot_expr_list) == 0) {
    rlang::abort("No plot layers remain after processing the selected inputs.")
  }

  for (expr in plot_expr_list) {
    validate_expr_safety(expr, expr_check)
  }

  plot_list <- lapply(plot_expr_list, eval, envir = envir)

  p <- plot_list[[1]]
  if (length(plot_list) == 1) {
    return(p)
  }

  for (i in seq_along(plot_list)[-1]) {
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
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return A structured runtime result.
#' @noRd
ptr_complete_expr_safe <- function(ptr_obj, input, envir = parent.frame(),
  eval_env = NULL, var_column_map = NULL, expr_check = TRUE,
  ns_fn = shiny::NS(NULL), safe_to_remove = character(),
  shared_bindings = list(), resolved_data = NULL,
  last_click_inputs = NULL) {
  tryCatch(
    {
      complete_result <- ptr_complete_expr(ptr_obj, input, envir = envir,
                                           eval_env = eval_env,
                                           var_column_map = var_column_map,
                                           expr_check = expr_check,
                                           ns_fn = ns_fn,
                                           safe_to_remove = safe_to_remove,
                                           shared_bindings = shared_bindings,
                                           resolved_data = resolved_data,
                                           last_click_inputs = last_click_inputs)
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
#' @param expr_check Logical or list controlling safety validation.
#'   Forwarded to \code{ptr_assemble_plot}. Defaults to \code{TRUE}.
#'
#' @return An updated runtime result.
#' @noRd
ptr_assemble_plot_safe <- function(runtime_result, envir = parent.frame(), expr_check = TRUE) {
  if (!isTRUE(runtime_result$ok)) {
    return(runtime_result)
  }

  plot_env <- runtime_result$eval_env
  if (is.null(plot_env)) {
    plot_env <- envir
  }

  tryCatch(
    {
      runtime_result$plot <- ptr_assemble_plot(runtime_result$complete_expr_list, envir = plot_env, expr_check = expr_check)
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
#' @param expr_check Controls `expr` placeholder validation at runtime.
#'   `TRUE` (default) applies the built-in denylist of dangerous
#'   functions.
#'   `FALSE` disables all checking.
#'   A named list with `deny_list` and/or `allow_list` character
#'   vectors supplies a custom check; when both are given,
#'   denied entries are removed from the allowlist.
#'   Note: the formula template itself is separately validated at parse
#'   time via \code{formula_check} in \code{\link{ptr_parse_formula}}.
#'   Disabling \code{expr_check} here does not affect that earlier check,
#'   and vice versa.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Extends the curated default set: `theme()`, `labs()`,
#'   `xlab()`, `ylab()`, `ggtitle()`, `facet_wrap()`, `facet_grid()`,
#'   `facet_null()`, `xlim()`, `ylim()`, `lims()`, `expand_limits()`,
#'   `guides()`, `annotate()`, `annotation_custom()`, `annotation_map()`,
#'   `annotation_raster()`, `aes()`, `aes_()`, `aes_q()`, `aes_string()`,
#'   `vars()`, `element_text()`, `element_line()`, `element_rect()`,
#'   `element_point()`, `element_polygon()`, `element_geom()`. `geom_*()` /
#'   `stat_*()` standalone layers are always preserved. Defaults to `character()`.
#' @param resolved_data Optional named list of Shiny `reactiveVal`s, one per
#'   data-pipeline layer (see `ptr_obj$data_pipeline_info`), each holding the
#'   most recently cached data frame for that layer. When supplied, the
#'   layer's data argument is replaced with the cached frame before
#'   evaluation, so the data pipeline is not re-executed. Pass `NULL`
#'   (the default) to fall back to evaluating the live data expression.
#' @param last_click_inputs Optional named list of placeholder-input snapshots,
#'   one per data-pipeline layer, captured at the moment the layer's
#'   "Update data" button was last clicked. When supplied, the snapshotted
#'   values (rather than the live `input`) are substituted into the
#'   data-pipeline placeholders during code-text generation, so the
#'   displayed code matches the cached frame the plot was rendered against.
#'   Pass `NULL` (the default) to use the live `input` values.
#'
#' @return A runtime result list containing `ok`, `stage`, `message`,
#'   `code_text`, `complete_expr_list`, `eval_env`, `condition`, and `plot`.
#'   Completion-stage validation failures return `stage = "complete"`;
#'   plot-construction or render failures return `stage = "plot"`.
#' @examples
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
ptr_exec <- function(ptr_obj, input, envir = parent.frame(),
                     expr_check = TRUE,
                     safe_to_remove = character(),
                     resolved_data = NULL,
                     last_click_inputs = NULL) {
  runtime_result <- ptr_complete_expr_safe(
    ptr_obj, input, envir = envir, expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    resolved_data = resolved_data,
    last_click_inputs = last_click_inputs
  )
  runtime_result <- ptr_assemble_plot_safe(runtime_result, envir = envir, expr_check = expr_check)
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

# Visible hint that the rendered plot reflects an older "Update data" snapshot
# (i.e. the user typed new data-pipeline values but hasn't clicked Update Data
# yet). Without this, the plot keeps showing the cached frame and users have
# no signal that their typing was ignored.
ptr_stale_notice_ui <- function(stale_layers) {
  stale_layers <- stale_layers[nzchar(stale_layers)]
  if (length(stale_layers) == 0L) return(NULL)

  layers_text <- paste(stale_layers, collapse = ", ")
  shiny::tags$div(
    style = paste(
      "margin-top: 12px;",
      "margin-bottom: 12px;",
      "padding: 12px;",
      "border: 1px solid #b59300;",
      "border-radius: 4px;",
      "background-color: #fff8db;",
      "color: #6a4f00;"
    ),
    shiny::tags$strong("Unsaved data inputs"),
    shiny::tags$div(
      style = "margin-top: 6px;",
      sprintf(
        "Click \"Update data\" to apply your changes for %s.",
        layers_text
      )
    )
  )
}
