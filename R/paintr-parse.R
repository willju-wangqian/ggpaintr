#' Parse a Paintr Formula
#'
#' Parse a single ggplot-like formula string into a `ptr_obj` containing
#' expression metadata, placeholder locations, and generated UI definitions.
#'
#' Supported placeholders come from the effective placeholder registry. The
#' built-in registry includes `var`, `text`, `num`, `expr`, and `upload`.
#'
#' @note The \code{formula} argument is validated by default using the denylist.
#'   Set \code{formula_check = FALSE} only for trusted developer input that you
#'   know is safe.
#' @param formula A single formula string describing a ggplot-like expression.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param formula_check Logical or list controlling safety validation of the
#'   formula text itself. \code{TRUE} (default) applies the default denylist.
#'   \code{FALSE} skips validation, treating the formula as trusted developer
#'   input. A list with \code{deny_list} / \code{allow_list} customises the
#'   check. See \code{validate_expr_safety} details.
#'
#' @return An object of class `ptr_obj`.
#' @examples
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' names(obj$expr_list)
#' @export
ptr_parse_formula <- function(formula, placeholders = NULL, formula_check = TRUE) {
  placeholder_registry <- ptr_merge_placeholders(placeholders)
  ptr_exprs <- tryCatch(
    rlang::parse_exprs(formula),
    error = function(e) {
      rlang::abort(
        paste0("ptr_parse_formula: could not parse formula as R expression: ",
               conditionMessage(e)),
        parent = e
      )
    }
  )
  if (length(ptr_exprs) != 1L) {
    rlang::abort(
      paste0("ptr_parse_formula: formula must contain exactly one top-level ",
             "expression, but ", length(ptr_exprs), " were found.")
    )
  }
  ptr_expr <- ptr_exprs[[1]]
  if (!identical(formula_check, FALSE)) {
    validate_expr_safety(ptr_expr, formula_check)
  }
  ptr_expr_list <- unlist(break_sum(ptr_expr))
  ptr_expr_names <- vapply(ptr_expr_list, get_fun_names, character(1))
  ptr_expr_names <- handle_duplicate_names(ptr_expr_names)
  ptr_expr_list <- rlang::set_names(ptr_expr_list, ptr_expr_names)

  index_path_list <- lapply(
    ptr_expr_list,
    get_index_path,
    target = names(placeholder_registry)
  )
  id_list <- lapply(names(index_path_list), function(.nn) {
    lapply(index_path_list[[.nn]], encode_id, .nn)
  })
  id_list <- rlang::set_names(id_list, names(index_path_list))
  index_path_list <- purrr::map2(index_path_list, id_list, rlang::set_names)

  keywords_list <- purrr::map2(
    index_path_list,
    ptr_expr_list,
    function(.path, .expr) {
      lapply(.path, function(.x, .exprr) expr_pluck(.exprr, .x), .exprr = .expr)
    }
  )

  ptr_expr_param_list <- purrr::map2(
    ptr_expr_list,
    index_path_list,
    function(.expr, .path_list) {
      lapply(.path_list, function(.path) get_expr_param(.expr, .path))
    }
  )

  placeholder_map <- ptr_build_placeholder_map(
    keywords_list = keywords_list,
    id_list = id_list,
    param_list = ptr_expr_param_list,
    index_path_list = index_path_list,
    placeholders = placeholder_registry
  )

  result <- list(
    formula_text = formula,
    param_list = ptr_expr_param_list,
    keywords_list = keywords_list,
    index_path_list = index_path_list,
    id_list = id_list,
    expr_list = ptr_expr_list,
    placeholder_map = placeholder_map,
    placeholders = placeholder_registry,
    custom_placeholders = placeholder_registry$custom_placeholders
  )

  class(result) <- "ptr_obj"
  result$ui_list <- ptr_build_ui_list(result)
  result
}

#' Describe the Runtime Inputs for a Parsed Formula
#'
#' Return the input ids consumed by `ptr_exec()` in a stable,
#' documented data frame. This is the supported low-level discovery helper for
#' tests, tooling, and advanced package authors who need to construct an input
#' list programmatically without hard-coding internal ids in documentation or
#' downstream code.
#'
#' The returned rows follow the current UI/runtime order:
#'
#' - placeholder inputs in parsed layer order
#' - derived upload dataset-name inputs immediately after each `upload`
#'   placeholder row
#' - layer checkbox inputs for every non-`ggplot` layer, appended in layer order
#'
#' Raw ids such as `"ggplot+3+2"` still appear in the returned data, but those
#' ids remain implementation details. Call this helper instead of relying on the
#' exact encoding scheme directly.
#'
#' @param ptr_obj A `ptr_obj`.
#'
#' @return A base `data.frame` with columns `input_id`, `role`, `layer_name`,
#'   `keyword`, `param_key`, and `source_id`.
#' @examples
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + labs(title = text)"
#' )
#' ptr_runtime_input_spec(obj)
#' @export
ptr_runtime_input_spec <- function(ptr_obj) {
  assertthat::assert_that(inherits(ptr_obj, "ptr_obj"))

  new_spec_row <- function(input_id,
                           role,
                           layer_name,
                           keyword = NA_character_,
                           param_key = NA_character_,
                           source_id = NA_character_) {
    data.frame(
      input_id = input_id,
      role = role,
      layer_name = layer_name,
      keyword = keyword,
      param_key = param_key,
      source_id = source_id,
      stringsAsFactors = FALSE
    )
  }

  layer_names <- names(ptr_obj$expr_list)
  spec_rows <- list()
  idx <- 0L

  for (layer_name in layer_names) {
    layer_meta <- ptr_obj$placeholder_map[[layer_name]]
    if (length(layer_meta) == 0) {
      next
    }

    for (meta in unname(layer_meta)) {
      param_key <- ptr_normalize_param_key(meta$param)

      idx <- idx + 1L
      spec_rows[[idx]] <- new_spec_row(
        input_id = meta$id,
        role = "placeholder",
        layer_name = meta$layer_name,
        keyword = meta$keyword,
        param_key = param_key,
        source_id = meta$id
      )

      if (identical(meta$keyword, "upload")) {
        idx <- idx + 1L
        spec_rows[[idx]] <- new_spec_row(
          input_id = ptr_upload_name_id(meta$id),
          role = "upload_name",
          layer_name = meta$layer_name,
          keyword = meta$keyword,
          param_key = param_key,
          source_id = meta$id
        )
      }
    }
  }

  for (layer_name in setdiff(layer_names, "ggplot")) {
    idx <- idx + 1L
    spec_rows[[idx]] <- new_spec_row(
      input_id = ptr_checkbox_input_id(layer_name),
      role = "layer_checkbox",
      layer_name = layer_name
    )
  }

  spec_rows <- spec_rows[seq_len(idx)]

  if (length(spec_rows) == 0) {
    return(new_spec_row(
      input_id = character(0),
      role = character(0),
      layer_name = character(0),
      keyword = character(0),
      param_key = character(0),
      source_id = character(0)
    ))
  }

  do.call(rbind, spec_rows)
}

#' Build Placeholder Metadata for a Parsed Formula
#'
#' @param keywords_list A parsed keyword list by layer.
#' @param id_list A parsed id list by layer.
#' @param param_list A parsed parameter list by layer.
#' @param index_path_list A parsed index-path list by layer.
#' @param placeholders An effective placeholder registry.
#'
#' @return A named list of placeholder metadata records by layer.
#' @noRd
ptr_build_placeholder_map <- function(keywords_list,
                                         id_list,
                                         param_list,
                                         index_path_list,
                                         placeholders) {
  layer_names <- names(keywords_list)

  placeholder_map <- lapply(seq_along(layer_names), function(i) {
    layer_name <- layer_names[[i]]
    layer_keywords <- keywords_list[[i]]
    layer_ids <- id_list[[i]]
    layer_params <- param_list[[i]]
    layer_index_paths <- index_path_list[[i]]
    layer_meta <- list()

    for (j in seq_along(layer_ids)) {
      keyword <- rlang::as_string(layer_keywords[[j]])
      if (!(keyword %in% names(placeholders))) {
        next
      }

      layer_meta[[layer_ids[[j]]]] <- list(
        id = layer_ids[[j]],
        keyword = keyword,
        layer_name = layer_name,
        param = layer_params[[j]],
        index_path = layer_index_paths[[j]]
      )
    }

    layer_meta
  })

  rlang::set_names(placeholder_map, layer_names)
}
