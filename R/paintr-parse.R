#' Parse a Paintr Formula
#'
#' Parse a single ggplot-like formula string into a `paintr_obj` containing
#' expression metadata, placeholder locations, and generated UI definitions.
#'
#' Supported placeholders come from the effective placeholder registry. The
#' built-in registry includes `var`, `text`, `num`, `expr`, and `upload`.
#'
#' @param formula A single formula string describing a ggplot-like expression.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#'
#' @return An object of class `paintr_obj`.
#' @examples
#' obj <- paintr_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' names(obj$expr_list)
#' @export
paintr_formula <- function(formula, placeholders = NULL) {
  placeholder_registry <- ggpaintr_effective_placeholders(placeholders)
  paintr_expr <- rlang::parse_expr(formula)
  paintr_expr_list <- unlist(break_sum(paintr_expr))
  paintr_expr_names <- vapply(paintr_expr_list, get_fun_names, character(1))
  paintr_expr_names <- handle_duplicate_names(paintr_expr_names)
  paintr_expr_list <- rlang::set_names(paintr_expr_list, paintr_expr_names)

  index_path_list <- lapply(
    paintr_expr_list,
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
    paintr_expr_list,
    function(.path, .expr) {
      lapply(.path, function(.x, .exprr) expr_pluck(.exprr, .x), .exprr = .expr)
    }
  )

  paintr_expr_param_list <- purrr::map2(
    paintr_expr_list,
    index_path_list,
    function(.expr, .path_list) {
      lapply(.path_list, function(.path) get_expr_param(.expr, .path))
    }
  )

  placeholder_map <- paintr_build_placeholder_map(
    keywords_list = keywords_list,
    id_list = id_list,
    param_list = paintr_expr_param_list,
    index_path_list = index_path_list,
    placeholders = placeholder_registry
  )

  result <- list(
    formula_text = formula,
    param_list = paintr_expr_param_list,
    keywords_list = keywords_list,
    index_path_list = index_path_list,
    id_list = id_list,
    expr_list = paintr_expr_list,
    placeholder_map = placeholder_map,
    placeholders = placeholder_registry,
    custom_placeholders = placeholder_registry$custom_placeholders
  )

  class(result) <- "paintr_obj"
  result$ui_list <- paintr_build_ui_list(result)
  result
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
#' @keywords internal
paintr_build_placeholder_map <- function(keywords_list,
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
