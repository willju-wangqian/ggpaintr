#' Parse a Paintr Formula
#'
#' Parse a single ggplot-like formula string into a `paintr_obj` containing
#' expression metadata, placeholder locations, and generated UI definitions.
#'
#' Supported placeholders are `var`, `text`, `num`, `expr`, and `upload`.
#'
#' @param formula A single formula string describing a ggplot-like expression.
#'
#' @return An object of class `paintr_obj`.
#' @examples
#' obj <- paintr_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' names(obj$expr_list)
#' @export
paintr_formula <- function(formula) {
  paintr_expr <- rlang::parse_expr(formula)
  paintr_expr_list <- unlist(break_sum(paintr_expr))
  paintr_expr_names <- vapply(paintr_expr_list, get_fun_names, character(1))
  paintr_expr_names <- handle_duplicate_names(paintr_expr_names)
  paintr_expr_list <- rlang::set_names(paintr_expr_list, paintr_expr_names)

  index_path_list <- lapply(paintr_expr_list, get_index_path)
  id_list <- lapply(names(index_path_list), function(.nn) {
    lapply(index_path_list[[.nn]], encode_id, .nn)
  })
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

  paintr_ui_list <- purrr::pmap(
    list(keywords_list, id_list, paintr_expr_param_list),
    function(k_l, id_l, p_l) {
      purrr::pmap(list(k_l, id_l, p_l), generate_ui_individual)
    }
  )

  paintr_ui_list <- purrr::map2(paintr_ui_list, names(paintr_ui_list), ui_insert_checkbox)

  result <- list(
    formula_text = formula,
    param_list = paintr_expr_param_list,
    keywords_list = keywords_list,
    index_path_list = index_path_list,
    id_list = id_list,
    expr_list = paintr_expr_list,
    ui_list = paintr_ui_list
  )

  class(result) <- "paintr_obj"
  result
}

