#' Parse a Paintr Formula
#'
#' Parse a single ggplot-like formula string into a `ptr_obj` containing
#' expression metadata, placeholder locations, and generated UI definitions.
#'
#' Supported placeholders come from the effective placeholder registry. The
#' built-in registry includes `var`, `text`, `num`, `expr`, and `upload`.
#'
#' @section Column-name collisions with placeholder keywords:
#' Placeholder detection is purely syntactic: any bare symbol whose name
#' matches a registered keyword (`var`, `text`, `num`, `expr`, `upload`, or
#' any custom keyword) is consumed as a placeholder, regardless of whether
#' the active dataset has a column with the same name. As a consequence, if
#' your data has a column literally named `"var"`, writing the bare symbol
#' `var` in the formula will *not* reference that column — it will be
#' replaced by the user's `var` widget selection. Backtick quoting
#' (`` `var` ``) parses to the same R symbol and does not escape the
#' conflict. To reference such a column, use `.data[["var"]]`, which
#' represents the column name as a string literal (not a symbol) and is
#' therefore ignored by the placeholder walker. Alternatively, pick the
#' column from the `var` dropdown, which still lists every column of the
#' dataset (including one named `"var"`).
#'
#' @note The \code{formula} argument is validated by default using the denylist.
#'   Set \code{formula_check = FALSE} only for trusted developer input that you
#'   know is safe — this is an advanced option for package authors who
#'   programmatically generate formula strings.  This check is independent of
#'   the per-placeholder \code{expr_check} applied at runtime; disabling one
#'   does not disable the other.
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
  assertthat::assert_that(rlang::is_string(formula))
  if (!nzchar(trimws(formula))) {
    rlang::abort("ptr_parse_formula: formula must not be empty or whitespace-only.")
  }
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
             "expression, but ", length(ptr_exprs), " were found. ",
             "Hint: ggplot layers must be joined with `+` \u2014 check for a ",
             "missing `+` between layers or a stray newline that splits the ",
             "expression.")
    )
  }
  ptr_expr <- ptr_exprs[[1]]
  ptr_expr <- rewrite_magrittr_pipe(ptr_expr)
  ggplot_pipe_chain_ops <- detect_ggplot_pipe_chain(formula)
  ggplot_pipe_op <- if (length(ggplot_pipe_chain_ops) == 0L) {
    NULL
  } else {
    ggplot_pipe_chain_ops[length(ggplot_pipe_chain_ops)]
  }
  if (!identical(formula_check, FALSE)) {
    validate_expr_safety(
      ptr_expr,
      formula_check,
      placeholder_names = names(placeholder_registry)
    )
  }
  ptr_expr_list <- if (rlang::is_call(ptr_expr, "+")) {
    unlist(break_sum(ptr_expr))
  } else {
    list(ptr_expr)
  }
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

  non_ggplot_layers <- setdiff(names(ptr_expr_list), "ggplot")
  checkbox_id_list <- stats::setNames(
    vapply(non_ggplot_layers, ptr_checkbox_input_id, character(1)),
    non_ggplot_layers
  )

  data_pipeline_info <- ptr_compute_data_pipeline_info(
    ptr_expr_list,
    placeholder_map
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
    custom_placeholders = placeholder_registry$custom_placeholders,
    checkbox_id_list = checkbox_id_list,
    ggplot_pipe_op = ggplot_pipe_op,
    ggplot_pipe_chain_ops = ggplot_pipe_chain_ops,
    data_pipeline_info = data_pipeline_info
  )

  class(result) <- "ptr_obj"
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
      parsed <- parse_placeholder_token(layer_keywords[[j]])
      keyword <- parsed$keyword
      if (!(keyword %in% names(placeholders))) {
        next
      }

      layer_meta[[layer_ids[[j]]]] <- list(
        id = layer_ids[[j]],
        keyword = keyword,
        shared = parsed$shared,
        layer_name = layer_name,
        param = layer_params[[j]],
        index_path = layer_index_paths[[j]]
      )
    }

    layer_meta
  })

  rlang::set_names(placeholder_map, layer_names)
}
