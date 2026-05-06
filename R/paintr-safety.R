# P5 — safety. Visitor over the typed tree. For Phase 1 each visit delegates
# to the existing language-level walker (`validate_expr_safety` in
# paintr-utils.R) on the node's `expr` field. Placeholder annotation calls
# (`var(shared = "x")` and friends) are skipped via `placeholder_names`.
# The denylist + `resolve_expr_check` live in paintr-utils.R during
# cohabitation; the cutover phase moves them into this file.

ptr_validate_tree_safety <- function(node, expr_check = TRUE) {
  resolved <- resolve_expr_check(expr_check)
  if (resolved$mode == "off") {
    return(invisible(TRUE))
  }
  ph_names <- ptr_registry_v2_keywords()
  walk_ptr_safety(node, expr_check = expr_check, ph_names = ph_names)
}

walk_ptr_safety <- function(node, expr_check, ph_names) {
  UseMethod("walk_ptr_safety")
}

#' @export
walk_ptr_safety.ptr_root <- function(node, expr_check, ph_names) {
  for (layer in node$layers) {
    walk_ptr_safety(layer, expr_check = expr_check, ph_names = ph_names)
  }
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_layer <- function(node, expr_check, ph_names) {
  validate_expr_safety(node$expr, expr_check = expr_check,
                       placeholder_names = ph_names)
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_pipeline <- function(node, expr_check, ph_names) {
  validate_expr_safety(node$expr, expr_check = expr_check,
                       placeholder_names = ph_names)
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_call <- function(node, expr_check, ph_names) {
  validate_expr_safety(node$expr, expr_check = expr_check,
                       placeholder_names = ph_names)
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_literal <- function(node, expr_check, ph_names) {
  validate_expr_safety(node$expr, expr_check = expr_check,
                       placeholder_names = ph_names)
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_user_expr <- function(node, expr_check, ph_names) {
  validate_expr_safety(node$inner, expr_check = expr_check,
                       placeholder_names = ph_names)
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_ph_value <- function(node, expr_check, ph_names) {
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_ph_data_consumer <- function(node, expr_check, ph_names) {
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_ph_data_source <- function(node, expr_check, ph_names) {
  invisible(TRUE)
}

#' @export
walk_ptr_safety.ptr_missing <- function(node, expr_check, ph_names) {
  invisible(TRUE)
}

#' @export
walk_ptr_safety.default <- function(node, expr_check, ph_names) {
  rlang::abort(paste0(
    "ptr_validate_tree_safety: unknown node class ",
    paste(class(node), collapse = "/")
  ))
}
