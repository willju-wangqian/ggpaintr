# P2 — classify-data. S3 visitor that walks the typed tree carrying a
# `ctx_data` pointer and stamps `upstream` onto every `ptr_ph_data_consumer`.
# `ptr_ph_data_source` ignores ctx_data (sources produce data).
# `ctx_data` is itself an AST subtree pointer; materializing it is P12's job.

ptr_classify_data <- function(node) {
  if (!is_ptr_root(node)) {
    rlang::abort("ptr_classify_data expects a ptr_root.")
  }
  classify_walk(node, ctx_data = NULL)
}

classify_walk <- function(node, ctx_data) UseMethod("classify_walk")

#' @export
classify_walk.ptr_root <- function(node, ctx_data) {
  ggplot_idx <- NA_integer_
  for (i in seq_along(node$layers)) {
    if (is_ptr_layer(node$layers[[i]]) && node$layers[[i]]$name == "ggplot") {
      ggplot_idx <- i
      break
    }
  }
  default_data <- ctx_data
  if (!is.na(ggplot_idx)) {
    node$layers[[ggplot_idx]] <- classify_walk(
      node$layers[[ggplot_idx]], ctx_data = ctx_data
    )
    layer_data <- node$layers[[ggplot_idx]]$data_arg
    if (!is.null(layer_data)) default_data <- layer_data
  }
  for (i in seq_along(node$layers)) {
    if (!is.na(ggplot_idx) && i == ggplot_idx) next
    node$layers[[i]] <- classify_walk(node$layers[[i]], ctx_data = default_data)
  }
  node
}

#' @export
classify_walk.ptr_layer <- function(node, ctx_data) {
  if (!is.null(node$data_arg)) {
    node$data_arg <- classify_walk(node$data_arg, ctx_data = ctx_data)
    children_ctx <- node$data_arg
  } else {
    children_ctx <- ctx_data
  }
  if (length(node$children) > 0L) {
    for (i in seq_along(node$children)) {
      node$children[[i]] <- classify_walk(
        node$children[[i]], ctx_data = children_ctx
      )
    }
  }
  node
}

#' @export
classify_walk.ptr_pipeline <- function(node, ctx_data) {
  if (length(node$stages) == 0L) return(node)
  for (i in seq_along(node$stages)) {
    if (i == 1L) {
      stage_ctx <- ctx_data
    } else if (i == 2L) {
      # Single prior stage — pass it through directly, not as a pipeline wrapper.
      stage_ctx <- node$stages[[1L]]
    } else {
      stage_ctx <- ptr_pipeline(
        stages = node$stages[seq_len(i - 1L)],
        op = node$op,
        expr = node$expr
      )
    }
    node$stages[[i]] <- classify_walk(node$stages[[i]], ctx_data = stage_ctx)
  }
  node
}

#' @export
classify_walk.ptr_call <- function(node, ctx_data) {
  if (length(node$args) > 0L) {
    for (i in seq_along(node$args)) {
      node$args[[i]] <- classify_walk(node$args[[i]], ctx_data = ctx_data)
    }
  }
  node
}

#' @export
classify_walk.ptr_ph_data_consumer <- function(node, ctx_data) {
  node$upstream <- ctx_data
  node
}

#' @export
classify_walk.ptr_ph_data_source <- function(node, ctx_data) {
  node$upstream <- NULL
  node
}

#' @export
classify_walk.ptr_ph_value <- function(node, ctx_data) node

#' @export
classify_walk.ptr_user_expr <- function(node, ctx_data) node

#' @export
classify_walk.ptr_literal <- function(node, ctx_data) node

#' @export
classify_walk.ptr_missing <- function(node, ctx_data) node

#' @export
classify_walk.default <- function(node, ctx_data) node
