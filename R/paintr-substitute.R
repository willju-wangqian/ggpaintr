# P8 — substitute. Walk the typed tree applying `input_snapshot` and
# `shared_bindings`. Each placeholder is replaced by a `ptr_literal`
# (resolved value), `ptr_user_expr` (expr-keyword provenance), or
# `ptr_missing` (input absent / empty / NA per built-in semantics).
# Layer activation: if the layer's `active_input_id` is in the snapshot
# and FALSE, the layer is marked `active = FALSE` for P9 to drop.

ptr_substitute <- function(node, input_snapshot = list(),
                          shared_bindings = list(),
                          eval_env = parent.frame(),
                          upstream_cols = list()) {
  ctx <- list(
    snapshot = input_snapshot,
    shared = shared_bindings,
    eval_env = eval_env,
    upstream_cols = upstream_cols
  )
  substitute_walk(node, ctx)
}

substitute_walk <- function(node, ctx) UseMethod("substitute_walk")

#' @export
substitute_walk.ptr_root <- function(node, ctx) {
  for (i in seq_along(node$layers)) {
    node$layers[[i]] <- substitute_walk(node$layers[[i]], ctx)
  }
  node
}

#' @export
substitute_walk.ptr_layer <- function(node, ctx) {
  if (!is.null(node$active_input_id)) {
    cb <- ctx$snapshot[[node$active_input_id]]
    node$active <- !isFALSE(cb)
  }
  if (!is.null(node$data_arg)) {
    node$data_arg <- substitute_walk(node$data_arg, ctx)
  }
  if (length(node$children) > 0L) {
    for (i in seq_along(node$children)) {
      node$children[[i]] <- substitute_walk(node$children[[i]], ctx)
    }
  }
  node
}

#' @export
substitute_walk.ptr_call <- function(node, ctx) {
  for (i in seq_along(node$args)) {
    node$args[[i]] <- substitute_walk(node$args[[i]], ctx)
  }
  node
}

#' @export
substitute_walk.ptr_pipeline <- function(node, ctx) {
  for (i in seq_along(node$stages)) {
    node$stages[[i]] <- substitute_walk(node$stages[[i]], ctx)
  }
  node
}

#' @export
substitute_walk.ptr_user_expr <- function(node, ctx) node

#' @export
substitute_walk.ptr_literal <- function(node, ctx) node

#' @export
substitute_walk.ptr_missing <- function(node, ctx) node

#' @export
substitute_walk.ptr_ph_value <- function(node, ctx) {
  value <- read_placeholder_value(node, ctx)
  if (is_missing_value_input(node, value)) return(ptr_missing())
  entry <- ptr_registry_lookup(node$keyword)
  if (is.null(entry)) {
    rlang::abort(paste0("No registry entry for placeholder keyword `", node$keyword, "`."))
  }
  resolved <- entry$resolve_expr(value, node)
  if (is.null(resolved)) return(ptr_missing())
  validate_resolve_expr_return(resolved, node$keyword)
  if (node$keyword == "expr") {
    return(ptr_user_expr(resolved))
  }
  ptr_literal(resolved)
}

#' @export
substitute_walk.ptr_ph_data_consumer <- function(node, ctx) {
  value <- read_placeholder_value(node, ctx)
  if (is_missing_value_input(node, value)) return(ptr_missing())
  if (length(value) > 1L) {
    rlang::abort(paste0(
      "Placeholder `", node$keyword, "` (id=", node$id %||% "?",
      ") got multiple values; expected exactly one."
    ))
  }
  entry <- ptr_registry_lookup(node$keyword)
  if (!is.null(entry$validate_input)) {
    cols <- ctx$upstream_cols[[node$id %||% ""]]
    if (!is.null(cols)) {
      ok <- entry$validate_input(value, cols)
      if (!isTRUE(ok)) {
        rlang::abort(paste0(
          "Invalid value for placeholder `", node$keyword, "`: ",
          if (is.character(ok)) ok else "validation failed."
        ))
      }
    }
  }
  resolved <- entry$resolve_expr(value, node)
  if (is.null(resolved)) return(ptr_missing())
  validate_resolve_expr_return(resolved, node$keyword)
  ptr_literal(resolved)
}

#' @export
substitute_walk.ptr_ph_data_source <- function(node, ctx) {
  name_value <- if (!is.null(node$companion_id)) {
    ctx$snapshot[[node$companion_id]]
  } else {
    NULL
  }
  if (is.null(name_value) || !is.character(name_value) ||
      length(name_value) != 1L || !nzchar(name_value)) {
    return(ptr_missing())
  }
  if (make.names(name_value) != name_value) {
    rlang::abort(paste0(
      "Upload name `", name_value, "` is not a valid R name. ",
      "Only valid R identifiers are accepted."
    ))
  }
  entry <- ptr_registry_lookup(node$keyword)
  resolved <- if (!is.null(entry) && !is.null(entry$resolve_expr)) {
    entry$resolve_expr(name_value, node)
  } else {
    rlang::sym(name_value)
  }
  validate_resolve_expr_return(resolved, node$keyword)
  ptr_literal(resolved)
}

#' @export
substitute_walk.default <- function(node, ctx) node

# ---- helpers ---------------------------------------------------------------

read_placeholder_value <- function(node, ctx) {
  if (!is.null(node$shared)) {
    rv <- ctx$shared[[node$shared]]
    if (is.null(rv)) return(NULL)
    if (is.function(rv)) {
      return(tryCatch(rv(), error = function(e) NULL))
    }
    return(rv)
  }
  ctx$snapshot[[node$id %||% ""]]
}

is_missing_value_input <- function(node, value) {
  if (is.null(value)) return(TRUE)
  if (length(value) == 0L) return(TRUE)
  if (node$keyword == "num") {
    if (is.numeric(value) && all(is.na(value))) return(TRUE)
    if (is.character(value) && (length(value) == 0L || !nzchar(value[[1]]))) return(TRUE)
  }
  if (node$keyword == "expr") {
    if (is.character(value) && (length(value) == 0L || !nzchar(value[[1]]))) return(TRUE)
  }
  FALSE
}

validate_resolve_expr_return <- function(x, keyword) {
  ok <- is.null(x) ||
    is.numeric(x) || is.character(x) || is.logical(x) || is.integer(x) ||
    is.language(x) || is.symbol(x) || is.expression(x)
  if (!ok) {
    rlang::abort(paste0(
      "resolve_expr for `", keyword, "` returned an unsupported type: ",
      class(x)[1L]
    ))
  }
}
