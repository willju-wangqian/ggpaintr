# P9 — prune. One S3 visitor that replaces the legacy three pruners
# (`prune_empty_substitution_artifacts`, `ptr_remove_empty_nonstandalone_layers`,
# `check_remove_null`).
#
# Rules per node class:
#   - ptr_user_expr: identity. Never recurse, never drop (G5).
#   - ptr_ph_*: not yet substituted -> ptr_missing().
#   - ptr_call: prune args; named missing dropped, positional missing
#     escalates the call (operators are calls — covers G2). After arg
#     filtering, if call has 0 args AND name in remove_set AND NOT
#     standalone-eligible -> escalate to ptr_missing().
#   - ptr_pipeline: prune each stage; drop missing stages; collapse 1-stage,
#     0-stage -> missing.
#   - ptr_layer: prune children; if active = FALSE -> NULL; if all-empty +
#     name in remove_set + NOT standalone -> NULL.
#   - ptr_root: prune layers, drop NULLs.

ptr_prune <- function(node, safe_to_remove = NULL,
                      is_standalone = NULL) {
  remove_set <- unique(c(default_drop_when_empty(),
                         validate_safe_to_remove(safe_to_remove)))
  is_standalone <- is_standalone %||% ptr_default_is_standalone
  if (!is.function(is_standalone)) {
    rlang::abort("`is_standalone` must be a function (name -> logical) or NULL.")
  }
  prune_walk(node, remove_set = remove_set, is_standalone = is_standalone)
}

ptr_default_is_standalone <- function(name) {
  if (!is.character(name) || length(name) != 1L) return(FALSE)
  startsWith(name, "geom_") || startsWith(name, "stat_")
}

prune_walk <- function(node, remove_set, is_standalone) {
  UseMethod("prune_walk")
}

#' @export
prune_walk.ptr_root <- function(node, remove_set, is_standalone) {
  out <- list()
  for (layer in node$layers) {
    pruned <- prune_walk(layer, remove_set, is_standalone)
    if (!is.null(pruned) && !is_ptr_missing(pruned)) {
      out[[length(out) + 1L]] <- pruned
    }
  }
  node$layers <- out
  node
}

#' @export
prune_walk.ptr_layer <- function(node, remove_set, is_standalone) {
  if (isFALSE(node$active)) return(NULL)
  if (!is.null(node$data_arg)) {
    pruned_da <- prune_walk(node$data_arg, remove_set, is_standalone)
    node$data_arg <- if (is_ptr_missing(pruned_da)) NULL else pruned_da
  }
  if (length(node$children) > 0L) {
    arg_names <- names(node$children) %||% rep_len("", length(node$children))
    new_children <- list()
    new_names <- character()
    for (i in seq_along(node$children)) {
      pruned <- prune_walk(node$children[[i]], remove_set, is_standalone)
      if (is_ptr_missing(pruned)) next
      new_children[[length(new_children) + 1L]] <- pruned
      new_names <- c(new_names, arg_names[i])
    }
    names(new_children) <- new_names
    node$children <- new_children
  }
  empty <- length(node$children) == 0L && is.null(node$data_arg)
  if (empty && node$name %in% remove_set && !is_standalone(node$name)) {
    return(NULL)
  }
  node
}

#' @export
prune_walk.ptr_call <- function(node, remove_set, is_standalone) {
  arg_names <- names(node$args) %||% rep_len("", length(node$args))
  pruned_args <- lapply(node$args, prune_walk,
                        remove_set = remove_set,
                        is_standalone = is_standalone)
  bare <- bare_call_name(node$fun)
  if (!is.null(bare) && bare %in% pruneable_operator_names) {
    if (any(vapply(pruned_args, is_ptr_missing, logical(1)))) {
      return(ptr_missing())
    }
    names(pruned_args) <- arg_names
    node$args <- pruned_args
    return(node)
  }
  keep <- !vapply(pruned_args, is_ptr_missing, logical(1))
  new_args <- pruned_args[keep]
  names(new_args) <- arg_names[keep]
  node$args <- new_args
  if (length(new_args) == 0L && !is.null(bare) &&
      bare %in% remove_set && !is_standalone(bare)) {
    return(ptr_missing())
  }
  node
}

#' @export
prune_walk.ptr_pipeline <- function(node, remove_set, is_standalone) {
  out <- list()
  for (s in node$stages) {
    pruned <- prune_walk(s, remove_set, is_standalone)
    if (!is_ptr_missing(pruned)) out[[length(out) + 1L]] <- pruned
  }
  if (length(out) == 0L) return(ptr_missing())
  if (length(out) == 1L) return(out[[1L]])
  node$stages <- out
  node
}

#' @export
prune_walk.ptr_user_expr <- function(node, remove_set, is_standalone) node

#' @export
prune_walk.ptr_literal <- function(node, remove_set, is_standalone) node

#' @export
prune_walk.ptr_missing <- function(node, remove_set, is_standalone) node

#' @export
prune_walk.ptr_ph_value <- function(node, remove_set, is_standalone) ptr_missing()

#' @export
prune_walk.ptr_ph_data_consumer <- function(node, remove_set, is_standalone) {
  ptr_missing()
}

#' @export
prune_walk.ptr_ph_data_source <- function(node, remove_set, is_standalone) {
  ptr_missing()
}

#' @export
prune_walk.default <- function(node, remove_set, is_standalone) node

# ---- helpers ---------------------------------------------------------------

bare_call_name <- function(fun) {
  if (is.symbol(fun)) return(as.character(fun))
  if (is.call(fun)) {
    op <- fun[[1L]]
    if (identical(op, quote(`::`)) || identical(op, quote(`:::`))) {
      return(as.character(fun[[3L]]))
    }
    if (identical(op, quote(`(`))) {
      return(bare_call_name(fun[[2L]]))
    }
  }
  NULL
}
