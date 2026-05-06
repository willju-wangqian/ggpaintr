# P3 — shared-binding. Collect every placeholder node carrying a non-NULL
# `shared` field; group by key; assign one canonical id per group; rewrite
# every member's `id` to point at it. Bare-symbol placeholders carry
# `shared = NULL` and pass through unchanged.

ptr_shared_bind <- function(node) {
  if (!is_ptr_root(node)) {
    rlang::abort("ptr_shared_bind expects a ptr_root.")
  }
  collect_then_rewrite_shared(node)
}

collect_then_rewrite_shared <- function(root) {
  groups <- list()
  visit_collect <- function(x) {
    if (is_ptr_placeholder(x) && !is.null(x$shared)) {
      key <- x$shared
      groups[[key]] <<- c(groups[[key]] %||% character(), x$id)
    }
    if (is_ptr_node(x)) {
      for (nm in names(x)) visit_collect(x[[nm]])
    } else if (is.list(x)) {
      for (el in x) visit_collect(el)
    }
  }
  visit_collect(root)
  if (length(groups) == 0L) return(root)

  canonical <- vapply(names(groups), canonical_shared_id, character(1))
  names(canonical) <- names(groups)

  rewrite_shared(root, canonical)
}

canonical_shared_id <- function(key) paste0("shared_", key)

rewrite_shared <- function(x, canonical) {
  if (is_ptr_placeholder(x) && !is.null(x$shared)) {
    new_id <- canonical[[x$shared]]
    if (!is.null(new_id)) x$id <- new_id
    return(x)
  }
  if (is_ptr_node(x)) {
    for (nm in names(x)) x[[nm]] <- rewrite_shared(x[[nm]], canonical)
    return(x)
  }
  if (is.list(x)) {
    for (i in seq_along(x)) x[[i]] <- rewrite_shared(x[[i]], canonical)
    return(x)
  }
  x
}
