# Helpers for the rewrite test suite (test-rewrite-*.R).

# Walk the typed tree, returning all nodes for which `pred(node)` is TRUE.
# `$upstream` is a metadata pointer at a (possibly shared) subtree elsewhere
# in the AST; recursing into it would double-count nodes. Skip it.
.find_skip_fields <- c("upstream")

find_nodes <- function(node, pred) {
  out <- list()
  visit <- function(x) {
    if (is_ptr_node(x)) {
      if (pred(x)) out[[length(out) + 1L]] <<- x
      nms <- names(x)
      for (nm in nms) {
        if (nm %in% .find_skip_fields) next
        visit(x[[nm]])
      }
    } else if (is.list(x)) {
      for (el in x) visit(el)
    }
  }
  visit(node)
  out
}
