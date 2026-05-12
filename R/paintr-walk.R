# Generic pre-order traversal of the typed AST.
#
# Almost every collector in the package needs the same recursion: visit a
# `ptr_node`, then descend into its child slots by name and into plain lists
# element-wise. The `upstream` slot is skipped by default — it is a
# back-pointer at a (possibly shared) subtree elsewhere in the tree, so
# recursing into it would double-count nodes or loop.
#
# `ptr_walk()` is that recursion; `ptr_collect()` is the "gather every node
# matching a predicate" wrapper built on top of it.

# Visit every node in `node` in pre-order. `visit(x)` is called for each
# `ptr_node`. `prune(x)` (if supplied) is checked after visiting; when it
# returns TRUE the node's children are not descended into. `descend_upstream`
# overrides the default skip of the `upstream` slot.
ptr_walk <- function(node, visit, descend_upstream = FALSE, prune = NULL) {
  rec <- function(x) {
    if (is_ptr_node(x)) {
      visit(x)
      if (!is.null(prune) && isTRUE(prune(x))) return(invisible())
      for (nm in names(x)) {
        if (!descend_upstream && identical(nm, "upstream")) next
        rec(x[[nm]])
      }
    } else if (is.list(x)) {
      for (el in x) rec(el)
    }
    invisible()
  }
  rec(node)
  invisible()
}

# Return every node where `pred(node)` is TRUE, in pre-order. `prune` (if
# supplied) stops descent below matching subtrees — pass `prune = pred` to
# collect only the topmost match in each branch.
ptr_collect <- function(node, pred, descend_upstream = FALSE, prune = NULL) {
  out <- list()
  ptr_walk(
    node,
    function(x) if (pred(x)) out[[length(out) + 1L]] <<- x,
    descend_upstream = descend_upstream,
    prune = prune
  )
  out
}
