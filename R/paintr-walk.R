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

# ---- cursor-threaded top-down rewrite -------------------------------------
#
# Placeholder ids and stage ids are location-derived. Rather than have each
# annotating pass hand-thread "where am I in the tree, which layer am I under,
# am I in data-arg position", `ptr_rewrite_pre()` computes that once and hands
# it to the callback as a cursor.
#
# The cursor is a plain list; fields are populated as known at each location.
#   layer_name        nearest enclosing layer's name (or a placeholder-layer's keyword)
#   path              integer index path relative to the enclosing layer
#                     (empty for a node sitting directly in `data_arg`)
#   slot              which container slot this node hangs off its parent by
#                     ("layers" | "data_arg" | "children" | "args" | "stages")
#   param             nearest enclosing argument NAME, or NA
#   in_data_position  TRUE iff this node sits where pipeline data flows
#   pipeline_index    this node's stage index, if it is a pipeline stage
ptr_cursor <- function(layer_name = NA_character_,
                       path = integer(),
                       slot = NA_character_,
                       param = NA_character_,
                       in_data_position = FALSE,
                       pipeline_index = NA_integer_) {
  list(layer_name = layer_name, path = path, slot = slot, param = param,
       in_data_position = in_data_position, pipeline_index = pipeline_index)
}

# Derive the child cursor from the parent cursor + (slot, 1-based index within
# the slot, arg name). Encodes the per-container update rules — this is the one
# place those rules live. `parent` is the (typed) parent node.
ptr_cursor_descend <- function(cur, parent, slot, idx, nm) {
  nm_or_na <- if (!is.null(nm) && !is.na(nm) && nzchar(nm)) nm else NA_character_
  if (identical(slot, "layers")) {
    child <- parent$layers[[idx]]
    ln <- if (is_ptr_placeholder(child)) child$keyword else child$name
    return(ptr_cursor(layer_name = ln, path = integer(), slot = slot))
  }
  if (identical(slot, "data_arg")) {
    return(ptr_cursor(layer_name = cur$layer_name, path = integer(), slot = slot,
                      param = "data", in_data_position = TRUE))
  }
  if (identical(slot, "children")) {
    return(ptr_cursor(layer_name = cur$layer_name, path = idx, slot = slot,
                      param = nm_or_na))
  }
  if (identical(slot, "args")) {
    pos <- data_arg_position(parent)
    return(ptr_cursor(layer_name = cur$layer_name, path = c(cur$path, idx), slot = slot,
                      param = nm_or_na,
                      in_data_position = isTRUE(cur$in_data_position) && identical(idx, pos)))
  }
  if (identical(slot, "stages")) {
    return(ptr_cursor(layer_name = cur$layer_name, path = c(cur$path, idx), slot = slot,
                      param = cur$param,
                      in_data_position = isTRUE(cur$in_data_position) && idx == 1L,
                      pipeline_index = idx))
  }
  ptr_cursor(layer_name = cur$layer_name)            # defensive
}

# Top-down rewrite, cursor-threaded. `fn(node, cur)` is called on every
# `ptr_node` BEFORE its children and must return a `ptr_node` (usually the same
# one, possibly with fields stamped). The `upstream` slot is never descended
# (same rule as `ptr_walk()`); the five container shapes are hardcoded.
ptr_rewrite_pre <- function(node, fn, cur = ptr_cursor()) {
  if (!is_ptr_node(node)) {
    if (is.list(node)) return(lapply(node, ptr_rewrite_pre, fn = fn, cur = cur))
    return(node)
  }
  node <- fn(node, cur)
  if (!is_ptr_node(node)) return(node)        # fn may have replaced it with a leaf

  desc <- function(child, slot, idx, nm) {
    ptr_rewrite_pre(child, fn, ptr_cursor_descend(cur, node, slot, idx, nm))
  }

  if (is_ptr_root(node)) {
    for (i in seq_along(node$layers)) {
      node$layers[[i]] <- desc(node$layers[[i]], "layers", i, NA_character_)
    }
  } else if (is_ptr_layer(node)) {
    if (!is.null(node$data_arg)) {
      node$data_arg <- desc(node$data_arg, "data_arg", NA_integer_, NA_character_)
    }
    nms <- names(node$children) %||% rep_len("", length(node$children))
    for (i in seq_along(node$children)) {
      node$children[[i]] <- desc(node$children[[i]], "children", i, nms[i])
    }
  } else if (is_ptr_call(node)) {
    nms <- names(node$args) %||% rep_len("", length(node$args))
    for (i in seq_along(node$args)) {
      node$args[[i]] <- desc(node$args[[i]], "args", i, nms[i])
    }
  } else if (is_ptr_pipeline(node)) {
    for (i in seq_along(node$stages)) {
      node$stages[[i]] <- desc(node$stages[[i]], "stages", i, NA_character_)
    }
  }
  # placeholders / ptr_literal / ptr_missing / ptr_user_expr: no structural
  # children to descend.
  node
}

# ---- tree validators ------------------------------------------------------
#
# Cheap O(nodes) post-condition checks at pass boundaries. They are additional
# guards, not replacements for the existing checks: when a pass-order mistake or
# a future change leaves a placeholder / `ptr_missing` in a tree handed to
# `ptr_eval`, these turn a cryptic error from deep in `eval()`/`ggplot2` into a
# local, named AST error — which `ptr_assemble_plot_safe()`'s `tryCatch` then
# surfaces as a clean inline error.

# After `ptr_assign_ids`: every placeholder has a non-NA id.
ptr_assert_ids_assigned <- function(tree) {
  bad <- ptr_collect(tree, function(n) {
    is_ptr_placeholder(n) && (is.null(n$id) || is.na(n$id))
  })
  if (length(bad)) {
    rlang::abort(paste0(
      "Internal: ", length(bad),
      " placeholder(s) without an id reached a pass that needs one (keyword(s): ",
      paste(unique(vapply(bad, `[[`, character(1), "keyword")), collapse = ", "),
      ")."
    ))
  }
  invisible(tree)
}

# After `ptr_classify_data`: a data-source placeholder produces data, so it
# never carries an `upstream`. (We can't symmetrically assert that every
# consumer has a non-NULL `upstream`: `classify_walk` sets it to the enclosing
# data context, which is legitimately NULL when no data source is in scope —
# e.g. `ggplot(aes(x = var()))` — and an unset list element is NULL in R, so
# "ran but ctx was NULL" is indistinguishable from "didn't run".)
ptr_assert_classified <- function(tree) {
  src <- ptr_collect(tree, function(n) {
    is_ptr_ph_data_source(n) && !is.null(n$upstream)
  })
  if (length(src)) {
    rlang::abort(
      "Internal: a data-source placeholder has a non-NULL `upstream` (sources produce data)."
    )
  }
  invisible(tree)
}

# Before `ptr_eval` / `ptr_render`: substitution must have removed every placeholder.
ptr_assert_no_placeholders <- function(tree) {
  bad <- ptr_collect(tree, is_ptr_placeholder)
  if (length(bad)) {
    rlang::abort(paste0(
      "Internal error: ", length(bad),
      " unresolved placeholder(s) reached evaluation (keyword(s): ",
      paste(unique(vapply(bad, `[[`, character(1), "keyword")), collapse = ", "),
      "). Please report this at https://github.com/willju-wangqian/ggpaintr/issues."
    ))
  }
  invisible(tree)
}

# After `ptr_prune`, before `ptr_eval`: no `ptr_missing` should remain.
ptr_assert_no_missing <- function(tree) {
  bad <- ptr_collect(tree, is_ptr_missing)
  if (length(bad)) {
    rlang::abort(paste0(
      "Internal: ", length(bad), " `ptr_missing` node(s) survived pruning."
    ))
  }
  invisible(tree)
}

# Test-only / debug-flag-only (NOT the hot path): no node id appears twice when
# descending WITH `upstream` — guards a future bug where `classify_walk` points
# an `upstream` at a descendant of the consumer, creating a cycle.
ptr_assert_acyclic <- function(tree) {
  seen <- new.env(parent = emptyenv())
  ptr_walk(tree, descend_upstream = TRUE, visit = function(n) {
    id <- n$id
    if (!is.null(id) && !is.na(id)) {
      if (!is.null(seen[[id]])) {
        rlang::abort(paste0("Internal: node id `", id, "` visited twice — cyclic `upstream`?"))
      }
      seen[[id]] <- TRUE
    }
  })
  invisible(tree)
}
