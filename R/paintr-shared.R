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


# ---- shared-consumer upstream resolution -----------------------------------
#
# Per the host contract (plan 06 follow-up): the shared widget for a
# `var(shared = "<key>")` placeholder lives at host scope (single-plot
# sidebar or grid top panel). To populate its picker we need ONE upstream
# per key, derived from possibly multiple per-layer occurrences.
#
# Algorithm:
#   1. For each occurrence, compute its "shared upstream" by truncating
#      the raw upstream chain at the first stage (in evaluation order)
#      that contains any placeholder. Keep the prefix from the source
#      side, exclusive of that stage. This naturally handles same-key
#      self-reference (the inner occurrence's upstream is cut off so
#      it never feeds the outer one).
#   2. Compare across occurrences:
#        - source datasets differ                  -> error
#        - sources match, all upstreams identical  -> use the upstream
#        - sources match, upstreams differ         -> use the source
#
# Returned shape from `resolve_shared_consumer`:
#   list(kind = "upstream"|"source"|"error",
#        value = <ptr_node or NULL>,
#        error = <chr or NULL>)

# TRUE if `n` (or any descendant, modulo the `upstream` back-pointer) is
# a placeholder node.
contains_placeholder <- function(n) {
  found <- FALSE
  visit <- function(x) {
    if (found || is.null(x)) return()
    if (is_ptr_placeholder(x)) { found <<- TRUE; return() }
    if (is_ptr_node(x)) {
      for (nm in names(x)) {
        if (identical(nm, "upstream")) next
        visit(x[[nm]])
      }
    } else if (is.list(x)) {
      for (el in x) visit(el)
    }
  }
  visit(n)
  found
}

# Bottom-most data leaf of an upstream chain. For a pipeline, this is
# `stages[[1]]`; for a non-pipeline single-stage upstream (e.g. a bare
# data literal), it's the node itself.
extract_source_leaf <- function(upstream) {
  if (is.null(upstream)) return(NULL)
  if (is_ptr_pipeline(upstream)) {
    if (length(upstream$stages) == 0L) return(NULL)
    return(extract_source_leaf(upstream$stages[[1L]]))
  }
  upstream
}

# Truncate `upstream` at the first stage (source-to-consumer order) that
# contains a placeholder. Returns the truncated subtree, or NULL if no
# placeholder-free prefix remains (only happens when the source itself
# is a placeholder).
truncate_upstream_at_placeholder <- function(upstream) {
  if (is.null(upstream)) return(NULL)
  if (is_ptr_pipeline(upstream)) {
    keep <- list()
    for (stage in upstream$stages) {
      if (contains_placeholder(stage)) break
      keep[[length(keep) + 1L]] <- stage
    }
    if (length(keep) == 0L) return(NULL)
    if (length(keep) == 1L) return(keep[[1L]])
    p <- upstream
    p$stages <- keep
    return(p)
  }
  if (contains_placeholder(upstream)) return(NULL)
  upstream
}

# Walk one or more typed-AST trees and bucket every shared
# `ptr_ph_data_consumer` node by its `$shared` key. Returns a named list
# of lists of nodes.
collect_shared_consumer_occurrences <- function(trees) {
  if (!is.list(trees) || is_ptr_node(trees)) trees <- list(trees)
  buckets <- list()
  for (tr in trees) {
    consumers <- find_nodes(tr, function(x) {
      is_ptr_ph_data_consumer(x) && !is.null(x$shared)
    })
    for (c in consumers) {
      key <- c$shared
      buckets[[key]] <- c(buckets[[key]] %||% list(), list(c))
    }
  }
  buckets
}

# Param-less form of a placeholder's default label -- e.g. "Enter a number"
# from the copy default "Enter a number for {param}", "Pick a column" from
# "Pick a column for {param}". Used when a shared key spans multiple params,
# so a single per-param label ("...for size") would mislead.
shared_widget_base_label <- function(keyword, fallback) {
  tmpl <- ptr_registry_lookup(keyword)$copy_defaults$label
  if (is.null(tmpl) || !nzchar(tmpl)) return(fallback)
  out <- trimws(sub("\\s*(for\\s+)?\\{[^}]*\\}.*$", "", tmpl))
  if (nzchar(out)) out else fallback
}

# Label for a shared widget referenced under more than one parameter, e.g. one
# `var(shared = "v")` feeding both `alpha` and `size` -- a single per-param
# copy ("Choose the transparency column") would mislead. Returns NULL ("keep
# the normal per-param copy") when there is only one distinct param. For a data
# consumer the params are enumerated ("Pick a column for: alpha, size"); for a
# value placeholder we fall back to the param-less default ("Enter a number"),
# since listing widget-arg names there reads worse than it helps.
shared_widget_label <- function(occurrences) {
  if (length(occurrences) == 0L) return(NULL)
  node1 <- occurrences[[1L]]
  params <- vapply(occurrences, function(n) n$param %||% "", character(1))
  params <- unique(params[nzchar(params) & params != "__unnamed__"])
  if (length(params) <= 1L) return(NULL)
  if (is_ptr_ph_data_consumer(node1)) {
    paste0(shared_widget_base_label(node1$keyword, "Pick a column"),
           " for: ", paste(params, collapse = ", "))
  } else {
    shared_widget_base_label(node1$keyword, "Set a value")
  }
}

# Apply the contract above to one bucket of occurrences.
resolve_shared_consumer <- function(occurrences) {
  if (length(occurrences) == 0L) {
    return(list(kind = "error",
                error = paste0("Internal error: no occurrences for this ",
                               "shared key. Please report this at ",
                               "https://github.com/willju-wangqian/ggpaintr/issues.")))
  }
  truncated <- lapply(occurrences, function(n) {
    truncate_upstream_at_placeholder(n$upstream)
  })
  if (any(vapply(truncated, is.null, logical(1)))) {
    return(list(kind = "error",
                error = paste0("This shared picker has no dataset to list ",
                               "columns from -- its data is produced entirely ",
                               "by other inputs that aren't filled in yet. Fill ",
                               "those in, or base the picker on a concrete ",
                               "dataset.")))
  }
  sources <- lapply(truncated, extract_source_leaf)
  src1 <- sources[[1L]]
  if (length(sources) > 1L) {
    for (i in 2:length(sources)) {
      if (!identical(src1, sources[[i]])) {
        return(list(kind = "error",
                    error = paste0("This shared picker is used on plots built ",
                                   "from different datasets, so one column list ",
                                   "can't apply to all of them. Point them at ",
                                   "the same dataset, or give each plot its own ",
                                   "`var` (without `shared =`).")))
      }
    }
  }
  up1 <- truncated[[1L]]
  all_eq <- TRUE
  if (length(truncated) > 1L) {
    for (i in 2:length(truncated)) {
      if (!identical(up1, truncated[[i]])) { all_eq <- FALSE; break }
    }
  }
  if (all_eq) list(kind = "upstream", value = up1)
  else list(kind = "source", value = src1)
}

# Convenience: per-key resolution for one or more trees.
ptr_resolve_shared_consumers <- function(trees) {
  buckets <- collect_shared_consumer_occurrences(trees)
  if (length(buckets) == 0L) return(list())
  lapply(buckets, resolve_shared_consumer)
}

rewrite_shared <- function(x, canonical) {
  if (is_ptr_placeholder(x) && !is.null(x$shared)) {
    new_id <- canonical[[x$shared]]
    if (!is.null(new_id)) {
      x$id <- new_id
      # Keep paired ids (e.g. an upload source's dataset-name companion)
      # in sync with the canonical id. Without this, two occurrences of
      # `upload(shared = "ds")` in different layers keep the distinct
      # companion ids `ptr_assign_ids()` gave them, so only the first
      # occurrence's companion widget is rendered/read and the other
      # layer's `data = ...` silently drops out.
      if (is_ptr_ph_data_source(x) && !is.null(x$companion_id)) {
        entry <- ptr_registry_lookup(x$keyword)
        if (!is.null(entry) && !is.null(entry$companion_id_fn)) {
          x$companion_id <- entry$companion_id_fn(new_id)
        }
      }
    }
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
