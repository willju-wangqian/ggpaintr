# Stage-disable — gate-2 manual escape for data-manipulation stages.
#
# `assign_stage_ids(tree)` annotates each call along a layer's data-arg
# data-manipulation chain (whose subtree contains a placeholder) with a stable
# `stage_id` of the form `<layer>+<dot-path>+stage_enabled`. P4 hooks this in
# at the end of `ptr_assign_ids` so all id derivation lives in one pass.
#
# `disable_walk(tree, stage_enabled)` is the runtime visitor: bottom-up,
# pipeline stages whose stage_id is FALSE in `stage_enabled` drop from the
# stages list; non-pipeline calls whose stage_id is FALSE collapse to their
# data-arg child. P9 prune still owns single-/zero-stage pipeline collapse.
#
# See `.claude/specs/stage-disable-checkbox.md` for the locked design.

# ---- predicates / helpers -------------------------------------------------

# A call qualifies for a stage-id iff it is a ptr_call AND either (a) its
# subtree contains at least one placeholder, or (b) it carries
# `has_user_control = TRUE` (stamped by `ppVerbSwitch` unwrap). Two
# disjuncts, single gate — see ADR 0021. Walk position ("in data-arg
# position") is enforced by the caller, not the predicate.
is_data_chain_call <- function(node) {
  is_ptr_call(node) &&
    (walk_has_placeholder(node) || isTRUE(node$has_user_control))
}

# Find the data-arg position of a call. Tidyverse convention: arg named
# `data` or `.data` first; otherwise the first positional arg. Returns the
# integer arg index, or NULL if no candidate exists.
data_arg_position <- function(call) {
  args <- call$args
  if (length(args) == 0L) return(NULL)
  arg_names <- names(args) %||% rep_len("", length(args))
  named <- which(arg_names %in% c("data", ".data"))
  if (length(named) > 0L) return(named[[1L]])
  positional <- which(!nzchar(arg_names))
  if (length(positional) > 0L) return(positional[[1L]])
  NULL
}

stage_id_from_path <- function(layer_name, path) {
  path_str <- if (length(path) == 0L) "0" else paste(path, collapse = "_")
  paste0(layer_name, "_", path_str, "_stage_enabled")
}

# ---- P4 hook --------------------------------------------------------------

# Annotate the typed root with stage_ids on every chain-eligible call along
# each layer's data-arg chain. Run at the end of `ptr_assign_ids`; idempotent.
#
# Implemented on `ptr_rewrite_pre`: the traversal cursor's `in_data_position`
# flag is exactly "this node sits on the data spine". A non-pipeline call there
# gets a stage_id from its `path`; a pipeline there gets stage_ids stamped on
# each data-chain stage from `c(path, i)` — including stage 1 (the source) and
# stages k >= 2 (which take data implicitly, so their *args* are not on the
# spine — `in_data_position` is FALSE for them, matching the old chain walk's
# "only stage 1 recurses into data-arg position").
assign_stage_ids <- function(node) {
  if (!is_ptr_root(node)) return(node)
  ptr_rewrite_pre(node, function(n, cur) {
    if (!isTRUE(cur$in_data_position)) return(n)
    if (is_ptr_pipeline(n)) {
      for (i in seq_along(n$stages)) {
        s <- n$stages[[i]]
        if (is_data_chain_call(s)) {
          s$stage_id <- stage_id_from_path(cur$layer_name, c(cur$path, i))
          n$stages[[i]] <- s
        }
      }
    } else if (is_data_chain_call(n)) {
      n$stage_id <- stage_id_from_path(cur$layer_name, cur$path)
    }
    n
  })
}

# Collect every stage_id present in the tree, in formula order.
collect_stage_ids <- function(node) {
  out <- character()
  ptr_walk(node, function(x) {
    sid <- x$stage_id
    if (!is.null(sid) && nzchar(sid)) out[[length(out) + 1L]] <<- sid
  })
  out
}

# ---- runtime visitor ------------------------------------------------------

disable_walk <- function(node, stage_enabled = list()) {
  if (is.null(node)) return(NULL)
  UseMethod("disable_walk")
}

#' @export
disable_walk.default <- function(node, stage_enabled = list()) node

#' @export
disable_walk.ptr_root <- function(node, stage_enabled = list()) {
  for (i in seq_along(node$layers)) {
    node$layers[[i]] <- disable_walk(node$layers[[i]], stage_enabled)
  }
  node
}

#' @export
disable_walk.ptr_layer <- function(node, stage_enabled = list()) {
  if (!is.null(node$data_arg)) {
    node$data_arg <- disable_walk(node$data_arg, stage_enabled)
  }
  if (length(node$children) > 0L) {
    for (i in seq_along(node$children)) {
      node$children[[i]] <- disable_walk(node$children[[i]], stage_enabled)
    }
  }
  node
}

#' @export
disable_walk.ptr_pipeline <- function(node, stage_enabled = list()) {
  out <- list()
  for (i in seq_along(node$stages)) {
    s <- node$stages[[i]]
    # Stage k >= 2 receives data implicitly from the previous stage: a
    # disabled call drops wholesale, not "replace with first positional arg"
    # (which is a predicate, not data, in pipeline form). Stage k = 1 IS the
    # data source, so it falls through to the call walker which replaces it
    # with its data-arg child (matching non-pipeline single-call semantics).
    if (i >= 2L && is_ptr_call(s) && is_stage_disabled(s, stage_enabled)) {
      next
    }
    rewritten <- disable_walk(s, stage_enabled)
    if (is_ptr_missing(rewritten)) next
    out[[length(out) + 1L]] <- rewritten
  }
  node$stages <- out
  node
}

#' @export
disable_walk.ptr_call <- function(node, stage_enabled = list()) {
  for (i in seq_along(node$args)) {
    node$args[[i]] <- disable_walk(node$args[[i]], stage_enabled)
  }
  if (is_stage_disabled(node, stage_enabled)) {
    pos <- data_arg_position(node)
    if (!is.null(pos)) return(node$args[[pos]])
    return(ptr_missing())
  }
  node
}

is_stage_disabled <- function(node, stage_enabled) {
  if (!is_ptr_node(node)) return(FALSE)
  sid <- node$stage_id
  if (is.null(sid)) return(FALSE)
  isFALSE(stage_enabled[[sid]])
}
