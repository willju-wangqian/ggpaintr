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

# A call qualifies for a stage-id iff it is a ptr_call AND its subtree
# contains at least one placeholder. Walk position ("in data-arg position")
# is enforced by the caller, not the predicate.
is_data_chain_call <- function(node) {
  is_ptr_call(node) && walk_has_placeholder(node)
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

# Annotate the typed root with stage_ids on every chain-eligible call.
# Run at the end of `ptr_assign_ids`; idempotent — safe to re-run.
assign_stage_ids <- function(node) {
  if (!is_ptr_root(node)) return(node)
  for (i in seq_along(node$layers)) {
    layer <- node$layers[[i]]
    if (is_ptr_layer(layer) && !is.null(layer$data_arg)) {
      node$layers[[i]]$data_arg <- assign_stage_ids_chain(
        layer$data_arg, layer_name = layer$name, path = integer()
      )
    }
  }
  node
}

# Walk the data-manipulation chain rooted at `node`. For each call along the
# chain whose subtree contains a placeholder, attach a stage_id derived from
# its index path. The chain follows pipelines (each stage is a chain element)
# and recurses into a non-pipeline call's data-arg position. Walk ends at
# bare symbols / non-calls.
assign_stage_ids_chain <- function(node, layer_name, path) {
  if (is.null(node)) return(NULL)
  if (is_ptr_pipeline(node)) {
    for (i in seq_along(node$stages)) {
      stage <- node$stages[[i]]
      sub_path <- c(path, i)
      if (is_data_chain_call(stage)) {
        stage$stage_id <- stage_id_from_path(layer_name, sub_path)
        # Only the leftmost stage's args sit in data-arg position; downstream
        # stages take their data implicitly from the previous stage.
        if (i == 1L) {
          pos <- data_arg_position(stage)
          if (!is.null(pos)) {
            stage$args[[pos]] <- assign_stage_ids_chain(
              stage$args[[pos]], layer_name = layer_name,
              path = c(sub_path, pos)
            )
          }
        }
        node$stages[[i]] <- stage
      }
    }
    return(node)
  }
  if (is_data_chain_call(node)) {
    node$stage_id <- stage_id_from_path(layer_name, path)
    pos <- data_arg_position(node)
    if (!is.null(pos)) {
      node$args[[pos]] <- assign_stage_ids_chain(
        node$args[[pos]], layer_name = layer_name, path = c(path, pos)
      )
    }
    return(node)
  }
  node
}

# Collect every stage_id present in the tree, in formula order.
collect_stage_ids <- function(node) {
  out <- character()
  visit <- function(x) {
    if (is_ptr_node(x)) {
      sid <- x$stage_id
      if (!is.null(sid) && nzchar(sid)) {
        out[[length(out) + 1L]] <<- sid
      }
      for (nm in names(x)) {
        if (identical(nm, "upstream")) next
        visit(x[[nm]])
      }
    } else if (is.list(x)) {
      for (el in x) visit(el)
    }
  }
  visit(node)
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
