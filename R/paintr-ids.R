# P4 — id-encoding. Walks the typed tree and assigns each placeholder a raw
# id of the form `<layer>_<underscore-joined-index-path>_<keyword>_<shared-or-NA>`,
# along with the enclosing arg name (`param`). The enclosing layer name is
# also stamped on the node as `node$layer_name`. Layer-derived ids
# (`<layer>_checkbox`) are attached to the layer node.
# `ns_fn` is validated here. It is not stored on the tree: every namespacing
# call site already receives it as a function parameter (UI build) or reads
# it from `state$server_ns_fn` / `state$ui_ns_fn` (server), so a third copy
# on the root would be dead weight (and would make two structurally-equal
# trees compare unequal under `ptr_tree_structural_equal`).
# Source shortcut ids are derived as `paste0(node$id, "_shortcut")` when the
# registry entry sets `shortcut = TRUE`.

# One cursor-threaded pre-order pass stamps every placeholder's id (the
# traversal cursor supplies `layer_name` / `path` / `param`); a second pass
# (`assign_stage_ids`) stamps stage-disable ids on the data-arg chain. Both
# run on `ptr_rewrite_pre` so the "where am I" bookkeeping lives once, in the
# traversal — not hand-threaded here.
ptr_assign_ids <- function(node, ns_fn = shiny::NS(NULL)) {
  if (!is_ptr_root(node)) {
    rlang::abort("ptr_assign_ids expects a ptr_root.")
  }
  validate_ns_fn(ns_fn)
  ptr_validate_reserved_shared_keys(node)
  node <- ptr_rewrite_pre(node, function(n, cur) {
    if (is_ptr_placeholder(n)) {
      return(assign_id_to_placeholder(n, cur$layer_name, cur$path, cur$param))
    }
    n
  })
  assign_stage_ids(node)
}

# ADR 0025 §1 Example #4: `"shortcut"` is reserved as a shared-key name
# because the surface now uses the suffix `"_shortcut"` to disambiguate the
# env-shortcut sibling input from the source's primary id. Allowing a user
# formula to also bind `shared = "shortcut"` would create a namespace
# collision between the reserved suffix and the shared-coordinator key. Abort
# at translate-time with a message naming the reserved key.
ptr_validate_reserved_shared_keys <- function(node) {
  reserved <- "shortcut"
  bad <- character()
  ptr_walk(node, function(n) {
    if (is_ptr_placeholder(n) && !is.null(n$shared) &&
        n$shared %in% reserved) {
      bad <<- c(bad, n$shared)
    }
  })
  if (length(bad) > 0L) {
    key <- bad[[1L]]
    rlang::abort(paste0(
      "Shared key '", key, "' is reserved (ADR 0025 §1): the suffix ",
      "'_", key, "' names the source env-shortcut sibling input. ",
      "Use a different shared key."
    ))
  }
  invisible(node)
}

validate_ns_fn <- function(ns_fn) {
  if (!is.function(ns_fn)) {
    rlang::abort("`ns_fn` must be a function (use shiny::NS or shiny::NS(NULL)).")
  }
  invisible(TRUE)
}

assign_id_to_placeholder <- function(node, layer_name, path, param) {
  shared_part <- if (is.null(node$shared)) "NA" else node$shared
  path_str <- if (length(path) == 0L) "0" else paste(path, collapse = "_")
  node$id <- paste0(layer_name, "_", path_str, "_", node$keyword, "_", shared_part)
  node$layer_name <- layer_name
  if (is.na(node$param) || is.null(node$param)) {
    node$param <- if (is.null(param) || is.na(param)) NA_character_ else param
  }
  if (is_ptr_ph_data_source(node)) {
    entry <- ptr_registry_lookup(node$keyword)
    if (isTRUE(entry$shortcut)) {
      node$shortcut_id <- paste0(node$id, "_shortcut")
    }
    # ADR 0025 §3 / PLAN-02: the auto-name is the source slot's binding
    # contract under the coordinator eval_env. For non-shared sources,
    # default to `node$default` (the verbatim user-supplied symbol, e.g.
    # `"penguins"` from `ppUpload(penguins)`) and fall back to `node$id`
    # (e.g. `ggplot_1_0_ppUpload_NA`). Shared sources are stamped later
    # by `ptr_setup_panel_sources()` (R/paintr-shared-ui.R) where the
    # canonical key + coordinator obj$id are in scope; we leave
    # `node$auto_name` NULL here so that runtime stamp is the single
    # source of truth for shared keys.
    if (is.null(node$shared)) {
      node$auto_name <- node$default %||% node$id
    }
  }
  node
}

walk_has_placeholder <- function(node) {
  if (is.null(node)) return(FALSE)
  if (is_ptr_placeholder(node)) return(TRUE)
  if (is_ptr_call(node)) {
    for (a in node$args) if (walk_has_placeholder(a)) return(TRUE)
    return(FALSE)
  }
  if (is_ptr_pipeline(node)) {
    for (s in node$stages) if (walk_has_placeholder(s)) return(TRUE)
    return(FALSE)
  }
  FALSE
}

# Apply `ns_fn` at UI emit / observer-binding time. Raw ids stay on nodes;
# rendered ids are computed on demand.
ptr_render_id <- function(raw_id, ns_fn) {
  if (is.null(ns_fn)) return(raw_id)
  ns_fn(raw_id)
}


# Output id used as the renderUI container for a consumer placeholder. The
# placeholder's input id (e.g. `ggplot_1_1_var_NA`) is reused with a `_ui`
# suffix so the picker, when emitted by renderUI, lives at the original id
# while the container that holds it is uniquely named.
consumer_output_id <- function(raw_id) {
  paste0(raw_id, "_ui")
}
