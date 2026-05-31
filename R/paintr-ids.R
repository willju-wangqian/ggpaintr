# P4 -- id-encoding. Walks the typed tree and assigns each placeholder a raw
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
# traversal -- not hand-threaded here.
# Short, deterministic, `make.names`-valid hash of an id. Mirrors the
# `substr(rlang::hash(...), 1L, n)` precedent in R/paintr-build-ui.R. Used
# to derive a non-shared source's `auto_name` from its translate-time
# `node$id` so the post-upload binding symbol is system-generated (not a
# leaked user `default`). 6 hex chars: 16^6 ~ 16.7M, ample for the handful
# of source slots in one formula.
ptr_hash <- function(id) substr(rlang::hash(id), 1L, 6L)

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

# ADR 0025 sec.1 Example #4: `"shortcut"` is reserved as a shared-key name
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
      "Shared key '", key, "' is reserved (ADR 0025 sec.1): the suffix ",
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
    # ADR 0025 sec.3 / PLAN-02: the auto-name is the source slot's binding
    # contract under the coordinator eval_env -- the symbol the upload
    # binder assigns into and the substitute walker resolves once the
    # shortcut textbox is empty (i.e. after an upload, when the mutex has
    # cleared it). For non-shared sources it is a system-generated
    # `df_<hash(node$id)>`: derived solely from the translate-time
    # `node$id`, so it is stable across readers and unique per source
    # slot. It must NOT be derived from `node$default` -- doing so leaked
    # the env-shortcut symbol (e.g. `df_rug` from `ppUpload(df_rug)`)
    # into the post-upload generated code. Shared sources are stamped
    # later by `ptr_setup_panel_sources()` (R/paintr-shared-ui.R) where
    # the canonical key + coordinator obj$id are in scope; we leave
    # `node$auto_name` NULL here so that runtime stamp is the single
    # source of truth for shared keys.
    if (is.null(node$shared)) {
      node$auto_name <- paste0("df_", ptr_hash(node$id))
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


# Output id used as the renderUI container for ANY placeholder (value,
# source, or consumer). The placeholder's input id (e.g. `ggplot_1_1_var_NA`)
# is reused with a `_ui` suffix so the widget, when emitted by renderUI,
# lives at the original raw id while the container that holds it is uniquely
# named (no collision between the bound widget's inputId and its container).
# Single source of truth for the `_ui` suffix contract: `build_ui_for.*`
# (R/paintr-build-ui.R), the `ptr_setup_*_uis` server helpers, and
# `emit_placeholder_rows` (R/paintr-input-spec.R) all route through it.
placeholder_output_id <- function(raw_id) {
  paste0(raw_id, "_ui")
}
