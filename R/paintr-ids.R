# P4 — id-encoding. Walks the typed tree and assigns each placeholder a raw
# id of the form `<layer>_<underscore-joined-index-path>_<keyword>_<shared-or-NA>`,
# along with the enclosing arg name (`param`). The enclosing layer name is
# also stamped on the node as `node$layer_name`. Layer-derived ids
# (`<layer>_checkbox`, `<layer>_update_data`) are attached to the layer node.
# `ns_fn` is validated and stored on the root for emit-time application.
# Source companion ids are computed via the registry's `companion_id_fn`.

ptr_assign_ids <- function(node, ns_fn = shiny::NS(NULL)) {
  if (!is_ptr_root(node)) {
    rlang::abort("ptr_assign_ids expects a ptr_root.")
  }
  validate_ns_fn(ns_fn)
  node$ns_fn <- ns_fn
  for (i in seq_along(node$layers)) {
    layer <- node$layers[[i]]
    if (is_ptr_layer(layer)) {
      node$layers[[i]] <- assign_ids_in_layer(layer)
    } else if (is_ptr_placeholder(layer)) {
      node$layers[[i]] <- assign_id_to_placeholder(
        layer, layer_name = layer$keyword, path = integer(),
        param = NA_character_
      )
    }
  }
  assign_stage_ids(node)
}

validate_ns_fn <- function(ns_fn) {
  if (!is.function(ns_fn)) {
    rlang::abort("`ns_fn` must be a function (use shiny::NS or shiny::NS(NULL)).")
  }
  invisible(TRUE)
}

assign_ids_in_layer <- function(layer) {
  if (!is.null(layer$data_arg)) {
    layer$data_arg <- assign_ids_walk(
      layer$data_arg, layer_name = layer$name, path = integer(),
      param = "data"
    )
  }
  if (length(layer$children) > 0L) {
    arg_names <- names(layer$children) %||% rep_len("", length(layer$children))
    for (i in seq_along(layer$children)) {
      child_param <- if (nzchar(arg_names[i])) arg_names[i] else NA_character_
      layer$children[[i]] <- assign_ids_walk(
        layer$children[[i]], layer_name = layer$name,
        path = i, param = child_param
      )
    }
  }
  if (layer_has_pipeline_placeholders(layer)) {
    layer$update_data_input_id <- paste0(layer$name, "_update_data")
  } else {
    layer$update_data_input_id <- NULL
  }
  layer
}

assign_ids_walk <- function(node, layer_name, path, param) {
  if (is.null(node)) return(NULL)
  if (is_ptr_placeholder(node)) {
    return(assign_id_to_placeholder(node, layer_name, path, param))
  }
  if (is_ptr_call(node)) {
    arg_names <- names(node$args) %||% rep_len("", length(node$args))
    for (i in seq_along(node$args)) {
      child_param <- if (nzchar(arg_names[i])) arg_names[i] else NA_character_
      node$args[[i]] <- assign_ids_walk(
        node$args[[i]], layer_name = layer_name,
        path = c(path, i), param = child_param
      )
    }
    return(node)
  }
  if (is_ptr_pipeline(node)) {
    for (i in seq_along(node$stages)) {
      node$stages[[i]] <- assign_ids_walk(
        node$stages[[i]], layer_name = layer_name,
        path = c(path, i), param = param
      )
    }
    return(node)
  }
  node
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
    if (!is.null(entry$companion_id_fn)) {
      node$companion_id <- entry$companion_id_fn(node$id)
    }
  }
  node
}

# A layer needs an `_update_data` id only when its data_arg is a pipeline
# AND that pipeline contains placeholders. Plain-data layers (e.g.,
# `ggplot(mtcars, aes(x = var))`) don't get an update-data button.
layer_has_pipeline_placeholders <- function(layer) {
  if (is.null(layer$data_arg)) return(FALSE)
  if (!is_ptr_pipeline(layer$data_arg)) return(FALSE)
  walk_has_placeholder(layer$data_arg)
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
