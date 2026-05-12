# P6 — per-node UI dispatch.
#
# `build_ui_for(node, ...)` is the S3 generic that callers (the layer-panel
# scaffolding in P6 and `ptr_module_*`) invoke once per tree node that
# wants Shiny widgets. The dispatch:
#   1. resolves the human-readable label via `ptr_resolve_ui_text`
#   2. namespaces `node$id` (and the source's `companion_id`) via `ns_fn`
#   3. forwards to the registered `build_ui` hook with a resolved label
#
# The hook only ever sees a final id and a final label string. It never
# resolves copy paths or ns prefixes itself.

#' Build a Shiny UI Widget for a Typed-Tree Node
#'
#' S3 generic dispatched per node class. Resolves the human-readable label
#' through `ptr_resolve_ui_text`, namespaces the node's id (and any
#' `companion_id` for source nodes) via `ns_fn`, and forwards to the
#' registered `build_ui` hook for the placeholder's keyword.
#'
#' @param node A typed AST node (e.g. `ptr_ph_value`, `ptr_ph_data_consumer`,
#'   `ptr_ph_data_source`, `ptr_layer`).
#' @param ... Additional arguments. Recognized by built-in methods:
#'   `ui_text`, `placeholders`, `layer_name`, `ns_fn`, `checkbox_defaults`,
#'   `shell_copy`. Consumer placeholders emit only a `uiOutput` container at
#'   static build time; their picker is rendered server-side via
#'   `ptr_setup_consumer_uis()`, which calls the registry's `build_ui(node,
#'   cols, ...)` inside `renderUI` once cols are resolved.
#'
#' @return A `shiny.tag` (or NULL for nodes that emit no UI).
#' @keywords internal
#' @export
build_ui_for <- function(node, ...) UseMethod("build_ui_for")

#' @export
build_ui_for.default <- function(node, ...) NULL

#' @export
build_ui_for.ptr_literal <- function(node, ...) NULL

#' @export
build_ui_for.ptr_missing <- function(node, ...) NULL

#' @export
build_ui_for.ptr_user_expr <- function(node, ...) NULL

#' @export
build_ui_for.ptr_ph_value <- function(node,
                                       ui_text = NULL,
                                       placeholders = NULL,
                                       layer_name = NULL,
                                       ns_fn = identity,
                                       ...) {
  invoke_build_ui(node, ui_text = ui_text, placeholders = placeholders,
                  layer_name = layer_name, ns_fn = ns_fn, extra = list(),
                  ...)
}

#' @export
build_ui_for.ptr_ph_data_consumer <- function(node,
                                                ns_fn = identity,
                                                ...) {
  shiny::uiOutput(ptr_render_id(consumer_output_id(node$id), ns_fn))
}

#' @export
build_ui_for.ptr_ph_data_source <- function(node,
                                              ui_text = NULL,
                                              placeholders = NULL,
                                              layer_name = NULL,
                                              ns_fn = identity,
                                              ...) {
  rendered_node <- node
  rendered_node$id <- ns_fn(node$id)
  if (!is.null(node$companion_id)) {
    rendered_node$companion_id <- ns_fn(node$companion_id)
  }
  copy <- ptr_resolve_ui_text(
    "control",
    keyword = node$keyword,
    layer_name = layer_name,
    param = node$param,
    ui_text = ui_text,
    placeholders = placeholders
  )
  entry <- ptr_registry_lookup(node$keyword)
  if (is.null(entry) || is.null(entry$build_ui)) {
    rlang::abort(paste0(
      "No `build_ui` hook registered for placeholder `", node$keyword, "`."
    ))
  }
  entry$build_ui(rendered_node, label = copy$label, ...)
}

# ---- ptr_layer panel scaffolding ----

#' @export
build_ui_for.ptr_layer <- function(node,
                                    ui_text = NULL,
                                    placeholders = NULL,
                                    ns_fn = identity,
                                    checkbox_defaults = NULL,
                                    shell_copy = NULL,
                                    ...) {
  layer_name <- node$name
  shell_copy <- shell_copy %||% layer_panel_default_shell_copy(ui_text)
  pipeline_entries <- find_layer_placeholders_with_stage(node$data_arg)
  control_phs  <- find_layer_placeholders(node$children)

  seen_stage_ids <- character()
  pipeline_ui <- list()
  for (entry in pipeline_entries) {
    ph <- entry$ph
    sid <- entry$stage_id
    ui <- build_ui_for(ph, ui_text = ui_text,
                       placeholders = placeholders, layer_name = layer_name,
                       ns_fn = ns_fn)
    if (is.null(ui)) next
    if (!is.na(sid)) {
      first_in_stage <- !sid %in% seen_stage_ids
      if (first_in_stage) {
        seen_stage_ids <- c(seen_stage_ids, sid)
        ui <- shiny::div(
          class = "ptr-stage-row",
          shiny::checkboxInput(
            inputId = ns_fn(sid), label = NULL, value = TRUE,
            width = "auto"
          ),
          ui
        )
      } else {
        ui <- shiny::div(class = "ptr-stage-row", ui)
      }
    }
    pipeline_ui[[length(pipeline_ui) + 1L]] <- ui
  }

  control_ui <- lapply(control_phs, function(ph) {
    build_ui_for(ph, ui_text = ui_text,
                 placeholders = placeholders, layer_name = layer_name,
                 ns_fn = ns_fn)
  })
  control_ui  <- drop_null(control_ui)

  data_label     <- shell_copy$data_subtab_label %||% "Data"
  controls_label <- shell_copy$controls_subtab_label %||% "Controls"

  inner <- layer_panel_inner(
    pipeline_ui = pipeline_ui,
    control_ui = control_ui,
    data_label = data_label,
    controls_label = controls_label,
    layer_name = layer_name,
    ns_fn = ns_fn
  )

  default_on <- resolve_layer_default(layer_name, checkbox_defaults,
                                      node$default_active)

  content_div <- shiny::div(
    id = ns_fn(layer_panel_content_id(layer_name)),
    class = if (default_on) "ptr-layer-content" else "ptr-layer-content ptr-layer-disabled",
    inner
  )

  body <- if (is.null(node$active_input_id)) {
    list(content_div)
  } else {
    list(
      shiny::checkboxInput(
        ns_fn(node$active_input_id),
        label = shell_copy$layer_checkbox_label %||% layer_name,
        value = default_on
      ),
      content_div
    )
  }
  do.call(shiny::tabPanel, c(list(layer_name), body))
}

# Every node where `pred(node)` is TRUE, in pre-order. See `ptr_walk()` in
# paintr-walk.R for the traversal (the `upstream` back-pointer is skipped).
find_nodes <- function(node, pred) ptr_collect(node, pred)

is_shared_placeholder <- function(x) {
  is_ptr_placeholder(x) && !is.null(x$shared)
}

# Every placeholder node in a subtree (or list of children), in formula
# order. Shared placeholders (`shared = "<key>"`) are excluded — they render
# once in a host-level shared section, never inside a layer panel — but
# still terminate descent (a placeholder has no placeholder children).
find_layer_placeholders <- function(x) {
  ptr_collect(
    x,
    pred = function(n) is_ptr_placeholder(n) && !is_shared_placeholder(n),
    prune = is_ptr_placeholder
  )
}


# Like `find_layer_placeholders`, but each entry is a list with `ph` and the
# innermost enclosing call's `stage_id` (or NA_character_ if none). Used to
# attach stage-disable checkboxes to the first placeholder of each stage.
find_layer_placeholders_with_stage <- function(x) {
  out <- list()
  visit <- function(n, current_sid) {
    if (is.null(n)) return()
    if (is_ptr_placeholder(n)) {
      if (is_shared_placeholder(n)) return()
      out[[length(out) + 1L]] <<- list(ph = n, stage_id = current_sid)
      return()
    }
    if (is_ptr_call(n)) {
      sid <- if (!is.null(n$stage_id)) n$stage_id else current_sid
      for (a in n$args) visit(a, sid)
      return()
    }
    if (is_ptr_pipeline(n)) {
      for (s in n$stages) {
        if (is_ptr_call(s) && !is.null(s$stage_id)) {
          # Stage IS the call: its args descend with the stage's id.
          for (a in s$args) visit(a, s$stage_id)
        } else {
          visit(s, NA_character_)
        }
      }
      return()
    }
    if (is_ptr_node(n)) {
      for (nm in names(n)) {
        if (identical(nm, "upstream")) next
        visit(n[[nm]], current_sid)
      }
    } else if (is.list(n)) {
      for (el in n) visit(el, current_sid)
    }
  }
  visit(x, NA_character_)
  out
}

# One entry per unique `shared` key, first occurrence (formula order) winning
# for the node used to drive `build_ui_for`. Entries:
# list(key = chr, node = ptr_placeholder, ns_id = canonical id).
collect_shared_placeholders <- function(tree) {
  seen <- character()
  out <- list()
  ptr_walk(tree, function(n) {
    if (is_shared_placeholder(n) && !(n$shared %in% seen)) {
      seen <<- c(seen, n$shared)
      out[[length(out) + 1L]] <<- list(key = n$shared, node = n, ns_id = n$id)
    }
  }, prune = is_shared_placeholder)
  out
}

layer_panel_inner <- function(pipeline_ui, control_ui,
                               data_label, controls_label,
                               layer_name = NULL, ns_fn = identity) {
  has_pipeline <- length(pipeline_ui) > 0L
  has_controls <- length(control_ui) > 0L

  data_panel_body <- if (has_pipeline) unname(pipeline_ui) else NULL

  # Tabset id makes the active-tab name observable as
  # `input[[<layer>_subtab]]` so per-consumer reactives can dep on
  # tab activation (Decision D3 in lazy-consumer-resolve.md). NULL when
  # there's no tabset (controls-only layer); reading a missing input is
  # an inert dep.
  subtab_id <- if (!is.null(layer_name)) {
    ns_fn(paste0(layer_name, "_subtab"))
  } else NULL

  if (has_pipeline && has_controls) {
    do.call(shiny::tabsetPanel, list(
      id = subtab_id,
      do.call(shiny::tabPanel, c(data_label, data_panel_body)),
      do.call(shiny::tabPanel, c(controls_label, unname(control_ui)))
    ))
  } else if (has_pipeline) {
    do.call(shiny::tabsetPanel, list(
      id = subtab_id,
      do.call(shiny::tabPanel, c(data_label, data_panel_body))
    ))
  } else if (has_controls) {
    do.call(shiny::tagList, unname(control_ui))
  } else {
    NULL
  }
}

layer_panel_content_id <- function(layer_name) {
  paste0("ptr_layer_content_", layer_name)
}

resolve_layer_default <- function(layer_name, checkbox_defaults,
                                   default_active) {
  if (identical(layer_name, "ggplot")) return(TRUE)
  if (!is.null(checkbox_defaults) &&
      layer_name %in% names(checkbox_defaults)) {
    return(isTRUE(checkbox_defaults[[layer_name]]))
  }
  isTRUE(default_active %||% TRUE)
}

layer_panel_default_shell_copy <- function(ui_text) {
  list(
    data_subtab_label = ptr_resolve_ui_text("data_subtab", ui_text = ui_text)$label,
    controls_subtab_label = ptr_resolve_ui_text("controls_subtab", ui_text = ui_text)$label,
    layer_checkbox_label = ptr_resolve_ui_text("layer_checkbox", ui_text = ui_text)$label,
    layer_picker_label = ptr_resolve_ui_text("layer_picker", ui_text = ui_text)$label
  )
}

# ---- internal helper ----

invoke_build_ui <- function(node, ui_text, placeholders, layer_name,
                            ns_fn, extra, ...) {
  rendered_node <- node
  rendered_node$id <- ns_fn(node$id)
  copy <- ptr_resolve_ui_text(
    "control",
    keyword = node$keyword,
    layer_name = layer_name,
    param = node$param,
    ui_text = ui_text,
    placeholders = placeholders
  )
  entry <- ptr_registry_lookup(node$keyword)
  if (is.null(entry) || is.null(entry$build_ui)) {
    rlang::abort(paste0(
      "No `build_ui` hook registered for placeholder `", node$keyword, "`."
    ))
  }
  do.call(entry$build_ui,
          c(list(rendered_node, label = copy$label), extra, list(...)))
}
