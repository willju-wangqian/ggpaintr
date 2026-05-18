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
#'   `ui_text`, `layer_name`, `ns_fn`, `checkbox_defaults`, `shell_copy`,
#'   `label_override` (force a specific widget label, used for shared widgets
#'   referenced under several params). Consumer placeholders emit only a `uiOutput` container at
#'   static build time; their picker is rendered server-side via
#'   `ptr_setup_consumer_uis()`, which calls the registry's `build_ui(node,
#'   cols, ...)` inside `renderUI` once cols are resolved.
#'
#' @return A `shiny.tag` (or NULL for nodes that emit no UI).
#' @keywords internal
build_ui_for <- function(node, ...) UseMethod("build_ui_for")

#' @exportS3Method
build_ui_for.default <- function(node, ...) NULL

#' @exportS3Method
build_ui_for.ptr_literal <- function(node, ...) NULL

#' @exportS3Method
build_ui_for.ptr_missing <- function(node, ...) NULL

#' @exportS3Method
build_ui_for.ptr_user_expr <- function(node, ...) NULL

#' @exportS3Method
build_ui_for.ptr_ph_value <- function(node,
                                       ui_text = NULL,
                                       layer_name = NULL,
                                       ns_fn = identity,
                                       param_override = NULL,
                                       label_suffix = NULL,
                                       ...) {
  invoke_build_ui(node, ui_text = ui_text,
                  layer_name = layer_name, ns_fn = ns_fn, extra = list(),
                  param_override = param_override, label_suffix = label_suffix,
                  ...)
}

#' @exportS3Method
build_ui_for.ptr_ph_data_consumer <- function(node,
                                                ns_fn = identity,
                                                ...) {
  shiny::uiOutput(ptr_render_id(consumer_output_id(node$id), ns_fn))
}

#' @exportS3Method
build_ui_for.ptr_ph_data_source <- function(node,
                                              ui_text = NULL,
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
    param = node$param,
    layer_name = layer_name,
    ui_text = ui_text
  )
  entry <- ptr_registry_lookup(node$keyword)
  if (is.null(entry) || is.null(entry$build_ui)) {
    rlang::abort(paste0(
      "Placeholder `", node$keyword, "` has no `build_ui` function. Pass ",
      "`build_ui = function(node, label, ...)` when registering it -- see ",
      "`?ptr_define_placeholder_value`."
    ))
  }
  fmls <- names(formals(entry$build_ui))
  accepts_dots <- "..." %in% fmls
  extra_named <- build_ui_copy_args(fmls, copy)
  if (identical(node$keyword, "upload") &&
      (accepts_dots || "file_copy" %in% fmls)) {
    extra_named$file_copy <- ptr_resolve_ui_text(
      "upload_file", ui_text = ui_text
    )
    extra_named$name_copy <- ptr_resolve_ui_text(
      "upload_name", ui_text = ui_text
    )
  }
  do.call(entry$build_ui,
          c(list(rendered_node, label = copy$label), extra_named, list(...)))
}

# ---- ptr_layer panel scaffolding ----

#' @exportS3Method
build_ui_for.ptr_layer <- function(node,
                                    ui_text = NULL,
                                    ns_fn = identity,
                                    checkbox_defaults = NULL,
                                    shell_copy = NULL,
                                    ...) {
  layer_name <- node$name
  shell_copy <- shell_copy %||% layer_panel_default_shell_copy(ui_text)
  pipeline_entries <- find_layer_placeholders_with_stage(node$data_arg)
  control_phs  <- find_layer_placeholders(node$children)

  pipeline_ui <- build_pipeline_stage_ui(
    pipeline_entries, ui_text = ui_text, layer_name = layer_name, ns_fn = ns_fn
  )

  control_ui <- lapply(control_phs, function(ph) {
    build_ui_for(ph, ui_text = ui_text,
                 layer_name = layer_name,
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

# Render the Data sub-tab body: pipeline placeholders grouped by stage.
# Each pipeline stage -- a `subset()`, `mutate()`, ... call carrying a
# `stage_id` -- becomes a `.ptr-stage` block: a `.ptr-stage-head` checkbox
# whose label is the verb (`verb()`), then the stage's placeholder widgets
# in an indented `.ptr-stage-fields`. Unticking the checkbox drops the
# stage from the generated pipeline (wired up in paintr-server.R via the
# `stage_id` input). The per-widget " in verb()" label suffix is dropped
# inside a stage group -- the header already names the verb -- but kept for
# a stand-alone pipeline placeholder (no enclosing stage), which renders
# bare in a `.ptr-stage-row`.
#
# `entries` is the output of `find_layer_placeholders_with_stage()`, in
# formula order; placeholders of one stage are contiguous. A `seen` guard
# stops a second appearance of the same `stage_id` from emitting a
# duplicate checkbox (it would collide on `inputId`).
build_pipeline_stage_ui <- function(entries, ui_text, layer_name, ns_fn) {
  build_ph <- function(entry, drop_suffix) {
    ph <- entry$ph
    verb <- entry$verb
    has_verb <- !is.null(verb) && !is.na(verb) && nzchar(verb)
    param_override <- if (has_verb && ptr_param_is_unnamed(ph$param)) {
      paste0(verb, "()")
    } else NULL
    label_suffix <- if (has_verb && !drop_suffix) {
      paste0(" in ", verb, "()")
    } else NULL
    build_ui_for(ph, ui_text = ui_text, layer_name = layer_name,
                 ns_fn = ns_fn, param_override = param_override,
                 label_suffix = label_suffix)
  }
  out <- list()
  seen <- character()
  i <- 1L
  n <- length(entries)
  while (i <= n) {
    sid <- entries[[i]]$stage_id
    if (is.na(sid)) {
      ui <- build_ph(entries[[i]], drop_suffix = FALSE)
      if (!is.null(ui)) {
        out[[length(out) + 1L]] <- shiny::div(class = "ptr-stage-row", ui)
      }
      i <- i + 1L
      next
    }
    verb <- entries[[i]]$verb
    fields <- list()
    j <- i
    while (j <= n && !is.na(entries[[j]]$stage_id) &&
           identical(entries[[j]]$stage_id, sid)) {
      ui <- build_ph(entries[[j]], drop_suffix = TRUE)
      if (!is.null(ui)) fields[[length(fields) + 1L]] <- ui
      j <- j + 1L
    }
    if (length(fields) > 0L) {
      if (sid %in% seen) {
        out[[length(out) + 1L]] <- controllable_region_continuation(fields)
      } else {
        seen <- c(seen, sid)
        has_verb <- !is.null(verb) && !is.na(verb) && nzchar(verb)
        head_label <- if (has_verb) {
          shiny::tags$code(paste0(verb, "()"))
        } else NULL
        out[[length(out) + 1L]] <- controllable_region(
          sid, head_label, fields, ns_fn = ns_fn
        )
      }
    }
    i <- j
  }
  out
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
  call_head_name <- function(fun) {
    nm <- bare_call_name(fun)
    if (is.null(nm) || !nzchar(nm)) NA_character_ else nm
  }
  visit <- function(n, current_sid, current_verb) {
    if (is.null(n)) return()
    if (is_ptr_placeholder(n)) {
      if (is_shared_placeholder(n)) return()
      out[[length(out) + 1L]] <<- list(
        ph = n, stage_id = current_sid, verb = current_verb
      )
      return()
    }
    if (is_ptr_call(n)) {
      sid <- if (!is.null(n$stage_id)) n$stage_id else current_sid
      # Keep the *stage's* verb: only adopt this call's head when it is itself
      # a stage, or when no verb has been picked yet. Otherwise a nested call
      # like `Species == text` inside `subset(...)` would mis-report "==" as
      # the verb for the placeholder.
      verb <- if (!is.null(n$stage_id) || is.na(current_verb)) {
        call_head_name(n$fun)
      } else {
        current_verb
      }
      for (a in n$args) visit(a, sid, verb)
      return()
    }
    if (is_ptr_pipeline(n)) {
      for (s in n$stages) {
        if (is_ptr_call(s) && !is.null(s$stage_id)) {
          # Stage IS the call: its args descend with the stage's id.
          verb <- call_head_name(s$fun)
          for (a in s$args) visit(a, s$stage_id, verb)
        } else {
          visit(s, NA_character_, NA_character_)
        }
      }
      return()
    }
    if (is_ptr_node(n)) {
      for (nm in names(n)) {
        if (identical(nm, "upstream")) next
        visit(n[[nm]], current_sid, current_verb)
      }
    } else if (is.list(n) && !is.pairlist(n)) {
      for (el in n) visit(el, current_sid, current_verb)
    }
  }
  visit(x, NA_character_, NA_character_)
  out
}

# One entry per unique `shared` key, first occurrence (formula order) winning
# for the node used to drive `build_ui_for`. Entries:
# list(key = chr, node = ptr_placeholder, ns_id = canonical id,
#      occurrences = list of nodes, label_override = chr or NULL).
# `occurrences` holds every shared node for the key in *this* tree (callers
# spanning several trees -- e.g. `ptr_app_grid()` -- union these before
# deriving a label). `label_override` is the label for this tree alone:
# non-NULL only when the key is referenced under more than one parameter, so
# a single per-param copy label would mislead (see `shared_widget_label()`).
collect_shared_placeholders <- function(tree) {
  order <- character()
  buckets <- list()
  ptr_walk(tree, function(n) {
    if (is_shared_placeholder(n)) {
      k <- n$shared
      if (!(k %in% order)) order <<- c(order, k)
      buckets[[k]] <<- c(buckets[[k]] %||% list(), list(n))
    }
  }, prune = is_shared_placeholder)
  lapply(order, function(k) {
    nodes <- buckets[[k]]
    rep_node <- nodes[[1L]]
    # When a shared widget serves more than one distinct param (e.g.
    # `alpha = num(shared = 'lvl')` *and* `size = num(shared = 'lvl')`),
    # the first occurrence's `param` would otherwise drag a param-specific
    # rule (`rules$params$alpha$num`) over the user's `defaults$num`
    # override at copy-resolution time. Clearing `param` here makes the
    # resolver fall through to `defaults$<keyword>`, matching the multi-
    # param widget's actual scope. Single-param shared widgets keep their
    # param so the param-specific copy still applies.
    params <- vapply(nodes, function(n) n$param %||% NA_character_,
                     character(1))
    distinct_params <- unique(params[!is.na(params) & nzchar(params) &
                                       params != "__unnamed__"])
    if (length(distinct_params) > 1L) {
      rep_node$param <- NA_character_
    }
    list(key = k, node = rep_node, ns_id = nodes[[1L]]$id,
         occurrences = nodes, label_override = shared_widget_label(nodes))
  })
}


# Pipeline stages whose only placeholders are shared. These stages get no
# checkbox from `build_pipeline_stage_ui()` (it skips shared placeholders),
# so the shared section is responsible for rendering one. Returns a list of
# `list(stage_id, verb, shared_keys)` in tree order.
collect_orphan_shared_stages <- function(tree) {
  out <- list()
  ptr_walk(tree, function(n) {
    if (!is_ptr_call(n) || is.null(n$stage_id)) return()
    phs <- ptr_collect(n, is_ptr_placeholder, prune = is_ptr_placeholder)
    if (length(phs) == 0L) return()
    if (!all(vapply(phs, is_shared_placeholder, logical(1)))) return()
    keys <- unique(vapply(phs, function(p) p$shared, character(1)))
    out[[length(out) + 1L]] <<- list(
      stage_id = n$stage_id,
      verb = bare_call_name(n$fun) %||% NA_character_,
      shared_keys = keys
    )
  })
  out
}


# Per-key orphan-stage info across one or many trees. Returns a named list
# `<key> -> list(verbs = chr, stages = list(list(tree_idx, stage_id)))`,
# only for keys with at least one orphan stage somewhere. Used by
# `ptr_shared_ui()` (which renders one synthetic checkbox per key) and by
# `ptr_shared_server()` (which exposes a reactive per key for mirroring).
collect_shared_stage_keys <- function(trees) {
  if (is_ptr_node(trees)) trees <- list(trees)
  by_key <- list()
  for (i in seq_along(trees)) {
    for (st in collect_orphan_shared_stages(trees[[i]])) {
      for (k in st$shared_keys) {
        b <- by_key[[k]] %||% list(verbs = character(), stages = list())
        if (!is.na(st$verb) && nzchar(st$verb)) {
          b$verbs <- unique(c(b$verbs, st$verb))
        }
        b$stages[[length(b$stages) + 1L]] <- list(
          tree_idx = i, stage_id = st$stage_id
        )
        by_key[[k]] <- b
      }
    }
  }
  by_key
}

# Wrap the shared-section widgets in `.ptr-stage` blocks for any pipeline
# stage that hosts only shared placeholders. The head checkbox uses
# `ns_fn(stage_id)` so `ptr_setup_stage_enabled()` picks it up via the same
# observer wiring as the non-shared per-layer Data panel -- one canonical
# stage_id input, one path into `state$stage_enabled`, one `disable_walk()`
# pass at evaluation. Widget order follows `entries`; subsequent widgets
# whose key belongs to an already-emitted stage are folded into a bare
# `.ptr-stage-fields` div (mirrors the `seen` guard in
# `build_pipeline_stage_ui()`).
wrap_shared_widgets_with_stage_blocks <- function(entries, widgets,
                                                  orphan_stages, ns_fn) {
  key_to_stage <- list()
  for (st in orphan_stages) {
    for (k in st$shared_keys) {
      if (is.null(key_to_stage[[k]])) key_to_stage[[k]] <- st
    }
  }
  if (length(key_to_stage) == 0L) return(widgets)

  out <- list()
  emitted <- character()
  for (i in seq_along(entries)) {
    w <- widgets[[i]]
    if (is.null(w)) next
    st <- key_to_stage[[entries[[i]]$key]]
    if (is.null(st)) {
      out[[length(out) + 1L]] <- w
      next
    }
    if (st$stage_id %in% emitted) {
      out[[length(out) + 1L]] <- controllable_region_continuation(w)
      next
    }
    emitted <- c(emitted, st$stage_id)
    has_verb <- !is.null(st$verb) && !is.na(st$verb) && nzchar(st$verb)
    head_label <- if (has_verb) {
      shiny::tags$code(paste0(st$verb, "()"))
    } else NULL
    out[[length(out) + 1L]] <- controllable_region(
      st$stage_id, head_label, w, ns_fn = ns_fn
    )
  }
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
    layer_picker_label = ptr_resolve_ui_text("layer_picker", ui_text = ui_text)$label,
    update_plot_label = ptr_resolve_ui_text("draw_button", ui_text = ui_text)$label
  )
}

# ---- internal helper ----

# Structural layer/pipeline-stage CSS + the `ptr_set_class` custom-message
# handler used to grey out a layer panel when its include-checkbox is
# unticked. Shipped as files (inst/www/ggpaintr-layer.{css,js}) and emitted
# as an htmlDependency (name "ggpaintr-layer") so htmltools dedupes it to a
# single <head> injection no matter how many module UIs a page nests --
# replacing the old hand-rolled `window.__ptr_set_class_registered` JS guard.
# `ptr_app_bslib()` includes only this dependency (never `core_assets_dep()`)
# because the layout rules are theme-agnostic but the cosmetic ggpaintr.css
# is Bootstrap-3/.ptr-app scoped.
ptr_layer_assets <- function() {
  htmltools::htmlDependency(
    name = "ggpaintr-layer",
    version = as.character(utils::packageVersion("ggpaintr")),
    src = c(file = system.file("www", package = "ggpaintr")),
    stylesheet = "ggpaintr-layer.css",
    script = "ggpaintr-layer.js",
    all_files = FALSE
  )
}

# Cosmetic assets for the polished default app shell: the bundled
# stylesheet (inst/www/ggpaintr.css, scoped under .ptr-app) plus the
# client-only JS that powers the code mini-window (toggle from the </>
# icon, drag by its title bar, copy-to-clipboard). All behaviour is
# DOM-local -- it scopes lookups to the nearest .ptr-output ancestor -- so
# a page with several ggpaintr modules (e.g. ptr_app_grid()) works
# correctly. Emitted as an htmlDependency (name "ggpaintr") so htmltools
# dedupes it to a single <head> injection regardless of how many shells /
# pieces a page nests -- replacing the old hand-rolled
# `window.__ptr_ui_assets_registered` JS guard.
core_assets_dep <- function() {
  www <- system.file("www", package = "ggpaintr")
  # The CSS/JS ride as a deduped htmlDependency, but ptr_ui_header() still
  # references the logo by the legacy absolute path "ggpaintr/..."; keep the
  # whole www dir served under that resource prefix so the logo (and any
  # other absolute-path asset) resolves regardless of the dependency lib path.
  if (nzchar(www)) {
    shiny::addResourcePath("ggpaintr", www)
  }
  htmltools::htmlDependency(
    name = "ggpaintr",
    version = as.character(utils::packageVersion("ggpaintr")),
    src = c(file = www),
    stylesheet = "ggpaintr.css",
    script = "ggpaintr-ui.js",
    all_files = FALSE
  )
}

# Resolve user-supplied stylesheet paths into <link> tags, served as static
# resources (not inlined) so relative url(...) refs and HTTP caching work.
# Each distinct parent directory is registered once under a hash-derived
# prefix; re-running is idempotent (addResourcePath overwrites a same-named
# prefix harmlessly). Emitted *after* ggpaintr.css so user rules win on equal
# specificity. Returns NULL when `css` is NULL so callers can splice it
# unconditionally into a tagList().
ptr_user_css_assets <- function(css) {
  if (is.null(css)) {
    return(NULL)
  }
  assertthat::assert_that(
    is.character(css), length(css) >= 1L, all(nzchar(css))
  )
  missing_files <- css[!file.exists(css)]
  if (length(missing_files) > 0L) {
    rlang::abort(c(
      "`css` file(s) not found:",
      rlang::set_names(missing_files, "x")
    ))
  }
  bad_ext <- css[!grepl("\\.css$", css, ignore.case = TRUE)]
  if (length(bad_ext) > 0L) {
    rlang::abort(c(
      "`css` paths must point to `.css` files:",
      rlang::set_names(bad_ext, "x")
    ))
  }
  links <- lapply(css, function(path) {
    dir <- normalizePath(dirname(path), mustWork = TRUE)
    prefix <- paste0("ggpaintr-user-", substr(rlang::hash(dir), 1L, 12L))
    shiny::addResourcePath(prefix, dir)
    shiny::tags$link(
      rel = "stylesheet", type = "text/css",
      href = paste0(prefix, "/", basename(path))
    )
  })
  do.call(shiny::tagList, links)
}


# Single asset bundle used by every raw-Shiny entry point. Emits, in order:
#   1. ptr_layer_assets()       -- htmlDependency "ggpaintr-layer":
#      structural stage CSS + the ptr_set_class custom-message handler.
#   2. core_assets_dep()        -- htmlDependency "ggpaintr": ggpaintr.css
#      (the cosmetic theme, scoped under .ptr-app) + the code-window JS.
#   3. ptr_user_css_assets(css) -- user override stylesheets, linked *after*
#      ggpaintr.css so equal-specificity rules win.
#
# Components 1 & 2 are htmlDependency objects, so htmltools dedupes each to
# a single <head> injection no matter how many times the bundle is emitted
# on a page. ptr_module_ui() emits the bundle once; ptr_app_bslib()
# emits it twice (sidebar + card .ptr-app scopes) -- htmltools collapses
# any number of emissions to one of each dependency. (Component 3 is
# plain <link> tags; each
# distinct user stylesheet is registered once via addResourcePath, which
# is itself idempotent for the same prefix.)
#
# The internal entry point; the exported wrapper is ptr_ui_assets().
# Note: ptr_app_bslib() must NOT link ggpaintr.css (its rules are gated on
# .ptr-app, which the bslib page chrome does not provide); it includes
# only the "ggpaintr-layer" dependency for structural stage CSS.
ptr_assets <- function(css = NULL) {
  shiny::tagList(
    ptr_layer_assets(),
    core_assets_dep(),
    ptr_user_css_assets(css)
  )
}

#' Bundled CSS / JS Assets Piece for `ggpaintr`
#'
#' The full `ggpaintr` asset bundle as a [shiny::tagList()]: the
#' structural-layer dependency (`ptr_set_class` handler + stage CSS), the
#' cosmetic `ggpaintr.css` theme dependency + code-window JavaScript, and
#' any user override stylesheets. The CSS/JS ship as
#' [htmltools::htmlDependency()] objects, so emitting this anywhere on a
#' page — even several times — yields exactly one `<head>` injection of
#' each. The single-piece UI builders ([ptr_ui_plot()],
#' [ptr_ui_controls()], ...) emit **no** assets; an L3 page that composes
#' pieces by hand normally gets them from the page shell, or includes
#' this directly for a non-`fluidPage` root. The bundled apps and the
#' `ptr_*_ui()` composites inject it for you.
#'
#' @param css Optional character vector of paths to additional CSS files;
#'   linked after `ggpaintr`'s bundled stylesheet so its rules win. See
#'   [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A [shiny::tagList()].
#' @seealso [ptr_ui_plot()], [ptr_ui_controls()], [ptr_ui_toggle_code()], [ptr_css()]
#' @export
ptr_ui_assets <- function(css = NULL) {
  ptr_assets(css = css)
}

# Decide whether the resolved `copy` leaf list can be forwarded to a
# `build_ui` hook as `copy =`. Third-party hooks declaring only
# `function(node, label)` (no `...`, no `copy`) must not break.
build_ui_copy_args <- function(fmls, copy) {
  if ("..." %in% fmls || "copy" %in% fmls) list(copy = copy) else list()
}

invoke_build_ui <- function(node, ui_text, layer_name,
                            ns_fn, extra,
                            param_override = NULL, label_suffix = NULL,
                            label_override = NULL, ...) {
  rendered_node <- node
  rendered_node$id <- ns_fn(node$id)
  copy <- ptr_resolve_ui_text(
    "control",
    keyword = node$keyword,
    param = param_override %||% node$param,
    layer_name = layer_name,
    ui_text = ui_text
  )
  if (!is.null(label_override)) {
    copy$label <- label_override
  }
  if (!is.null(label_suffix) && nzchar(label_suffix) && !is.null(copy$label)) {
    # `label_suffix` is " in verb()". For an unnamed positional arg the verb is
    # also fed into the `{param}` slot of the copy template (param_override),
    # so a default like "Enter a number for {param}" already resolves to
    # "...for head()". Appending " in head()" on top of that double-names the
    # verb -- only append when the label does not already mention it.
    verb_token <- trimws(sub("^\\s*in\\s+", "", label_suffix))
    if (!nzchar(verb_token) || !grepl(verb_token, copy$label, fixed = TRUE)) {
      copy$label <- paste0(copy$label, label_suffix)
    }
  }
  entry <- ptr_registry_lookup(node$keyword)
  if (is.null(entry) || is.null(entry$build_ui)) {
    rlang::abort(paste0(
      "Placeholder `", node$keyword, "` has no `build_ui` function. Pass ",
      "`build_ui = function(node, label, ...)` when registering it -- see ",
      "`?ptr_define_placeholder_value`."
    ))
  }
  extra_named <- build_ui_copy_args(names(formals(entry$build_ui)), copy)
  do.call(entry$build_ui,
          c(list(rendered_node, label = copy$label), extra_named, extra,
            list(...)))
}
