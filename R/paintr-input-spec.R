# P7 — input-spec. One walk over the typed tree, emitting a row per node
# that needs a Shiny binding. Columns: `input_id`, `role`, `layer_name`,
# `keyword`, `param_key`, `source_id`, `shared`. Order: placeholder rows
# first (with companions adjacent), then derived layer rows.

.spec_columns <- c(
  "input_id", "role", "layer_name", "keyword",
  "param_key", "source_id", "shared"
)

#' Runtime input spec for a typed AST (internal)
#'
#' Walks the typed AST and returns one row per node that needs a Shiny
#' binding: placeholder rows first (with companion rows adjacent for
#' upload-style sources), then derived layer rows (`layer_checkbox`,
#' `stage_enabled`).
#'
#' @param node A `ptr_root` node returned by `ptr_translate()`.
#'
#' @return A `data.frame` with columns `input_id`, `role`, `layer_name`,
#'   `keyword`, `param_key`, `source_id`, and `shared`.
#' @noRd
ptr_runtime_input_spec <- function(node) {
  if (!is_ptr_root(node)) {
    rlang::abort("ptr_runtime_input_spec expects a ptr_root.")
  }
  ph_rows <- list()
  layer_rows <- list()
  stage_rows <- list()
  for (layer in node$layers) {
    if (is_ptr_layer(layer)) {
      pls <- collect_layer_placeholders(layer)
      for (pl in pls) {
        ph_rows[[length(ph_rows) + 1L]] <- placeholder_row(pl, layer$name)
        if (is_ptr_ph_data_source(pl) && !is.null(pl$shortcut_id)) {
          ph_rows[[length(ph_rows) + 1L]] <- companion_row(pl, layer$name)
        }
      }
      if (!is.null(layer$active_input_id)) {
        layer_rows[[length(layer_rows) + 1L]] <- empty_row(
          input_id = layer$active_input_id,
          role = "layer_checkbox",
          layer_name = layer$name
        )
      }
      for (sid in collect_stage_ids(layer)) {
        stage_rows[[length(stage_rows) + 1L]] <- empty_row(
          input_id = sid,
          role = "stage_enabled",
          layer_name = layer$name
        )
      }
    } else if (is_ptr_placeholder(layer)) {
      ph_rows[[length(ph_rows) + 1L]] <- placeholder_row(
        layer, layer$name %||% layer$keyword
      )
      if (is_ptr_ph_data_source(layer) && !is.null(layer$shortcut_id)) {
        ph_rows[[length(ph_rows) + 1L]] <- companion_row(
          layer, layer$name %||% layer$keyword
        )
      }
    }
  }
  rows_to_data_frame(c(ph_rows, layer_rows, stage_rows))
}

collect_layer_placeholders <- function(layer) {
  out <- list()
  visit <- function(x) {
    if (is_ptr_placeholder(x)) {
      out[[length(out) + 1L]] <<- x
      return(invisible())
    }
    if (is_ptr_call(x)) {
      for (a in x$args) visit(a)
      return(invisible())
    }
    if (is_ptr_pipeline(x)) {
      for (s in x$stages) visit(s)
      return(invisible())
    }
    if (is_ptr_closure(x)) {
      visit(x$body)
      return(invisible())
    }
  }
  if (!is.null(layer$data_arg)) visit(layer$data_arg)
  if (length(layer$children) > 0L) {
    for (c in layer$children) visit(c)
  }
  out
}

placeholder_row <- function(pl, layer_name) {
  list(
    input_id = pl$id %||% NA_character_,
    role = "placeholder",
    layer_name = layer_name %||% NA_character_,
    keyword = pl$keyword %||% NA_character_,
    param_key = pl$param %||% NA_character_,
    source_id = if (is_ptr_ph_data_source(pl)) pl$id %||% NA_character_ else NA_character_,
    shared = pl$shared %||% NA_character_
  )
}

companion_row <- function(pl, layer_name) {
  list(
    input_id = pl$shortcut_id %||% NA_character_,
    role = "source_companion",
    layer_name = layer_name %||% NA_character_,
    keyword = pl$keyword %||% NA_character_,
    param_key = pl$param %||% NA_character_,
    source_id = pl$id %||% NA_character_,
    shared = pl$shared %||% NA_character_
  )
}

empty_row <- function(input_id, role, layer_name) {
  list(
    input_id = input_id,
    role = role,
    layer_name = layer_name,
    keyword = NA_character_,
    param_key = NA_character_,
    source_id = NA_character_,
    shared = NA_character_
  )
}

rows_to_data_frame <- function(rows) {
  if (length(rows) == 0L) {
    return(data.frame(
      input_id = character(),
      role = character(),
      layer_name = character(),
      keyword = character(),
      param_key = character(),
      source_id = character(),
      shared = character(),
      stringsAsFactors = FALSE
    ))
  }
  cols <- lapply(.spec_columns, function(col) {
    vapply(rows, function(r) as.character(r[[col]] %||% NA_character_), character(1))
  })
  names(cols) <- .spec_columns
  as.data.frame(cols, stringsAsFactors = FALSE)
}

# ---- ptr_id_table: public id enumerator -----------------------------------
#
# `ptr_id_table(formula, id)` walks the same typed AST but emits one row per
# Shiny id ever rendered by `ptr_ui()` / `ptr_server()` for the formula:
# placeholder sleeves AND inner widgets, source companions, layer-level
# controls (checkbox, subtab, content container), pipeline stage toggles,
# plus the static infrastructure ids (plot/error/code/code-mode/update).
# `ptr_runtime_input_spec()` above is intentionally narrower — it covers
# only the server-side bindings; this one targets L3 users hand-building
# their own UI panel and so includes the sleeve/output ids too.

.id_table_columns <- c(
  "id", "kind", "role", "scope", "include_in_ui",
  "layer", "keyword", "param", "parent_call", "shared"
)

.id_table_static_infra <- list(
  list(id = "ptr_update_plot", kind = "input_widget", role = "ptr_update_plot"),
  list(id = "ptr_plot",        kind = "output_slot", role = "ptr_plot"),
  list(id = "ptr_error",       kind = "output_slot", role = "ptr_error"),
  list(id = "ptr_code",        kind = "output_slot", role = "ptr_code"),
  list(id = "ptr_code_mode",   kind = "input_widget", role = "ptr_code_mode")
)

#' Enumerate every Shiny id a ggpaintr formula produces
#'
#' Walks the typed AST of a single formula and returns one row per Shiny
#' id ever rendered by [`ptr_ui()`] / [`ptr_server()`] for that formula:
#' placeholder sleeves, inner widgets, source companions, layer-level
#' controls (checkbox, subtab, content container), pipeline stage
#' toggles, plus the static infrastructure ids (`ptr_plot`, `ptr_error`,
#' `ptr_code`, `ptr_code_mode`, `ptr_update_plot`).
#'
#' Advanced (L3) users call this to build their own UI by hand and still
#' get the server's bindings to match. The default-layout L2 path does
#' not need it; `ptr_ui()` emits these ids internally.
#'
#' @param formula A single ggpaintr formula string (the same input you
#'   would pass to `ptr_app()` / `ptr_server()`).
#' @param id Optional outer namespace — the same string you pass to
#'   `ptr_server(formula, id = ...)`. When supplied, rows whose `scope`
#'   is `"instance"` come back with that namespace prefixed
#'   (`<id>-<raw>`); `"global"` rows stay bare. When `NULL`, every row
#'   shows its bare id.
#'
#' @return A `data.frame` with one row per Shiny id and ten columns:
#'   * `id` — the Shiny id (prefixed when `id=` is given and
#'     `scope == "instance"`).
#'   * `kind` — `"input_widget"` or `"output_slot"`.
#'   * `role` — semantic role: `"placeholder"`, `"source_companion"`,
#'     `"layer_checkbox"`, `"layer_subtab"`, `"layer_content"`,
#'     `"stage_enabled"`, `"ptr_plot"`, `"ptr_error"`, `"ptr_code"`,
#'     `"ptr_code_mode"`, `"ptr_update_plot"`.
#'   * `scope` — `"instance"` (namespaced via `shiny::NS(id)`) or
#'     `"global"` (un-namespaced; only used by cross-formula shared-panel
#'     keys in a `ptr_shared()` setup — see Single-formula section).
#'   * `include_in_ui` — `TRUE` when the row is something the user
#'     *places* in their custom UI; `FALSE` when the server populates
#'     it inside a sleeve (`<id>_ui`) and the user must not place it
#'     manually. The two `FALSE` cases are the inner placeholder widget
#'     and `ppUpload`'s file-name companion.
#'   * `layer` — layer name (`"ggplot"`, `"geom_point"`, …) or `NA`.
#'   * `keyword` — placeholder keyword (`"ppVar"`/`"ppText"`/…) or `NA`.
#'   * `param` — argument or aesthetic name (`"x"`, `"color"`, `"data"`,
#'     …) or `NA`.
#'   * `parent_call` — immediate enclosing call (`"aes"`, `"head"`, or
#'     the layer itself when the placeholder is a direct layer arg) or
#'     `NA`.
#'   * `shared` — shared key (`"xcol"`, …) or `NA`.
#'
#' @section Stability under formula edits:
#' Adding a layer at the end keeps existing ids stable. Reordering
#' arguments inside `aes()` or a pipeline shifts positional paths and
#' therefore changes ids; renaming a placeholder keyword or its
#' `shared=` annotation also changes ids.
#'
#' @section Single-formula:
#' `ptr_id_table()` accepts a single formula. In multi-instance
#' (`ptr_shared(formulas = list(…))`) layouts the partition rule
#' decides whether `shared_<key>` lives in an instance's inline section
#' or the cross-formula panel. The `scope` column reflects the
#' single-formula interpretation (`"instance"`); when embedding into a
#' shared-panel context those rows are bare (`"global"`) and you should
#' override accordingly.
#'
#' @examples
#' ptr_id_table("ggplot(ppUpload, aes(x = ppVar, y = ppVar)) + geom_point(color = ppText)")
#' ptr_id_table("ggplot(ppUpload, aes(x = ppVar))", id = "myplot")
#' @export
ptr_id_table <- function(formula, id = NULL) {
  assertthat::assert_that(
    is.character(formula), length(formula) == 1L, !is.na(formula),
    msg = "`formula` must be a single string."
  )
  if (!is.null(id)) {
    assertthat::assert_that(
      is.character(id), length(id) == 1L, !is.na(id), nzchar(id),
      msg = "`id` must be a single non-empty string or NULL."
    )
  }
  tree <- ptr_translate(formula)
  rows <- collect_id_table_rows(tree)
  if (!is.null(id)) {
    ns_fn <- shiny::NS(id)
    for (i in seq_along(rows)) {
      if (identical(rows[[i]]$scope, "instance")) {
        rows[[i]]$id <- ns_fn(rows[[i]]$id)
      }
    }
  }
  id_table_rows_to_df(rows)
}

# Top-level walker: static infra first, then per-layer rows, then
# placeholder rows (sleeve + inner + companion) collected layer-by-layer.
collect_id_table_rows <- function(tree) {
  rows <- list()
  emit <- function(...) rows[[length(rows) + 1L]] <<- list(...)

  for (spec in .id_table_static_infra) {
    emit(
      id = spec$id, kind = spec$kind, role = spec$role,
      scope = "instance", include_in_ui = TRUE,
      layer = NA_character_, keyword = NA_character_,
      param = NA_character_, parent_call = NA_character_,
      shared = NA_character_
    )
  }

  for (layer in tree$layers) {
    if (!is_ptr_layer(layer)) next
    layer_name <- layer$name

    if (!is.null(layer$active_input_id)) {
      emit(
        id = layer$active_input_id, kind = "input_widget",
        role = "layer_checkbox", scope = "instance", include_in_ui = TRUE,
        layer = layer_name, keyword = NA_character_,
        param = NA_character_, parent_call = NA_character_,
        shared = NA_character_
      )
    }

    emit(
      id = layer_panel_content_id(layer_name), kind = "output_slot",
      role = "layer_content", scope = "instance", include_in_ui = TRUE,
      layer = layer_name, keyword = NA_character_,
      param = NA_character_, parent_call = NA_character_,
      shared = NA_character_
    )

    if (length(find_layer_placeholders_with_stage(layer$data_arg)) > 0L) {
      emit(
        id = paste0(layer_name, "_subtab"), kind = "input_widget",
        role = "layer_subtab", scope = "instance", include_in_ui = TRUE,
        layer = layer_name, keyword = NA_character_,
        param = NA_character_, parent_call = NA_character_,
        shared = NA_character_
      )
    }

    placeholders <- walk_layer_placeholders(layer)
    for (item in placeholders) {
      emit_placeholder_rows(item$ph, item$parent_call, layer_name, emit)
    }

    for (sid in collect_stage_ids(layer)) {
      emit(
        id = sid, kind = "input_widget", role = "stage_enabled",
        scope = "instance", include_in_ui = TRUE,
        layer = layer_name, keyword = NA_character_,
        param = NA_character_, parent_call = NA_character_,
        shared = NA_character_
      )
    }
  }

  rows
}

# Three rows per non-source placeholder (sleeve + inner); four for a
# source with a companion (sleeve + inner + companion). The inner row
# and the companion row carry include_in_ui = FALSE because the server
# renders them *inside* the sleeve via renderUI; the L3 user must not
# place them manually.
emit_placeholder_rows <- function(ph, parent_call, layer_name, emit) {
  shared_key <- ph$shared %||% NA_character_
  scope <- "instance"
  raw_id <- ph$id
  sleeve_id <- placeholder_output_id(raw_id)
  base <- list(
    layer = layer_name %||% NA_character_,
    keyword = ph$keyword %||% NA_character_,
    param = ph$param %||% NA_character_,
    parent_call = parent_call %||% NA_character_,
    shared = shared_key
  )
  emit(
    id = sleeve_id, kind = "output_slot", role = "placeholder",
    scope = scope, include_in_ui = TRUE,
    layer = base$layer, keyword = base$keyword, param = base$param,
    parent_call = base$parent_call, shared = base$shared
  )
  emit(
    id = raw_id, kind = "input_widget", role = "placeholder",
    scope = scope, include_in_ui = FALSE,
    layer = base$layer, keyword = base$keyword, param = base$param,
    parent_call = base$parent_call, shared = base$shared
  )
  if (is_ptr_ph_data_source(ph) && !is.null(ph$shortcut_id)) {
    emit(
      id = ph$shortcut_id, kind = "input_widget",
      role = "source_companion", scope = scope, include_in_ui = FALSE,
      layer = base$layer, keyword = base$keyword, param = base$param,
      parent_call = base$parent_call, shared = base$shared
    )
  }
}

# Layer-scoped placeholder walk that tracks the *immediate* enclosing
# call name as `parent_call`:
#   * direct layer arg (data_arg or children entry) → parent_call = layer name
#   * inside a ptr_call's args (`aes(...)`, `head(...)`)   → parent_call = call$fun
#   * inside a pipeline stage call's args                  → parent_call = stage$fun
# Same placeholder may appear under multiple parents (e.g. `upload` as
# data_arg of `ggplot()` *and* in the upstream chain of children) — we
# de-duplicate by `ph$id` so each id renders once.
walk_layer_placeholders <- function(layer) {
  seen_ids <- character()
  out <- list()
  push <- function(ph, parent_call) {
    if (!is.null(ph$id) && ph$id %in% seen_ids) return()
    if (!is.null(ph$id)) seen_ids <<- c(seen_ids, ph$id)
    out[[length(out) + 1L]] <<- list(ph = ph, parent_call = parent_call)
  }
  visit <- function(n, parent_call) {
    if (is.null(n)) return()
    if (is_ptr_placeholder(n)) {
      push(n, parent_call)
      return()
    }
    if (is_ptr_call(n)) {
      this_call <- bare_call_name(n$fun) %||% NA_character_
      for (a in n$args) visit(a, this_call)
      return()
    }
    if (is_ptr_pipeline(n)) {
      for (s in n$stages) visit(s, parent_call)
      return()
    }
    if (is_ptr_closure(n)) {
      visit(n$body, parent_call)
      return()
    }
    # ptr_literal / ptr_missing / ptr_user_expr: no descendants.
  }
  if (!is.null(layer$data_arg)) visit(layer$data_arg, layer$name)
  if (length(layer$children) > 0L) {
    for (c in layer$children) visit(c, layer$name)
  }
  out
}

id_table_rows_to_df <- function(rows) {
  if (length(rows) == 0L) {
    return(data.frame(
      id = character(), kind = character(), role = character(),
      scope = character(), include_in_ui = logical(),
      layer = character(), keyword = character(), param = character(),
      parent_call = character(), shared = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    id           = vapply(rows, function(r) as.character(r$id), character(1)),
    kind         = vapply(rows, function(r) as.character(r$kind), character(1)),
    role         = vapply(rows, function(r) as.character(r$role), character(1)),
    scope        = vapply(rows, function(r) as.character(r$scope), character(1)),
    include_in_ui = vapply(rows, function(r) isTRUE(r$include_in_ui), logical(1)),
    layer        = vapply(rows, function(r) as.character(r$layer %||% NA_character_), character(1)),
    keyword      = vapply(rows, function(r) as.character(r$keyword %||% NA_character_), character(1)),
    param        = vapply(rows, function(r) as.character(r$param %||% NA_character_), character(1)),
    parent_call  = vapply(rows, function(r) as.character(r$parent_call %||% NA_character_), character(1)),
    shared       = vapply(rows, function(r) as.character(r$shared %||% NA_character_), character(1)),
    stringsAsFactors = FALSE
  )
}
