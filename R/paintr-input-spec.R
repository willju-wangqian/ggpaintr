# P7 — input-spec. One walk over the typed tree, emitting a row per node
# that needs a Shiny binding. Columns: `input_id`, `role`, `layer_name`,
# `keyword`, `param_key`, `source_id`, `shared`. Order: placeholder rows
# first (with companions adjacent), then derived layer rows.

.spec_columns <- c(
  "input_id", "role", "layer_name", "keyword",
  "param_key", "source_id", "shared"
)

#' Runtime Input Spec for a Typed AST
#'
#' Walks the typed AST and returns one row per node that needs a Shiny
#' binding: placeholder rows first (with companion rows adjacent for
#' upload-style sources), then derived layer rows (`layer_checkbox`,
#' `layer_update_data`, `stage_enabled`).
#'
#' @param node A `ptr_root` node returned by `ptr_translate()`.
#'
#' @return A `data.frame` with columns `input_id`, `role`, `layer_name`,
#'   `keyword`, `param_key`, `source_id`, and `shared`.
#' @export
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
        if (is_ptr_ph_data_source(pl) && !is.null(pl$companion_id)) {
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
      if (!is.null(layer$update_data_input_id)) {
        layer_rows[[length(layer_rows) + 1L]] <- empty_row(
          input_id = layer$update_data_input_id,
          role = "layer_update_data",
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
      if (is_ptr_ph_data_source(layer) && !is.null(layer$companion_id)) {
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
    input_id = pl$companion_id %||% NA_character_,
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
