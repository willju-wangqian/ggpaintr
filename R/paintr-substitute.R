# P8 — substitute. Walk the typed tree applying `input_snapshot` and
# `shared_bindings`. Each placeholder is replaced by a `ptr_literal`
# (resolved value), `ptr_user_expr` (expr-keyword provenance), or
# `ptr_missing` (input absent / empty / NA per built-in semantics).
# Layer activation: if the layer's `active_input_id` is in the snapshot
# and FALSE, the layer is marked `active = FALSE` for P9 to drop.
#
# PLAN-07 invariant: `node$default` (set by the PLAN-06 parser) is
# consumed only at boot, via the UI layer (`R/paintr-build-ui.R` +
# `R/paintr-builtins.R` seed the widget's initial value from it).
# Substitute reads the resolved runtime value from `input_snapshot`
# alone -- it never falls back to `node$default`. Once Shiny boots,
# every input slot has a value (or has been emptied by the user),
# so the snapshot is authoritative.

ptr_substitute <- function(node, input_snapshot = list(),
                          shared_bindings = list(),
                          eval_env = parent.frame(),
                          upstream_cols = list(),
                          upstream_data = list()) {
  ctx <- list(
    snapshot = input_snapshot,
    shared = shared_bindings,
    eval_env = eval_env,
    upstream_cols = upstream_cols,
    upstream_data = upstream_data
  )
  substitute_walk(node, ctx)
}

substitute_walk <- function(node, ctx) UseMethod("substitute_walk")

#' @export
substitute_walk.ptr_root <- function(node, ctx) {
  for (i in seq_along(node$layers)) {
    node$layers[[i]] <- substitute_walk(node$layers[[i]], ctx)
  }
  node
}

#' @export
substitute_walk.ptr_layer <- function(node, ctx) {
  if (!is.null(node$active_input_id)) {
    cb <- ctx$snapshot[[node$active_input_id]]
    node$active <- !isFALSE(cb)
  }
  if (!is.null(node$data_arg)) {
    node$data_arg <- substitute_walk(node$data_arg, ctx)
  }
  if (length(node$children) > 0L) {
    for (i in seq_along(node$children)) {
      node$children[[i]] <- substitute_walk(node$children[[i]], ctx)
    }
  }
  node
}

#' @export
substitute_walk.ptr_call <- function(node, ctx) {
  for (i in seq_along(node$args)) {
    node$args[[i]] <- substitute_walk(node$args[[i]], ctx)
  }
  node
}

#' @export
substitute_walk.ptr_pipeline <- function(node, ctx) {
  for (i in seq_along(node$stages)) {
    node$stages[[i]] <- substitute_walk(node$stages[[i]], ctx)
  }
  node
}

#' @export
substitute_walk.ptr_closure <- function(node, ctx) {
  node$body <- substitute_walk(node$body, ctx)
  node
}

#' @export
substitute_walk.ptr_user_expr <- function(node, ctx) node

#' @export
substitute_walk.ptr_literal <- function(node, ctx) node

#' @export
substitute_walk.ptr_missing <- function(node, ctx) node

#' @export
substitute_walk.ptr_ph_value <- function(node, ctx) {
  value <- read_placeholder_value(node, ctx)
  if (is_missing_value_input(node, value)) return(ptr_missing())
  entry <- ptr_registry_lookup(node$keyword)
  if (is.null(entry)) {
    rlang::abort(paste0(
      "Placeholder `", node$keyword, "` is not registered. Register it with ",
      "`ptr_define_placeholder_value()`, `ptr_define_placeholder_consumer()`, ",
      "or `ptr_define_placeholder_source()` before launching -- see ",
      "`?ptr_define_placeholder_value`."
    ))
  }
  if (!is.null(entry$validate_input)) {
    # Value-role ctx: `upstream_cols` / `data` are always NULL by contract
    # (value placeholders have no upstream column scope; `data_aware = FALSE`
    # on the registry entry). Hook still receives `node` + `keyword` so the
    # validator can branch on the keyword if it's reused across registrations.
    hook_ctx <- list(
      node = node, keyword = node$keyword,
      upstream_cols = NULL, data = NULL
    )
    ok <- entry$validate_input(value, hook_ctx)
    # Accept TRUE or NULL as "valid" -- NULL is the standard R idiom for
    # "no message to report". A character vector is the error message.
    # Any other return value fails closed with a generic message.
    if (!is.null(ok) && !isTRUE(ok)) {
      rlang::abort(paste0(
        "Invalid value for placeholder `", node$keyword, "`: ",
        if (is.character(ok)) ok else "validation failed."
      ))
    }
  }
  resolved <- entry$resolve_expr(value, node)
  if (is.null(resolved)) return(ptr_missing())
  validate_resolve_expr_return(resolved, node$keyword)
  if (node$keyword == "ppExpr") {
    return(ptr_user_expr(resolved))
  }
  ptr_literal(resolved)
}

#' @export
substitute_walk.ptr_ph_data_consumer <- function(node, ctx) {
  value <- read_placeholder_value(node, ctx)
  # Empty-string sentinel from a prepended placeholder choice (e.g. the
  # var picker's "Pick a column" disabled option) is treated as missing —
  # the user has not yet chosen a real value.
  if (is.character(value) && length(value) == 1L && !nzchar(value)) {
    return(ptr_missing())
  }
  if (is_missing_value_input(node, value)) return(ptr_missing())
  # Arity is the placeholder's responsibility: built-in `var` rejects
  # length != 1 in its `validate_input`; custom multi-column consumers
  # (e.g. a `colvars` selectInput with multiple = TRUE) accept vectors
  # and emit `c(...)` from `resolve_expr`.
  entry <- ptr_registry_lookup(node$keyword)
  if (!is.null(entry$validate_input)) {
    cols <- ctx$upstream_cols[[node$id %||% ""]]
    if (!is.null(cols)) {
      # Consumer-role ctx: populate both `upstream_cols` and `data` when
      # the upstream has resolved. `data` is the same data.frame the
      # consumer's `build_ui` received as its `data` arg (see
      # `runtime_upstream_data()` in `R/paintr-server.R`), allowing the
      # validator to inspect column types / ranges / levels and not just
      # the column names.
      hook_ctx <- list(
        node = node, keyword = node$keyword,
        upstream_cols = cols,
        data = ctx$upstream_data[[node$id %||% ""]]
      )
      ok <- entry$validate_input(value, hook_ctx)
      # Accept TRUE or NULL as "valid" -- NULL is the standard R idiom for
      # "no message to report". A character vector is the error message.
      # Any other return value fails closed with a generic message.
      if (!is.null(ok) && !isTRUE(ok)) {
        rlang::abort(paste0(
          "Invalid value for placeholder `", node$keyword, "`: ",
          if (is.character(ok)) ok else "validation failed."
        ))
      }
    }
  }
  resolved <- entry$resolve_expr(value, node)
  if (is.null(resolved)) return(ptr_missing())
  validate_resolve_expr_return(resolved, node$keyword)
  ptr_literal(resolved)
}

#' @export
substitute_walk.ptr_ph_data_source <- function(node, ctx) {
  entry <- ptr_registry_lookup(node$keyword)
  if (!is.null(node$shortcut_id)) {
    # Shortcut-driven source (e.g. `upload`): the shortcut text input
    # carries the binding name; resolve_expr maps name -> symbol.
    name_value <- ctx$snapshot[[node$shortcut_id]]
    has_name_value <- !is.null(name_value) && is.character(name_value) &&
      length(name_value) == 1L && nzchar(name_value)
    if (!has_name_value) {
      # ADR 0025 §5 / PLAN-02: empty shortcut snapshot falls through to
      # `node$auto_name` (the deterministic translate-/runtime-stamped
      # binding name) when present. This is the same symbol the upload
      # binder assigns into eval_env in `resolve_upload_source()`, so the
      # substitute walk emits an expression that resolves at eval-time.
      # When both the snapshot and `node$auto_name` are empty, retain
      # pre-PLAN-02 behaviour (return `ptr_missing()`).
      if (!is.null(node$auto_name) && is.character(node$auto_name) &&
          length(node$auto_name) == 1L && nzchar(node$auto_name)) {
        return(ptr_literal(as.name(node$auto_name)))
      }
      return(ptr_missing())
    }
    if (make.names(name_value) != name_value) {
      rlang::abort(paste0(
        "Upload name `", name_value, "` is not a valid R variable name. ",
        "Use letters, numbers, dots, and underscores, starting with a letter ",
        "-- for example: `my_data`."
      ))
    }
    resolved <- if (!is.null(entry) && !is.null(entry$resolve_expr)) {
      entry$resolve_expr(name_value, node)
    } else {
      rlang::sym(name_value)
    }
  } else {
    # Companion-less source (e.g. a `selectInput`-style chooser): the
    # source's own input value drives resolve_expr directly. NULL/empty
    # value or NULL return from the hook -> prune the slot.
    value <- ctx$snapshot[[node$id %||% ""]]
    if (is.null(value)) return(ptr_missing())
    if (is.null(entry) || is.null(entry$resolve_expr)) return(ptr_missing())
    resolved <- entry$resolve_expr(value, node)
    if (is.null(resolved)) return(ptr_missing())
  }
  validate_resolve_expr_return(resolved, node$keyword)
  ptr_literal(resolved)
}

#' @export
substitute_walk.default <- function(node, ctx) node

# ---- helpers ---------------------------------------------------------------

read_placeholder_value <- function(node, ctx) {
  if (!is.null(node$shared)) {
    rv <- ctx$shared[[node$shared]]
    if (is.null(rv)) return(NULL)
    if (is.function(rv)) {
      return(tryCatch(rv(), error = function(e) NULL))
    }
    return(rv)
  }
  ctx$snapshot[[node$id %||% ""]]
}

is_missing_value_input <- function(node, value) {
  if (is.null(value)) return(TRUE)
  if (length(value) == 0L) return(TRUE)
  if (node$keyword == "ppNum") {
    # numericInput's blank state arrives at the server as logical `NA`
    # (not NA_real_), which earlier checks missed because `is.numeric(NA)`
    # is FALSE. Catch any all-NA scalar regardless of storage type.
    if (length(value) == 1L && is.atomic(value) && is.na(value)) return(TRUE)
    if (is.character(value) && !nzchar(value[[1]])) return(TRUE)
  }
  if (node$keyword == "ppExpr") {
    if (is.character(value) && !nzchar(value[[1]])) return(TRUE)
  }
  if (node$keyword == "ppText") {
    # textInput's blank state is `""`; treat as missing so empty
    # `labs(title = ppText())` collapses to nothing rather than rendering
    # `labs(title = "")`.
    if (is.character(value) && !nzchar(value[[1]])) return(TRUE)
  }
  FALSE
}

validate_resolve_expr_return <- function(x, keyword) {
  ok <- is.null(x) ||
    is.numeric(x) || is.character(x) || is.logical(x) || is.integer(x) ||
    is.language(x) || is.symbol(x) || is.expression(x)
  if (!ok) {
    rlang::abort(paste0(
      "resolve_expr for `", keyword, "` returned an unsupported type: ",
      class(x)[1L]
    ))
  }
  # Spec L98: re-run P5 on code returned by `resolve_expr` before it
  # enters the tree. Translate-time P5 saw the original formula; resolve
  # hooks can synthesize new calls (especially for the `expr` keyword),
  # and the denylist must screen them again.
  if (is.language(x) || is.expression(x)) {
    placeholder_names <- ptr_registry_keywords()
    if (is.expression(x)) {
      for (e in x) {
        validate_expr_safety(e, expr_check = TRUE,
                             placeholder_names = placeholder_names)
      }
    } else {
      validate_expr_safety(x, expr_check = TRUE,
                           placeholder_names = placeholder_names)
    }
  }
}
