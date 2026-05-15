# Shared placeholder widgets across multiple ptr_module_ui()/server() pairs.
#
# `ptr_shared_ui()` renders a single wellPanel hosting one widget per shared
# key declared across the supplied formulas (value placeholders + var
# consumers). `ptr_shared_server()` builds the matching reactives, wires the
# top-level consumer pickers via `ptr_bind_shared_consumer_uis()`, and
# returns a `ptr_shared_state` that the embedder threads into each
# `ptr_module_server(..., shared_state = ...)`.
#
# Design constraints (see dev/plans/shared-ui-multi-instance.html):
#   - NS(NULL) only; one panel per page; flat `shared_<key>` ids.
#   - Errors when no formula declares a shared placeholder (calling the
#     pair is a declaration of intent, not a no-op).
#   - The default sidebar-embedded path under `ptr_app()` is left alone;
#     two shared-widget renderers coexist by design.


# ---- ptr_shared_state ------------------------------------------------------

new_ptr_shared_state <- function(shared, draw_trigger, shared_resolutions,
                                 shared_stage_enabled = list()) {
  structure(
    list(
      shared = shared,
      draw_trigger = draw_trigger,
      shared_resolutions = shared_resolutions,
      shared_stage_enabled = shared_stage_enabled
    ),
    class = c("ptr_shared_state", "list")
  )
}

validate_ptr_shared_state <- function(x) {
  if (!inherits(x, "ptr_shared_state")) {
    rlang::abort("`shared_state` must be a `ptr_shared_state` object created by `ptr_shared_server()`.")
  }
  if (!is.list(x$shared)) {
    rlang::abort("`shared_state$shared` must be a (possibly empty) named list of reactives.")
  }
  if (length(x$shared) > 0L) {
    nms <- names(x$shared)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort("`shared_state$shared` must have unique non-empty names.")
    }
    if (!all(vapply(x$shared, shiny::is.reactive, logical(1)))) {
      rlang::abort("`shared_state$shared` values must be Shiny reactives.")
    }
  }
  if (!is.null(x$draw_trigger) && !shiny::is.reactive(x$draw_trigger)) {
    rlang::abort("`shared_state$draw_trigger` must be a Shiny reactive or NULL.")
  }
  if (!is.list(x$shared_resolutions)) {
    rlang::abort("`shared_state$shared_resolutions` must be a (possibly empty) named list.")
  }
  sse <- x$shared_stage_enabled %||% list()
  if (!is.list(sse)) {
    rlang::abort("`shared_state$shared_stage_enabled` must be a (possibly empty) named list of reactives.")
  }
  if (length(sse) > 0L) {
    nms <- names(sse)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort("`shared_state$shared_stage_enabled` must have unique non-empty names.")
    }
    if (!all(vapply(sse, shiny::is.reactive, logical(1)))) {
      rlang::abort("`shared_state$shared_stage_enabled` values must be Shiny reactives.")
    }
  }
  invisible(x)
}

#' @export
print.ptr_shared_state <- function(x, ...) {
  cat("<ptr_shared_state>\n")
  keys <- names(x$shared) %||% character()
  cat("  shared keys     :",
      if (length(keys)) paste(keys, collapse = ", ") else "<none>", "\n")
  cat("  draw_trigger    :",
      if (is.null(x$draw_trigger)) "<none>" else "<reactive>", "\n")
  consumer_keys <- names(x$shared_resolutions) %||% character()
  cat("  consumer keys   :",
      if (length(consumer_keys)) paste(consumer_keys, collapse = ", ") else "<none>",
      "\n")
  invisible(x)
}


# ---- Plumbing shared between ptr_shared_ui() and ptr_shared_server() -------

# Translate every formula to a typed AST, accepting either a character vector
# or a list of strings (mirrors `ptr_app_grid()` input handling).
shared_translate_formulas <- function(formulas, expr_check) {
  if (is.character(formulas)) formulas <- as.list(formulas)
  assertthat::assert_that(
    is.list(formulas),
    length(formulas) >= 1L,
    all(vapply(formulas, rlang::is_string, logical(1)))
  )
  lapply(formulas, ptr_translate, expr_check = expr_check)
}

# Per-key first-occurrence node for value-shared placeholders, in
# formula-discovery order. The first formula to mention a key wins.
shared_first_nodes <- function(trees) {
  out <- list()
  occ <- list()
  for (tr in trees) {
    for (entry in collect_shared_placeholders(tr)) {
      if (is.null(out[[entry$key]])) out[[entry$key]] <- entry$node
      occ[[entry$key]] <- c(occ[[entry$key]] %||% list(), entry$occurrences)
    }
  }
  list(nodes = out, occurrences = occ)
}

# Representative node per shared `var` consumer key, mirroring the inline
# block in `ptr_app_grid_components()`. The node's `$id` becomes the bare
# shared id (`shared_<key>`) so the host renderUI binds at the right output.
shared_consumer_representatives <- function(trees) {
  occ_buckets <- collect_shared_consumer_occurrences(trees)
  reps <- lapply(names(occ_buckets), function(k) {
    occ <- occ_buckets[[k]]
    n <- occ[[1L]]
    n$id <- canonical_shared_id(k)
    n$shared_label <- shared_widget_label(occ)
    params <- vapply(occ, function(x) x$param %||% NA_character_,
                     character(1))
    distinct_params <- unique(params[!is.na(params) & nzchar(params) &
                                       params != "__unnamed__"])
    if (length(distinct_params) > 1L) n$param <- NA_character_
    n
  })
  names(reps) <- names(occ_buckets)
  reps
}


# ---- ptr_shared_ui ---------------------------------------------------------

#' Render the Shared Widget Panel for a Multi-Instance Embedding
#'
#' Renders one [`shiny::wellPanel()`] that hosts every shared placeholder
#' referenced by the supplied formulas. Drop it into a user-authored Shiny
#' app alongside two or more [`ptr_module_ui()`] instances when those
#' instances need to share a `var`, `text`, `num`, `expr`, or `upload`
#' widget via `shared = "<key>"` annotations.
#'
#' One `ptr_shared_ui()` per page. The companion [`ptr_shared_server()`]
#' must be called in the same Shiny server function, and the resulting
#' `ptr_shared_state` threaded into each [`ptr_module_server()`].
#'
#' Errors when no formula in `formulas` declares a `shared = "..."`
#' annotation -- calling this function is a declaration of intent.
#'
#' @param formulas A character vector or list of formula strings, one per
#'   embedded [`ptr_module_ui()`] instance.
#' @param shared_ui Named list of `function(id) -> shiny.tag` builders, one
#'   per shared key the embedder wants to customise. Unsupplied keys are
#'   auto-rendered from the first formula that mentions them.
#' @param ui_text Optional copy overrides forwarded to the auto-built
#'   widgets (see [`ptr_app()`]'s `ui_text` argument).
#' @param expr_check Whether to validate `expr` placeholders during
#'   formula translation. Default `TRUE`.
#' @param draw_all_label Label for the "Draw all" button. The button is
#'   rendered only when `length(formulas) >= 2`.
#'
#' @return A `shiny.tag` wellPanel suitable for direct placement in the
#'   embedder's UI.
#' @seealso [`ptr_shared_server()`], [`ptr_module_ui()`],
#'   [`ptr_module_server()`].
#' @export
ptr_shared_ui <- function(formulas,
                          shared_ui = list(),
                          ui_text = NULL,
                          expr_check = TRUE,
                          draw_all_label = "Draw all") {
  assertthat::assert_that(is.list(shared_ui))
  trees <- shared_translate_formulas(formulas, expr_check = expr_check)
  formula_count <- length(trees)

  firsts <- shared_first_nodes(trees)
  value_keys <- names(firsts$nodes)
  consumer_reps <- shared_consumer_representatives(trees)
  consumer_keys <- names(consumer_reps)
  # Discovery order across both kinds: a key may show up in either bucket
  # (a `var(shared = "...")` only lives in `consumer_reps`); a value-shared
  # widget (`text/num/expr/upload`) only lives in `firsts`. Same key never
  # appears in both because the binding rewrite is exclusive per node kind.
  formula_keys <- unique(c(value_keys, consumer_keys))

  if (length(formula_keys) == 0L) {
    rlang::abort(
      "`ptr_shared_ui()` was called with formulas that declare no `shared = \"...\"` placeholders. Drop the call, or annotate at least one placeholder in `formulas`."
    )
  }

  if (length(shared_ui) > 0L) {
    nms <- names(shared_ui)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort(
        "`shared_ui` must have unique non-empty names matching the `shared` annotations in `formulas`."
      )
    }
    if (!all(vapply(shared_ui, is.function, logical(1)))) {
      rlang::abort(
        "Every entry of `shared_ui` must be a function `function(id) -> shiny.tag`."
      )
    }
    extra <- setdiff(nms, formula_keys)
    if (length(extra) > 0L) {
      rlang::abort(paste0(
        "`shared_ui` references key ",
        paste0("\"", extra, "\"", collapse = ", "),
        " which is not used in any plot formula. Available formula keys: ",
        paste0("\"", formula_keys, "\"", collapse = ", "), "."
      ))
    }
  }

  shared_label_override <- lapply(firsts$occurrences, shared_widget_label)

  shared_widgets <- lapply(formula_keys, function(k) {
    canonical <- canonical_shared_id(k)
    if (k %in% names(shared_ui)) {
      shared_ui[[k]](canonical)
    } else if (k %in% consumer_keys) {
      # A `var` consumer's widget is a uiOutput placeholder that
      # `ptr_shared_server()` fills via `ptr_bind_shared_consumer_uis()`.
      node <- consumer_reps[[k]]
      build_ui_for(
        node,
        ns_fn = identity,
        label_override = node$shared_label
      )
    } else {
      node <- firsts$nodes[[k]]
      node$id <- canonical
      build_ui_for(
        node,
        ui_text = ui_text,
        ns_fn = identity,
        label_override = shared_label_override[[k]]
      )
    }
  })
  kept_mask <- !vapply(shared_widgets, is.null, logical(1))
  shared_widgets <- shared_widgets[kept_mask]
  rendered_keys <- formula_keys[kept_mask]

  # Wrap widgets whose key hosts at least one orphan pipeline stage (a
  # stage whose only placeholders are shared) in a `.ptr-stage` block. The
  # head checkbox uses a synthetic input id (`shared_stage_input_id(k)`);
  # `ptr_shared_server()` exposes its value as a reactive that each module
  # mirrors into its own `state$stage_enabled` for the underlying stage_ids
  # (see paintr-server.R).
  orphan_info <- collect_shared_stage_keys(trees)
  if (length(orphan_info) > 0L && length(shared_widgets) > 0L) {
    shared_widgets <- lapply(seq_along(shared_widgets), function(i) {
      k <- rendered_keys[[i]]
      w <- shared_widgets[[i]]
      info <- orphan_info[[k]]
      if (is.null(info)) return(w)
      verbs <- info$verbs
      head_label <- if (length(verbs) > 0L) {
        shiny::tags$code(paste0(paste(verbs, collapse = "/"), "()"))
      } else NULL
      controllable_region(
        shared_stage_input_id(k), head_label, w, ns_fn = identity
      )
    })
  }

  body <- c(
    list(
      shiny::tags$p(class = "ptr-shared-panel__title", "Shared controls"),
      shiny::tags$p(class = "ptr-shared-panel__hint",
                    "These widgets are linked across every plot below.")
    ),
    shared_widgets,
    if (formula_count >= 2L) {
      list(shiny::actionButton("ptr_shared_draw_all", draw_all_label))
    } else list(),
    list(shiny::uiOutput("ptr_shared_errors"))
  )

  shiny::tags$div(
    class = "ptr-app",
    shiny::wellPanel(
      do.call(shiny::div, c(list(class = "ptr-shared-panel"), body))
    )
  )
}


# ---- ptr_shared_server -----------------------------------------------------

#' Server-Side Counterpart to ptr_shared_ui()
#'
#' Builds the shared input reactives and binds the host-level
#' `var(shared = "...")` consumer pickers for the [`ptr_shared_ui()`]
#' panel. Returns a `ptr_shared_state` that the embedder threads into
#' each [`ptr_module_server()`] via the `shared_state` argument.
#'
#' Reads its session via [`shiny::getDefaultReactiveDomain()`] -- call
#' it inside the top-level Shiny server function (or any reactive
#' context that inherits the session). Errors when called outside any
#' reactive domain.
#'
#' Errors when no formula declares a `shared = "..."` annotation -- the
#' pair is a declaration of intent.
#'
#' @param formulas A character vector or list of formula strings,
#'   matching the ones passed to [`ptr_shared_ui()`].
#' @param envir Environment used to resolve symbols in the shared
#'   `var()` upstream chains. Default [`parent.frame()`] picks up the
#'   embedder's caller scope so `mtcars` etc. resolve naturally.
#' @param expr_check Whether to validate `expr` placeholders.
#' @param shared Optional named list overriding the auto-derived
#'   reactives. Each named entry replaces the reactive for that key;
#'   unsupplied keys keep the default that reads
#'   `input[[canonical_shared_id(key)]]`.
#' @param draw_trigger Optional reactive overriding the auto-derived
#'   "Draw all" trigger (`input$ptr_shared_draw_all`). Useful when an
#'   embedder wants to drive the cross-module redraw from their own
#'   button.
#'
#' @return A `ptr_shared_state` S3 object with public fields `shared`,
#'   `draw_trigger`, `shared_resolutions`.
#' @seealso [`ptr_shared_ui()`], [`ptr_module_server()`].
#' @export
ptr_shared_server <- function(formulas,
                              envir = parent.frame(),
                              expr_check = TRUE,
                              shared = list(),
                              draw_trigger = NULL) {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    rlang::abort(
      "`ptr_shared_server()` must be called inside a Shiny server function (no default reactive domain)."
    )
  }
  input <- session$input
  output <- session$output

  trees <- shared_translate_formulas(formulas, expr_check = expr_check)
  formula_count <- length(trees)

  firsts <- shared_first_nodes(trees)
  value_keys <- names(firsts$nodes)
  shared_resolutions <- ptr_resolve_shared_consumers(trees)
  consumer_keys <- names(shared_resolutions)
  formula_keys <- unique(c(value_keys, consumer_keys))

  if (length(formula_keys) == 0L) {
    rlang::abort(
      "`ptr_shared_server()` was called with formulas that declare no `shared = \"...\"` placeholders. Drop the call, or annotate at least one placeholder in `formulas`."
    )
  }

  assertthat::assert_that(is.list(shared))
  if (length(shared) > 0L) {
    nms <- names(shared)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort(
        "`shared` overrides must have unique non-empty names matching the `shared` annotations in `formulas`."
      )
    }
    if (!all(vapply(shared, shiny::is.reactive, logical(1)))) {
      rlang::abort("`shared` override values must be Shiny reactives.")
    }
    extra <- setdiff(nms, formula_keys)
    if (length(extra) > 0L) {
      rlang::abort(paste0(
        "`shared` override references key ",
        paste0("\"", extra, "\"", collapse = ", "),
        " which is not used in any plot formula."
      ))
    }
  }
  if (!is.null(draw_trigger) && !shiny::is.reactive(draw_trigger)) {
    rlang::abort("`draw_trigger` must be a Shiny reactive or NULL.")
  }

  # For each shared key, build a reactive that reads `input$shared_<key>`
  # at the host's top-level namespace -- modules cannot reach that slot
  # through their own snapshot path. When the embedder supplies an
  # override for a key, the picker input still wins **once the user
  # actually picks something**; the override only seeds the initial value
  # (and the picker's default selection) so first render isn't empty.
  # NULL / "" / NA in the input slot mean "not picked yet" -> fall through
  # to the override.
  is_unset_shared <- function(v) {
    is.null(v) ||
      (is.character(v) && length(v) == 1L && !nzchar(v)) ||
      (is.atomic(v) && length(v) == 1L && is.na(v))
  }
  shared_reactives <- stats::setNames(
    lapply(formula_keys, function(k) {
      cid <- canonical_shared_id(k)
      override_rv <- shared[[k]]
      if (is.null(override_rv)) {
        shiny::reactive(input[[cid]])
      } else {
        shiny::reactive({
          v <- input[[cid]]
          if (is_unset_shared(v)) override_rv() else v
        })
      }
    }),
    formula_keys
  )

  effective_draw_trigger <- if (!is.null(draw_trigger)) {
    draw_trigger
  } else if (formula_count >= 2L) {
    shiny::reactive(input$ptr_shared_draw_all)
  } else {
    NULL
  }

  if (length(consumer_keys) > 0L) {
    representative_nodes <- shared_consumer_representatives(trees)
    errors_rv <- shiny::reactiveVal(character())
    ptr_bind_shared_consumer_uis(
      output = output, input = input, ns = identity,
      resolutions = shared_resolutions,
      representative_nodes = representative_nodes,
      eval_env = envir,
      expr_check = expr_check,
      errors_rv = errors_rv
    )
    output$ptr_shared_errors <- shiny::renderUI({
      msgs <- errors_rv()
      if (length(msgs) == 0L) return(NULL)
      ptr_error_ui(paste(msgs, collapse = "\n"))
    })
  } else {
    # No consumer keys to surface, but the embedder still has the slot in
    # the panel; clear it explicitly so a stale prior render does not stick.
    output$ptr_shared_errors <- shiny::renderUI({ NULL })
  }

  # One reactive per shared key whose orphan pipeline stages should be
  # toggleable from the shared panel. Default to TRUE so that an unset
  # input (no checkbox rendered, or panel not yet touched) preserves the
  # existing semantics (stages enabled). Each `ptr_module_server()` mirrors
  # the value into its own `state$stage_enabled` for every orphan stage_id
  # in that module's tree -- see `ptr_setup_shared_stage_enabled()` in
  # paintr-server.R.
  orphan_info <- collect_shared_stage_keys(trees)
  shared_stage_enabled <- if (length(orphan_info) > 0L) {
    stats::setNames(
      lapply(names(orphan_info), function(k) {
        sid_input <- shared_stage_input_id(k)
        shiny::reactive({
          v <- input[[sid_input]]
          if (is.null(v)) TRUE else isTRUE(v)
        })
      }),
      names(orphan_info)
    )
  } else list()

  # Visual grey-out of the shared stage block when its checkbox is off.
  # Mirrors `ptr_setup_layer_panel_classes()` -- uses the `ptr_set_class`
  # custom message registered by `ptr_layer_assets()`.
  for (k in names(orphan_info)) {
    local({
      sid_input <- shared_stage_input_id(k)
      block_dom_id <- paste0(sid_input, "_stage_block")
      shiny::observeEvent(input[[sid_input]], {
        val <- input[[sid_input]]
        if (is.null(val)) return()
        session$sendCustomMessage("ptr_set_class", list(
          id = block_dom_id,
          cls = "ptr-stage-disabled",
          add = !isTRUE(val)
        ))
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
    })
  }

  new_ptr_shared_state(
    shared = shared_reactives,
    draw_trigger = effective_draw_trigger,
    shared_resolutions = shared_resolutions,
    shared_stage_enabled = shared_stage_enabled
  )
}
