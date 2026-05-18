# Server side + shared plumbing for the multi-instance shared coordinator.
#
# The panel UI (`ptr_shared_panel()` / `ptr_ui_shared_panel()`) and the
# `ptr_shared()` coordinator live in paintr-shared-coordinator.R. This file
# holds the `ptr_shared_state` value type, the AST/key plumbing shared with
# the coordinator (`shared_translate_formulas`, `shared_first_nodes`,
# `shared_consumer_representatives`), and `ptr_shared_server(obj)` -- which
# builds the matching reactives, wires the top-level consumer pickers via
# `ptr_bind_shared_consumer_uis()`, and returns a `ptr_shared_state` that
# the embedder threads into each `ptr_module_server(..., shared_state = ...)`.
#
# Design constraints (see dev/plans/shared-ui-multi-instance.html):
#   - NS(NULL) only; one panel per page; flat `shared_<key>` ids.
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


# ---- ptr_shared_server -----------------------------------------------------

#' Server-Side Counterpart to the Shared Coordinator
#'
#' Builds the shared input reactives and binds the host-level
#' `var(shared = "...")` consumer pickers for the [`ptr_shared_panel()`].
#' Returns a `ptr_shared_state` that the embedder threads into each
#' [`ptr_module_server()`] via the `shared_state` argument.
#'
#' Reads its session via [`shiny::getDefaultReactiveDomain()`] -- call
#' it inside the top-level Shiny server function (or any reactive
#' context that inherits the session). Errors when called outside any
#' reactive domain.
#'
#' @param obj A `ptr_shared_spec` from [`ptr_shared()`] -- the single
#'   source of truth for the cross-formula partition. Replaces the old
#'   `formulas`/`expr_check` arguments, which are now baked into `obj`.
#' @param envir Environment used to resolve symbols in the shared
#'   `var()` upstream chains. Default [`parent.frame()`] picks up the
#'   embedder's caller scope so `mtcars` etc. resolve naturally.
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
#'   `draw_trigger`, `shared_resolutions`, and `shared_stage_enabled`
#'   (a named list of reactives, one per shared key, indicating whether
#'   that key's shared stage is active for each embedded module).
#' @seealso [`ptr_shared()`], [`ptr_shared_panel()`],
#'   [`ptr_module_server()`].
#' @examples
#' if (interactive()) {
#'   obj <- ptr_shared(c(
#'     "ggplot(mtcars, aes(x = var(shared='x'), y = var)) + geom_point()",
#'     "ggplot(mtcars, aes(x = var(shared='x'), y = var)) + geom_bar()"
#'   ))
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(ptr_shared_panel(obj)),
#'     server = function(input, output, session) {
#'       ptr_shared_server(obj)
#'     }
#'   )
#' }
#' @export
ptr_shared_server <- function(obj,
                              envir = parent.frame(),
                              shared = list(),
                              draw_trigger = NULL) {
  if (!inherits(obj, "ptr_shared_spec")) {
    rlang::abort("`ptr_shared_server()` requires a `ptr_shared_spec` from `ptr_shared()`.")
  }
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    rlang::abort(
      "`ptr_shared_server()` must be called inside a Shiny server function (no default reactive domain)."
    )
  }
  input <- session$input
  output <- session$output

  # Step 02 (#P2): consume the coordinator's precomputed partition as the
  # single source of truth (ADR 0005). `panel_keys` are the cross-formula
  # keys this top-level server owns; formula-local keys are intentionally
  # absent from the bundle and bound by each module itself (the module's
  # `auto_bind_shared` self-bind path). `formula_keys` is kept only to
  # validate `shared =` override names against the full declared set.
  trees <- obj$trees
  formula_count <- obj$formula_count
  expr_check <- obj$expr_check

  firsts <- obj$firsts
  value_keys <- names(firsts$nodes)
  shared_resolutions <- ptr_resolve_shared_consumers(trees)
  consumer_keys <- names(shared_resolutions)
  formula_keys <- obj$formula_keys
  panel_keys <- obj$panel_keys

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

  # Build one reactive per **panel** (cross-formula) key, reading
  # `input$shared_<key>` at the host's top-level namespace -- modules
  # cannot reach that slot through their own snapshot path. Formula-local
  # keys are deliberately excluded: they have exactly one consuming
  # formula, so the owning module binds them itself (Step 02 invariant --
  # exactly one widget per shared key, correctly scoped). When the embedder
  # supplies an override for a panel key, the picker input still wins
  # **once the user actually picks something**; the override only seeds the
  # initial value (and the picker's default selection) so first render
  # isn't empty. NULL / "" / NA in the input slot mean "not picked yet" ->
  # fall through to the override.
  is_unset_shared <- function(v) {
    is.null(v) ||
      (is.character(v) && length(v) == 1L && !nzchar(v)) ||
      (is.atomic(v) && length(v) == 1L && is.na(v))
  }
  shared_reactives <- stats::setNames(
    lapply(panel_keys, function(k) {
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
    panel_keys
  )

  effective_draw_trigger <- if (!is.null(draw_trigger)) {
    draw_trigger
  } else if (formula_count >= 2L) {
    shiny::reactive(input$ptr_shared_draw_all)
  } else {
    NULL
  }

  # Bug B1b fix: the host owns ONLY cross-formula (panel) consumer keys --
  # the same partition `shared_reactives` uses. Formula-local consumer keys
  # are bound by each owning `ptr_module_server()` under its own namespace
  # (the helper's embed call); binding them here too would double-write at
  # global ids no panel emits. Route through the single binder helper,
  # marking the formula-local keys as host-owned-elsewhere so only
  # `panel_keys ∩ consumer_keys` are bound at `ns = identity`.
  panel_consumer_keys <- intersect(consumer_keys, panel_keys)
  if (length(panel_consumer_keys) > 0L) {
    errors_rv <- shiny::reactiveVal(character())
    ptr_bind_local_shared_consumers(
      tree = trees, output = output, input = input, ns = identity,
      host_owned_keys = setdiff(consumer_keys, panel_keys),
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
    # No panel consumer keys to surface, but the embedder still has the
    # slot in the panel; clear it explicitly so a stale prior render does
    # not stick. Safe no-op when `panel_keys` is empty (formula-local-only
    # multi-instance app) and the `ptr_shared_errors` id is absent.
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
