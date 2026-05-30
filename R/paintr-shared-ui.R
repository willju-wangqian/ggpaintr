# Server side + shared plumbing for the multi-instance shared coordinator.
#
# The panel UI (`ptr_shared_panel()` / `ptr_ui_shared_panel()`) and the
# `ptr_shared()` coordinator live in paintr-shared-coordinator.R. This file
# holds the `ptr_shared_state` value type, the AST/key plumbing shared with
# the coordinator (`shared_translate_formulas`, `shared_first_nodes`,
# `shared_consumer_representatives`), and `ptr_shared_server(obj)` -- which
# builds the matching reactives, wires the top-level consumer pickers via
# `ptr_bind_shared_consumer_uis()`, and returns a `ptr_shared_state` that
# the embedder threads into each `ptr_server(..., shared_state = ...)`.
#
# Design constraints (see dev/plans/shared-ui-multi-instance.html):
#   - NS(NULL) only; one panel per page; flat `shared_<key>` ids.
#   - The default sidebar-embedded path under `ptr_app()` is left alone;
#     two shared-widget renderers coexist by design.


# ---- ptr_shared_state ------------------------------------------------------

new_ptr_shared_state <- function(shared, draw_trigger, shared_resolutions,
                                 shared_stage_enabled = list(),
                                 panel_sources = list()) {
  structure(
    list(
      shared = shared,
      draw_trigger = draw_trigger,
      shared_resolutions = shared_resolutions,
      shared_stage_enabled = shared_stage_enabled,
      # ADR 0023 §1: per-panel-owned source key (`shared_<key>`) reactive
      # `data.frame`, written by the host's `ptr_setup_panel_sources()`
      # (Plan 04) and read by per-instance `ptr_setup_pipelines()` /
      # consumer pickers (Plans 05/07). Bundle-as-single-channel keeps
      # ADR-0006's no-extra-top-level-state rule.
      panel_sources = panel_sources
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
  # ADR 0023 §1: panel_sources is a (possibly empty) named list of reactive
  # data.frame values, keyed by canonical shared id (`shared_<key>`). Same
  # shape rules as `shared` / `shared_stage_enabled`. Missing slot is
  # tolerated for embedder-built shared_state values constructed before
  # this field existed (treated as empty).
  ps <- x$panel_sources %||% list()
  if (!is.list(ps)) {
    rlang::abort("`shared_state$panel_sources` must be a (possibly empty) named list of reactives.")
  }
  if (length(ps) > 0L) {
    nms <- names(ps)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort("`shared_state$panel_sources` must have unique non-empty names.")
    }
    if (!all(vapply(ps, shiny::is.reactive, logical(1)))) {
      rlang::abort("`shared_state$panel_sources` values must be Shiny reactives.")
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
  panel_source_keys <- names(x$panel_sources) %||% character()
  cat("  panel source keys :",
      if (length(panel_source_keys)) paste(panel_source_keys, collapse = ", ") else "<none>",
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
    # PLAN-07: carry the first-occurrence default so the dynamic shared-
    # consumer renderUI (`ptr_bind_shared_consumer_uis`) seeds the picker
    # from it on first render. `shared_widget_default` mirrors the
    # first-occurrence-wins tiebreak used for `shared_label`.
    n$default <- shared_widget_default(occ)
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
#' `ppVar(shared = "...")` consumer pickers for the [`ptr_shared_panel()`].
#' Returns a `ptr_shared_state` that the embedder threads into each
#' [`ptr_server()`] via the `shared_state` argument.
#'
#' Namespacing is inherited from `obj$id`; supply it to [`ptr_shared()`].
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
#'   `ppVar()` upstream chains. Default [`parent.frame()`] picks up the
#'   embedder's caller scope so `mtcars` etc. resolve naturally.
#' @param shared Optional named list overriding the auto-derived
#'   reactives. Each named entry replaces the reactive for that key;
#'   unsupplied keys keep the default that reads
#'   `input[[canonical_shared_id(key)]]`.
#' @param draw_trigger Optional reactive overriding the auto-derived
#'   "Draw all" trigger (`input$ptr_shared_draw_all`). Useful when an
#'   embedder wants to drive the cross-module redraw from their own
#'   button.
#' @param spec Optional named list of fully-qualified Shiny input id ->
#'   value, used to override widget defaults at session boot for
#'   host-owned panel-shared widgets (`shared_<k>` for consumer / value
#'   placeholders, `shared_<k>_name` for panel-owned source companion
#'   textInputs). Multi-instance entrypoints like [`ptr_app_grid()`]
#'   forward the same flat spec to every per-plot [`ptr_server()`] AND
#'   to this host-scope server; per-plot servers prefix-filter by their
#'   own module namespace and drop un-namespaced ids, leaving this host
#'   apply path to claim them. See [ADR
#'   0012](dev/adr/0012-role-based-tree-and-ptr-spec.html).
#'
#' @return A `ptr_shared_state` S3 object with public fields `shared`,
#'   `draw_trigger`, `shared_resolutions`, and `shared_stage_enabled`
#'   (a named list of reactives, one per shared key, indicating whether
#'   that key's shared stage is active for each embedded module).
#' @seealso [`ptr_shared()`], [`ptr_shared_panel()`],
#'   [`ptr_server()`].
#' @examples
#' if (interactive()) {
#'   obj <- ptr_shared(c(
#'     "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
#'     "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
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
                              draw_trigger = NULL,
                              spec = NULL) {
  if (!inherits(obj, "ptr_shared_spec")) {
    rlang::abort("`ptr_shared_server()` requires a `ptr_shared_spec` from `ptr_shared()`.")
  }
  ns <- shared_ns(obj)
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
      cid <- ns(canonical_shared_id(k))
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
    local({
      draw_id <- ns("ptr_shared_draw_all")
      shiny::reactive(input[[draw_id]])
    })
  } else {
    NULL
  }

  # Bug B1b fix: the host owns ONLY cross-formula (panel) consumer keys --
  # the same partition `shared_reactives` uses. Formula-local consumer keys
  # are bound by each owning `ptr_server()` under its own namespace
  # (the helper's embed call); binding them here too would double-write at
  # global ids no panel emits. Route through the single binder helper,
  # marking the formula-local keys as host-owned-elsewhere so only
  # `panel_keys ∩ consumer_keys` are bound at `ns = identity`.
  panel_consumer_keys <- intersect(consumer_keys, panel_keys)
  errors_output_id <- ns("ptr_shared_errors")
  # ADR 0023 / PLAN-04: hoist `errors_rv` so that both the consumer binder
  # (existing) and `ptr_setup_panel_sources()` (new, below) can push into
  # the same panel-level error sink, symmetric with consumer-side errors.
  # The sink remains a character() vector; per-source error messages are
  # bridged through it from each source's resolve_errors store.
  errors_rv <- shiny::reactiveVal(character())
  # ADR 0023 / PLAN-04 + PLAN-07: bind the panel-owned source widgets
  # and build the per-source resolved-data reactives BEFORE the consumer
  # binder runs, so the binder can take a direct dep on
  # `panel_sources[[sid]]()` for consumers whose upstream source id is
  # panel-owned. The helper is a no-op (returns `list()`) when no panel-
  # owned source keys exist, preserving the existing observable shape
  # for all multi-instance apps that don't yet share a source across
  # formulas.
  # FINDING #1 + #7 (placeholder-role-coverage2.html v7): host-scope
  # `spec=` channel. The flat spec is passed verbatim from `ptr_app_grid()`;
  # each per-instance `ptr_server()` prefix-filters by its own module ns
  # and DROPS un-namespaced ids targeting host-owned panel widgets
  # (`shared_<k>`, `shared_<k>_name`). `apply_spec_at_boot_host()` claims
  # those entries, normalizes per keyword, and writes them into
  # `host_spec_seed` for downstream `ptr_setup_panel_values()` and the
  # `spec_seed=` arg of `ptr_bind_local_shared_consumers()` below. Source
  # companions are also dispatched via deferred `updateTextInput` so the
  # companion textInput first-renders with the spec value rather than
  # `node$default` (mirrors `apply_spec_at_boot()`'s `source_companion`
  # branch).
  host_spec_seed <- new.env(parent = emptyenv())
  apply_spec_at_boot_host(spec, session, host_spec_seed, obj)

  panel_sources <- ptr_setup_panel_sources(
    obj, input = input, output = output, envir = envir,
    errors_rv = errors_rv
  )
  # FINDING #7: bind host-scope renderUI for panel-shared VALUE keys, so
  # the un-namespaced `output[["shared_<k>_ui"]]` slot is populated
  # (per-instance shared-value loop registers under a namespaced id that
  # never reaches the host's UI div).
  ptr_setup_panel_values(
    obj, output = output, input = input,
    host_spec_seed = host_spec_seed,
    ui_text = obj$ui_text
  )
  if (length(panel_consumer_keys) > 0L) {
    ptr_bind_local_shared_consumers(
      tree = trees, output = output, input = input, ns = ns,
      host_owned_keys = setdiff(consumer_keys, panel_keys),
      eval_env = envir,
      expr_check = expr_check,
      errors_rv = errors_rv,
      panel_sources = panel_sources,
      spec_seed = host_spec_seed
    )
  }
  output[[errors_output_id]] <- shiny::renderUI({
    msgs <- errors_rv()
    if (length(msgs) == 0L) return(NULL)
    ptr_error_ui(paste(msgs, collapse = "\n"))
  })

  # One reactive per shared key whose orphan pipeline stages should be
  # toggleable from the shared panel. Default to TRUE so that an unset
  # input (no checkbox rendered, or panel not yet touched) preserves the
  # existing semantics (stages enabled). Each `ptr_server()` mirrors
  # the value into its own `state$stage_enabled` for every orphan stage_id
  # in that module's tree -- see `ptr_setup_shared_stage_enabled()` in
  # paintr-server.R.
  orphan_info <- collect_shared_stage_keys(trees)
  shared_stage_enabled <- if (length(orphan_info) > 0L) {
    stats::setNames(
      lapply(names(orphan_info), function(k) {
        sid_input <- ns(shared_stage_input_id(k))
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
      sid_input <- ns(shared_stage_input_id(k))
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

  # ADR 0023 / PLAN-07: `panel_sources` is now built earlier (before the
  # consumer binder) so the binder can take a direct dep on
  # `panel_sources[[sid]]()`. The reactive bundle is still threaded into
  # `new_ptr_shared_state()` below.

  new_ptr_shared_state(
    shared = shared_reactives,
    draw_trigger = effective_draw_trigger,
    shared_resolutions = shared_resolutions,
    shared_stage_enabled = shared_stage_enabled,
    panel_sources = panel_sources
  )
}


# ---- ptr_setup_panel_sources ----------------------------------------------
#
# ADR 0023 / PLAN-04: host-side renderer + reactive builder for the panel-
# owned source keys. Mirrors the per-instance `ptr_setup_source_uis()` /
# `ptr_setup_pipelines()` pair from R/paintr-server.R but at *panel* scope
# (un-namespaced ids; one renderUI per source key) so the partition rule
# ("ownership equals binding") holds for the source role too.
#
# For each panel-owned data-source key:
#   * binds `output[[shared_<key>_ui]]` to a renderUI body equivalent to
#     the per-instance source body (same `invoke_build_ui`-shaped
#     dispatch: registry `build_ui`, `file_copy` / `name_copy` injection
#     for ppUpload, `named_args` pass-through, and the same
#     `outputOptions(..., suspendWhenHidden = FALSE)` mount).
#   * builds a `reactiveVal` slot and an observer that calls
#     `resolve_upload_source()` (Plan 02) on every change to the
#     panel-level `input[[shared_<key>]]` / `input[[shared_<key>_name]]`
#     pair, including the default-arg fallback when no upload is present.
#   * surfaces upload errors into the supplied `errors_rv` so the panel's
#     `ptr_shared_errors` slot reports them symmetrically with consumer-
#     side errors.
#
# Returns a named list of `shiny::reactive` values keyed by canonical
# shared id (`shared_<key>`), one per panel-owned source key. Returns
# `list()` when there are zero such keys (the common shape today).
ptr_setup_panel_sources <- function(obj, input, output, envir,
                                    errors_rv = NULL) {
  panel_keys <- obj$panel_keys %||% character()
  firsts_nodes <- obj$firsts$nodes %||% list()
  if (length(panel_keys) == 0L) return(list())

  ns <- shared_ns(obj)
  ui_text <- obj$ui_text

  # Keep panel-owned keys whose first-occurrence node is a data-source
  # placeholder; value-shared keys (ppText/ppNum/ppExpr/ppVar consumers)
  # stay on the existing `shared_reactives` / consumer-binder paths.
  source_keys <- character()
  for (k in panel_keys) {
    node <- firsts_nodes[[k]]
    if (!is.null(node) && is_ptr_ph_data_source(node)) {
      source_keys <- c(source_keys, k)
    }
  }
  if (length(source_keys) == 0L) return(list())

  out <- list()
  for (k in source_keys) {
    bundle <- local({
      key <- k
      node <- firsts_nodes[[key]]
      # Stamp the canonical id (and keep the shortcut id, derived as
      # `<canonical>_shortcut` when the registry entry opts in) so the
      # rendered widget binds at the global panel ids the panel body emits.
      canonical <- canonical_shared_id(key)
      node$id <- canonical
      entry <- ptr_registry_lookup(node$keyword)
      if (!is.null(entry) && isTRUE(entry$shortcut)) {
        node$shortcut_id <- paste0(canonical, "_shortcut")
      }
      # ADR 0025 §3 / PLAN-02: stamp the panel-scope auto-name. For shared
      # sources under a coordinator with `obj$id = "main"`, the auto-name
      # is `"main_ds"`; under `id = NULL` it collapses to `"ds"`. This is
      # the single source of truth for the substitute walker fallback,
      # the upload binding name in `resolve_upload_source()`, and the
      # Plan-04 code-panel prologue.
      node$auto_name <- if (!is.null(obj$id)) paste0(obj$id, "_", key) else key

      output_id <- ns(source_output_id(canonical))
      input_id <- ns(canonical)
      shortcut_input_id <- if (!is.null(node$shortcut_id)) ns(node$shortcut_id) else NULL

      # ADR 0025 item #7 (host port): rising-edge re-render of the source
      # widget + the data-side `file_reset` flag. Byte-shape-equal to
      # `ptr_setup_source_uis()` (R/paintr-server.R). When the shortcut
      # textbox transitions empty -> non-empty (the user types a dataset
      # name) bump `source_bump` so the source uiOutput re-renders a fresh
      # fileInput -- clearing the stale filename pill now that the typed
      # shortcut owns the data (the `resolve_upload_source` gate already
      # ignores the lingering file). The edge is ONE-directional (only
      # empty -> non-empty), so a file-pick (for which the mutex clears the
      # textbox) does NOT re-render and therefore does NOT wipe the
      # just-uploaded display. `file_reset_rv` is the data-side mirror: a
      # re-rendered fileInput cannot report its cleared state, so the resolve
      # call below keys its env-load gate + vacate-on-empty off THIS flag
      # rather than the stale server-side `input[[input_id]]`. Unlike the
      # single-instance path it needs no `state$source_file_reset` publish --
      # the renderUI, the flag, and the resolve call all live in this one
      # `local()` scope, so the flag is read directly at the call site.
      source_bump <- shiny::reactiveVal(0L)
      file_reset_rv <- shiny::reactiveVal(FALSE)
      if (!is.null(shortcut_input_id)) {
        prev_shortcut <- shiny::reactiveVal("")
        shortcut_debounced <- shiny::debounce(
          shiny::reactive(input[[shortcut_input_id]]), 400L
        )
        shiny::observeEvent(shortcut_debounced(), {
          cur <- shortcut_debounced() %||% ""
          if (nzchar(cur) && !nzchar(shiny::isolate(prev_shortcut()))) {
            source_bump(shiny::isolate(source_bump()) + 1L)
            file_reset_rv(TRUE)
          }
          prev_shortcut(cur)
        }, ignoreInit = TRUE, ignoreNULL = FALSE)
        # A genuine new file pick clears the "display was reset" flag so the
        # fresh upload is honoured again.
        shiny::observeEvent(input[[input_id]], {
          file_reset_rv(FALSE)
        }, ignoreInit = TRUE)
      }

      # Render the panel-side source widget. Body shape matches
      # `ptr_setup_source_uis()` in R/paintr-server.R: same registry
      # dispatch, same copy resolution, same outputOptions mount.
      output[[output_id]] <- shiny::renderUI({
        source_bump()  # ADR 0025 #7: re-render on the shortcut rising edge
        rendered_node <- node
        # Bind the rendered widget at the coordinator-namespaced DOM ids so
        # two `ptr_shared(..., id = ...)` coordinators sharing a panel-owned
        # source key on one page don't collide on bare `shared_<key>`.
        # Mirrors `invoke_build_ui()` (R/paintr-build-ui.R) and
        # `ptr_setup_source_uis()` (R/paintr-server.R) -- both stamp the
        # namespaced id onto the local `rendered_node` copy before passing
        # it to `entry$build_ui()`.
        rendered_node$id <- input_id
        if (!is.null(node$shortcut_id)) {
          rendered_node$shortcut_id <- shortcut_input_id
        }
        copy <- ptr_resolve_ui_text(
          "control",
          keyword = node$keyword,
          param = node$param,
          layer_name = node$layer_name,
          ui_text = ui_text
        )
        if (is.null(entry) || is.null(entry$build_ui)) {
          rlang::abort(paste0(
            "Placeholder `", node$keyword, "` has no `build_ui` function. ",
            "Pass `build_ui = function(node, label, ...)` when registering ",
            "it -- see `?ptr_define_placeholder_value`."
          ))
        }
        fmls <- names(formals(entry$build_ui))
        accepts_dots <- "..." %in% fmls
        extra_named <- build_ui_copy_args(fmls, copy)
        if (identical(node$keyword, "ppUpload") &&
            (accepts_dots || "file_copy" %in% fmls)) {
          extra_named$file_copy <- ptr_resolve_ui_text(
            "upload_file", ui_text = ui_text
          )
          extra_named$name_copy <- ptr_resolve_ui_text(
            "upload_name", ui_text = ui_text
          )
        }
        if (accepts_dots || "named_args" %in% fmls) {
          extra_named$named_args <- node$named_args %||% list()
        }
        if (accepts_dots || "selected" %in% fmls) {
          if (!is.null(node$default)) extra_named$selected <- node$default
        }
        do.call(entry$build_ui,
                c(list(rendered_node, label = copy$label), extra_named))
      })
      shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)

      # Synthetic host-local `state` exposing only the slots
      # `resolve_upload_source()` touches: `eval_env`, `bound_names[[key]]`,
      # and `resolve_errors()`. The eval_env's parent chain is `envir`
      # so the default-arg fallback in `try_bind_source_default_resolved()`
      # walks to the caller's frame and resolves data symbols like
      # `mtcars` / `df_main`.
      eval_env <- new.env(parent = envir)
      bound_names <- new.env(parent = emptyenv())
      bound_names[[canonical]] <- shiny::reactiveVal(NULL)
      resolve_errors <- shiny::reactiveVal(stats::setNames(list(),
                                                           character()))
      host_state <- list(
        eval_env = eval_env,
        bound_names = bound_names,
        resolve_errors = resolve_errors
      )

      slot <- shiny::reactiveVal(NULL)

      shiny::observe({
        resolve_upload_source(
          input_slot     = input[[input_id]],
          shortcut_slot = if (!is.null(shortcut_input_id)) {
            list(present = TRUE, value = input[[shortcut_input_id]])
          } else {
            list(present = FALSE, value = NULL)
          },
          node           = node,
          entry          = entry,
          envir          = eval_env,
          state          = host_state,
          key            = canonical,
          slot           = slot,
          file_reset     = isTRUE(file_reset_rv())
        )
      })

      # Bridge per-source resolve errors into the panel-level
      # `ptr_shared_errors` sink. The store under `resolve_errors` is a
      # named list keyed by source id; transform into a character vector
      # (one line per outstanding source error) and merge with whatever
      # consumer-side errors the host already pushed.
      if (!is.null(errors_rv)) {
        local({
          prev_lines <- character()
          shiny::observe({
            err_list <- resolve_errors()
            new_lines <- character()
            if (length(err_list) > 0L) {
              src_keys <- names(err_list)
              new_lines <- vapply(seq_along(err_list), function(i) {
                paste0(src_keys[[i]], ": ", err_list[[i]])
              }, character(1))
            }
            cur <- errors_rv()
            kept <- setdiff(cur, prev_lines)
            errors_rv(c(kept, new_lines))
            prev_lines <<- new_lines
          })
        })
      }

      # ADR 0025 §2 (Q3-B): mutex between fileInput and shortcut textInput
      # -- same contract as `ptr_setup_pipelines()`.
      if (!is.null(shortcut_input_id)) {
        session <- shiny::getDefaultReactiveDomain()
        ptr_bind_source_mutex(input_id, shortcut_input_id, input, session)
      }

      shiny::reactive(slot())
    })
    out[[canonical_shared_id(k)]] <- bundle
  }
  out
}


# ---- Host-scope spec= application -------------------------------------
# FINDING #1 + #7 (placeholder-role-coverage2.html v7) fix. Multi-instance
# `ptr_app_grid()` passes the same flat `spec=` to every per-instance
# `ptr_server()`, where `apply_spec_at_boot()`'s `startsWith(nms, prefix)`
# filter drops every un-namespaced key (the host's panel-shared widget
# ids `shared_<k>` and `shared_<k>_name` live at the host's un-namespaced
# root). With no host-scope spec applier, the seed never lands.
#
# `apply_spec_at_boot_host` claims un-namespaced spec entries that target
# host-owned panel widgets (the canonical `shared_<k>` for consumer/value
# placeholders, and `shared_<k>_name` for panel-owned source companions).
# Per-keyword value normalization mirrors `apply_spec_at_boot()` so the
# host seed reads see the same shape `updateXyzInput()` would. The result
# is written into `host_spec_seed` (an env supplied by `ptr_shared_server()`)
# which `ptr_setup_panel_values()` and `ptr_bind_shared_consumer_uis()`
# read at first render via their new `spec_seed=` parameter (no `state`
# coupling — the host has no per-instance `state` by ADR 0006).
#
# Source companion rows (`shared_<k>_name`) also dispatch a deferred
# `updateTextInput` once the first flush mounts the companion textInput,
# mirroring `apply_spec_at_boot()`'s `source_companion` branch (the
# seed-only path is insufficient for textInput initial render — see
# FINDING #8 entry in placeholder-role-coverage2.html v7).
apply_spec_at_boot_host <- function(spec, session, host_spec_seed, obj) {
  if (is.null(spec) || length(spec) == 0L) return(invisible())
  if (!is.list(spec)) {
    rlang::abort("`spec` must be a named list of input id -> value.")
  }
  nms <- names(spec)
  if (is.null(nms) || any(!nzchar(nms)) || anyNA(nms)) {
    rlang::abort("`spec` must be fully named with non-empty input ids.")
  }

  ns <- shared_ns(obj)
  prefix <- tryCatch(ns(""), error = function(e) "")
  if (is.null(prefix)) prefix <- ""
  keep <- startsWith(nms, prefix)
  if (!any(keep)) return(invisible())
  spec <- spec[keep]
  nms <- nms[keep]
  bare_ids <- substring(nms, nchar(prefix) + 1L)

  panel_keys <- obj$panel_keys
  if (length(panel_keys) == 0L) return(invisible())

  firsts_nodes <- obj$firsts$nodes
  consumer_reps <- obj$consumer_reps %||% list()

  rows <- list()
  for (i in seq_along(bare_ids)) {
    bid <- bare_ids[[i]]
    val <- spec[[i]]
    matched <- NULL
    for (k in panel_keys) {
      canonical <- canonical_shared_id(k)
      if (identical(bid, canonical)) {
        node <- consumer_reps[[k]]
        if (is.null(node)) node <- firsts_nodes[[k]]
        if (is.null(node)) next
        if (is_ptr_ph_data_source(node)) {
          # fileInput / non-programmatic source self id; skip silently —
          # the spec target for a ppUpload source is the companion at
          # `shared_<k>_name`, mirroring per-instance behaviour.
          matched <- "skip"; break
        }
        matched <- list(keyword = node$keyword, is_companion = FALSE)
        break
      } else if (identical(bid, paste0(canonical, "_name"))) {
        node <- firsts_nodes[[k]]
        if (is.null(node) || !is_ptr_ph_data_source(node)) next
        # Treat the companion textInput as ppText for normalization.
        matched <- list(keyword = "ppText", is_companion = TRUE)
        break
      }
    }
    if (is.null(matched) || identical(matched, "skip")) next
    rows[[length(rows) + 1L]] <- list(
      bare_id = bid, value = val,
      keyword = matched$keyword,
      is_companion = matched$is_companion
    )
  }
  if (length(rows) == 0L) return(invisible())

  defer <- list()
  for (row in rows) {
    val <- row$value
    if (row$is_companion || identical(row$keyword, "ppText")) {
      if (is.null(val)) val <- "" else val <- as.character(val)[[1L]]
    } else if (identical(row$keyword, "ppNum")) {
      if (is.null(val) || (is.character(val) && !nzchar(val))) {
        val <- NA_real_
      } else {
        num <- suppressWarnings(as.numeric(val)[[1L]])
        if (is.na(num) &&
            !identical(row$value, NA_real_) &&
            !identical(row$value, NA_integer_)) {
          next
        }
        val <- num
      }
    } else if (identical(row$keyword, "ppExpr")) {
      if (is.null(val)) {
        val <- ""
      } else if (is.language(val)) {
        val <- paste(deparse(val), collapse = "\n")
      } else {
        val <- as.character(val)[[1L]]
      }
    } else if (identical(row$keyword, "ppVar")) {
      if (is.null(val)) val <- character() else val <- as.character(val)
    }
    # Custom value-keyword keywords (non-built-in) fall through with `val`
    # untouched, mirroring `apply_spec_at_boot()`'s policy.
    host_spec_seed[[row$bare_id]] <- val
    if (row$is_companion) {
      defer[[length(defer) + 1L]] <- list(id = row$bare_id, value = val)
    }
  }

  if (length(defer) > 0L) {
    session$onFlushed(once = TRUE, function() {
      for (d in defer) {
        shiny::updateTextInput(session, inputId = d$id, value = d$value)
      }
    })
  }

  invisible()
}

# ---- Host-scope renderUI for panel-shared VALUE placeholders ----------
# FINDING #7 fix. The UI emits `uiOutput("shared_<k>_ui")` at the host's
# un-namespaced root for every panel-shared VALUE key (`build_ui_for.ptr_ph_value`
# in R/paintr-build-ui.R:46-63 — post-PLAN-01 reshape). The per-instance
# shared-value loop in `ptr_setup_value_uis()` registers a renderUI at
# the namespaced output id (`<module>-shared_<k>_ui`), which never
# matches the un-namespaced UI div. This helper closes the gap: one
# renderUI per panel-shared value key at host scope, reading the host's
# `spec_seed` env so `spec = list(shared_<k> = ...)` lands at boot.
#
# Parallel to `ptr_setup_panel_sources()` (sources) and to the shared-
# value loop in `ptr_setup_value_uis()` (per-instance values). The host
# has no `state` / `state$tree()` / `state$stage_enabled()` reactives
# (ADR 0006), so the renderUI's dependency set is intentionally thin:
# `spec_seed` is isolated (boot-only); `input[[raw_id]]` is isolated to
# avoid an unwanted re-fire on the user's own keystroke (matches the
# per-instance loop's `has_rendered` semantics).
ptr_setup_panel_values <- function(obj, output, input, host_spec_seed,
                                   ui_text = NULL) {
  panel_keys <- obj$panel_keys
  if (length(panel_keys) == 0L) return(invisible())
  firsts_nodes <- obj$firsts$nodes
  consumer_keys <- names(obj$consumer_reps %||% list())
  ns <- shared_ns(obj)
  shared_label_override <- lapply(obj$firsts$occurrences, shared_widget_label)
  shared_default_override <- lapply(obj$firsts$occurrences, shared_widget_default)

  for (key in panel_keys) {
    # Skip non-value keys: panel-shared consumers go through
    # `ptr_bind_shared_consumer_uis()` (which also reads spec_seed);
    # panel-shared sources go through `ptr_setup_panel_sources()`.
    if (key %in% consumer_keys) next
    rep <- firsts_nodes[[key]]
    if (is.null(rep)) next
    if (is_ptr_ph_data_source(rep)) next
    if (!is_ptr_ph_value(rep)) next
    # Stamp the canonical id + first-occurrence default onto the rep node
    # to match `shared_panel_body_tag()`'s else-branch (UI side stamps
    # the same fields before `build_ui_for(node, ...)`), so the
    # `invoke_build_ui` call below renders a widget byte-equivalent to
    # the no-spec path.
    rep$id <- canonical_shared_id(key)
    rep$default <- shared_default_override[[key]]
    local({
      node <- rep
      label_override <- shared_label_override[[key]]
      raw_id <- node$id
      output_id <- ns(value_output_id(raw_id))
      has_rendered <- FALSE
      output[[output_id]] <- shiny::renderUI({
        current <- shiny::isolate(input[[raw_id]])
        seed <- shiny::isolate(host_spec_seed[[raw_id]])
        # Boot-only seed precedence, shared with the per-instance binders via
        # `boot_seed_selected()`. NOTE: this renderUI has no reactive
        # dependencies (both `current` and `seed` are isolated and the host
        # has no tree()/stage_enabled() reactives), so it renders exactly once
        # and the has_rendered branch is currently unreachable -- routing it
        # through the shared helper is defensive consistency so the contract
        # can't drift if a future dep is ever added here.
        selected_arg <- boot_seed_selected(has_rendered, seed, current)
        extra <- list()
        if (!is.null(selected_arg)) extra$selected <- selected_arg
        result <- invoke_build_ui(
          node,
          ui_text = ui_text,
          layer_name = NULL,
          ns_fn = ns,
          extra = extra,
          label_override = label_override
        )
        has_rendered <<- TRUE
        result
      })
      # Same `suspendWhenHidden = FALSE` rationale as the per-instance
      # shared-value loop: keep the input element mounted regardless of
      # which layer panel is currently active in any embedded module, so
      # spec seeding lands on a live widget and `app$get_value(...)` /
      # `app$get_html(...)` reads non-NULL under shinytest2.
      shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
    })
  }
  invisible()
}
