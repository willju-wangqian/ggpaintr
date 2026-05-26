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
                              draw_trigger = NULL) {
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
  panel_sources <- ptr_setup_panel_sources(
    obj, input = input, output = output, envir = envir,
    errors_rv = errors_rv
  )
  if (length(panel_consumer_keys) > 0L) {
    ptr_bind_local_shared_consumers(
      tree = trees, output = output, input = input, ns = ns,
      host_owned_keys = setdiff(consumer_keys, panel_keys),
      eval_env = envir,
      expr_check = expr_check,
      errors_rv = errors_rv,
      panel_sources = panel_sources
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
      # Stamp the canonical id (and keep the companion-id-fn-derived
      # companion id) so the rendered widget binds at the global panel
      # ids the panel body emits.
      canonical <- canonical_shared_id(key)
      node$id <- canonical
      entry <- ptr_registry_lookup(node$keyword)
      if (!is.null(entry) && !is.null(entry$companion_id_fn)) {
        node$companion_id <- entry$companion_id_fn(canonical)
      }

      output_id <- ns(source_output_id(canonical))
      input_id <- ns(canonical)
      comp_id <- if (!is.null(node$companion_id)) ns(node$companion_id) else NULL

      # Render the panel-side source widget. Body shape matches
      # `ptr_setup_source_uis()` in R/paintr-server.R: same registry
      # dispatch, same copy resolution, same outputOptions mount.
      output[[output_id]] <- shiny::renderUI({
        rendered_node <- node
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
          companion_slot = if (!is.null(comp_id)) {
            list(present = TRUE, value = input[[comp_id]])
          } else {
            list(present = FALSE, value = NULL)
          },
          node           = node,
          entry          = entry,
          envir          = eval_env,
          state          = host_state,
          key            = canonical,
          slot           = slot
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

      # Autoname companion text input from uploaded filename (same
      # contract as `ptr_setup_pipelines()`).
      if (!is.null(comp_id)) {
        session <- shiny::getDefaultReactiveDomain()
        ptr_bind_source_autoname(input_id, comp_id, input, session,
                                  default_name = node$default)
      }

      shiny::reactive(slot())
    })
    out[[canonical_shared_id(k)]] <- bundle
  }
  out
}
