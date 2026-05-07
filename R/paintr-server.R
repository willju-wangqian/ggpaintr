# P12 — server-state + observer wiring for the typed-AST core.
#
# `ptr_server_state` builds a long-lived state list that the Shiny server
# carries: the typed tree, per-pipeline-layer resolved-data caches, the latest
# runtime result (post-substitute → post-prune → post-eval), the resolved
# checkbox-defaults vector, and an upstream-resolution memo cache.
#
# `ptr_server` is the wiring sugar that calls the state constructor and
# attaches the observers (pipeline updates + runtime).

#' Long-Lived Server State for a `ggpaintr` Formula
#'
#' Builds the `ptr_state` list consumed by [ptr_server()] and the friend
#' helpers (`ptr_register_*`, `ptr_extract_*`, `ptr_gg_extra`). Holds the
#' typed AST as a `reactiveVal`, the runtime result, per-pipeline-layer
#' resolved-data caches, the input snapshot machinery, and the shared
#' bindings / draw trigger when used inside [ptr_app_grid()].
#'
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Optional named list of copy overrides.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes.
#' @param expr_check Controls `expr` placeholder validation.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after substitution.
#' @param shared Named list of reactives (one per shared key) supplied by an
#'   outer wrapper such as [ptr_app_grid()]. Defaults to `list()`.
#' @param draw_trigger Optional reactive whose invalidation forces a redraw
#'   (e.g. the grid app's "Draw all" button).
#' @param ns A namespace function used for rendered ids (UI side).
#' @param server_ns A namespace function used for server-side input lookups.
#'   Defaults to `ns`.
#'
#' @return A `ptr_state` list (S3 class `c("ptr_state", "list")`).
#' @export
ptr_server_state <- function(formula,
                                envir = parent.frame(),
                                ui_text = NULL,
                                checkbox_defaults = NULL,
                                expr_check = TRUE,
                                safe_to_remove = character(),
                                shared = list(),
                                draw_trigger = NULL,
                                ns = shiny::NS(NULL),
                                server_ns = ns) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\")).")
  }
  if (!is.function(server_ns)) {
    rlang::abort("`server_ns` must be a namespace function.")
  }
  if (!is.null(draw_trigger) && !shiny::is.reactive(draw_trigger)) {
    rlang::abort("`draw_trigger` must be a Shiny reactive or NULL.")
  }
  shared_bindings <- ptr_validate_shared_bindings(shared)
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)

  tree <- ptr_translate(formula, expr_check = expr_check)

  layer_names <- vapply(tree$layers, function(l) l$name, character(1))
  # `ptr_resolve_checkbox_defaults` keys off `names(expr_list)`; supply a
  # named placeholder list so the layer names land where it expects them.
  expr_list_proxy <- stats::setNames(
    as.list(rep(NA, length(layer_names))),
    layer_names
  )
  resolved_cd <- ptr_resolve_checkbox_defaults(checkbox_defaults,
                                                expr_list_proxy)

  pipeline_layer_names <- character()
  for (l in tree$layers) {
    if (!is.null(l$update_data_input_id)) {
      pipeline_layer_names <- c(pipeline_layer_names, l$name)
    }
  }
  resolved_data <- stats::setNames(
    lapply(pipeline_layer_names, function(.) shiny::reactiveVal(NULL)),
    pipeline_layer_names
  )
  last_click_inputs <- stats::setNames(
    lapply(pipeline_layer_names, function(.) shiny::reactiveVal(NULL)),
    pipeline_layer_names
  )

  initial_stage_ids <- collect_stage_ids(tree)
  initial_stage_enabled <- stats::setNames(
    rep(list(TRUE), length(initial_stage_ids)),
    initial_stage_ids
  )

  structure(list(
    tree = shiny::reactiveVal(tree),
    runtime = shiny::reactiveVal(NULL),
    extras = shiny::reactiveVal(list()),
    eval_env = envir,
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    raw_ui_text = ui_text,
    effective_ui_text = ui_text,
    checkbox_defaults = resolved_cd,
    shared_bindings = shared_bindings,
    draw_trigger = draw_trigger,
    resolved_data = resolved_data,
    last_click_inputs = last_click_inputs,
    is_stale_env = new.env(parent = emptyenv()),
    upstream_cache = new.env(parent = emptyenv()),
    stage_enabled = shiny::reactiveVal(initial_stage_enabled),
    ui_ns_fn = ns,
    server_ns_fn = server_ns,
    ns_fn = server_ns,
    input_spec = ptr_runtime_input_spec(tree)
  ), class = c("ptr_state", "list"))
}

# ---- public wiring entry point ----

#' Wire a `ggpaintr` Server From a Formula
#'
#' Convenience wrapper that builds the `ptr_state` via [ptr_server_state()]
#' and attaches the per-pipeline observers, stage-enabled toggles, runtime
#' observer, and plot/code/error output bindings. Returns the state list so
#' embedders can attach extras (`ptr_gg_extra`) or drive the typed tree
#' programmatically.
#'
#' @param input,output,session The standard Shiny server arguments.
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ... Forwarded to [ptr_server_state()] (e.g. `shared`,
#'   `draw_trigger`, `ui_text`, `checkbox_defaults`, `expr_check`,
#'   `safe_to_remove`, `ns`).
#'
#' @return The `ptr_state` list, invisibly.
#' @export
ptr_server <- function(input, output, session, formula,
                          envir = parent.frame(), ...) {
  state <- ptr_server_state(formula, envir = envir, ...)
  ptr_setup_pipelines(state, input, output, session)
  ptr_setup_stage_enabled(state, input, output, session)
  ptr_setup_runtime(state, input, output, session)
  ptr_setup_consumer_uis(state, input, output, session)
  ptr_setup_layer_picker(state, input, output, session)
  ptr_register_plot(output, state)
  ptr_register_error(output, state)
  ptr_register_code(output, state)
  state
}

# ---- per-pipeline-layer observers ----

ptr_setup_pipelines <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn

  for (layer in tree$layers) {
    if (is.null(layer$update_data_input_id)) next

    # Bind layer-specific values via local() so each iteration captures freshly.
    local({
      lyr <- layer
      ln <- lyr$name
      uid <- ns(lyr$update_data_input_id)

      # G6.1 — initial seed: prune on empty snapshot, eval, store.
      seed <- ptr_resolve_upstream(
        lyr$data_arg,
        snapshot = list(),
        shared_bindings = state$shared_bindings,
        eval_env = state$eval_env,
        cache = state$upstream_cache,
        expr_check = state$expr_check,
        stage_enabled = shiny::isolate(state$stage_enabled())
      )
      state$resolved_data[[ln]](seed)
      state$last_click_inputs[[ln]](list())

      # G6.2 / G6.3 — Update-Data click: validate atomically. ignoreNULL keeps
      # the handler dormant until the user actually clicks (input[[uid]]
      # starts as NULL); we don't ignoreInit so the first non-NULL value
      # (the first click in a fresh session) still fires.
      shiny::observeEvent(input[[uid]], {
        snapshot <- snapshot_for_layer(lyr, input, state)
        ok <- validate_pipeline_atomic(lyr, snapshot, state)
        if (!isTRUE(ok)) return(invisible())
        new_data <- ptr_resolve_upstream(
          lyr$data_arg,
          snapshot = snapshot,
          shared_bindings = state$shared_bindings,
          eval_env = state$eval_env,
          cache = state$upstream_cache,
          expr_check = state$expr_check,
          stage_enabled = state$stage_enabled()
        )
        state$resolved_data[[ln]](new_data)
        state$last_click_inputs[[ln]](snapshot)
      }, ignoreNULL = TRUE)

      # Stale flag.
      state$is_stale_env[[ln]] <- shiny::reactive({
        last <- state$last_click_inputs[[ln]]()
        if (is.null(last)) return(FALSE)
        current <- snapshot_for_layer(lyr, input, state)
        !identical(current, last)
      })
    })
  }
  invisible(state)
}


# ---- per-stage-enabled observers ----

# Mirror checkbox inputs into `state$stage_enabled`. Each stage-id input
# starts NULL; only after the user toggles does it produce a value, so we
# use ignoreNULL = TRUE. The reactiveVal carries the canonical bool.
ptr_setup_stage_enabled <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn
  for (sid in collect_stage_ids(tree)) {
    local({
      sid_local <- sid
      bound_id <- ns(sid_local)
      shiny::observeEvent(input[[bound_id]], {
        cur <- state$stage_enabled()
        cur[[sid_local]] <- isTRUE(input[[bound_id]])
        state$stage_enabled(cur)
      }, ignoreNULL = TRUE)
    })
  }
  invisible(state)
}

# Build a snapshot keyed by raw id from the placeholders inside one layer's
# pipeline (`data_arg` subtree).
snapshot_for_layer <- function(layer, input, state) {
  ns <- state$server_ns_fn
  out <- list()
  for (ph in find_layer_placeholders(layer$data_arg)) {
    if (is.null(ph$id)) next
    out[[ph$id]] <- input[[ns(ph$id)]]
    if (is_ptr_ph_data_source(ph) && !is.null(ph$companion_id)) {
      out[[ph$companion_id]] <- input[[ns(ph$companion_id)]]
    }
  }
  out
}

# Per-position validation: each consumer's chosen value must lie within its
# own upstream's column set. Missing values are skipped (will be pruned).
validate_pipeline_atomic <- function(layer, snapshot, state) {
  for (ph in find_layer_placeholders(layer$data_arg)) {
    if (!is_ptr_ph_data_consumer(ph)) next
    val <- snapshot[[ph$id %||% ""]]
    if (is.null(val) || !nzchar(as.character(val[1L]) %||% "")) next
    cols_df <- ptr_resolve_upstream(
      ph$upstream,
      snapshot = snapshot,
      shared_bindings = state$shared_bindings,
      eval_env = state$eval_env,
      cache = state$upstream_cache,
      expr_check = state$expr_check
    )
    if (is.null(cols_df)) next
    if (!all(val %in% names(cols_df))) return(FALSE)
  }
  TRUE
}

# ---- runtime observer ----

ptr_setup_runtime <- function(state, input, output, session) {
  ns <- state$server_ns_fn

  # Spec L142 + BDD G11.12 — plot rendering is gated on a single trigger
  # ("Update Plot" for standalone, "Draw all" for grid). With
  # `ignoreInit = TRUE` the observer does nothing until the user clicks;
  # state$runtime() stays NULL, so the plot/code/error outputs all
  # render blank on first launch. No expression eval happens until
  # the user opts in via the trigger.
  trigger <- if (!is.null(state$draw_trigger)) {
    state$draw_trigger
  } else {
    shiny::reactive(input[[ns("ptr_update_plot")]])
  }

  shiny::observe({
    val <- trigger()
    # actionButton starts at 0 (or NULL before first interaction); the
    # standalone Update Plot button and the grid Draw-all button both use
    # this convention. Skip until the first real click.
    if (is.null(val)) return(invisible())
    if (is.numeric(val) && val < 1L) return(invisible())

    shiny::isolate({
      spec <- state$input_spec
      snapshot <- list()
      if (nrow(spec) > 0L) {
        for (i in seq_len(nrow(spec))) {
          raw_id <- spec$input_id[i]
          snapshot[[raw_id]] <- input[[ns(raw_id)]]
        }
      }

      tree <- state$tree()
      stage_enabled <- state$stage_enabled()
      tree <- disable_walk(tree, stage_enabled)
      upstream_cols <- runtime_upstream_cols(state, snapshot)

      res <- ptr_complete_expr_safe(
        tree,
        snapshot = snapshot,
        shared_bindings = state$shared_bindings,
        eval_env = state$eval_env,
        safe_to_remove = state$safe_to_remove,
        upstream_cols = upstream_cols
      )
      # Spec L105 + L217 (G6.3 terminal upstream): once a layer's `data_arg`
      # has been pre-resolved into `state$resolved_data` (initial seed at
      # app start, or post-snapshot after Update Data), replace the
      # pruned layer's `data_arg` with a literal carrying the cached frame.
      # `code_text` is already rendered above from the original pruned
      # tree, so the user-visible code panel still shows the pipeline; eval
      # below consumes the cached value rather than re-running the pipeline.
      res$pruned <- inject_resolved_data(res$pruned, state)
      res <- ptr_assemble_plot_safe(res, expr_check = state$expr_check)

      extras <- state$extras()
      if (isTRUE(res$ok) && length(extras) > 0L) {
        res$plot <- Reduce(`+`, extras, res$plot)
      }
      res <- ptr_validate_plot_render_safe(res)
      state$runtime(res)
    })
  })
  invisible(state)
}

# Walk the pruned tree and replace each pipeline-data layer's `data_arg`
# with a `ptr_literal` carrying the cached frame from
# `state$resolved_data[[layer$name]]`. Layers without a cache entry (no
# Update Data button, or seed not yet computed) are left intact, so eval
# falls back to evaluating the original (post-substitute) `data_arg`.
inject_resolved_data <- function(pruned, state) {
  if (!is_ptr_root(pruned)) return(pruned)
  if (is.null(pruned)) return(pruned)
  for (i in seq_along(pruned$layers)) {
    layer <- pruned$layers[[i]]
    if (!is_ptr_layer(layer)) next
    rv <- state$resolved_data[[layer$name]]
    if (is.null(rv)) next
    df <- rv()
    if (is.null(df)) next
    pruned$layers[[i]]$data_arg <- ptr_literal(df)
  }
  pruned
}

# Sync the layer-select picker (`ptr_layer_select`) with the hidden tabset
# (`ptr_layer_tabset`) that holds per-layer panels. Without this, picker
# changes don't switch which panel is shown.
ptr_setup_layer_picker <- function(state, input, output, session) {
  ns <- state$server_ns_fn
  shiny::observeEvent(input[[ns("ptr_layer_select")]], {
    shiny::updateTabsetPanel(
      session, ns("ptr_layer_tabset"),
      selected = input[[ns("ptr_layer_select")]]
    )
  })
  invisible(state)
}

# Per-consumer column set, keyed by consumer raw id. Used to validate `var`
# selections at substitute time and to drive `cols` for picker UI updates.
#
# Spec L76 + L217 (G6.3 dual upstream): choices come from
# `ptr_resolve_upstream(node$upstream, ...)` — the per-position upstream,
# resolved against whatever snapshot the caller supplies. Callers in P12's
# reactive layer pass the LIVE input snapshot so consumer pickers reflect
# placeholder edits immediately (BDD G11.12). The earlier preference for
# `state$resolved_data[[layer_name]]` conflated terminal upstream
# (the layer's `data_arg` post-snapshot) with per-position upstream
# (each consumer's own `node$upstream`); they differ for consumers in
# upstream pipeline stages.
runtime_upstream_cols <- function(state, snapshot = list()) {
  tree <- shiny::isolate(state$tree())
  stage_enabled <- state$stage_enabled()
  out <- list()
  consumers <- find_nodes(tree, is_ptr_ph_data_consumer)
  for (c in consumers) {
    if (is.null(c$id)) next
    df <- ptr_resolve_upstream(
      c$upstream,
      snapshot = snapshot,
      shared_bindings = state$shared_bindings,
      eval_env = state$eval_env,
      cache = state$upstream_cache,
      expr_check = state$expr_check,
      stage_enabled = stage_enabled
    )
    if (!is.null(df)) out[[c$id]] <- names(df)
  }
  out
}


# ---- per-consumer UI observers ----
#
# Bridges P12 (reactive cols) and P6 (consumer build_ui). For every
# `ptr_ph_data_consumer` in the tree, we render its widget inside a
# `renderUI` so that whenever upstream cols change the picker is rebuilt
# with the fresh `cols`. The static UI emits an empty `uiOutput` container
# at `consumer_output_id(node$id)`; this function fills it.
#
# `cols_memo` is a once-per-tick reactive that calls `runtime_upstream_cols`
# exactly once and returns the full per-consumer named list, so two
# consumers in the same tick share the computation.

ptr_setup_consumer_uis <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn
  ui_ns <- state$ui_ns_fn
  ui_text <- state$effective_ui_text
  placeholders <- state$placeholders

  # Spec L76 + BDD G11.12: consumer dropdowns refresh on live placeholder
  # edits. The reactive reads `input` directly so any placeholder change
  # invalidates `cols_memo`; per-position upstream is then re-resolved with
  # the live snapshot. Plot rendering remains gated separately by
  # `ptr_setup_runtime`.
  cols_memo <- shiny::reactive({
    spec <- state$input_spec
    snapshot <- list()
    if (nrow(spec) > 0L) {
      for (i in seq_len(nrow(spec))) {
        raw_id <- spec$input_id[i]
        snapshot[[raw_id]] <- input[[ns(raw_id)]]
      }
    }
    runtime_upstream_cols(state, snapshot)
  })

  consumers <- find_nodes(tree, is_ptr_ph_data_consumer)
  for (c in consumers) {
    if (is.null(c$id)) next
    local({
      node <- c
      raw_id <- node$id
      output_id <- ui_ns(consumer_output_id(raw_id))
      cols_reactive <- shiny::reactive({
        cols_memo()[[raw_id]] %||% character()
      })
      output[[output_id]] <- shiny::renderUI({
        invoke_build_ui(
          node,
          ui_text = ui_text,
          placeholders = placeholders,
          layer_name = node$layer_name,
          ns_fn = ui_ns,
          extra = list(cols = cols_reactive())
        )
      })
    })
  }
  invisible(state)
}

# ---- public output bindings ----

#' Register Plot / Error / Code Outputs
#'
#' Attaches the standard plot, error, and code outputs to the Shiny `output`
#' object. Already invoked by [ptr_server()]; exposed for embedders that
#' compose outputs manually.
#'
#' @param output The Shiny output object.
#' @param state A `ptr_state` from [ptr_server_state()].
#'
#' @return The `output` object, invisibly.
#' @name ptr_register
#' @export
ptr_register_plot <- function(output, state) {
  output[[state$ui_ns_fn("ptr_plot")]] <- shiny::renderPlot({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    res$plot
  })
  invisible(state)
}

#' @rdname ptr_register
#' @export
ptr_register_error <- function(output, state) {
  output[[state$ui_ns_fn("ptr_error")]] <- shiny::renderText({
    res <- state$runtime()
    if (is.null(res) || isTRUE(res$ok)) "" else (res$error %||% "")
  })
  invisible(state)
}

#' @rdname ptr_register
#' @export
ptr_register_code <- function(output, state) {
  output[[state$ui_ns_fn("ptr_code")]] <- shiny::renderText({
    res <- state$runtime()
    if (is.null(res)) "" else (res$code_text %||% "")
  })
  invisible(state)
}

#' Extract Runtime Outputs From a `ptr_state`
#'
#' Read the latest plot object, error message, or generated code text from
#' the runtime result stored on a `ptr_state`. Use these to compose custom
#' UIs or to test the runtime in `shiny::testServer`.
#'
#' @param state A `ptr_state` from [ptr_server_state()].
#'
#' @return `ptr_extract_plot` returns a `ggplot` object (or `NULL` on
#'   failure); `ptr_extract_error` returns a string or `NULL`;
#'   `ptr_extract_code` returns a single string.
#' @name ptr_extract
#' @export
ptr_extract_plot  <- function(state) shiny::isolate(state$runtime())$plot

#' @rdname ptr_extract
#' @export
ptr_extract_error <- function(state) shiny::isolate(state$runtime())$error

#' @rdname ptr_extract
#' @export
ptr_extract_code  <- function(state) shiny::isolate(state$runtime())$code_text

# ---- ptr_gg_extra ----

#' Add `ggplot2` Layers Programmatically
#'
#' Evaluate one or more `ggplot2` expressions and attach the results as
#' "extras" on the state. Extras are folded into the plot during the next
#' runtime cycle when `state$runtime()$ok` is `TRUE`. Eval failures leave
#' the existing extras untouched.
#'
#' @param state A `ptr_state` from [ptr_server_state()].
#' @param exprs A list of expressions or quosures producing `ggplot2` layers
#'   (e.g. `list(quote(ggplot2::scale_x_log10()))`).
#'
#' @return `state`, invisibly.
#' @export
ptr_gg_extra <- function(state, exprs = list()) {
  if (!is.list(exprs) || !length(exprs)) return(invisible(state))
  evaluated <- list()
  for (e in exprs) {
    val <- if (rlang::is_quosure(e)) {
      rlang::eval_tidy(e, env = state$eval_env)
    } else {
      eval(e, envir = state$eval_env)
    }
    evaluated[[length(evaluated) + 1L]] <- val
  }
  current <- shiny::isolate(state$extras())
  state$extras(c(current, evaluated))
  invisible(state)
}
