# P12 — server-state + observer wiring for the typed-AST core.
#
# `ptr_server_state_v2` builds a long-lived state list that the Shiny server
# carries: the typed tree, per-pipeline-layer resolved-data caches, the latest
# runtime result (post-substitute → post-prune → post-eval), the resolved
# checkbox-defaults vector, and an upstream-resolution memo cache.
#
# `ptr_server_v2` is the wiring sugar that calls the state constructor and
# attaches the observers (pipeline updates + runtime).

ptr_server_state_v2 <- function(formula,
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
    input_spec = ptr_runtime_input_spec_v2(tree)
  ), class = c("ptr_state_v2", "ptr_state", "list"))
}

# ---- public wiring entry point ----

ptr_server_v2 <- function(input, output, session, formula,
                          envir = parent.frame(), ...) {
  state <- ptr_server_state_v2(formula, envir = envir, ...)
  ptr_setup_pipelines_v2(state, input, output, session)
  ptr_setup_stage_enabled_v2(state, input, output, session)
  ptr_setup_runtime_v2(state, input, output, session)
  ptr_register_plot_v2(output, state)
  ptr_register_error_v2(output, state)
  ptr_register_code_v2(output, state)
  state
}

# ---- per-pipeline-layer observers ----

ptr_setup_pipelines_v2 <- function(state, input, output, session) {
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
        snapshot <- snapshot_for_layer_v2(lyr, input, state)
        ok <- validate_pipeline_atomic_v2(lyr, snapshot, state)
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
        current <- snapshot_for_layer_v2(lyr, input, state)
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
ptr_setup_stage_enabled_v2 <- function(state, input, output, session) {
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
snapshot_for_layer_v2 <- function(layer, input, state) {
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
validate_pipeline_atomic_v2 <- function(layer, snapshot, state) {
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

ptr_setup_runtime_v2 <- function(state, input, output, session) {
  shiny::observe({
    if (!is.null(state$draw_trigger)) state$draw_trigger()

    spec <- state$input_spec
    ns <- state$server_ns_fn
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
    upstream_cols <- runtime_upstream_cols_v2(state)

    res <- ptr_complete_expr_safe_v2(
      tree,
      snapshot = snapshot,
      shared_bindings = state$shared_bindings,
      eval_env = state$eval_env,
      safe_to_remove = state$safe_to_remove,
      upstream_cols = upstream_cols
    )
    res <- ptr_assemble_plot_safe_v2(res, expr_check = state$expr_check)

    extras <- state$extras()
    if (isTRUE(res$ok) && length(extras) > 0L) {
      res$plot <- Reduce(`+`, extras, res$plot)
    }
    res <- ptr_validate_plot_render_safe_v2(res)
    state$runtime(res)
  })
  invisible(state)
}

# Per-consumer column set, keyed by consumer raw id. Used to validate `var`
# selections at substitute time and to drive `cols` for picker UI updates.
runtime_upstream_cols_v2 <- function(state) {
  tree <- shiny::isolate(state$tree())
  stage_enabled <- state$stage_enabled()
  out <- list()
  consumers <- find_nodes(tree, is_ptr_ph_data_consumer)
  for (c in consumers) {
    if (is.null(c$id)) next
    layer_name <- consumer_layer_name(tree, c$id)
    if (!is.null(layer_name) &&
        !is.null(state$resolved_data[[layer_name]])) {
      df <- state$resolved_data[[layer_name]]()
      if (!is.null(df)) {
        out[[c$id]] <- names(df)
        next
      }
    }
    df <- ptr_resolve_upstream(
      c$upstream,
      snapshot = list(),
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

# Look up which layer a consumer belongs to (by id-prefix convention).
consumer_layer_name <- function(tree, consumer_id) {
  parts <- strsplit(consumer_id, "+", fixed = TRUE)[[1L]]
  if (length(parts) >= 1L) parts[1L] else NULL
}

# ---- public output bindings ----

ptr_register_plot_v2 <- function(output, state) {
  output[[state$ui_ns_fn("ptr_plot")]] <- shiny::renderPlot({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    res$plot
  })
  invisible(state)
}

ptr_register_error_v2 <- function(output, state) {
  output[[state$ui_ns_fn("ptr_error")]] <- shiny::renderText({
    res <- state$runtime()
    if (is.null(res) || isTRUE(res$ok)) "" else (res$error %||% "")
  })
  invisible(state)
}

ptr_register_code_v2 <- function(output, state) {
  output[[state$ui_ns_fn("ptr_code")]] <- shiny::renderText({
    res <- state$runtime()
    if (is.null(res)) "" else (res$code_text %||% "")
  })
  invisible(state)
}

ptr_extract_plot_v2  <- function(state) shiny::isolate(state$runtime())$plot
ptr_extract_error_v2 <- function(state) shiny::isolate(state$runtime())$error
ptr_extract_code_v2  <- function(state) shiny::isolate(state$runtime())$code_text

# ---- ptr_gg_extra ----

ptr_gg_extra_v2 <- function(state, exprs = list()) {
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
