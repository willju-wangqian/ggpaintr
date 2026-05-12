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
#' @param producer_debounce_ms Optional. Controls the debounce window applied
#'   to producer-style placeholder inputs (`text`, `num`, `expr`) before they
#'   invalidate downstream consumer caches. `NULL` (default) enables auto
#'   mode: window starts at 0 ms and the runtime flips to 300 ms after three
#'   consecutive upstream resolutions exceed 150 ms (and back to 0 after five
#'   consecutive resolutions under 80 ms). Pass `0` to force off forever, or a
#'   positive integer to pin a manual window.
#' @param ns A namespace function used for rendered ids (UI side).
#' @param server_ns A namespace function used for server-side input lookups.
#'   Defaults to `ns`.
#' @param auto_bind_shared If `TRUE`, the host (single-plot `ptr_app()`
#'   or `ptr_app_grid()` auto-render path) binds shared widgets at host
#'   scope. Relaxes the "missing-from-bindings" check in
#'   `ptr_validate_shared_bindings()` (the host auto-binds instead).
#' @param shared_resolutions Named list (keyed by raw shared key) of
#'   host-computed resolutions for shared data-consumer (`var`) widgets,
#'   as returned by `ptr_resolve_shared_consumers()`. When an entry is
#'   present, the runtime validates that key's selection against the
#'   host-resolved upstream (the same data the host picker was built
#'   from) instead of the per-layer `node$upstream`, so a value valid in
#'   the host picker is never rejected by one layer's narrower upstream.
#'   Defaults to `list()` (no host resolutions; per-layer behaviour).
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
                                producer_debounce_ms = NULL,
                                ns = shiny::NS(NULL),
                                server_ns = ns,
                                auto_bind_shared = FALSE,
                                shared_resolutions = list()) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\")).")
  }
  if (!is.function(server_ns)) {
    rlang::abort("`server_ns` must be a namespace function.")
  }
  if (!is.null(draw_trigger) && !shiny::is.reactive(draw_trigger)) {
    rlang::abort("`draw_trigger` must be a Shiny reactive or NULL.")
  }
  if (!is.null(producer_debounce_ms)) {
    if (!is.numeric(producer_debounce_ms) ||
        length(producer_debounce_ms) != 1L ||
        is.na(producer_debounce_ms) ||
        producer_debounce_ms < 0) {
      rlang::abort(
        "`producer_debounce_ms` must be a single non-negative number, or NULL."
      )
    }
    initial_debounce_ms <- as.integer(producer_debounce_ms)
    debounce_mode <- "manual"
  } else {
    initial_debounce_ms <- 0L
    debounce_mode <- "auto"
  }
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)

  tree <- ptr_translate(formula, expr_check = expr_check)

  shared_bindings <- ptr_validate_shared_bindings(
    shared, tree = tree,
    strict_missing = !isTRUE(auto_bind_shared)
  )

  layer_names <- vapply(tree$layers, function(l) l$name, character(1))
  # `ptr_resolve_checkbox_defaults` keys off `names(expr_list)`; supply a
  # named placeholder list so the layer names land where it expects them.
  expr_list_proxy <- stats::setNames(
    as.list(rep(NA, length(layer_names))),
    layer_names
  )
  resolved_cd <- ptr_resolve_checkbox_defaults(checkbox_defaults,
                                                expr_list_proxy)

  data_layer_names <- character()
  for (l in tree$layers) {
    if (is_bare_data_source_layer(l)) {
      data_layer_names <- c(data_layer_names, l$name)
    }
  }
  resolved_data <- stats::setNames(
    lapply(data_layer_names, function(.) shiny::reactiveVal(NULL)),
    data_layer_names
  )

  initial_stage_ids <- collect_stage_ids(tree)
  initial_stage_enabled <- stats::setNames(
    rep(list(TRUE), length(initial_stage_ids)),
    initial_stage_ids
  )

  producer_perf_env <- new.env(parent = emptyenv())
  producer_perf_env$slow_count <- 0L
  producer_perf_env$fast_count <- 0L
  producer_perf_env$first_eval_done <- FALSE

  structure(list(
    tree = shiny::reactiveVal(tree),
    runtime = shiny::reactiveVal(NULL),
    extras = shiny::reactiveVal(list()),
    # Source expressions paired with `extras`. `ptr_gg_extra()` updates
    # both atomically so the code panel can append the user-typed source
    # (extras themselves are evaluated ggplot objects with no
    # round-trippable deparse).
    extras_exprs = shiny::reactiveVal(list()),
    eval_env = envir,
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    raw_ui_text = ui_text,
    effective_ui_text = ui_text,
    checkbox_defaults = resolved_cd,
    shared_bindings = shared_bindings,
    shared_resolutions = if (is.list(shared_resolutions)) shared_resolutions else list(),
    draw_trigger = draw_trigger,
    resolved_data = resolved_data,
    upstream_cache = new.env(parent = emptyenv()),
    stage_enabled = shiny::reactiveVal(initial_stage_enabled),
    # Producer-input debounce + auto-flip (D7 / D8). `producer_input` is
    # populated by `ptr_setup_producer_inputs()` after the server boots;
    # entries are debounced reactives reading `input[[ns(id)]]`. The
    # `producer_debounce_ms` window is read dynamically by every debounced
    # reactive, so flipping it at runtime takes effect on the next
    # invalidation without recreating the reactive (verified against
    # `shiny::debounce` source).
    producer_input = new.env(parent = emptyenv()),
    producer_debounce_ms = shiny::reactiveVal(initial_debounce_ms),
    producer_debounce_mode = debounce_mode,
    producer_perf_env = producer_perf_env,
    # Host-level resolver errors for shared `var` widgets, surfaced into
    # the error panel by `ptr_register_error()`. Populated by
    # `ptr_bind_shared_consumer_uis()` when auto-binding.
    shared_resolution_errors = shiny::reactiveVal(character()),
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
  ptr_setup_producer_inputs(state, input, output, session)
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

is_bare_data_source_layer <- function(layer) {
  if (!is_ptr_layer(layer)) return(FALSE)
  if (is.null(layer$data_arg)) return(FALSE)
  is_ptr_ph_data_source(layer$data_arg)
}

ptr_setup_pipelines <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn

  # Bare-data-source layers reactively resolve their `data_arg` from the
  # source widget (e.g. an upload's file input) and cache the resolved df
  # on `state$resolved_data[[ln]]`. Pipeline-data layers no longer have a
  # click-gated cache; their consumers resolve lazily through the
  # per-consumer reactive in `ptr_setup_consumer_uis()`.
  for (layer in tree$layers) {
    if (!is_bare_data_source_layer(layer)) next
    local({
      lyr <- layer
      ln <- lyr$name
      src <- lyr$data_arg
      src_id <- ns(src$id)
      comp_id <- if (!is.null(src$companion_id)) ns(src$companion_id) else NULL
      entry <- ptr_registry_lookup(src$keyword)

      shiny::observe({
        file_info <- input[[src_id]]
        if (!is.null(comp_id)) input[[comp_id]]  # take dep
        if (is.null(file_info) || is.null(file_info$datapath)) {
          state$resolved_data[[ln]](NULL)
          return(invisible())
        }
        df <- if (!is.null(entry) && !is.null(entry$resolve_data)) {
          tryCatch(entry$resolve_data(file_info, src),
                   error = function(e) NULL)
        } else NULL
        state$resolved_data[[ln]](df)
      })

      # Auto-fill the dataset-name companion from the uploaded filename
      # when the user left it blank. Without a name, `substitute_walk`
      # on the source placeholder yields `ptr_missing()`, so the code
      # panel drops the `data = ...` argument entirely (the plot itself
      # still renders, since `inject_resolved_data()` patches the eval
      # tree). Never clobber a name the user typed.
      if (!is.null(comp_id)) {
        shiny::observeEvent(input[[src_id]], {
          fi <- input[[src_id]]
          nm <- ptr_upload_autoname(
            input[[comp_id]],
            if (!is.null(fi)) fi$name else NULL
          )
          if (!is.null(nm)) {
            shiny::updateTextInput(session, comp_id, value = nm)
          }
        })
      }
    })
  }
  invisible(state)
}


# Wire one debounced reactive per `ptr_ph_value` (text/num/expr) input. The
# debounce window is read dynamically from `state$producer_debounce_ms`, so
# the auto-flip in `record_eval_time()` (D8) can flip every producer's
# window in unison without recreating the reactive graph. Consumer-side
# pickers (`var`) are NOT debounced — their commits are intentional, not
# noisy keystrokes.
ptr_setup_producer_inputs <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn
  for (p in find_nodes(tree, is_ptr_ph_value)) {
    if (is.null(p$id)) next
    local({
      raw_id <- p$id
      r <- shiny::reactive({ input[[ns(raw_id)]] })
      state$producer_input[[raw_id]] <- shiny::debounce(
        r,
        millis = function() state$producer_debounce_ms()
      )
    })
  }
  invisible(state)
}

# Auto-flip thresholds for the producer-debounce mechanism (D8). Internal
# constants; not exposed as `ptr_app` arguments. Asymmetric thresholds
# (FAST_THRESHOLD < SLOW_THRESHOLD) and asymmetric counts
# (CONSECUTIVE_FAST > CONSECUTIVE_SLOW) provide hysteresis to prevent flap
# at values near the boundary, with a slight bias toward staying debounced.
.PTR_SLOW_THRESHOLD_MS         <- 150
.PTR_FAST_THRESHOLD_MS         <- 80
.PTR_CONSECUTIVE_SLOW_REQUIRED <- 3L
.PTR_CONSECUTIVE_FAST_REQUIRED <- 5L
.PTR_DEFAULT_DEBOUNCE_WINDOW   <- 300L

# Update the auto-flip counters after a single upstream resolve. Implements
# the symmetric flip-flop in spec D8 — see `lazy-consumer-resolve.md`.
# The very first eval per session is excluded (cold-start bias).
record_eval_time <- function(state, elapsed_ms) {
  if (identical(state$producer_debounce_mode, "manual")) return(invisible())
  perf <- state$producer_perf_env
  if (!isTRUE(perf$first_eval_done)) {
    perf$first_eval_done <- TRUE
    return(invisible())
  }
  current_ms <- shiny::isolate(state$producer_debounce_ms())
  if (current_ms == 0L) {
    if (elapsed_ms > .PTR_SLOW_THRESHOLD_MS) {
      perf$slow_count <- perf$slow_count + 1L
      if (perf$slow_count >= .PTR_CONSECUTIVE_SLOW_REQUIRED) {
        state$producer_debounce_ms(.PTR_DEFAULT_DEBOUNCE_WINDOW)
        perf$slow_count <- 0L
        perf$fast_count <- 0L
        if (ptr_get_setting(ptr_settings$verbose)) {
          cli::cli_inform(paste0(
            "Producer debounce auto-enabled (",
            .PTR_DEFAULT_DEBOUNCE_WINDOW,
            " ms): slow upstream resolution detected."
          ))
        }
      }
    } else {
      perf$slow_count <- 0L
    }
  } else {
    if (elapsed_ms < .PTR_FAST_THRESHOLD_MS) {
      perf$fast_count <- perf$fast_count + 1L
      if (perf$fast_count >= .PTR_CONSECUTIVE_FAST_REQUIRED) {
        state$producer_debounce_ms(0L)
        perf$fast_count <- 0L
        perf$slow_count <- 0L
        if (ptr_get_setting(ptr_settings$verbose)) {
          cli::cli_inform(
            "Producer debounce auto-disabled: upstream resolution back to fast."
          )
        }
      }
    } else {
      perf$fast_count <- 0L
    }
  }
  invisible()
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
# ---- runtime observer ----

ptr_setup_runtime <- function(state, input, output, session) {
  ns <- state$server_ns_fn

  # Spec L142 + BDD G11.12 — runtime fires on ANY user trigger: the
  # per-instance Update Plot button OR the host-supplied `draw_trigger`
  # (e.g. grid app's "Draw all" button), plus extras changes
  # (`ptr_gg_extra()`). The body re-runs on every invalidation of those
  # three but bails until one has actually fired. An
  # `observeEvent(..., ignoreInit = TRUE)` can't replace this guard:
  # under `shiny::testServer` the first `setInputs()` IS the observer's
  # creation flush, which `ignoreInit` would swallow.
  clicked <- function(x) is.numeric(x) && length(x) == 1L && x >= 1L
  triggered <- function() {
    clicked(input[[ns("ptr_update_plot")]]) ||
      (!is.null(state$draw_trigger) && clicked(state$draw_trigger())) ||
      length(state$extras()) > 0L
  }

  shiny::observe({
    if (!triggered()) return(invisible())

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
      # Spec L105 + L217 (G6.3 terminal upstream): bare-data-source layers
      # (e.g. `ggplot(data = upload, ...)`) have their resolved frame cached
      # in `state$resolved_data[[layer$name]]` by the upload observer. Swap
      # the pruned layer's `data_arg` with a literal carrying that frame so
      # eval below skips re-running the resolve. `code_text` was rendered
      # above from the original pruned tree, so the user-visible code panel
      # still shows the source expression. Pipeline-data layers no longer
      # have a click-gated cache (lazy-consumer-resolve) and fall through.
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

# Build the styled inline error UI (red bordered card). Ported from
# legacy paintr (paintr-runtime.R::ptr_error_ui) — gives the user a
# visually distinct error box rather than raw text. Returns NULL when
# the message is missing/blank so renderUI emits nothing.
ptr_error_ui <- function(message) {
  if (is.null(message) || identical(trimws(message), "")) {
    return(NULL)
  }
  shiny::tags$div(
    style = paste(
      "margin-top: 12px;",
      "margin-bottom: 12px;",
      "padding: 12px;",
      "border: 1px solid #c62828;",
      "border-radius: 4px;",
      "background-color: #fff3f3;",
      "color: #7f1d1d;"
    ),
    shiny::tags$strong("Error"),
    shiny::tags$div(
      style = "white-space: pre-wrap; margin-top: 6px;",
      message
    )
  )
}

# Walk the pruned tree and, for any layer with a cached frame in
# `state$resolved_data[[layer$name]]`, replace its `data_arg` with a
# `ptr_literal` carrying that frame. Only bare-data-source layers carry a
# cache after the lazy-consumer-resolve refactor; layers without a slot
# fall through and eval the original (post-substitute) `data_arg`.
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
runtime_upstream_data <- function(state, snapshot = list()) {
  tree <- shiny::isolate(state$tree())
  stage_enabled <- state$stage_enabled()
  out <- list()
  consumers <- find_nodes(tree, is_ptr_ph_data_consumer)
  for (c in consumers) {
    if (is.null(c$id)) next
    # Shared consumers: when the host supplied a resolution for this key,
    # validate against the host-resolved upstream (the data the host
    # picker was built from), not the per-layer `node$upstream`. The
    # latter can be a narrower pipeline that legitimately omits a column
    # the host picker offered, and would wrongly reject the selection —
    # and because every occurrence shares the canonical id, the per-layer
    # path also clobbers across layers nondeterministically. Falls
    # through to the per-position path when no host resolution exists
    # (custom embedder owns the widget).
    if (!is.null(c$shared)) {
      res <- state$shared_resolutions[[c$shared]]
      if (!is.null(res)) {
        if (!identical(res$kind, "error") && !is.null(res$value)) {
          df <- ptr_resolve_upstream(
            res$value,
            snapshot = list(),
            shared_bindings = list(),
            eval_env = state$eval_env,
            cache = NULL,
            expr_check = state$expr_check,
            stage_enabled = list()
          )
          if (!is.null(df)) out[[c$id]] <- list(cols = names(df), data = df)
        }
        next
      }
    }
    layer_name <- c$layer_name
    if (!is.null(layer_name) &&
        !is.null(state$resolved_data[[layer_name]])) {
      df <- state$resolved_data[[layer_name]]()
      if (!is.null(df)) {
        out[[c$id]] <- list(cols = names(df), data = df)
        next
      }
    }
    df <- ptr_resolve_upstream(
      c$upstream,
      snapshot = snapshot,
      shared_bindings = state$shared_bindings,
      eval_env = state$eval_env,
      cache = state$upstream_cache,
      expr_check = state$expr_check,
      stage_enabled = stage_enabled
    )
    if (!is.null(df)) out[[c$id]] <- list(cols = names(df), data = df)
  }
  out
}

# Backward-compat wrapper for callers that only need column names
# (validation/substitute path). The richer per-consumer data lives on
# `runtime_upstream_data`; this wrapper extracts the cols slot.
runtime_upstream_cols <- function(state, snapshot = list()) {
  res <- runtime_upstream_data(state, snapshot)
  lapply(res, function(x) x$cols)
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

  consumers <- find_nodes(tree, is_ptr_ph_data_consumer)
  for (c in consumers) {
    if (is.null(c$id)) next
    # Shared consumers are owned by the host (single-plot sidebar /
    # grid top panel) via `ptr_bind_shared_consumer_uis()`; skip here
    # so we don't double-write to the same output id.
    if (!is.null(c$shared)) next
    local({
      node <- c
      raw_id <- node$id
      output_id <- ns(consumer_output_id(raw_id))

      # Per-consumer dep set:
      #   - structural: tree, stage_enabled
      #   - bare-data layer resolves: state$resolved_data reactiveVals
      #   - upstream consumer-input commits (other `var` pickers in our
      #     own upstream): selecting a column above us invalidates our
      #     cache so our choices reflect that pick.
      #   - upstream producer-input changes (`text`/`num`/`expr` in our
      #     own upstream): read through `state$producer_input[[id]]`,
      #     which is debounced with a dynamically auto-flipped window.
      #   - inner Data/Controls tab activation: switching to Controls
      #     invalidates control-tab consumers, ditto Data; lets Bootstrap
      #     keep all panes in DOM but still gives consumers a refresh
      #     hook on tab visit (Decision D3 in lazy-consumer-resolve.md).
      #   - global: Update Plot click invalidates every consumer.
      upstream_consumer_ids <- find_consumer_ids_in_upstream(node$upstream)
      upstream_producer_ids <- find_producer_ids_in_upstream(node$upstream)
      subtab_id <- if (!is.null(node$layer_name)) {
        paste0(node$layer_name, "_subtab")
      } else NULL

      entry_reactive <- shiny::reactive({
        state$tree()
        state$stage_enabled()
        for (rv in state$resolved_data) rv()
        for (cid in upstream_consumer_ids) input[[ns(cid)]]
        producer_values <- list()
        for (pid in upstream_producer_ids) {
          r <- state$producer_input[[pid]]
          val <- if (!is.null(r)) r() else input[[ns(pid)]]
          if (!is.null(val)) producer_values[[pid]] <- val
        }
        if (!is.null(subtab_id)) input[[ns(subtab_id)]]
        input[[ns("ptr_update_plot")]]

        snapshot <- producer_values
        for (cid in upstream_consumer_ids) {
          val <- input[[ns(cid)]]
          if (!is.null(val)) snapshot[[cid]] <- val
        }
        runtime_consumer_entry(state, node, snapshot)
      })

      output[[output_id]] <- shiny::renderUI({
        entry <- entry_reactive()
        cols <- entry$cols %||% character()
        data <- entry$data
        # Preserve the user's current pick across renderUI re-fires
        # (cols change, layer toggle, etc.). `intersect` inside the
        # builtin's build_ui drops it if it's no longer a valid column.
        current <- shiny::isolate(input[[ns(raw_id)]])
        invoke_build_ui(
          node,
          ui_text = ui_text,
          placeholders = placeholders,
          layer_name = node$layer_name,
          ns_fn = ui_ns,
          extra = list(cols = cols, data = data,
                       selected = current %||% character(0))
        )
      })
    })
  }
  invisible(state)
}


# Bind a top-level renderUI per shared `var(shared = "<key>")` key. The
# renderUI either emits the placeholder's normal picker (using a host-
# resolved upstream) or an inline alert when resolution failed. Errors
# also propagate to `errors_rv` (a reactiveVal) so a host-level error
# panel can mirror them.
#
# `representative_nodes` is a named list (key -> placeholder node). The
# host is expected to set `node$id` to the id used by its rendered
# `uiOutput` so `output[[consumer_output_id(node$id)]]` lines up.
ptr_bind_shared_consumer_uis <- function(output, input, ns,
                                            resolutions,
                                            representative_nodes,
                                            ui_text = NULL,
                                            placeholders = NULL,
                                            eval_env = parent.frame(),
                                            expr_check = TRUE,
                                            errors_rv = NULL) {
  # Push host-level resolver errors (per key) into the reactive bag.
  if (!is.null(errors_rv)) {
    err_msgs <- character()
    for (k in names(resolutions)) {
      if (identical(resolutions[[k]]$kind, "error")) {
        err_msgs <- c(err_msgs, paste0(
          "Shared `var(shared = \"", k, "\")`: ",
          resolutions[[k]]$error
        ))
      }
    }
    errors_rv(err_msgs)
  }

  for (key in names(resolutions)) {
    local({
      k <- key
      resolution <- resolutions[[k]]
      rep_node <- representative_nodes[[k]]
      if (is.null(rep_node)) return(NULL)
      output_id <- ns(consumer_output_id(rep_node$id))

      output[[output_id]] <- shiny::renderUI({
        if (identical(resolution$kind, "error")) {
          return(shiny::div(
            class = "alert alert-danger",
            shiny::strong(paste0(
              "Shared `var(shared = \"", k, "\")` cannot be resolved."
            )),
            shiny::br(),
            resolution$error
          ))
        }
        df <- tryCatch(
          ptr_resolve_upstream(
            resolution$value,
            snapshot = list(),
            shared_bindings = list(),
            eval_env = eval_env,
            cache = NULL,
            expr_check = expr_check,
            stage_enabled = list()
          ),
          error = function(e) NULL
        )
        cols <- if (!is.null(df)) names(df) else character()
        current <- shiny::isolate(input[[ns(rep_node$id)]])
        invoke_build_ui(
          rep_node,
          ui_text = ui_text,
          placeholders = placeholders,
          layer_name = NULL,
          ns_fn = ns,
          extra = list(cols = cols, data = df,
                       selected = current %||% character(0))
        )
      })
    })
  }
  invisible(NULL)
}

# Ids of every placeholder matching `pred` inside a consumer's
# `node$upstream` subtree, deduplicated. Drives the per-consumer reactive
# cache — see the two named wrappers below for the two predicates in use.
collect_upstream_ids <- function(upstream, pred) {
  if (is.null(upstream)) return(character())
  ids <- character()
  ptr_walk(upstream, function(n) {
    if (pred(n) && !is.null(n$id)) ids[[length(ids) + 1L]] <<- n$id
  })
  unique(ids)
}

# Upstream `var` consumers: any commit on an upstream consumer's picker
# invalidates this consumer.
find_consumer_ids_in_upstream <- function(upstream) {
  collect_upstream_ids(upstream, is_ptr_ph_data_consumer)
}

# Upstream `ptr_ph_value` producers (text/num/expr): their values flow
# through the shared debounced reactives in `state$producer_input`, so
# producer keystrokes invalidate downstream consumers only after the
# (possibly auto-flipped) debounce window elapses.
find_producer_ids_in_upstream <- function(upstream) {
  collect_upstream_ids(upstream, is_ptr_ph_value)
}

# Resolve a single consumer's upstream against a snapshot, mirroring the
# per-consumer slot of runtime_upstream_data. Bare-data layers short-circuit
# via the cached `state$resolved_data` slot; pipeline-data layers fall
# through to a fresh `ptr_resolve_upstream` call.
runtime_consumer_entry <- function(state, node, snapshot = list()) {
  layer_name <- node$layer_name
  if (!is.null(layer_name) &&
      !is.null(state$resolved_data[[layer_name]])) {
    df <- state$resolved_data[[layer_name]]()
    if (!is.null(df)) {
      return(list(cols = names(df), data = df))
    }
  }
  t0 <- Sys.time()
  df <- ptr_resolve_upstream(
    node$upstream,
    snapshot = snapshot,
    shared_bindings = state$shared_bindings,
    eval_env = state$eval_env,
    cache = state$upstream_cache,
    expr_check = state$expr_check,
    stage_enabled = shiny::isolate(state$stage_enabled())
  )
  elapsed_ms <- as.numeric(Sys.time() - t0, units = "secs") * 1000
  record_eval_time(state, elapsed_ms)
  if (is.null(df)) return(NULL)
  list(cols = names(df), data = df)
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
  output[[state$server_ns_fn("ptr_plot")]] <- shiny::renderPlot({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    res$plot
  })
  invisible(state)
}

#' @rdname ptr_register
#' @export
ptr_register_error <- function(output, state) {
  output[[state$server_ns_fn("ptr_error")]] <- shiny::renderUI({
    res <- state$runtime()
    shared_errs <- state$shared_resolution_errors()
    runtime_msg <- if (!is.null(res) && !isTRUE(res$ok)) {
      cli::ansi_strip(res$error %||% "")
    } else NULL
    parts <- c(shared_errs, if (!is.null(runtime_msg)) runtime_msg)
    if (length(parts) == 0L) return(NULL)
    ptr_error_ui(paste(parts, collapse = "\n"))
  })
  invisible(state)
}

# Render the code-panel text for a runtime result + extras source list.
# Used by both `ptr_register_code` (reactive output) and
# `ptr_extract_code` (read accessor).
format_code_with_extras <- function(res, extras_exprs) {
  base <- if (is.null(res)) "" else (res$code_text %||% "")
  if (is.null(res) || !isTRUE(res$ok) || length(extras_exprs) == 0L) {
    return(base)
  }
  extra_text <- vapply(extras_exprs, function(e) {
    if (rlang::is_quosure(e)) rlang::quo_text(e) else
      paste(deparse(e), collapse = " ")
  }, character(1))
  if (!nzchar(base)) {
    return(paste(extra_text, collapse = " +\n  "))
  }
  paste(c(base, extra_text), collapse = " +\n  ")
}

#' @rdname ptr_register
#' @export
ptr_register_code <- function(output, state) {
  output[[state$server_ns_fn("ptr_code")]] <- shiny::renderText({
    format_code_with_extras(state$runtime(), state$extras_exprs())
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
ptr_extract_code  <- function(state) {
  shiny::isolate(format_code_with_extras(state$runtime(), state$extras_exprs()))
}

# ---- ptr_gg_extra ----

#' Add `ggplot2` Layers Programmatically
#'
#' Evaluate one or more `ggplot2` expressions and attach the results as
#' "extras" on the state. Extras are folded into the plot during the next
#' runtime cycle when `state$runtime()$ok` is `TRUE`. Eval failures leave
#' the existing extras untouched.
#'
#' @param state A `ptr_state` from [ptr_server_state()].
#' @param ... `ggplot2` layer expressions (e.g.
#'   `ptr_gg_extra(state, ggplot2::scale_x_log10(), theme_minimal())`).
#'   Captured unevaluated and stored as quosures, then evaluated in
#'   `state$eval_env`. Eval errors propagate and leave the existing
#'   extras untouched (atomic update).
#'
#' @return `state`, invisibly.
#' @export
ptr_gg_extra <- function(state, ...) {
  quos <- rlang::enquos(..., .named = FALSE)
  if (length(quos) == 0L) return(invisible(state))
  # Evaluate all quosures up-front; if any raises, the assignment to
  # `state$extras_exprs` / `state$extras` below never runs, so a failed
  # call leaves the existing extras intact (P12.12).
  evaluated <- lapply(quos, rlang::eval_tidy, env = state$eval_env)
  # Replace-per-call: each invocation overwrites the previously captured
  # extras (legacy semantics; documented in `dev/scripts/feature-sweep.R`
  # note 16). Update source exprs first so any renderText that reads
  # both `state$extras_exprs()` and `state$extras()` sees a consistent
  # pair under reactive flushing.
  state$extras_exprs(quos)
  state$extras(evaluated)
  invisible(state)
}
