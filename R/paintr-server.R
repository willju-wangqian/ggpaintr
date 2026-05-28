# P12 — server-state + observer wiring for the typed-AST core.
#
# `ptr_init_state` builds a long-lived state list that the Shiny server
# carries: the typed tree, per-pipeline-layer resolved-data caches, the latest
# runtime result (post-substitute → post-prune → post-eval), the resolved
# checkbox-defaults vector, and an upstream-resolution memo cache.
#
# `ptr_server_internal` is the wiring sugar that calls the state constructor and
# attaches the observers (pipeline updates + runtime).

#' Construct the ggpaintr runtime state container
#'
#' Builds the `ptr_state` object — the translated typed AST (as a
#' `reactiveVal`), the runtime result, the per-layer resolved-data caches,
#' the eval environment, the input-snapshot machinery, and the shared
#' bindings / draw trigger — used by [ptr_server()] and the advanced-embedder
#' accessors ([ptr_extract_plot()] / [ptr_extract_error()] /
#' [ptr_extract_code()], [ptr_gg_extra()]).
#'
#' This is a *state container*, not a from-scratch reactive-app builder: it
#' allocates the reactives but does not attach the pipeline / runtime
#' observers (those live in internal `ptr_setup_*` helpers wired by
#' [ptr_server()]). Reach for `ptr_init_state()` directly when you want to
#' drive the typed tree programmatically or exercise ggpaintr under
#' [shiny::testServer()]; for a fully wired app use [ptr_server()].
#'
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Optional named list of copy overrides; see [ptr_ui_text()]
#'   for the full schema and current defaults.
#' @param expr_check Controls `ppExpr` placeholder validation: `TRUE` (default)
#'   applies the built-in denylist + AST walker; `FALSE` disables all
#'   validation; a `list` with `deny_list`/`allow_list` entries customises
#'   the policy. See `vignette("ggpaintr-safety")`.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after substitution.
#' @param shared Named list of reactives (one per shared key) supplied by an
#'   outer wrapper such as [ptr_app_grid()]. Defaults to `list()`.
#' @param draw_trigger Optional reactive whose invalidation forces a redraw
#'   (e.g. the grid app's "Draw all" button).
#' @param producer_debounce_ms Optional. Controls the debounce window applied
#'   to producer-style placeholder inputs (`ppText`, `ppNum`, `ppExpr`) before they
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
#'   host-computed resolutions for shared data-consumer (`ppVar`) widgets,
#'   as returned by `ptr_resolve_shared_consumers()`. When an entry is
#'   present, the runtime validates that key's selection against the
#'   host-resolved upstream (the same data the host picker was built
#'   from) instead of the per-layer `node$upstream`, so a value valid in
#'   the host picker is never rejected by one layer's narrower upstream.
#'   Defaults to `list()` (no host resolutions; per-layer behaviour).
#' @param shared_stage_enabled Named list (keyed by raw shared key) of
#'   reactives, each returning a logical, that toggle the orphan pipeline
#'   stages owned by that shared key (as carried in a
#'   [ptr_shared_server()] bundle). A missing or unset entry leaves the
#'   stage enabled. Defaults to `list()`.
#' @param plots Optional list of formula strings for grid contexts. When
#'   supplied (typically by [ptr_app_grid()]), the validator for `shared`
#'   bindings cross-checks shared-key references against every plot's
#'   placeholder set, so a `shared = "..."` annotation that exists in
#'   plot 2 but not plot 1 still validates. Defaults to `NULL` (single-
#'   plot context — only `formula` is checked).
#'
#' @return A `ptr_state` list (S3 class `c("ptr_state", "list")`).
#' @examples
#' shiny::isolate({
#'   state <- ptr_init_state(
#'     "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
#'   )
#'   is.list(state)
#' })
#' @export
ptr_init_state <- function(formula,
                                envir = parent.frame(),
                                ui_text = NULL,
                                expr_check = TRUE,
                                safe_to_remove = character(),
                                shared = list(),
                                draw_trigger = NULL,
                                producer_debounce_ms = NULL,
                                ns = shiny::NS(NULL),
                                server_ns = ns,
                                auto_bind_shared = FALSE,
                                shared_resolutions = list(),
                                shared_stage_enabled = list(),
                                panel_sources = list(),
                                plots = NULL) {
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
  pipe_layer_warnings <- detect_pipe_layer_misuse(tree)

  shared_bindings <- ptr_validate_shared_bindings(
    shared, tree = tree, plots = plots,
    strict_missing = !isTRUE(auto_bind_shared)
  )

  # ADR 0020: the node-level `default_active` field (stamped by `ppLayerOff`)
  # is the single source of truth for layer-checkbox boot state, read at the
  # snapshot site (`ptr_default_snapshot()`) and at the spec-emission diff
  # baseline (`ptr_spec_defaults_from_state()`). The deprecated per-call
  # argument and the global option were removed straight out in Plan 04
  # (no external users to deprecate-warn for; see ADR 0020 §Deprecation).
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

  # Data-source placeholders that are NOT a layer's bare `data_arg` -- e.g.
  # `upload` at the head of a pipeline (`upload |> head(num) |> ggplot(...)`).
  # A bare-layer source has its resolved frame swapped into `data_arg` by
  # `inject_resolved_data()`; a pipeline-head source survives substitution
  # only as a symbol, so `ptr_setup_pipelines()` binds the resolved frame
  # into `eval_env` and bumps these reactiveVals so the per-consumer reactives
  # in `ptr_setup_consumer_uis()` re-resolve once a file lands. Keyed by
  # source-node id.
  bare_source_ids <- vapply(
    tree$layers,
    function(l) {
      if (is_bare_data_source_layer(l)) l$data_arg$id %||% NA_character_
      else NA_character_
    },
    character(1)
  )
  bare_source_ids <- bare_source_ids[!is.na(bare_source_ids)]
  pipeline_source_ids <- character()
  for (s in find_nodes(tree, is_ptr_ph_data_source)) {
    if (!is.null(s$id) && !(s$id %in% bare_source_ids)) {
      pipeline_source_ids <- c(pipeline_source_ids, s$id)
    }
  }
  resolved_sources <- stats::setNames(
    lapply(pipeline_source_ids, function(.) shiny::reactiveVal(NULL)),
    pipeline_source_ids
  )

  # ADR 0015 PLAN-02 / Option E: per-source reactiveVal holding the binding
  # name actually bound into `state$eval_env` by `ptr_setup_pipelines()`.
  # Written by `bind_source_value()` AFTER `assign(name, df, eval_env)`;
  # consumers `req()` it to halt until the server-side binding lands,
  # replacing PLAN-01's client-text + `exists()` poll. Keyed identically
  # to `resolved_data` (layer_name) and `resolved_sources` (source_id);
  # union of both keysets so a single lookup `state$bound_names[[k]]()`
  # works for both bare-data and pipeline-head sources.
  bound_names_keys <- unique(c(data_layer_names, pipeline_source_ids))
  bound_names <- stats::setNames(
    lapply(bound_names_keys, function(.) shiny::reactiveVal(NULL)),
    bound_names_keys
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

  state <- structure(list(
    tree = shiny::reactiveVal(tree),
    runtime = shiny::reactiveVal(NULL),
    # Cache of the most recent ok-runtime result. Populated by
    # `ptr_register_last_ok_cache()` whenever `runtime()` resolves with
    # `isTRUE(res$ok)`; read as a fallback by `ptr_register_code()` /
    # `ptr_register_plot()` so a transient `validate_input` failure
    # surfaces the new error WITHOUT discarding the prior successful
    # code panel and plot panel. NULL until the first successful draw.
    last_ok_runtime = shiny::reactiveVal(NULL),
    extras = shiny::reactiveVal(list()),
    # Source expressions paired with `extras`. `ptr_gg_extra()` updates
    # both atomically so the code panel can append the user-typed source
    # (extras themselves are evaluated ggplot objects with no
    # round-trippable deparse).
    extras_exprs = shiny::reactiveVal(list()),
    # A child of the caller's env so the pipeline-head data-source observer
    # in `ptr_setup_pipelines()` can bind uploaded frames under their
    # dataset name without mutating the caller's environment.
    eval_env = new.env(parent = envir),
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    raw_ui_text = ui_text,
    effective_ui_text = ui_text,
    # Static diagnostics from `ptr_translate()` — surfaced in `#ptr_error`
    # alongside runtime errors. Set once at init; never mutates.
    pipe_layer_warnings = pipe_layer_warnings,
    shared_bindings = shared_bindings,
    shared_resolutions = if (is.list(shared_resolutions)) shared_resolutions else list(),
    # ADR 0023 §1: bundle's `panel_sources` slot, threaded through the
    # existing `do.call(ptr_init_state, c(list(formula, envir), dots))`
    # channel in `ptr_server_internal()`. Named list of reactive
    # data.frame values, keyed by canonical shared id (`shared_<key>`);
    # populated by host `ptr_setup_panel_sources()` (Plan 04) and read
    # by per-instance pipelines/consumer pickers (Plans 05/07). Empty
    # list when no panel-owned source is bound.
    panel_sources = if (is.list(panel_sources)) panel_sources else list(),
    draw_trigger = draw_trigger,
    resolved_data = resolved_data,
    resolved_sources = resolved_sources,
    # ADR 0015 PLAN-02 / Option E: per-source `reactiveVal` of the bound
    # name. Written by `bind_source_value()` AFTER the eval_env assign;
    # `req()`'d by consumer entry to remove PLAN-01's polling workaround.
    bound_names = bound_names,
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
    # Per-source resolve-stage errors (e.g. uploading a file with an
    # unsupported extension). Populated by the upload/source observers in
    # `ptr_setup_pipelines()` and surfaced inline by `ptr_register_error()`
    # so the user sees the abort message verbatim instead of the
    # downstream "object '<name>' not found". Keyed by source id (or layer
    # name for bare-data sources). A successful resolve clears that key.
    resolve_errors = shiny::reactiveVal(stats::setNames(list(), character())),
    # ADR 0025 §4 / PLAN-04: per-active-upload prologue ledger. Named list
    # keyed by `key` (layer name for bare-data sources, source id for
    # pipeline-head sources). Each entry is
    # `list(auto_name=<chr>, file_name=<chr>, ext=<chr>)`. Written by
    # `resolve_upload_source()` / `try_bind_source_default_resolved()` on a
    # successful upload bind (built-in `ppUpload` only); cleared on a
    # vacated bind (file_info NULL, no shortcut value, no default). Read by
    # `emit_upload_prologue()` and prepended to the code-panel text by
    # `format_code_with_extras()`. Insertion order preserves declaration
    # order across tree traversal, so the rendered prologue lines appear
    # in formula order. Empty list when no upload-bound source is active.
    active_uploads = shiny::reactiveVal(stats::setNames(list(), character())),
    ui_ns_fn = ns,
    server_ns_fn = server_ns,
    input_spec = ptr_runtime_input_spec(tree),
    # ADR 0012 / PLAN-01 (Bug B): per-instance, bare-id-keyed seed of
    # initial values for placeholder widgets. `apply_spec_at_boot()`
    # writes here BEFORE its deferred `session$onFlushed` dispatch so the
    # first fire of every value/source/consumer renderUI seeds its widget
    # via `extra$selected = isolate(state$spec_seed[[bare]]) %||% ...`.
    # The single hook contract (`build_ui = function(node, label, ...)`)
    # is the choke-point through which both built-in and custom-keyword
    # placeholders receive the spec value — there is no second hook.
    #
    # Stored as an environment (not a list) so writes from
    # `apply_spec_at_boot()` — which receives `state` by value — propagate
    # to the renderUI closures captured by `ptr_setup_*_uis()`. R lists are
    # pass-by-value (copy-on-modify); envs are pass-by-reference. Same
    # pattern as `producer_input` / `upstream_cache` above. `spec_seed[[id]]`
    # access works identically for env or list, so all read sites are
    # untouched. Empty env when no `spec=` was passed.
    spec_seed = new.env(parent = emptyenv())
  ), class = c("ptr_state", "list"))

  # ADR 0012 §3.5 / PLAN-05: `state$spec` is a pull-style reactive over
  # `state$runtime()`'s frozen snapshot. It is a sparse named list of
  # fully-qualified namespaced input ids -> values, dropping every entry
  # whose value `identical()`-matches the registry/default for that id.
  # Empty named list when runtime hasn't fired yet (no observer
  # side-effects). The reactive captures the constructed `state` by
  # reference so callers may treat `state$spec` like any other reactive
  # on the object.
  st <- state
  st$spec <- shiny::reactive({
    res <- st$runtime()
    snapshot <- if (is.null(res)) list() else (res$snapshot %||% list())
    if (length(snapshot) == 0L) {
      return(stats::setNames(list(), character()))
    }
    # Source placeholders with a shortcut textInput (e.g. ppUpload) carry
    # the round-trip identity at the shortcut's input_id; the source's
    # own value is a Shiny artifact (e.g. a fileInput data.frame holding a
    # per-session tempfile path) that neither survives a session nor
    # round-trips through deparse(). Same rule as
    # `stamp_current_pick_walk.ptr_ph_data_source` (R/paintr-render.R) and
    # the apply-side ppUpload seed skip in `apply_spec_at_boot()` below;
    # enforced structurally on the `source_id` linkage in `input_spec` so
    # every shortcut-shaped source — including third-party ones registered
    # via `ptr_define_placeholder_source(shortcut = TRUE)` — is
    # covered. See `?ptr_define_placeholder_source` ("spec= round-trip")
    # for the contract sources WITHOUT a shortcut must honour. (The
    # role-label spec_df$role == "source_companion" is preserved as the
    # spec/seed dispatch key — see ADR 0025 §1.)
    spec_df <- st$input_spec
    if (is.data.frame(spec_df) && nrow(spec_df) > 0L) {
      companioned <- spec_df$source_id[
        !is.na(spec_df$role) & spec_df$role == "source_companion"
      ]
      if (length(companioned) > 0L) {
        snapshot <- snapshot[setdiff(names(snapshot), companioned)]
        if (length(snapshot) == 0L) {
          return(stats::setNames(list(), character()))
        }
      }
    }
    defaults <- ptr_spec_defaults_from_state(st)
    sparse <- ptr_spec_from_snapshot(snapshot, defaults)
    if (length(sparse) == 0L) {
      return(stats::setNames(list(), character()))
    }
    # Namespace the raw input ids -> fully-qualified ids
    # (ADR 0012 §3.5: keyed by fully-qualified Shiny input id).
    nm <- vapply(names(sparse), st$server_ns_fn, character(1))
    stats::setNames(sparse, nm)
  })
  st
}

# ADR 0012 §3.5 / PLAN-05 + ADR 0020 PLAN-02 — derive the default-input
# snapshot the runtime would have started from, for diffing against the
# frozen runtime snapshot. Mirrors `ptr_default_snapshot()` exactly,
# reading the carrier-node fields via the shared `find_layer_by_name()` /
# `find_stage_call_by_id()` helpers so the snapshot site and this site
# stay aligned on which field they consult. Placeholder/source-companion
# roles default to NULL (no registry-documented default => emit on diff).
# `layer_checkbox` defaults to `isTRUE(node$default_active %||% TRUE)`.
# `stage_enabled` defaults to `isTRUE(node$default_stage_enabled %||% TRUE)`.
ptr_spec_defaults_from_state <- function(state) {
  spec <- state$input_spec
  defaults <- list()
  if (nrow(spec) == 0L) return(defaults)
  # `state$tree` is a `reactiveVal` (function) in production calls (this
  # function fires inside the `state$spec` reactive built in
  # `ptr_init_state`) but can also be a plain ptr_root in unit-test calls.
  # `isolate()` is safe in both contexts — for a non-reactive plain object
  # the call passes through unchanged.
  tree <- if (is.function(state$tree)) {
    shiny::isolate(state$tree())
  } else {
    state$tree
  }
  for (i in seq_len(nrow(spec))) {
    raw_id <- spec$input_id[i]
    role   <- spec$role[i]
    if (identical(role, "layer_checkbox")) {
      layer_name <- spec$layer_name[i]
      carrier <- find_layer_by_name(tree, layer_name)
      defaults[[raw_id]] <- isTRUE(carrier$default_active %||% TRUE)
    } else if (identical(role, "stage_enabled")) {
      carrier <- find_stage_call_by_id(tree, raw_id)
      defaults[[raw_id]] <- isTRUE(carrier$default_stage_enabled %||% TRUE)
    } else {
      defaults[raw_id] <- list(NULL)
    }
  }
  defaults
}

# ADR 0012 §3.5 / PLAN-05 — pure: compute the sparse spec by diffing the
# runtime snapshot against the registry/default snapshot. Keys are raw
# input ids (namespacing is the caller's job). Default comparison is
# `base::identical()` only -- no type coercion (Shiny preserves type and
# coerced equality would silently drop genuine picks). Missing default
# for an id => treat snapshot value as non-default (emit it).
ptr_spec_from_snapshot <- function(snapshot, defaults) {
  if (length(snapshot) == 0L) {
    return(stats::setNames(list(), character()))
  }
  if (is.null(defaults)) defaults <- list()
  keep_names <- character()
  out <- list()
  for (nm in names(snapshot)) {
    if (!is.null(nm) && nzchar(nm) && nm %in% names(defaults) &&
        identical(snapshot[[nm]], defaults[[nm]])) {
      next
    }
    out[[length(out) + 1L]] <- snapshot[[nm]]
    keep_names <- c(keep_names, nm)
  }
  names(out) <- keep_names
  out
}

# ADR 0012 §3.5 / PLAN-05 — pure: union the per-plot specs into one flat
# named list keyed by the already-fully-qualified namespaced id. If two
# specs supply the same fully-qualified id (a namespacing bug), abort
# naming the colliding id; never silently choose. Used by grid wrappers
# (plan 06) to produce a single grid-wide spec from the per-plot
# `state$spec()` reactives.
ptr_spec_combine <- function(specs) {
  if (length(specs) == 0L) {
    return(stats::setNames(list(), character()))
  }
  out <- list()
  keep_names <- character()
  for (s in specs) {
    if (is.null(s) || length(s) == 0L) next
    for (nm in names(s)) {
      if (nm %in% keep_names) {
        rlang::abort(paste0(
          "ptr_spec_combine: colliding fully-qualified id: ", nm, "."
        ))
      }
      out[[length(out) + 1L]] <- s[[nm]]
      keep_names <- c(keep_names, nm)
    }
  }
  names(out) <- keep_names
  out
}

# ADR 0012 §3.5 / PLAN-05 — render a sparse spec list as R source code
# text for the preserve-mode code panel. Empty spec collapses to "" so
# the caller can concatenate unconditionally. Backtick-quotes every id
# (Shiny ids may contain digits, hyphens, and underscores; backticks are
# the safest deparse-stable shape). Values go through `deparse()`.
format_spec_for_panel <- function(spec) {
  if (length(spec) == 0L) return("")
  nms <- names(spec)
  if (is.null(nms) || any(!nzchar(nms))) {
    rlang::abort("format_spec_for_panel: every spec entry must have a non-empty name.")
  }
  entries <- vapply(seq_along(spec), function(i) {
    val_text <- paste(deparse(spec[[i]]), collapse = "\n")
    paste0("  `", nms[[i]], "` = ", val_text)
  }, character(1))
  paste0("ptr_spec <- list(\n", paste(entries, collapse = ",\n"), "\n)")
}

# Lightweight shape check for the `ptr_state` object created by
# `ptr_init_state()`. Public functions that take `state` (the
# `ptr_register_*` wirings, the `ptr_extract_*` accessors) run this first so
# a mis-constructed state fails loudly instead of erroring deep in a reactive.
ptr_validate_state <- function(state) {
  if (!is.list(state)) {
    rlang::abort("`state` must be a `ptr_state` list (from `ptr_init_state()`).")
  }
  needed <- c("tree", "runtime", "last_ok_runtime", "extras", "extras_exprs",
              "server_ns_fn", "ui_ns_fn", "eval_env", "input_spec")
  miss <- setdiff(needed, names(state))
  if (length(miss) > 0L) {
    rlang::abort(paste0(
      "`state` is missing required entries: ", paste(miss, collapse = ", "), "."
    ))
  }
  for (nm in c("tree", "runtime", "last_ok_runtime", "extras", "extras_exprs",
               "server_ns_fn", "ui_ns_fn")) {
    if (!is.function(state[[nm]])) {
      rlang::abort(paste0("`state$", nm, "` must be a function."))
    }
  }
  invisible(TRUE)
}

# ADR 0012 §3.6 / PLAN-06 — apply a session-boot `spec=` to widget state.
#
# `spec` is a named list of fully-qualified Shiny input ids -> values
# (the shape PLAN-05's `state$spec` reactive emits and `format_spec_for_panel`
# round-trips). This helper:
#
#   1. Filters `spec` to entries whose id starts with this engine instance's
#      namespace prefix (so a flat spec for `ptr_app_grid` reaches each
#      per-plot engine and each takes only its own slice).
#   2. Drops entries whose stripped (bare) id is unknown to this instance's
#      `state$input_spec` -- aggregated into ONE `cli::cli_inform` listing
#      every dropped id.
#   3. Dispatches `updateXyzInput(session, bare_id, value)` per surviving
#      entry, keyed by the widget kind implied by `role` + `keyword` (see
#      `apply_spec_entry` below). Bare ids are used because under
#      `shiny::moduleServer` the session auto-namespaces; at top level
#      `session$ns` is identity.
#   4. Wraps every `updateXyz` call inside `session$onFlushed(once = TRUE)`
#      so widgets exist when touched -- in particular the suspended `var`
#      pickers under the layer "Data" subtab and the renderUI-built
#      shared widgets are bound by the time the first flush fires.
#
# `var` placeholders (multi-select picker with `maxOptions = 1L`) accept
# whatever value is set; the widget's own choice set may still be empty at
# boot (e.g. before an upload resolves), so `selected = value` is harmless
# -- the picker shows nothing until the choices arrive, then auto-selects
# matching value(s).
#
# One-shot only -- the spec is consumed once and never re-applied (ADR
# 0012 §2 row "Apply timing of spec="; reactive re-application is
# deferred). No observer is created here.
apply_spec_at_boot <- function(spec, session, state) {
  if (is.null(spec) || length(spec) == 0L) return(invisible())
  if (!is.list(spec)) {
    rlang::abort("`spec` must be a named list of input id -> value.")
  }
  nms <- names(spec)
  if (is.null(nms) || any(!nzchar(nms)) || anyNA(nms)) {
    rlang::abort("`spec` must be fully named with non-empty input ids.")
  }
  # Prefix derivation: use `state$server_ns_fn("")` -- the same NS function
  # the widgets are bound under -- NOT `session$ns("")`. Under a real
  # browser session at top level, both return ""; under a moduleServer
  # ptr_server_internal auto-wires server_ns_fn from session$ns. But
  # under shiny::testServer, MockShinySession's session$ns prefixes with
  # "mock-session-" -- which never matches user-supplied spec ids like
  # "ggplot_1_1_ppCustomChoice_NA", silently dropping the entire spec.
  # `state$server_ns_fn` is whatever ptr_init_state was given (default
  # NS(NULL) at top level) so it always matches the widgets' actual
  # namespace. Pre-existing PLAN-06 bug, surfaced by PLAN-01's testServer
  # SC-1.
  prefix <- tryCatch(state$server_ns_fn(""), error = function(e) "")
  if (is.null(prefix)) prefix <- ""
  keep <- startsWith(nms, prefix)
  if (!any(keep)) return(invisible())
  spec <- spec[keep]
  nms <- nms[keep]
  bare_ids <- substring(nms, nchar(prefix) + 1L)

  # `state$input_spec` rows: input_id, role, layer_name, keyword,
  # param_key, source_id, shared.
  spec_df <- state$input_spec
  known_ids <- if (is.data.frame(spec_df) && nrow(spec_df) > 0L) {
    spec_df$input_id
  } else character()

  dropped <- character()
  rows_to_apply <- list()
  for (i in seq_along(bare_ids)) {
    bid <- bare_ids[[i]]
    fq  <- nms[[i]]
    idx <- match(bid, known_ids, nomatch = 0L)
    if (idx == 0L) {
      dropped <- c(dropped, fq)
      next
    }
    rows_to_apply[[length(rows_to_apply) + 1L]] <- list(
      fq = fq,
      bare_id = bid,
      value = spec[[i]],
      role = spec_df$role[idx],
      keyword = spec_df$keyword[idx]
    )
  }

  if (length(dropped) > 0L) {
    cli::cli_inform(c(
      "i" = "Dropped {length(dropped)} unknown id{?s} from {.arg spec}: {.val {dropped}}"
    ))
  }

  if (length(rows_to_apply) == 0L) return(invisible())

  # ADR 0012 / PLAN-01 (Bug B): write per-instance seed values into
  # `state$spec_seed` BEFORE registering the deferred dispatch. The
  # renderUI bodies in `ptr_setup_value_uis()` / `ptr_setup_source_uis()`
  # / `ptr_setup_consumer_uis()` read `isolate(state$spec_seed[[raw_id]])`
  # on first fire; if we deferred the seed write into the onFlushed
  # callback, those reads would race the first render and miss the seed.
  #
  # Seed eligibility: roles that route through renderUI (the renderUI is
  # the choke-point that respects the seed). `placeholder` rows (except
  # ppUpload — its fileInput cannot accept a programmatic value) and
  # `source_companion` rows (textInput inside the source's tagList).
  # `layer_checkbox` / `stage_enabled` rows are framework-internal
  # checkboxes that stay on `updateCheckboxInput` per PLAN-01's "no
  # framework-internal renderUI conversion" constraint.
  # Type normalization happens HERE (PLAN-02 collapse) so the seed value
  # the renderUI hook reads is the same shape the equivalent
  # `updateXyzInput` would have set. Built-in keywords get their type
  # branch; unknown keywords (custom placeholders) write `value` verbatim
  # so the hook's `selected` formal sees exactly what was specified.
  # Invalid built-in values (e.g. non-numeric for ppNum) are left OUT of
  # the seed so the dispatch path still surfaces them as warnings.
  seeded <- character()
  for (row in rows_to_apply) {
    write_seed <- (identical(row$role, "placeholder") &&
                   !identical(row$keyword, "ppUpload")) ||
                  identical(row$role, "source_companion")
    if (!write_seed) next

    value <- row$value
    if (identical(row$role, "source_companion") ||
        identical(row$keyword, "ppText")) {
      if (is.null(value)) value <- ""
      else value <- as.character(value)[[1L]]
    } else if (identical(row$keyword, "ppNum")) {
      if (is.null(value) || (is.character(value) && !nzchar(value))) {
        value <- NA_real_
      } else {
        num <- suppressWarnings(as.numeric(value)[[1L]])
        if (is.na(num) &&
            !identical(row$value, NA_real_) &&
            !identical(row$value, NA_integer_)) {
          next
        }
        value <- num
      }
    } else if (identical(row$keyword, "ppExpr")) {
      if (is.null(value)) {
        value <- ""
      } else if (is.language(value)) {
        value <- paste(deparse(value), collapse = "\n")
      } else {
        value <- as.character(value)[[1L]]
      }
    } else if (identical(row$keyword, "ppVar")) {
      if (is.null(value)) value <- character()
      else value <- as.character(value)
    }
    state$spec_seed[[row$bare_id]] <- value
    # FINDING #8 (placeholder-role-coverage2 v4): `source_companion` rows
    # have NO downstream `state$spec_seed[[<companion-id>]]` reader. The
    # author's intent in the seed-eligibility comment above was that the
    # companion textInput inside the source's tagList would read the seed
    # on first render, but `ptr_builtin_upload_build_ui()` initial-renders
    # the companion from `node$default %||% ""` only (the spec_seed reads
    # in renderUI bodies use `raw_id = node$id` — the source id, not the
    # companion id). So source_companion rows must fall through to the
    # onFlushed `updateTextInput` dispatch in `apply_spec_entry()` —
    # don't mark them seeded. The seed write is kept (harmless; reserved
    # for a future renderUI-side reader if one is added).
    if (!identical(row$role, "source_companion")) {
      seeded <- c(seeded, row$bare_id)
    }
  }

  # Dispatch is deferred to the first flush so framework-internal rows
  # (layer_checkbox / stage_enabled) can call `updateCheckboxInput()`
  # after the stage checkboxes have mounted. Seed-applied rows are
  # SKIPPED here (PLAN-02 collapse) — they are already carried into the
  # renderUI's `selected` argument. Re-firing `updateXyzInput()` for them
  # would be a no-op for built-ins but a FALSE return for custom keywords
  # (no dispatch branch), which aggregated into the spurious "Skipped N
  # spec entries with invalid value" warning (Bug B).
  session$onFlushed(once = TRUE, function() {
    invalid <- character()
    for (row in rows_to_apply) {
      if (row$bare_id %in% seeded) next
      ok <- apply_spec_entry(session, row)
      if (isFALSE(ok)) invalid <- c(invalid, row$fq)
    }
    if (length(invalid) > 0L) {
      cli::cli_warn(c(
        "!" = "Skipped {length(invalid)} {.arg spec} entr{?y/ies} with an invalid value: {.val {invalid}}"
      ))
    }
  })

  invisible()
}

# Dispatch one spec entry to the matching `updateXyzInput`. Returns TRUE
# on a successful update, FALSE if the value was invalid for the widget
# (so the caller can aggregate per-id warnings). Per PLAN-02 (Bug B
# spec-apply uniform), placeholder rows (`role == "placeholder"`) are
# seed-only: their values are written to `state$spec_seed` in
# `apply_spec_at_boot`, and the renderUI hook's `selected` formal carries
# the value into the widget on first fire. The dispatch callback skips
# seeded rows entirely (`if (row$bare_id %in% seeded) next`), so the
# per-keyword update branches for ppText / ppNum / ppExpr / ppVar are
# gone. The only reasons control flow now reaches this function:
#   role == "layer_checkbox" | "stage_enabled" -> updateCheckboxInput
#   role == "source_companion"                 -> updateTextInput
#   keyword == "ppUpload"                      -> silent skip (fileInput
#       is not programmatically settable; its companion textInput is
#       reached via the "source_companion" branch above).
# Any other row (e.g. a built-in keyword whose value was invalid in the
# seed loop and therefore left unseeded) falls through to the terminal
# FALSE, which the dispatch callback aggregates into the
# "Skipped N spec entries with an invalid value" warning.
apply_spec_entry <- function(session, row) {
  role    <- row$role
  keyword <- row$keyword
  id      <- row$bare_id
  value   <- row$value

  if (identical(role, "layer_checkbox") || identical(role, "stage_enabled")) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(FALSE)
    }
    shiny::updateCheckboxInput(session, id, value = value)
    return(TRUE)
  }
  if (identical(role, "source_companion")) {
    if (is.null(value)) value <- ""
    shiny::updateTextInput(session, id, value = as.character(value)[[1L]])
    return(TRUE)
  }
  if (identical(keyword, "ppUpload")) {
    return(TRUE)
  }
  FALSE
}

# ---- public wiring entry point ----

#' Internal: wire a `ggpaintr` server from a formula (engine)
#'
#' Unexported plumbing behind the single public [ptr_server()]; not a user
#' entry point (ADR 0006 — it does not bind shared placeholders on its own).
#' Convenience wrapper that builds the `ptr_state` via [ptr_init_state()]
#' and attaches the per-pipeline observers, stage-enabled toggles, runtime
#' observer, and plot/code/error output bindings. Returns the state list so
#' embedders can attach extras (`ptr_gg_extra`) or drive the typed tree
#' programmatically.
#'
#' The bindings write to a fixed set of `ptr_`-prefixed top-level ids
#' (`ptr_plot`, `ptr_error`, `ptr_code`, `ptr_update_plot`, plus the
#' internal layer-nav inputs). Treat the whole `ptr_` prefix as reserved
#' — most relevant when hand-placing L3 pieces. The single source of truth
#' for the id contract (and the `ptr_server()` namespacing) is the
#' *"How input ids are built"* section of `vignette("ggpaintr-use-cases")`.
#'
#' @param input,output,session The standard Shiny server arguments.
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ... Forwarded to [ptr_init_state()] (e.g. `shared`,
#'   `draw_trigger`, `ui_text`, `expr_check`, `safe_to_remove`, `ns`).
#' @param shared_state Optional `ptr_shared_state` bundle from
#'   [ptr_shared_server()]. When supplied, its `shared`, `draw_trigger`,
#'   `shared_resolutions` and `shared_stage_enabled` slots seed the
#'   matching [ptr_init_state()] arguments (an explicit value passed
#'   through `...` still wins). This is the convenience path for wiring a
#'   page-level [ptr_shared_panel()] panel to a single embedded plot — the
#'   same bundle [ptr_server()] accepts. Defaults to `NULL`.
#' @param spec An optional named list of fully-qualified Shiny input id ->
#'   value, used to override widget defaults at session boot. See
#'   [ADR 0012](dev/adr/0012-role-based-tree-and-ptr-spec.html).
#'
#' @return The `ptr_state` list.
#' @seealso [ptr_shared_server()], [ptr_server()]
#' @examples
#' if (interactive()) {
#'   f <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       ptr_ui_assets(),
#'       shiny::tags$div(
#'         class = "ptr-app",
#'         shiny::sidebarLayout(
#'           shiny::sidebarPanel(ptr_ui_controls(formula = f)),
#'           shiny::mainPanel(ptr_ui_plot())
#'         )
#'       )
#'     ),
#'     server = function(input, output, session) {
#'       ptr_server_internal(input, output, session, f)
#'     }
#'   )
#' }
#' @keywords internal
#' @noRd
ptr_server_internal <- function(input, output, session, formula,
                          envir = parent.frame(), ...,
                          shared_state = NULL,
                          spec = NULL) {
  dots <- list(...)
  # `shared_state` mirrors `ptr_server()`'s convenience path: a
  # one-shot bundle from `ptr_shared_server()` carrying `shared`,
  # `draw_trigger`, `shared_resolutions`, `shared_stage_enabled`. Each
  # slot only seeds the corresponding `...`/`ptr_init_state()` arg when
  # the caller did not pass it explicitly (explicit `...` wins).
  if (!is.null(shared_state)) {
    validate_ptr_shared_state(shared_state)
    if (is.null(dots$shared))               dots$shared <- shared_state$shared
    if (is.null(dots$draw_trigger))         dots$draw_trigger <- shared_state$draw_trigger
    if (is.null(dots$shared_resolutions))   dots$shared_resolutions <- shared_state$shared_resolutions
    if (is.null(dots$shared_stage_enabled)) dots$shared_stage_enabled <- shared_state$shared_stage_enabled
    # ADR 0023 §1: same bundle-channel rule for the panel-owned source
    # payload. Explicit `...` wins, otherwise the bundle's value flows
    # through to `ptr_init_state(panel_sources = ...)` via `do.call()`
    # without changing the call site.
    if (is.null(dots$panel_sources))        dots$panel_sources <- shared_state$panel_sources
  }
  # When called inside `shiny::moduleServer(id, ...)` without explicit
  # `ns` / `server_ns`, auto-wire them the same way `ptr_server()`
  # does: tags emitted via `renderUI` must carry the module prefix
  # (`session$ns`), while `input[[]]` / `output[[]]` / `updateXxx()` use
  # bare ids since the moduleServer session auto-namespaces those. At the
  # top level, `session$ns` is `NS(NULL)`, so both paths collapse to the
  # historical identity behaviour. Top-level callers like `ptr_app()` pass
  # `ns` explicitly and skip this branch.
  if (is.null(dots$ns) && inherits(session, "session_proxy")) {
    dots$ns <- session$ns
    if (is.null(dots$server_ns)) dots$server_ns <- shiny::NS(NULL)
  }
  state <- do.call(ptr_init_state,
                   c(list(formula = formula, envir = envir), dots))
  shared_stage_enabled <- dots[["shared_stage_enabled"]] %||% list()
  ptr_setup_producer_inputs(state, input, output, session)
  ptr_setup_pipelines(state, input, output, session)
  ptr_setup_stage_enabled(state, input, output, session)
  ptr_setup_shared_stage_enabled(state, shared_stage_enabled)
  ptr_setup_runtime(state, input, output, session)
  # ADR 0012 / PLAN-01 (Bug B): value + source renderUIs must mount in
  # the same first-flush cycle as the consumer renderUI so the
  # `state$spec_seed` write inside `apply_spec_at_boot()` (also synchronous
  # at boot, BEFORE its deferred `session$onFlushed` dispatch) reaches
  # every placeholder's first render. Call order mirrors the registry-row
  # order in `ptr_runtime_input_spec()`.
  ptr_setup_value_uis(state, input, output, session)
  ptr_setup_source_uis(state, input, output, session)
  ptr_setup_consumer_uis(state, input, output, session)
  ptr_setup_layer_picker(state, input, output, session)
  ptr_setup_layer_panel_classes(state, input, output, session)
  # `ptr_register_last_ok_cache` runs BEFORE `ptr_register_plot` /
  # `ptr_register_code` so the cache observer fires first when
  # `state$runtime()` updates (Shiny observers run in registration order
  # under default priority) — the panel renderers below read the cache
  # as their retain-on-error fallback.
  ptr_register_last_ok_cache(output, state)
  ptr_register_plot(output, state)
  ptr_register_error(output, state)
  ptr_register_code(output, state)
  apply_spec_at_boot(spec, session, state)
  state
}

# ---- per-pipeline-layer observers ----

# Record / clear an upload-or-source resolve error in `state$resolve_errors`.
# `id` is the resolution-stage key (a layer name for bare-data sources, a
# source-node id for pipeline-head sources); `msg` is the abort message, or
# `NULL` to clear. The error panel reads the resulting named list via
# `ptr_register_error()`.
set_resolve_error <- function(state, id, msg) {
  # ADR 0024: isolate the read so the resolve observer (which calls
  # set_resolve_error from its reactive body) does not take a
  # dependency on `state$resolve_errors`. Without the isolate, the
  # clear-then-set pattern inside `resolve_upload_source` becomes a
  # self-invalidating loop the moment a structured error is written:
  # observer reads resolve_errors via this helper -> writes new value
  # -> own dependency invalidates -> observer re-fires -> ... Pre
  # ADR-0024 the no-upload path only ever wrote NULL (idempotent on
  # an empty list), so the latent loop never triggered.
  cur <- shiny::isolate(state$resolve_errors())
  if (is.null(msg)) {
    cur[[id]] <- NULL
  } else {
    cur[[id]] <- msg
  }
  state$resolve_errors(cur)
}

is_bare_data_source_layer <- function(layer) {
  if (!is_ptr_layer(layer)) return(FALSE)
  if (is.null(layer$data_arg)) return(FALSE)
  is_ptr_ph_data_source(layer$data_arg)
}

# ADR 0015 PLAN-02 / Option E: the ONLY writer of `state$bound_names`.
# Encapsulates the load-bearing ordering invariant — `assign()` MUST land
# in `state$eval_env` BEFORE `state$bound_names[[key]](name)` fires, so any
# downstream consumer that `req()`s the reactiveVal observes `eval_env`
# in its post-assign configuration. `slot(df)` runs unconditionally so
# the source's `resolved_data` / `resolved_sources` reactive bumps even
# when no binding name is available (e.g. invalid shortcut text), giving
# downstream observers a chance to halt via the existing `req()` chain.
# Do not split these three writes across call sites — every assign into
# eval_env must travel through this helper so the invariant is physical,
# not conventional. (See dev/audit/audit-adr15-autoname-race-*.html for
# the race PLAN-01's polling workaround was working around.)
bind_source_value <- function(state, key, name, df, slot) {
  if (!is.null(df) && !is.null(name)) {
    assign(name, df, envir = state$eval_env)
    state$bound_names[[key]](name)
  }
  # A source binding just changed (or was vacated). Cached upstream
  # resolves in `state$upstream_cache` are keyed only on the deparsed
  # substituted expression (R/paintr-resolve.R), not on eval_env
  # contents -- so a re-upload that swaps `eval_env[["df_main"]]` would
  # otherwise return the prior result under the same key. Drop the cache
  # wholesale; consumer entry_reactive's are about to fire on the
  # `slot(df)` write below and will re-resolve from scratch. The cache
  # still absorbs the steady-state traffic (stage toggles, producer
  # input edits, Update Plot clicks) where no binding moved. The
  # is.environment guard keeps unit-test mocks that pass a bare list
  # `state` (no `upstream_cache` slot) working.
  if (is.environment(state$upstream_cache)) {
    rm(list = ls(state$upstream_cache, all.names = TRUE),
       envir = state$upstream_cache)
  }
  slot(df)
}

# ADR 0025 §4 / PLAN-04: register / clear an entry on
# `state$active_uploads`. Built-in `ppUpload` only (the source-registry
# `shortcut == TRUE` predicate filters out custom sources whose code-panel
# prologue is deferred per ADR scope cuts -- `prologue_emit_fn` hook is
# explicitly out-of-scope here). The clear path is also used by Plan 05's
# vacate-on-empty observer (file_info NULL + empty shortcut + no default).
register_active_upload <- function(state, key, node, file_info) {
  # Built-in upload only. The keyword stamp is the structural signal:
  # third-party shortcut sources (registered via
  # `ptr_define_placeholder_source(shortcut = TRUE)`) carry their own
  # keyword and are intentionally excluded from prologue emission.
  if (is.null(node) || !identical(node$keyword, "ppUpload")) {
    return(invisible(NULL))
  }
  # `resolve_upload_source()` is also called from the panel-source
  # observer in `paintr-shared-ui.R` with a minimal ad-hoc `state` that
  # has no `active_uploads` slot (host-side panel source bind; the
  # per-instance state populated by `ptr_server()` carries the slot).
  # The prologue is rendered off the per-instance state's code panel, so
  # the host-side panel-source bind has no prologue to register; silently
  # no-op when the slot is absent.
  if (!is.function(state$active_uploads)) return(invisible(NULL))
  if (is.null(file_info) || is.null(file_info$name)) {
    clear_active_upload(state, key)
    return(invisible(NULL))
  }
  auto_name <- node$auto_name
  if (!is.character(auto_name) || length(auto_name) != 1L || !nzchar(auto_name)) {
    return(invisible(NULL))
  }
  ext <- tolower(tools::file_ext(file_info$name))
  current <- shiny::isolate(state$active_uploads())
  entry <- list(auto_name = auto_name,
                file_name = file_info$name,
                ext       = ext)
  prior <- current[[key]]
  if (!is.null(prior) && identical(prior, entry)) {
    return(invisible(NULL))
  }
  # Insertion-order preservation: dropping then re-appending keeps the
  # most-recently-set entry last. The natural first-bind sequence (each
  # key registered once at first successful upload) yields formula /
  # declaration order; later re-binds of the same key keep that key's
  # position only if it was the latest one set, which matches the
  # "current snapshot" semantics callers expect.
  current[[key]] <- NULL
  current[[key]] <- entry
  state$active_uploads(current)
  invisible(NULL)
}

clear_active_upload <- function(state, key) {
  # See note in `register_active_upload()` -- the host-side panel-source
  # observer hands in a minimal ad-hoc `state` without this slot.
  if (!is.function(state$active_uploads)) return(invisible(NULL))
  current <- shiny::isolate(state$active_uploads())
  if (!(key %in% names(current))) return(invisible(NULL))
  current[[key]] <- NULL
  state$active_uploads(current)
  invisible(NULL)
}

# ADR 0025 §7 / PLAN-05 (A1) -- vacate-on-empty. Called from any path that
# falls through to `slot(NULL)` because the source has no value to bind
# (file_info NULL AND shortcut empty AND no default). Reverses three pieces
# of prior-resolve state so downstream system-state surfaces (consumer
# pickers, code-panel prologue, `ptr_runtime_input_spec()`'s spec dump)
# reflect the *current* (vacated) state, not the historic last-good:
#   (1) drop the prior name from `state$eval_env` (scoped: `inherits = FALSE`
#       so caller-env shadows reached via the parent chain are untouched);
#   (2) reset `state$bound_names[[key]]` to NULL so the ADR 0015 §2.1
#       source-ready gate (`req(state$bound_names[[k]]())`) re-blocks until
#       the user supplies a new value;
#   (3) clear `state$active_uploads[[key]]` so the code-panel prologue stops
#       emitting a line for this key (delegates to `clear_active_upload()`).
# Idempotent: safe to call multiple times for the same already-vacated key.
# Note: `state$bound_names[[key]]` is read with `shiny::isolate()` because
# the read happens inside an observer that already subscribes to `input`
# slots; subscribing to `bound_names` from here would create a feedback loop.
vacate_source_binding <- function(state, key) {
  if (!is.list(state$bound_names) || !(key %in% names(state$bound_names))) {
    clear_active_upload(state, key)
    return(invisible(NULL))
  }
  prior_name <- shiny::isolate(state$bound_names[[key]]())
  # Same syntactic-name predicate Plan 02 uses for `binding_name` derivation
  # (see `resolve_upload_source()` / `try_bind_source_default()`). Required
  # because `exists()` with `inherits = FALSE` errors on non-syntactic names
  # on some R versions, and we will only ever have written via that same
  # predicate so any present name must satisfy it.
  if (is.character(prior_name) && length(prior_name) == 1L &&
      nzchar(prior_name) && make.names(prior_name) == prior_name) {
    if (exists(prior_name, envir = state$eval_env, inherits = FALSE)) {
      rm(list = prior_name, envir = state$eval_env, inherits = FALSE)
    }
  }
  state$bound_names[[key]](NULL)
  clear_active_upload(state, key)
  invisible(NULL)
}

# Walk `state$active_uploads()` in declaration (insertion) order and emit
# one prologue line per entry. Unknown extensions are silently omitted
# (ADR 0025 §4 documented default-skip: never substitute a fake reader).
emit_upload_prologue <- function(active_uploads) {
  if (length(active_uploads) == 0L) return("")
  lines <- character(0)
  for (entry in active_uploads) {
    if (!is.list(entry)) next
    auto_name <- entry$auto_name
    file_name <- entry$file_name
    ext       <- entry$ext
    if (!is.character(auto_name) || length(auto_name) != 1L || !nzchar(auto_name)) next
    if (!is.character(file_name) || length(file_name) != 1L || !nzchar(file_name)) next
    reader <- reader_fn_name_for_ext(ext)
    if (is.na(reader)) next
    lines <- c(
      lines,
      paste0(auto_name, " <- ", reader, "(\"", file_name, "\")")
    )
  }
  if (length(lines) == 0L) return("")
  paste0(paste(lines, collapse = "\n"), "\n")
}

# ADR 0023 §4 PLAN-05: derive the binding name for a panel-owned source
# on the per-instance side. Mirrors `resolve_upload_source()`'s name
# derivation but reads the shortcut text-input value at the host's
# top-level (`session$rootScope()$input[[node$shortcut_id]]`) -- the
# panel-owned source widget lives at top-level, not under the
# per-instance namespace ("one widget, one owner"). Shortcut-less
# sources derive via `entry$resolve_expr` exactly as the input-watching
# path does. Returns NULL when no valid name can be derived; in that
# case `bind_source_value()` still mirrors the resolved df into the
# instance's per-source slot (so downstream consumers see the value),
# only `state$eval_env` / `state$bound_names` are skipped.
panel_owned_binding_name <- function(node, entry, session,
                                     shortcut_value, df) {
  has_shortcut <- !is.null(node$shortcut_id)
  if (has_shortcut) {
    nm <- shortcut_value
    # INT-3 (ADR 0023 / GAP-3A): mirror the boot-race fallback in
    # `try_bind_source_default()` -- when the shortcut textInput hasn't
    # registered yet, fall back to `node$default` so the panel-owned
    # binding name can be computed at boot from a `default_arg`-primed
    # source (worked example #2). Without this, the host-scope consumer
    # picker stays empty: `panel_sources[[sid]]()` returns the primed
    # df, but the binder block in `ptr_bind_shared_consumer_uis()` sees
    # `bname = NULL` and skips. The per-instance call sites in
    # `ptr_setup_pipelines()` benefit too: they wrote NULL into
    # `state$bound_names[[layer]]` previously, leaving the source-ready
    # gate unsatisfied at boot for default-arg priming.
    if (!is.character(nm) || length(nm) != 1L || !nzchar(nm)) {
      nm <- node$default
    }
    if (is.character(nm) && length(nm) == 1L && nzchar(nm) &&
        make.names(nm) == nm) nm else NULL
  } else if (!is.null(entry) && !is.null(entry$resolve_expr)) {
    sym <- tryCatch(entry$resolve_expr(df, node),
                    error = function(e) NULL)
    if (is.symbol(sym)) {
      cand <- as.character(sym)
      if (nzchar(cand) && make.names(cand) == cand) cand else NULL
    } else NULL
  } else NULL
}

# Boot-time fallback for source placeholders with a `default_arg`-validated
# value: when no live input has been provided through the source widget
# (e.g. ppUpload's fileInput is still empty), but the placeholder carries a
# default symbol/name that resolves to a data.frame somewhere up
# `state$eval_env`'s parent chain (typically the app.R script env), prime
# `state$resolved_sources` / `state$bound_names` via the same
# `bind_source_value()` path the upload observer uses. This clears the
# ADR 0015 §2.1 source-ready gate (`req(state$bound_names[[k]]())` in
# `ptr_setup_consumer_uis()` / `ptr_bind_shared_consumer_uis()`) at boot
# so consumer pickers populate from the default-bound frame without an
# upload. Returns TRUE on bind, FALSE otherwise (caller falls through to
# the existing `slot(NULL)` path).
#
# Generalizes via `node$default` + `entry$resolve_expr` -- both already
# required for any source placeholder definition -- so third-party
# `ptr_define_placeholder_source()` callers with a default_arg get this
# fallback for free, regardless of whether they registered a
# `shortcut = TRUE`.
try_bind_source_default <- function(state, key, node, input, shortcut_input_id,
                                    entry, slot) {
  # ADR 0024: the shortcut is a data-loading entry point. When the user
  # has typed a non-empty name into the shortcut textInput (or spec=
  # has dispatched one in via the source_companion role-label path), bind
  # the named frame from state$eval_env even if `node$default` is NULL.
  # Pre-ADR-0024 the helper returned FALSE here unconditionally; the
  # widened guard preserves that behavior when no shortcut value is
  # available (no default + blank shortcut = nothing to look up).
  shortcut_value <- if (!is.null(shortcut_input_id)) input[[shortcut_input_id]] else NULL
  has_shortcut_value <- is.character(shortcut_value) && length(shortcut_value) == 1L &&
                    nzchar(shortcut_value)
  if (is.null(node$default) && !has_shortcut_value) return(FALSE)
  # ADR 0025 §3 / PLAN-02: `lookup_name` selects the object we GET from
  # `state$eval_env`; `binding_name` selects the symbol we ASSIGN it
  # under. Pre-PLAN-02 they were the same; post-PLAN-02 the shortcut-
  # empty path binds under `node$auto_name` (the deterministic
  # translate-/runtime-stamped symbol the substitute walker also reads)
  # while still LOOKING UP under the user-supplied default-arg name.
  lookup_name <- if (!is.null(shortcut_input_id)) {
    nm <- shortcut_value
    # Boot race: the shortcut textInput is seeded with node$default by
    # `ptr_builtin_upload_build_ui()`, but `input[[shortcut_input_id]]` is NULL until
    # the widget registers. Fall back to `node$default` so the first
    # observer fire can bind immediately rather than waiting an extra
    # invalidation. (Post ADR 0024: when default is NULL, the
    # has_shortcut_value guard above has already ensured shortcut_value is a
    # usable string, so this fallback never fires in that branch.)
    if (!is.character(nm) || length(nm) != 1L || !nzchar(nm)) {
      nm <- node$default
    }
    if (is.character(nm) && length(nm) == 1L && nzchar(nm) &&
        make.names(nm) == nm) nm else NULL
  } else if (!is.null(entry) && !is.null(entry$resolve_expr)) {
    sym <- tryCatch(entry$resolve_expr(node$default, node),
                    error = function(e) NULL)
    if (is.symbol(sym)) {
      cand <- as.character(sym)
      if (nzchar(cand) && make.names(cand) == cand) cand else NULL
    } else NULL
  } else NULL
  if (is.null(lookup_name)) return(FALSE)
  df <- tryCatch(
    get(lookup_name, envir = state$eval_env, inherits = TRUE),
    error = function(e) NULL
  )
  # ADR 0024 §2: structured error surface. Pre-ADR-0024 both branches
  # below returned FALSE silently; the shortcut-as-entry-point contract
  # owes the user a reason when their typed name didn't bind.
  if (is.null(df)) {
    set_resolve_error(state, key,
      sprintf("Object '%s' not found in environment.", lookup_name))
    return(FALSE)
  }
  if (!is.data.frame(df)) {
    set_resolve_error(state, key,
      sprintf("Object '%s' is not a data frame.", lookup_name))
    return(FALSE)
  }
  # ADR 0025 §3 / PLAN-02: when the shortcut surface is present and the
  # textbox is empty (shortcut_value not a usable string), bind under
  # `node$auto_name` so the substitute walker's empty-snapshot fallback
  # (`as.name(node$auto_name)`) resolves at eval-time. With a non-empty
  # textbox, `shortcut_value` wins (existing behaviour).
  binding_name <- lookup_name
  if (!is.null(shortcut_input_id) && !has_shortcut_value) {
    auto <- node$auto_name
    if (is.character(auto) && length(auto) == 1L && nzchar(auto) &&
        make.names(auto) == auto) {
      binding_name <- auto
    }
  }
  bind_source_value(state, key, binding_name, df, slot)
  TRUE
}

# ADR 0023 §3 — Single resolver for upload-source mechanics, two call
# sites today (bare-data-source layer loop and pipeline-head source loop,
# both in `ptr_setup_pipelines()`) and a third forthcoming via PLAN-04
# (`ptr_setup_panel_sources()` at host scope). Encapsulates the full
# resolve cycle for one source instance: read the upload `input_slot`,
# dispatch `entry$resolve_data` for csv/tsv/rds/xlsx (and any future
# format the registry entry handles), fall back to
# `try_bind_source_default()` when no upload is present, derive the
# binding name (shortcut text input value OR `entry$resolve_expr`),
# and route everything through `bind_source_value()` /
# `set_resolve_error()` so the ADR 0015 PLAN-02 assign-before-signal
# invariant is preserved. Always returns `invisible(NULL)`.
#
# Args:
#   input_slot      — the value of `input[[<namespaced upload id>]]`
#                     (the fileInput's list, or NULL). Caller pre-reads
#                     it so reactive deps are taken in the caller's
#                     reactive context; the helper is Shiny-shape
#                     agnostic for unit testing.
#   shortcut_slot  — list(present = logical, value = character|NULL).
#                     `present = FALSE` ⇒ source has no shortcut sibling
#                     (e.g. a selectInput chooser); `present = TRUE` ⇒
#                     source has a shortcut text input whose live value
#                     is `value` (NULL during the boot race before the
#                     widget registers, "" once registered & empty).
#   node            — the source placeholder node (`ptr_ph_data_source`).
#   entry           — the registry entry (`ptr_registry_lookup(...)`),
#                     may be NULL for un-registered keywords.
#   envir           — the eval environment (`state$eval_env`); reserved
#                     for symmetry with the default-fallback lookup.
#   state, key, slot — `bind_source_value()` / `set_resolve_error()`
#                     destinations as in the inlined original.
resolve_upload_source <- function(input_slot, shortcut_slot, node, entry,
                                  envir, state, key, slot) {
  file_info <- input_slot
  has_shortcut <- isTRUE(shortcut_slot$present)
  shortcut_value <- if (has_shortcut) shortcut_slot$value else NULL
  if (is.null(file_info)) {
    set_resolve_error(state, key, NULL)
    # ADR 0025 §4 / PLAN-04: vacated upload binding — drop any prior
    # prologue ledger entry for this key. Plan 05 widens the vacate
    # below to cover the textbox-empty fall-through (eval_env binding +
    # bound_names + the same active_uploads slot).
    clear_active_upload(state, key)
    # Source-default fallback: when no upload has been provided but the
    # source carries a default-arg symbol resolvable in `envir`'s parent
    # chain, bind it so the ADR 0015 §2.1 source-ready gate clears at
    # boot. Falls through to `slot(NULL)` when the source has no default
    # or the default doesn't resolve.
    if (try_bind_source_default_resolved(state, key, node, has_shortcut,
                                         shortcut_value, entry, slot)) {
      return(invisible(NULL))
    }
    # ADR 0025 §7 / PLAN-05 (A1): vacate-on-empty. We reached the
    # `slot(NULL)` fall-through because file_info is NULL AND the
    # default-arg fallback could not bind. Plan-05 vacate fires ONLY in
    # the literal "empty textbox" case (file gone + textbox cleared to
    # ""), NOT when the user typed a name that failed to resolve --
    # otherwise a typo would clobber the prior-good upload binding and
    # blank every downstream consumer picker (regression observed in
    # `test-shared-source-panel-multi-instance.R` where typing a
    # mistyped object name after an auto-name upload bind dropped the
    # picker to empty). The empty-textbox condition is:
    #   - source has a shortcut surface (`has_shortcut == TRUE`) AND
    #   - the shortcut value is NULL (boot-time, no widget binding yet)
    #     or a zero-length string (mutex auto-cleared or user-cleared).
    # Sources with no shortcut surface (selectInput choosers etc.) get
    # vacated when file_info is NULL, since they have no other input
    # channel that could legitimately hold a "bad" value.
    shortcut_is_empty <- !has_shortcut ||
      is.null(shortcut_value) ||
      (is.character(shortcut_value) && length(shortcut_value) == 1L &&
       !nzchar(shortcut_value))
    if (shortcut_is_empty) vacate_source_binding(state, key)
    slot(NULL)
    return(invisible(NULL))
  }
  df <- if (!is.null(entry) && !is.null(entry$resolve_data)) {
    tryCatch(entry$resolve_data(file_info, node),
             error = function(e) {
               set_resolve_error(state, key, conditionMessage(e))
               NULL
             })
  } else NULL
  if (!is.null(df)) set_resolve_error(state, key, NULL)
  # Bind the resolved frame under the same symbol that
  # `substitute_walk.ptr_ph_data_source()` will produce.
  # - shortcut-driven sources (upload): name = the shortcut text
  #   input; invalid names yield NULL (substitute walk rejects loudly).
  # - shortcut-less sources (e.g. selectInput chooser): name comes
  #   from `entry$resolve_expr(value, node)` -- must be a symbol whose
  #   character form is a valid R name.
  binding_name <- if (has_shortcut) {
    nm <- shortcut_value
    if (is.character(nm) && length(nm) == 1L && nzchar(nm) &&
        make.names(nm) == nm) {
      nm
    } else {
      # ADR 0025 §3 / PLAN-02: when the shortcut textbox is empty (or
      # not a syntactic R name), fall back to the deterministic
      # `node$auto_name` so the upload binds to a stable symbol under
      # the coordinator eval_env. The substitute walker reads the same
      # field via its empty-snapshot fallback, keeping eval-time symbol
      # resolution in lockstep with bind-time assignment.
      auto <- node$auto_name
      if (is.character(auto) && length(auto) == 1L && nzchar(auto) &&
          make.names(auto) == auto) auto else NULL
    }
  } else if (!is.null(entry) && !is.null(entry$resolve_expr)) {
    sym <- tryCatch(entry$resolve_expr(file_info, node),
                    error = function(e) NULL)
    if (is.symbol(sym)) {
      cand <- as.character(sym)
      if (nzchar(cand) && make.names(cand) == cand) cand else NULL
    } else NULL
  } else NULL
  # ADR 0015 PLAN-02 / Option E: enforces assign-before-signal so a
  # consumer `req(state$bound_names[[key]]())` cannot fire before
  # `state$eval_env` holds `binding_name`.
  bind_source_value(state, key, binding_name, df, slot)
  # ADR 0025 §4 / PLAN-04: register the upload entry on
  # `state$active_uploads` so the code-panel prologue emits a
  # `<auto_name> <- read.<ext>(...)` line for this source. Built-in
  # `ppUpload` only (the helper short-circuits on other keywords). Only
  # registers when the bind succeeded (df non-NULL and a binding_name
  # was usable); otherwise clears any stale entry.
  if (!is.null(df) && !is.null(binding_name)) {
    register_active_upload(state, key, node, file_info)
  } else {
    clear_active_upload(state, key)
  }
  invisible(NULL)
}

# Internal sibling of `resolve_upload_source()`: same contract as
# `try_bind_source_default()` but takes the *resolved* shortcut
# presence flag + value the helper already read from
# `input[[shortcut_input_id]]`, instead of re-reading
# `input[[shortcut_input_id]]`. Keeps `resolve_upload_source()`
# Shiny-input-shape agnostic so unit tests can drive it with plain R
# values.
try_bind_source_default_resolved <- function(state, key, node, has_shortcut,
                                             shortcut_value, entry, slot) {
  # ADR 0024: the shortcut is a data-loading entry point. See the
  # parallel comment in `try_bind_source_default()` above — both helpers
  # must stay in lockstep (the `_resolved` variant is called by
  # `resolve_upload_source()` for pipeline-head + panel scopes; the
  # unresolved one by the bare-data-source-layer loop).
  has_shortcut_value <- has_shortcut &&
                    is.character(shortcut_value) && length(shortcut_value) == 1L &&
                    nzchar(shortcut_value)
  # ADR 0025 §7 / PLAN-05 (A1): inner vacates live in the caller
  # (`resolve_upload_source()`'s `slot(NULL)` fall-through) only.
  # Vacating from here too would clobber a still-good prior binding
  # when the user types a name that fails to resolve (e.g. typo) --
  # observed regression in `test-shared-source-panel-multi-instance.R`
  # where typing "penguins" after an auto-name bind dropped the picker
  # to empty even though the upload's data was still resolvable.
  if (is.null(node$default) && !has_shortcut_value) return(FALSE)
  # ADR 0025 §3 / PLAN-02: see the parallel comment in
  # `try_bind_source_default()` — `lookup_name` chooses what we GET from
  # `state$eval_env`; `binding_name` chooses the symbol we ASSIGN under,
  # which on the shortcut-empty path is `node$auto_name`.
  lookup_name <- if (has_shortcut) {
    nm <- shortcut_value
    # Boot race: the shortcut textInput is seeded with `node$default`
    # by `ptr_builtin_upload_build_ui()`, but `input[[shortcut_input_id]]` is
    # NULL until the widget registers. Fall back to `node$default` so
    # the first observer fire can bind immediately rather than waiting
    # an extra invalidation. (Post ADR 0024: when default is NULL, the
    # has_shortcut_value guard above has already ensured shortcut_value is a
    # usable string, so this fallback never fires in that branch.)
    if (!is.character(nm) || length(nm) != 1L || !nzchar(nm)) {
      nm <- node$default
    }
    if (is.character(nm) && length(nm) == 1L && nzchar(nm) &&
        make.names(nm) == nm) nm else NULL
  } else if (!is.null(entry) && !is.null(entry$resolve_expr)) {
    sym <- tryCatch(entry$resolve_expr(node$default, node),
                    error = function(e) NULL)
    if (is.symbol(sym)) {
      cand <- as.character(sym)
      if (nzchar(cand) && make.names(cand) == cand) cand else NULL
    } else NULL
  } else NULL
  if (is.null(lookup_name)) return(FALSE)
  df <- tryCatch(
    get(lookup_name, envir = state$eval_env, inherits = TRUE),
    error = function(e) NULL
  )
  # ADR 0024 §2: structured error surface — mirrors try_bind_source_default().
  if (is.null(df)) {
    set_resolve_error(state, key,
      sprintf("Object '%s' not found in environment.", lookup_name))
    return(FALSE)
  }
  if (!is.data.frame(df)) {
    set_resolve_error(state, key,
      sprintf("Object '%s' is not a data frame.", lookup_name))
    return(FALSE)
  }
  binding_name <- lookup_name
  if (has_shortcut && !has_shortcut_value) {
    auto <- node$auto_name
    if (is.character(auto) && length(auto) == 1L && nzchar(auto) &&
        make.names(auto) == auto) {
      binding_name <- auto
    }
  }
  bind_source_value(state, key, binding_name, df, slot)
  TRUE
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
      shortcut_input_id <- if (!is.null(src$shortcut_id)) ns(src$shortcut_id) else NULL
      entry <- ptr_registry_lookup(src$keyword)
      slot <- state$resolved_data[[ln]]

      # ADR 0023 §4 PLAN-05: panel-owned source substitution. When this
      # source's canonical id (e.g. `shared_ds`) is owned by the panel
      # (host-level `panel_sources` reactive populated by
      # `ptr_setup_panel_sources()`), redirect the per-instance binder to
      # read the resolved df from the panel reactive instead of watching
      # the per-instance input slot (which has no widget --
      # "one widget, one owner"). Skip wiring the input-watching observer
      # for this source -- otherwise we would watch a permanently-NULL
      # slot and cause spurious invalidations (ADR §4 note).
      panel_owned <- !is.null(src$id) &&
        src$id %in% names(state$panel_sources %||% list())
      if (panel_owned) {
        sid <- src$id
        panel_reactive <- state$panel_sources[[sid]]
        shiny::observe({
          df <- panel_reactive()
          if (is.null(df)) { slot(NULL); return(invisible(NULL)) }
          binding_name <- panel_owned_binding_name(
            src, entry, session,
            shortcut_value = if (!is.null(src$shortcut_id)) {
              session$rootScope()$input[[src$shortcut_id]]
            } else NULL,
            df = df
          )
          bind_source_value(state, ln, binding_name, df, slot)
        })
        return(invisible())
      }

      # ADR 0025 §7 / PLAN-05 A2: 400ms debounce on the shortcut textbox
      # so a keystroke burst (m -> mt -> mtc -> ... -> mtcars) resolves
      # exactly once, not six times with five transient "not found" errors.
      shortcut_r <- if (!is.null(shortcut_input_id)) {
        shiny::debounce(shiny::reactive(input[[shortcut_input_id]]), 400L)
      } else NULL
      shiny::observe({
        resolve_upload_source(
          input_slot     = input[[src_id]],
          shortcut_slot = if (!is.null(shortcut_input_id)) {
            list(present = TRUE, value = shortcut_r())
          } else {
            list(present = FALSE, value = NULL)
          },
          node           = src,
          entry          = entry,
          envir          = state$eval_env,
          state          = state,
          key            = ln,
          slot           = slot
        )
      })

      # ADR 0025 §2 (Q3-B): mutex between fileInput and shortcut textInput
      # -- typing one auto-clears the other so the concurrently-active
      # state is structurally impossible. The legacy filename-derived
      # textbox auto-fill is retired; the binding name comes from the
      # auto-name path (paintr-ids.R). Pass `shortcut_r` so the
      # text-side mutex gates on the debounced reactive (see the
      # ADR 0025 §7 A2 race-fix comment in `ptr_bind_source_mutex`).
      ptr_bind_source_mutex(src_id, shortcut_input_id, input, session,
                            shortcut_r = shortcut_r)
    })
  }

  # Data-source placeholders that are NOT a layer's bare `data_arg` --
  # e.g. `upload |> head(num) |> ... |> ggplot(...)`. A bare-layer source
  # has its resolved frame swapped into `data_arg` by
  # `inject_resolved_data()`; a pipeline-head source survives substitution
  # only as a symbol (`<name> |> head(num) |> ...`), so the resolved frame
  # must be bound under `<name>` in `state$eval_env` for the lazy consumer
  # resolves (`ptr_resolve_upstream()`) *and* the plot eval to find it.
  # Setting `state$resolved_sources[[id]]` invalidates the per-consumer
  # reactives in `ptr_setup_consumer_uis()` so downstream pickers refresh.
  for (src in find_nodes(tree, is_ptr_ph_data_source)) {
    if (is.null(src$id) || is.null(state$resolved_sources[[src$id]])) next
    local({
      node <- src
      sid <- node$id
      src_id <- ns(sid)
      shortcut_input_id <- if (!is.null(node$shortcut_id)) ns(node$shortcut_id) else NULL
      entry <- ptr_registry_lookup(node$keyword)
      slot <- state$resolved_sources[[sid]]

      # ADR 0023 §4 PLAN-05: panel-owned source substitution (pipeline-head
      # variant). Symmetric with the bare-data-source-layer branch above:
      # when this source's canonical id is owned by the panel, read the
      # resolved df from the panel reactive and skip the input-watching
      # observer (no per-instance widget exists for this id).
      if (sid %in% names(state$panel_sources %||% list())) {
        panel_reactive <- state$panel_sources[[sid]]
        shiny::observe({
          df <- panel_reactive()
          if (is.null(df)) { slot(NULL); return(invisible(NULL)) }
          binding_name <- panel_owned_binding_name(
            node, entry, session,
            shortcut_value = if (!is.null(node$shortcut_id)) {
              session$rootScope()$input[[node$shortcut_id]]
            } else NULL,
            df = df
          )
          bind_source_value(state, sid, binding_name, df, slot)
        })
        return(invisible())
      }

      # ADR 0025 §7 / PLAN-05 A2: see the parallel block in the bare-
      # data-source-layer branch above. 400ms debounce on shortcut.
      shortcut_r <- if (!is.null(shortcut_input_id)) {
        shiny::debounce(shiny::reactive(input[[shortcut_input_id]]), 400L)
      } else NULL
      shiny::observe({
        resolve_upload_source(
          input_slot     = input[[src_id]],
          shortcut_slot = if (!is.null(shortcut_input_id)) {
            list(present = TRUE, value = shortcut_r())
          } else {
            list(present = FALSE, value = NULL)
          },
          node           = node,
          entry          = entry,
          envir          = state$eval_env,
          state          = state,
          key            = sid,
          slot           = slot
        )
      })

      ptr_bind_source_mutex(src_id, shortcut_input_id, input, session,
                            shortcut_r = shortcut_r)
    })
  }
  invisible(state)
}

# UI mutex between a data source's fileInput and its shortcut textInput
# (ADR 0025 §2 / Q3-B). The two affordances feed the same source slot, so
# concurrent file+typed-text is forbidden by construction: filling one
# side auto-clears the other. The legacy filename-derived textbox
# auto-fill is retired -- the shortcut textbox is now reserved for
# env-shortcut typing and the upload's binding name comes from the
# auto-name path (paintr-ids.R). Both observers gate on `ignoreInit = TRUE` so
# the boot-time seed write (`ppUpload(penguins)` -> shortcut = "penguins")
# does NOT count as a first interaction and does NOT shiny::reset() the
# already-empty fileInput. Both run in the per-instance session scope so
# multi-instance / multi-coordinator apps stay isolated. Shared by the
# bare-layer and pipeline-head source wiring in `ptr_setup_pipelines()`.
# `src_id` / `shortcut_input_id` are already namespaced.
ptr_bind_source_mutex <- function(src_id, shortcut_input_id, input, session,
                                  shortcut_r = NULL) {
  if (is.null(shortcut_input_id)) return(invisible())
  # File picked -> wipe the sibling textbox.
  shiny::observeEvent(input[[src_id]], {
    shiny::updateTextInput(session, shortcut_input_id, value = "")
  }, ignoreInit = TRUE)
  # Text typed (non-empty) -> reset the sibling fileInput. shiny has no
  # built-in reset for fileInput (shinyjs::reset() is the canonical tool
  # but shinyjs is not a dep); the package ships a tiny custom JS
  # handler `ptr_reset_file_input` in inst/www/ggpaintr-layer.js that
  # wipes the DOM file pill and writes NULL into the Shiny input slot.
  #
  # ADR 0025 §7 A2 race-fix: when `shortcut_r` (the same 400ms-debounced
  # reactive that drives `resolve_upload_source()` in `ptr_setup_pipelines`)
  # is supplied, gate the file-reset on it instead of the raw text input.
  # Both observers then fire on the same debounced tick, so the bind sees
  # `file=FILE, shortcut="<name>"` in the SAME flush -- which lets
  # `resolve_upload_source` bind the uploaded df under `<name>` before
  # the JS file-reset round-trip blanks `input[[src_id]]`. Without this,
  # the mutex fires on the raw keystroke and the file is already NULL by
  # the time the debounced bind observer runs, dropping into
  # `try_bind_source_default_resolved()` which `inherits=TRUE` walks the
  # parent chain and binds a same-named namespace export (e.g.
  # `palmerpenguins::penguins`) instead of the uploaded df. Verified
  # against `test-shared-source-panel-multi-instance.R` worked-example-#4
  # (single-instance `ppUpload(shared='ds') |> ggplot(...)`).
  text_event <- if (!is.null(shortcut_r)) {
    shiny::reactive(shortcut_r())
  } else {
    shiny::reactive(input[[shortcut_input_id]])
  }
  shiny::observeEvent(text_event(), {
    if (isTRUE(nzchar(text_event() %||% ""))) {
      session$sendCustomMessage("ptr_reset_file_input", list(id = src_id))
    }
  }, ignoreInit = TRUE)
  invisible()
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
            "ggpaintr: refreshing column choices has been slow, so edits to ",
            "text, number, and expression inputs now take effect after a ",
            .PTR_DEFAULT_DEBOUNCE_WINDOW,
            " ms pause. Disable with `producer_debounce_ms = 0` -- see `?ptr_app`."
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
          cli::cli_inform(paste0(
            "ggpaintr: column choices are refreshing quickly again -- edits to ",
            "text, number, and expression inputs now take effect immediately."
          ))
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
  ui_ns <- state$ui_ns_fn
  for (sid in collect_stage_ids(tree)) {
    local({
      sid_local <- sid
      bound_id <- ns(sid_local)
      block_dom_id <- ui_ns(paste0(sid_local, "_stage_block"))
      shiny::observeEvent(input[[bound_id]], {
        val <- input[[bound_id]]
        cur <- state$stage_enabled()
        cur[[sid_local]] <- isTRUE(val)
        state$stage_enabled(cur)
        session$sendCustomMessage("ptr_set_class", list(
          id = block_dom_id,
          cls = "ptr-stage-disabled",
          add = !isTRUE(val)
        ))
      }, ignoreNULL = TRUE)
    })
  }
  invisible(state)
}


# Mirror `shared_state$shared_stage_enabled` reactives into this module's
# `state$stage_enabled` for every orphan pipeline stage in this tree. Each
# orphan stage has at least one shared key; if any of those keys has a
# reactive in `shared_stage_enabled`, that reactive drives the stage's
# enabled flag in this module's state. This is how `ptr_app_grid()` /
# `ptr_shared_ui()` propagate one shared-panel checkbox to many per-module
# pipelines (the inputs themselves live outside any module namespace).
ptr_setup_shared_stage_enabled <- function(state, shared_stage_enabled) {
  if (length(shared_stage_enabled %||% list()) == 0L) return(invisible(state))
  tree <- shiny::isolate(state$tree())
  orphans <- collect_orphan_shared_stages(tree)
  if (length(orphans) == 0L) return(invisible(state))
  for (st in orphans) {
    keys <- intersect(st$shared_keys, names(shared_stage_enabled))
    if (length(keys) == 0L) next
    # When several shared keys cohabit one orphan stage, any of them
    # disabling drops the stage (AND of TRUEs == stage on). Rare in
    # practice but the semantic stays consistent with the "checkbox =
    # this stage runs" promise.
    local({
      sid_local <- st$stage_id
      keys_local <- keys
      shiny::observe({
        flags <- vapply(keys_local, function(k) {
          isTRUE(shared_stage_enabled[[k]]())
        }, logical(1))
        cur <- state$stage_enabled()
        cur[[sid_local]] <- all(flags)
        state$stage_enabled(cur)
      })
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
    # Read ALL three reactive sources every call -- no `||` short-circuit.
    # If we let `||` short-circuit after the per-instance Update Plot button,
    # the enclosing observe never establishes a dependency on `draw_trigger`
    # / `extras`, so once a panel's own button has fired the grid app's
    # "Draw all" (and `ptr_gg_extra()` changes) stop redrawing it.
    update_clicked <- clicked(input[[ns("ptr_update_plot")]])
    draw_clicked <- !is.null(state$draw_trigger) && clicked(state$draw_trigger())
    extras_present <- length(state$extras()) > 0L
    update_clicked || draw_clicked || extras_present
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
          # ADR 0025 §6 / PLAN-06: spec round-trip fallback. For
          # source-companion rows (the shortcut textInput sibling of a
          # source placeholder), an empty textbox + a non-NULL
          # `state$bound_names[[key]]()` means the user uploaded a file
          # whose auto-name (Plan 02) is the live identity of that
          # source. Overwrite the snapshot with the bound name so the
          # spec dump records the auto-name. A boot-2 with `spec=` then
          # seeds the textbox with this name; the consumer's
          # `<auto-name> <- read.csv("...")` prologue (Plan 04) binds the
          # frame under it; the env-shortcut resolves -> reproduces the
          # rendered plot. Textbox wins when non-empty (no override).
          if (identical(spec$role[i], "source_companion")) {
            val <- snapshot[[raw_id]]
            if (is.null(val) || (is.character(val) && length(val) == 1L &&
                                 !nzchar(val))) {
              # `state$bound_names` keys vary by source kind: pipeline-
              # head sources publish under `node$id` (= spec$source_id);
              # bare-data-source layers (`ppUpload |> ggplot(...)`)
              # publish under `layer_name`. Try source_id first, then
              # layer_name. Both come from the same spec row.
              src_key   <- spec$source_id[i]
              layer_key <- spec$layer_name[i]
              bound <- NULL
              if (!is.na(src_key) && nzchar(src_key) &&
                  src_key %in% names(state$bound_names)) {
                bound <- state$bound_names[[src_key]]()
              }
              if (is.null(bound) && !is.na(layer_key) && nzchar(layer_key) &&
                  layer_key %in% names(state$bound_names)) {
                bound <- state$bound_names[[layer_key]]()
              }
              if (!is.null(bound)) {
                snapshot[[raw_id]] <- bound
              }
            }
          }
        }
      }

      # `upstream_cols` is deeply state-coupled (reads `state$tree()`,
      # `state$resolved_data`, `state$upstream_cache`, ...) so it stays
      # here, computed against the live state, and is passed in to the
      # otherwise-pure `ptr_exec_headless()`.
      upstream_cols <- runtime_upstream_cols(state, snapshot)

      # Spec L105 + L217 (G6.3 terminal upstream): bare-data-source layers
      # (e.g. `ggplot(data = upload, ...)`) have their resolved frame cached
      # in `state$resolved_data[[layer$name]]` by the upload observer. The
      # headless step swaps the pruned layer's `data_arg` with a literal
      # carrying that frame so eval skips re-running the resolve; `code_text`
      # is rendered from the original pruned tree, so the user-visible code
      # panel still shows the source expression.
      res <- ptr_exec_headless(
        tree            = state$tree(),
        snapshot        = snapshot,
        shared_bindings = state$shared_bindings,
        eval_env        = state$eval_env,
        safe_to_remove  = state$safe_to_remove,
        expr_check      = state$expr_check,
        extras          = state$extras(),
        stage_enabled   = state$stage_enabled(),
        resolved_data   = lapply(state$resolved_data, function(rv) rv()),
        upstream_cols   = upstream_cols,
        upstream_data   = runtime_upstream_data_frames(state, snapshot)
      )
      # Attach the snapshot used for this run to the runtime result so
      # preserve-mode rendering can stamp current_pick from the SAME
      # values that final-mode substitute saw. Preserve thus shows
      # exactly what was drawn — no disagreement with final on which
      # picks were "set" (ADR 0009 bug-1 follow-up 2026-05-21).
      res$snapshot <- snapshot
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
    class = "ptr-alert ptr-alert--error",
    shiny::tags$strong("Error"),
    shiny::tags$div(
      class = "ptr-alert__detail",
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
  inject_resolved_data_list(
    pruned,
    lapply(state$resolved_data, function(rv) rv())
  )
}

# Plain-list core of inject_resolved_data(): `frames` is a named list mapping
# layer name -> data.frame (or NULL). For each ptr_layer whose name has a
# non-null frame, replace `data_arg` with a literal carrying that frame.
inject_resolved_data_list <- function(pruned, frames) {
  if (!is_ptr_root(pruned)) return(pruned)
  if (is.null(pruned)) return(pruned)
  for (i in seq_along(pruned$layers)) {
    layer <- pruned$layers[[i]]
    if (!is_ptr_layer(layer)) next
    df <- frames[[layer$name]]
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

# Keep each layer panel's `ptr-layer-disabled` class in sync with its
# include-checkbox. The class is set once at UI-build time; here we toggle
# it at runtime via the `ptr_set_class` custom message (handler injected by
# `ptr_layer_assets()`). The content-div id is `layer_panel_content_id()`
# namespaced with the *UI* ns (the same `ns_fn` `build_ui_for.ptr_layer`
# used); the checkbox input is read with the server ns.
ptr_setup_layer_panel_classes <- function(state, input, output, session) {
  ns <- state$server_ns_fn
  ui_ns <- state$ui_ns_fn
  spec <- state$input_spec
  ck_rows <- spec[spec$role == "layer_checkbox", , drop = FALSE]
  for (i in seq_len(nrow(ck_rows))) {
    local({
      input_id <- ck_rows$input_id[[i]]
      layer_name <- ck_rows$layer_name[[i]]
      dom_id <- ui_ns(layer_panel_content_id(layer_name))
      shiny::observeEvent(input[[ns(input_id)]], {
        val <- input[[ns(input_id)]]
        if (is.null(val)) return()
        session$sendCustomMessage("ptr_set_class", list(
          id = dom_id,
          cls = "ptr-layer-disabled",
          add = !isTRUE(val)
        ))
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
    })
  }
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
# `state$resolved_data` keyed by layer name conflated terminal upstream
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
    # ADR 0012 §3.7 / PLAN-04: the per-layer fast-path that short-circuited
    # off `state$resolved_data` (keyed by layer name) has been deleted. Every
    # consumer's `c$upstream` carries its true per-position resolution
    # point (in-stage → prior stage; in-aes → the layer's data_arg
    # pipeline). `ptr_resolve_upstream(c$upstream, ...)` handles every
    # case uniformly across `|>` / `%>%` / nested-call surface forms.
    # Partial-input passthrough: a placeholder upstream of this consumer
    # may signal `ptr_partial_input` (see [ptr_signal_partial()]) when
    # the user is mid-typing a free-text widget like `ppExpr`. This
    # helper is called from both the gated plot-build observer
    # (R/paintr-server.R ~1214/1234) and is a prefetch step before
    # `ptr_exec_headless()`. Letting the condition escape here would
    # kill the Shiny `observe` (session abort) before the substitute
    # pass inside `ptr_complete_expr_safe()` -- which already has a
    # `tryCatch(error=)` that converts the same condition into a clean
    # `ok = FALSE` runtime result -- ever runs. Skipping this consumer
    # is identical to the existing `is.null(df)` skip; the downstream
    # substitute will hit the same condition and report it inline via
    # the error pane.
    df <- tryCatch(
      ptr_resolve_upstream(
        c$upstream,
        snapshot = snapshot,
        shared_bindings = state$shared_bindings,
        eval_env = state$eval_env,
        cache = state$upstream_cache,
        expr_check = state$expr_check,
        stage_enabled = stage_enabled
      ),
      ptr_partial_input = function(e) NULL
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

# Sibling of `runtime_upstream_cols()` that returns the per-consumer
# data.frame slot instead of the column-names slot. Used to feed
# `ctx$data` into `validate_input(value, ctx)` hooks for data-aware
# consumer validators (column-type / range / level checks). The
# underlying `runtime_upstream_data()` already produces the per-consumer
# data frame -- this wrapper just projects it out.
runtime_upstream_data_frames <- function(state, snapshot = list()) {
  res <- runtime_upstream_data(state, snapshot)
  lapply(res, function(x) x$data)
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

# ADR 0012 / PLAN-01 (Bug B): server-side renderUI for `ptr_ph_value`
# placeholders (ppText / ppNum / ppExpr and any custom value keyword).
# Mirrors `ptr_setup_consumer_uis()` shape; the renderUI body calls
# `invoke_build_ui()` whose `extra$selected` precedence seeds the widget
# from `state$spec_seed[[raw_id]]` on first fire. `apply_spec_at_boot()`
# writes the seed synchronously at boot, BEFORE its deferred onFlushed
# dispatch, so the seed is in place by the time this renderUI fires.
# After the first fire `current` (the persisted input value) takes over
# -- the `%||%` chain picks the first non-NULL.
ptr_setup_value_uis <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn
  ui_ns <- state$ui_ns_fn
  ui_text <- state$effective_ui_text

  values <- find_nodes(tree, is_ptr_ph_value)
  for (v in values) {
    if (is.null(v$id)) next
    # Shared value widgets bind ONCE per shared key, after this loop, using
    # the deduped representative from `collect_shared_placeholders()` so
    # cross-occurrence param resolution applies. Per-occurrence binding
    # here would overwrite the same output slot N times for N occurrences.
    if (!is.null(v$shared)) next
    local({
      node <- v
      raw_id <- node$id
      output_id <- ns(value_output_id(raw_id))
      # Implements the widget-seeding contract from
      # ?ptr_define_placeholder_value (Widget-seeding contract section).
      # `has_rendered` distinguishes the first renderUI fire (input not
      # bound; framework injects node$default via invoke_build_ui branch
      # A) from subsequent fires (current %||% character(0) flows
      # verbatim, so a user-emptied widget stays empty).
      has_rendered <- FALSE
      output[[output_id]] <- shiny::renderUI({
        # Re-fire on tree changes (layer toggles, formula rebuilds) the
        # same way consumer renderUIs do; value widgets don't depend on
        # resolved cols/data so the dependency set is intentionally thin.
        state$tree()
        state$stage_enabled()
        current <- shiny::isolate(input[[ns(raw_id)]])
        seed <- shiny::isolate(state$spec_seed[[raw_id]])
        # Pipeline-stage placeholders carry a verb-derived `param_override`
        # / `label_suffix` (e.g. "Enter a number for head()"). Pre-PLAN-01
        # the layer panel threaded these into `invoke_build_ui` directly;
        # now build_ui_for.ptr_ph_value emits a static uiOutput so the
        # renderUI body re-derives them by walking the live tree via
        # `pipeline_override_for_node()` (R/paintr-build-ui.R).
        override <- pipeline_override_for_node(
          shiny::isolate(state$tree()), node$id
        )
        selected_arg <- if (has_rendered) {
          seed %||% current %||% character(0)
        } else {
          seed
        }
        extra <- list()
        if (!is.null(selected_arg)) extra$selected <- selected_arg
        result <- invoke_build_ui(
          node,
          ui_text = ui_text,
          layer_name = node$layer_name,
          ns_fn = ui_ns,
          extra = extra,
          param_override = override$param_override,
          label_suffix = override$label_suffix
        )
        has_rendered <<- TRUE
        result
      })
      # ADR 0012 / PLAN-01 (Bug B): value widgets must exist in the DOM
      # regardless of whether the layer panel's tab is currently visible.
      # Shiny's default `suspendWhenHidden = TRUE` would skip rendering
      # for tabs other than the selected layer; pre-PLAN-01 the widget
      # was a static tag inside the layer panel and survived layer
      # switches with the input element always mounted. Post-PLAN-01 it
      # lives in a renderUI -- opting out of suspension keeps the input
      # binding alive across layer changes, so spec seeding lands on a
      # mounted widget and `app$get_html("#<raw_id>")` reads non-NULL
      # for ids on inactive layers under shinytest2.
      shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
    })
  }

  # ADR 0012 / PLAN-01 (Bug B): shared value widgets. Pre-PLAN-01 the
  # shared section's `build_ui_for(rep_node, ...)` emitted the widget tag
  # directly, so no server-side binding was needed. After the reshape it
  # emits a `uiOutput(rep_node$id_ui)` -- we must register a renderUI for
  # that slot. One renderUI per shared *key* (not per occurrence), using
  # the deduped representative node from `collect_shared_placeholders()`:
  # the rep_node clears `$param` to NA when multiple distinct params share
  # the key, so copy resolution falls through to `defaults$<keyword>`
  # instead of picking the first occurrence's param-specific copy (the
  # BUG-A2 invariant: `defaults$ppNum.help` reaches a multi-param shared
  # num widget without bleed from alpha's "0 and 1" hint).
  shared_entries <- collect_shared_placeholders(tree)
  for (entry in shared_entries) {
    rep <- entry$node
    if (!is_ptr_ph_value(rep)) next
    if (is.null(rep$id)) next
    local({
      node <- rep
      label_override <- entry$label_override
      raw_id <- node$id
      output_id <- ns(value_output_id(raw_id))
      # See `has_rendered` comment in the non-shared value loop above.
      has_rendered <- FALSE
      output[[output_id]] <- shiny::renderUI({
        state$tree()
        state$stage_enabled()
        current <- shiny::isolate(input[[ns(raw_id)]])
        seed <- shiny::isolate(state$spec_seed[[raw_id]])
        selected_arg <- if (has_rendered) {
          seed %||% current %||% character(0)
        } else {
          seed
        }
        extra <- list()
        if (!is.null(selected_arg)) extra$selected <- selected_arg
        result <- invoke_build_ui(
          node,
          ui_text = ui_text,
          layer_name = node$layer_name,
          ns_fn = ui_ns,
          extra = extra,
          label_override = label_override
        )
        has_rendered <<- TRUE
        result
      })
      # Shared widgets live in the host's shared section (not a layer
      # tab), so suspension is moot there -- but keeping the option
      # explicit guards against future restructuring that drops the
      # shared section into a hidden container.
      shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
    })
  }
  invisible(state)
}

# ADR 0012 / PLAN-01 (Bug B): server-side renderUI for `ptr_ph_data_source`
# placeholders (ppVar-as-source, ppUpload and any custom source keyword).
# Replicates the pre-PLAN-01 `build_ui_for.ptr_ph_data_source` body
# (R/paintr-build-ui.R pre-reshape) -- copy resolution, ppUpload's
# file_copy/name_copy injection, `named_args` pass-through, formals
# guard -- but rendered inside a renderUI so the seed-precedence path
# applies. The hook still emits both the fileInput AND the shortcut
# textInput as a tagList; the shortcut's seed (role "source_companion")
# is set by `updateTextInput` from the boot dispatch (the shortcut
# textInput sits inside this same renderUI but is addressed by its own
# `node$shortcut_id`, which Shiny will (re-)bind on this renderUI's
# first fire just as for the consumer pattern).
ptr_setup_source_uis <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn
  ui_ns <- state$ui_ns_fn
  ui_text <- state$effective_ui_text

  sources <- find_nodes(tree, is_ptr_ph_data_source)
  for (s in sources) {
    if (is.null(s$id)) next
    # ADR 0023 §7 / PLAN-06: partition-aware skip. Panel-owned source
    # ids (populated on `state$panel_sources` by the host's
    # `ptr_setup_panel_sources()`, Plan 04) are rendered once at host
    # scope; the per-instance binder must not also write to those
    # output slots. Formula-local shared ids and single-instance configs
    # (where `state$panel_sources` is an empty list) fall through and
    # render per-instance as before.
    if (s$id %in% names(state$panel_sources %||% list())) next
    local({
      node <- s
      raw_id <- node$id
      output_id <- ns(source_output_id(raw_id))
      output[[output_id]] <- shiny::renderUI({
        state$tree()
        # Intentionally NOT a dep on `state$stage_enabled()`: source
        # placeholders sit at the pipeline head, so toggling a downstream
        # stage cannot alter their UI. Adding the dep here re-fires the
        # renderUI on every stage checkbox click, which calls the upload
        # build_ui afresh and resets the shortcut textInput to
        # `node$default` (wiping user-typed text / upload auto-name).
        # Stage disabling propagates to source widgets via CSS only
        # (`ptr_set_class` in `ptr_setup_stage_enabled`).
        current <- shiny::isolate(input[[ns(raw_id)]])
        seed <- shiny::isolate(state$spec_seed[[raw_id]])

        rendered_node <- node
        rendered_node$id <- ui_ns(node$id)
        if (!is.null(node$shortcut_id)) {
          rendered_node$shortcut_id <- ui_ns(node$shortcut_id)
        }
        copy <- ptr_resolve_ui_text(
          "control",
          keyword = node$keyword,
          param = node$param,
          layer_name = node$layer_name,
          ui_text = ui_text
        )
        entry <- ptr_registry_lookup(node$keyword)
        if (is.null(entry) || is.null(entry$build_ui)) {
          rlang::abort(paste0(
            "Placeholder `", node$keyword, "` has no `build_ui` function. Pass ",
            "`build_ui = function(node, label, ...)` when registering it -- see ",
            "`?ptr_define_placeholder_value`."
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
        # PLAN-07: pass node$named_args through so custom hooks can
        # consume them; built-in hooks swallow via `...`.
        if (accepts_dots || "named_args" %in% fmls) {
          extra_named$named_args <- node$named_args %||% list()
        }
        # PLAN-01: seed precedence for the source widget. Only inject
        # when the hook accepts the arg (formal or `...` sink); ppUpload
        # has neither, so the seed is silently ignored at this layer
        # (the fileInput cannot be set programmatically anyway -- the
        # exclusion is enforced upstream in `apply_spec_at_boot`).
        if (accepts_dots || "selected" %in% fmls) {
          sel <- seed %||% current
          if (!is.null(sel)) extra_named$selected <- sel
        }
        do.call(entry$build_ui,
                c(list(rendered_node, label = copy$label), extra_named))
      })
      # ADR 0012 / PLAN-01 (Bug B): source widgets (ppUpload, ppVar-as-
      # source, custom source keywords) must stay mounted across layer-
      # tab switches for the same reason value widgets do (see the
      # parallel `outputOptions` in `ptr_setup_value_uis()` above).
      shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
    })
  }
  invisible(state)
}

ptr_setup_consumer_uis <- function(state, input, output, session) {
  tree <- shiny::isolate(state$tree())
  ns <- state$server_ns_fn
  ui_ns <- state$ui_ns_fn
  ui_text <- state$effective_ui_text

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
      # Implements the widget-seeding contract from
      # ?ptr_define_placeholder_consumer (see also the seeding-contract
      # block on ?ptr_define_placeholder_value).
      has_rendered <- FALSE

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
      upstream_source_shortcut_ids <-
        find_source_companion_ids_in_upstream(node$upstream)
      upstream_source_self_ids <-
        find_source_self_ids_in_upstream(node$upstream)
      # ADR 0023 §4 FINDING #3 fix: when an upstream source is panel-owned
      # (`state$panel_sources[[<sid>]]` populated), its shortcut textInput
      # and its source widget live at HOST scope -- there is no per-instance
      # widget under `input[[ns(<id>)]]` to read from. Pre-compute which
      # upstream shortcut/self ids belong to a panel-owned source so the
      # snapshot loops below can read those values from
      # `session$rootScope()$input[[<bare-id>]]` instead. Without this,
      # `substitute_walk.ptr_ph_data_source` sees a NULL snapshot entry and
      # short-circuits to `ptr_missing()` -> `ptr_resolve_upstream()`
      # returns NULL -> non-shared `ppVar` pickers downstream of a
      # panel-owned `ppUpload` boot empty (shared-section consumers go
      # through `ptr_bind_shared_consumer_uis()` and are not affected).
      upstream_panel_shortcut_ids <- character()
      upstream_panel_self_ids <- character()
      for (s in find_nodes(node$upstream, is_ptr_ph_data_source)) {
        is_panel <- !is.null(s$id) &&
          s$id %in% names(state$panel_sources %||% list())
        if (!is_panel) next
        if (!is.null(s$shortcut_id)) {
          upstream_panel_shortcut_ids <-
            c(upstream_panel_shortcut_ids, s$shortcut_id)
        }
        if (!is.null(s$id)) {
          upstream_panel_self_ids <- c(upstream_panel_self_ids, s$id)
        }
      }
      # ADR 0015 §2.1: pick the source-ready reactive this consumer must
      # wait on, computed once per consumer. Bare-data layers publish their
      # frame into `state$resolved_data` under `layer_name`; pipeline-head
      # source placeholders publish into `state$resolved_sources` under the
      # placeholder id. Consumers with no source-headed upstream get no
      # guard and pre-warm at boot (today's no-source-upstream behavior).
      # NOTE: walk the upstream for ALL source-placeholder ids regardless
      # of shortcut_id (find_source_self_ids_in_upstream excludes nodes
      # with shortcuts, which is the common ppUpload-with-name case).
      source_ready <- if (!is.null(node$layer_name) &&
                          node$layer_name %in% names(state$resolved_data)) {
        list(kind = "data", id = node$layer_name)
      } else {
        upstream_source_ids <- vapply(
          find_nodes(node$upstream, is_ptr_ph_data_source),
          function(n) n$id %||% NA_character_,
          character(1)
        )
        upstream_source_ids <- upstream_source_ids[!is.na(upstream_source_ids)]
        hit <- intersect(upstream_source_ids, names(state$resolved_sources))
        if (length(hit) > 0L) list(kind = "source", id = hit[[1L]]) else NULL
      }

      entry_reactive <- shiny::reactive({
        state$tree()
        state$stage_enabled()
        for (rv in state$resolved_data) rv()
        # Upstream `upload`-style sources: their resolved frame is bound
        # into `state$eval_env` by `ptr_setup_pipelines()`, which also
        # bumps `state$resolved_sources` -- depend on it so we re-resolve
        # once a file lands (the dataset-name shortcut input is read below).
        for (rv in state$resolved_sources) rv()
        for (cid in upstream_consumer_ids) input[[ns(cid)]]
        producer_values <- list()
        for (pid in upstream_producer_ids) {
          r <- state$producer_input[[pid]]
          val <- if (!is.null(r)) r() else input[[ns(pid)]]
          if (!is.null(val)) producer_values[[pid]] <- val
        }
        input[[ns("ptr_update_plot")]]

        # ADR 0015 §2.1: halt this entry until the upstream source actually
        # has data. Closes the cache-pollution race that motivated the old
        # visibility-gate pre-warm skip without re-introducing suspension.
        if (!is.null(source_ready)) {
          .rv_val <- if (identical(source_ready$kind, "data")) {
            state$resolved_data[[source_ready$id]]()
          } else {
            state$resolved_sources[[source_ready$id]]()
          }
          shiny::req(.rv_val)
          # ADR 0015 PLAN-02 / Option E: also halt until the source
          # observer has bound the resolved frame into `state$eval_env`
          # under `binding_name`. `state$bound_names[[id]]` is written
          # by `bind_source_value()` AFTER the `assign()`, so passing
          # this `req()` guarantees R's symbol lookup will find the
          # bound frame in `eval_env` (no scoping fallback to
          # `datasets::penguins`-style parent-chain leaks). Replaces
          # PLAN-01's client-text + `exists()` poll + `invalidateLater(50)`.
          shiny::req(state$bound_names[[source_ready$id]]())
        }

        snapshot <- producer_values
        for (cid in upstream_consumer_ids) {
          val <- input[[ns(cid)]]
          if (!is.null(val)) snapshot[[cid]] <- val
        }
        for (cmp in upstream_source_shortcut_ids) {
          val <- if (cmp %in% upstream_panel_shortcut_ids) {
            session$rootScope()$input[[cmp]]
          } else {
            input[[ns(cmp)]]
          }
          if (!is.null(val)) snapshot[[cmp]] <- val
        }
        for (sid in upstream_source_self_ids) {
          val <- if (sid %in% upstream_panel_self_ids) {
            session$rootScope()$input[[sid]]
          } else {
            input[[ns(sid)]]
          }
          if (!is.null(val)) snapshot[[sid]] <- val
        }
        # ADR 0015 PLAN-02 / Option E: `state$upstream_cache` is safe
        # again now that `entry_reactive` cannot fire during the
        # autoname-vs-assign race window — the `req(bound_names)` above
        # gates on a server-side reactiveVal that the source observer
        # writes after `assign()` lands. Restored from PLAN-01's
        # `cache = NULL` workaround.
        #
        # Partial-input catch: a placeholder hook upstream of this picker
        # (e.g. ppExpr mid-typing) may signal `ptr_partial_input` via
        # `ptr_signal_partial()`. That is the live-edit equivalent of "no
        # upstream yet" -- cancel this output silently with
        # `cancelOutput = TRUE` so the picker keeps its previous content
        # instead of strobing blank + writing a Shiny warning to stderr.
        # The gated plot-build path (paintr-server.R ~1197) does not
        # catch, so a partial value at Update/Draw time still surfaces as
        # a normal inline error.
        .df <- tryCatch(
          ptr_resolve_upstream(
            node$upstream,
            snapshot = snapshot,
            shared_bindings = state$shared_bindings,
            eval_env = state$eval_env,
            cache = state$upstream_cache,
            expr_check = state$expr_check,
            stage_enabled = shiny::isolate(state$stage_enabled())
          ),
          ptr_partial_input = function(e) {
            shiny::req(FALSE, cancelOutput = TRUE)
          }
        )
        if (is.null(.df)) NULL else list(cols = names(.df), data = .df)
      })

      output[[output_id]] <- shiny::renderUI({
        entry <- entry_reactive()
        cols <- entry$cols %||% character()
        data <- entry$data
        # Preserve the user's current pick across renderUI re-fires
        # (cols change, layer toggle, etc.). `intersect` inside the
        # builtin's build_ui drops it if it's no longer a valid column.
        current <- shiny::isolate(input[[ns(raw_id)]])
        # ADR 0012 / PLAN-01 (Bug B): the spec-seed takes precedence over
        # `current` so a `spec=` entry naming this consumer's id seeds the
        # picker on first render. After the user touches the widget,
        # `current` becomes non-NULL and persistence resumes (the seed is
        # written once at boot; it is not re-read on subsequent fires
        # because by then `current` already shadows it logically — the
        # `%||%` chain picks the first non-NULL).
        seed <- shiny::isolate(state$spec_seed[[raw_id]])
        selected_arg <- if (has_rendered) {
          seed %||% current %||% character(0)
        } else {
          seed
        }
        extra <- list(cols = cols, data = data)
        if (!is.null(selected_arg)) extra$selected <- selected_arg
        result <- invoke_build_ui(
          node,
          ui_text = ui_text,
          layer_name = node$layer_name,
          ns_fn = ui_ns,
          extra = extra
        )
        # Only flip `has_rendered` after a populated render. Two cases that
        # used to lock the picker into an empty selection forever:
        #
        # (1) ppUpload-headed upstream: the first `entry_reactive()` fire
        #     returns NULL because `substitute_walk.ptr_ph_data_source()`
        #     reads the shortcut value from `ctx$snapshot[[node$shortcut_id]]`,
        #     which is briefly NULL post-boot before the client reports the
        #     widget's `value=` back. cols = character() on that fire.
        #
        # (2) Derived-column default (e.g. `aes(y = ppVar(adj))` over an
        #     upstream that includes `dplyr::mutate(adj = ppExpr(mpg/wt))`):
        #     the first fire happens before the ppExpr widget has echoed
        #     its initial value back to the snapshot, so the mutate prunes
        #     out of the upstream and cols = base-data cols without `adj`.
        #     The framework's `node$default = "adj"` injection in
        #     invoke_build_ui (R/paintr-build-ui.R:842-855) reaches
        #     `ptr_builtin_var_build_ui`'s `intersect(selected, cols)`, which
        #     drops it -- and on the next fire `current = character(0)`
        #     shadows the default through the `seed %||% current %||%
        #     character(0)` branch. Result: picker permanently empty even
        #     after the snapshot stabilizes and cols include "adj".
        #
        # Fix: flip `has_rendered` only when this fire could have actually
        # persisted the framework default (or a seed/current). Concretely:
        # `selected_arg` is non-NULL (seed-or-current drove the choice), OR
        # the effective default landed in cols (or there's no default to
        # land). Until then, the next fire stays in the "branch B" path
        # (`extra$selected` unset) so invoke_build_ui re-injects node$default.
        # Explicit-deselect semantics is preserved: once a valid render
        # lands and has_rendered flips TRUE, a user-driven `character(0)`
        # current sticks via the `seed %||% current %||% character(0)`
        # branch exactly as before.
        effective_default <- node$default
        if (is.language(effective_default)) {
          effective_default <- paste(deparse(effective_default), collapse = "\n")
        }
        default_landed <-
          is.null(effective_default) ||
          (is.character(effective_default) &&
           length(effective_default) == 1L &&
           effective_default %in% cols)
        if (length(cols) > 0L && (!is.null(selected_arg) || default_landed)) {
          has_rendered <<- TRUE
        }
        result
      })
      # ADR 0015 §2.1: bind every consumer picker eagerly so it does not
      # hang on inner-tab visibility. Source-headed pipelines no longer
      # race the upload observer because `entry_reactive` now `req()`s on
      # the upstream source-ready reactive (see `source_ready` above),
      # which halts the entry before `ptr_resolve_upstream()` touches
      # `state$upstream_cache` with a stale `eval_env`.
      shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
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
                                            eval_env = parent.frame(),
                                            expr_check = TRUE,
                                            errors_rv = NULL,
                                            state = NULL,
                                            panel_sources = list(),
                                            ui_ns = ns,
                                            spec_seed = NULL) {
  # `ns` namespaces server-side `output[[]]` / `input[[]]` slots; `ui_ns`
  # namespaces the `inputId` of the tag this renderUI emits. They differ
  # under `moduleServer`: there `output`/`input` are auto-namespaced (so
  # `ns` = identity / `NS(NULL)` = `state$server_ns_fn`), but Shiny does
  # NOT auto-namespace a string `inputId` on a dynamically rendered tag,
  # so it must be wrapped explicitly with `session$ns` = `ui_ns` =
  # `state$ui_ns_fn`. Single-instance / panel hosts pass one ns for both,
  # so the `ui_ns = ns` default keeps them byte-stable (mirrors the
  # server_ns/ui_ns split already in `ptr_setup_consumer_uis`).
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
      # Widget-seeding contract — see ?ptr_define_placeholder_consumer.
      has_rendered <- FALSE
      # Upload-shortcut ids in our resolved upstream. When `state` is
      # provided, the renderUI below builds a snapshot from these so
      # `ptr_resolve_upstream` can substitute a data-source placeholder
      # (e.g. `upload`) with its uploaded dataset symbol at runtime.
      upstream_source_shortcut_ids <- if (!is.null(resolution$value)) {
        find_source_companion_ids_in_upstream(resolution$value)
      } else character()
      upstream_source_self_ids <- if (!is.null(resolution$value)) {
        find_source_self_ids_in_upstream(resolution$value)
      } else character()
      # ADR 0023 / PLAN-07: subset of `upstream_source_shortcut_ids`
      # belonging to a panel-owned source (i.e. paired with an `id` in
      # `names(panel_sources)`). The snap loop reads these at the
      # top-level (un-namespaced) input id to match the panel's global
      # id convention; the per-instance loop in `ptr_setup_pipelines()`
      # has no widget for them.
      panel_sources_shortcut_ids <- if (!is.null(resolution$value) &&
                                          length(panel_sources) > 0L) {
        ids <- character()
        ptr_walk(resolution$value, function(n) {
          if (is_ptr_ph_data_source(n) && !is.null(n$shortcut_id) &&
              !is.null(n$id) && n$id %in% names(panel_sources)) {
            ids[[length(ids) + 1L]] <<- n$shortcut_id
          }
        })
        unique(ids)
      } else character()
      # ADR 0015 PLAN-02 / Option E: keys this shared consumer must wait
      # on before `ptr_resolve_upstream` runs. Two kinds:
      #   - pipeline-head sources publish into `state$bound_names[[id]]`
      #     keyed by source-id; surfaced via the upstream's source nodes.
      #   - bare-data layers publish into `state$bound_names[[layer]]`
      #     keyed by layer name; surfaced via `rep_node$layer_name`
      #     (multi-layer cross-shared still gates on the representative
      #     layer; an edge-case noted in PLAN-02 §Out-of-scope).
      upstream_source_ids_for_req <- if (!is.null(resolution$value)) {
        ids <- vapply(
          find_nodes(resolution$value, is_ptr_ph_data_source),
          function(n) n$id %||% NA_character_,
          character(1)
        )
        ids[!is.na(ids)]
      } else character()

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
        # When `state` is wired, prefer the runtime env (where
        # `ptr_setup_pipelines()` binds resolved upload data) and react
        # to data-source resolution so the picker refreshes once a file
        # lands. Snapshot is built from upload-shortcut input values so
        # `ptr_substitute` can resolve a data-source placeholder to its
        # dataset symbol; without these, an `upload`-headed upstream
        # would prune away and `ptr_resolve_upstream` would return NULL
        # even though the data is sitting in `state$eval_env`.
        snap <- list()
        use_env <- eval_env
        # ADR 0023 / PLAN-07: when the consumer's resolved upstream
        # source id sits in `names(panel_sources)`, take a reactive
        # dep on `panel_sources[[sid]]()` so this renderUI re-fires
        # when the panel-owned source resolves (NULL -> df) or
        # transitions to a new frame. At host scope (`state = NULL`)
        # this is the sole signal; at per-instance scope the
        # `state$resolved_sources` / `state$resolved_data` deps below
        # also fire because PLAN-05 mirrors the panel reactive into
        # the per-instance slot, but depending directly here shortens
        # the chain and makes the dep explicit when no `state` is
        # threaded. Read as a separate line near the top of the body
        # (NOT inside the `selected_arg` ternary) so the
        # `has_rendered` closure-flag region remains verbatim.
        # `upstream_source_self_ids` excludes shortcut-driven sources
        # (e.g. `ppUpload`, whose `node$id` is paired with a
        # `shortcut_id` like `<id>_shortcut`); walk `upstream_source_ids_for_req`
        # instead since it collects EVERY data-source `node$id` regardless
        # of shortcut shape.
        for (sid in upstream_source_ids_for_req) {
          if (sid %in% names(panel_sources)) panel_sources[[sid]]()
        }
        # INT-2 (ADR 0023): at host scope (`state = NULL`) we have no
        # `state$eval_env` to thread the panel-resolved frame into, and
        # the per-instance snap block below is gated on `state`. Without
        # this block the host-scope shared consumer picker stays empty
        # even though `panel_sources[[sid]]()` is now wired (the dep
        # loop above re-fires the renderUI, but `ptr_resolve_upstream`
        # finds no symbol to substitute the `ppUpload(shared=)` node
        # to). Build an env extending `eval_env` with the panel-resolved
        # df under the binding name `panel_owned_binding_name()`
        # produces (matches the per-instance binding contract in
        # `ptr_setup_pipelines()`), and seed `snap` so the substitute
        # walk emits that symbol. Per-instance scope (`!is.null(state)`)
        # is unchanged: PLAN-05 already binds the panel-resolved df
        # into `state$eval_env` / `state$bound_names`, and the snap
        # loop below reads the panel-owned shortcut ids from the
        # un-namespaced input.
        if (is.null(state) && length(panel_sources) > 0L &&
            !is.null(resolution$value)) {
          host_env <- NULL
          panel_source_nodes <- find_nodes(
            resolution$value,
            function(n) is_ptr_ph_data_source(n) &&
              !is.null(n$id) && n$id %in% names(panel_sources)
          )
          for (psn in panel_source_nodes) {
            sid <- psn$id
            df_val <- panel_sources[[sid]]()
            if (is.null(df_val)) next
            entry <- ptr_registry_lookup(psn$keyword)
            shortcut_value <- if (!is.null(psn$shortcut_id)) {
              input[[psn$shortcut_id]]
            } else NULL
            bname <- panel_owned_binding_name(
              psn, entry, session = NULL,
              shortcut_value = shortcut_value, df = df_val
            )
            # ADR 0025 §6 / PLAN-06: spec round-trip fallback at the
            # panel-shared snap update. When the host-side shortcut text
            # is empty AND `panel_owned_binding_name` only had
            # `node$default` to fall back on, prefer the
            # coordinator-eval-env-bound auto-name (Plan 02:
            # `node$auto_name`) so the dumped spec/snap matches the
            # symbol the per-instance pipelines bind under. Mirrors the
            # first-site fallback (snapshot-write loop) so a panel-
            # shared `ppUpload(shared='ds')` round-trips through
            # `state$spec()` as the auto-name when no textbox was typed.
            shortcut_empty <- is.null(shortcut_value) ||
              (is.character(shortcut_value) && length(shortcut_value) == 1L &&
               !nzchar(shortcut_value))
            if (shortcut_empty) {
              auto <- psn$auto_name
              if (is.character(auto) && length(auto) == 1L && nzchar(auto) &&
                  make.names(auto) == auto) {
                bname <- auto
              }
            }
            if (is.null(bname)) next
            if (is.null(host_env)) host_env <- new.env(parent = eval_env)
            assign(bname, df_val, envir = host_env)
            if (!is.null(psn$shortcut_id)) {
              snap[[psn$shortcut_id]] <- bname
            } else {
              snap[[sid]] <- bname
            }
          }
          if (!is.null(host_env)) use_env <- host_env
        }
        if (!is.null(state)) {
          # ADR 0015 §2.2: bare-data-source layers publish their frame
          # into `state$resolved_data` under `layer_name`, pipeline-head
          # sources into `state$resolved_sources` under placeholder id.
          # Watch both so a shared picker under a bare-data ppUpload
          # re-fires when the file lands (symmetric with the dep set in
          # `ptr_setup_consumer_uis()`).
          for (rv in state$resolved_sources) rv()
          for (rv in state$resolved_data) rv()
          # ADR 0015 PLAN-02 / Option E: halt this renderUI until every
          # upstream source has bound its frame into `state$eval_env`
          # (symmetric with the `req(bound_names)` guard in the non-shared
          # `ptr_setup_consumer_uis()`). Without this, the same autoname-
          # vs-assign race that PLAN-01 worked around in the non-shared
          # site could let a shared picker fire while R's parent-chain
          # scoping resolves the substituted symbol to a wrong frame.
          if (!is.null(rep_node$layer_name) &&
              rep_node$layer_name %in% names(state$bound_names)) {
            shiny::req(state$bound_names[[rep_node$layer_name]]())
          }
          for (sid in upstream_source_ids_for_req) {
            if (sid %in% names(state$bound_names)) {
              shiny::req(state$bound_names[[sid]]())
            }
          }
          use_env <- state$eval_env
          # INT-3 (ADR 0023 / GAP-3B): when this binder runs inside an
          # embedded `ptr_server()` (moduleServer), `input` is the
          # *namespaced* module input -- `input[[cmp]]` resolves to
          # `input[[<module>-<cmp>]]`, which never exists for a
          # panel-owned shortcut id (its widget lives at the host's
          # top-level un-namespaced slot). Use `rootScope()$input` to
          # read the un-namespaced host slot from inside the module. At
          # host scope (`state` is set on the single-instance / coordinator
          # paths) `rootScope()` returns the same session, so this stays
          # byte-equivalent there.
          root_input <- {
            dom <- shiny::getDefaultReactiveDomain()
            if (!is.null(dom)) dom$rootScope()$input else input
          }
          for (cmp in upstream_source_shortcut_ids) {
            # ADR 0023 / PLAN-07: panel-owned source shortcuts live at
            # the host's top-level (un-namespaced) input id; read from
            # the root session's input (not the module's).
            val <- if (cmp %in% panel_sources_shortcut_ids) {
              root_input[[cmp]]
            } else {
              input[[ns(cmp)]]
            }
            if (!is.null(val)) snap[[cmp]] <- val
          }
          for (sid in upstream_source_self_ids) {
            # ADR 0023 / PLAN-07: panel-owned source self ids live at
            # the host's top-level (un-namespaced) input id.
            val <- if (sid %in% names(panel_sources)) {
              root_input[[sid]]
            } else {
              input[[ns(sid)]]
            }
            if (!is.null(val)) snap[[sid]] <- val
          }
        }
        df <- tryCatch(
          ptr_resolve_upstream(
            resolution$value,
            snapshot = snap,
            shared_bindings = list(),
            eval_env = use_env,
            cache = NULL,
            expr_check = expr_check,
            stage_enabled = list()
          ),
          error = function(e) NULL
        )
        cols <- if (!is.null(df)) names(df) else character()
        current <- shiny::isolate(input[[ns(rep_node$id)]])
        # `state$spec_seed[[rep_node$id]]` is written by
        # `apply_spec_at_boot()` for `spec = list(shared_<key> = ...)`
        # entries; mirrors the seed precedence in
        # `ptr_setup_consumer_uis()` (non-shared) and
        # `ptr_setup_value_uis()`'s shared-value loop. Without this
        # the shared consumer picker boots blank even though the
        # seed sits in `state$spec_seed`.
        #
        # FINDING #1 (placeholder-role-coverage2.html v7) fix: at host
        # scope `state` is NULL by design (ADR 0006), so the seed read
        # against `state$spec_seed` returns NULL and the panel-shared
        # consumer falls back to `shared_widget_default()`. The host
        # passes its un-namespaced `spec_seed` env directly via the
        # `spec_seed=` arg; check it first so the host-scope binder
        # honours `spec=list(shared_<k> = ...)` at boot.
        seed <- if (!is.null(spec_seed)) {
          shiny::isolate(spec_seed[[rep_node$id]])
        } else if (!is.null(state)) {
          shiny::isolate(state$spec_seed[[rep_node$id]])
        } else NULL
        selected_arg <- if (has_rendered) {
          seed %||% current %||% character(0)
        } else {
          seed
        }
        extra <- list(cols = cols, data = df)
        if (!is.null(selected_arg)) extra$selected <- selected_arg
        picker <- invoke_build_ui(
          rep_node,
          ui_text = ui_text,
          layer_name = NULL,
          ns_fn = ui_ns,
          extra = extra,
          label_override = rep_node$shared_label
        )
        has_rendered <<- TRUE
        # Soft advisory: when upstream resolution returns NULL and the
        # cause is an unresolved data-source placeholder (e.g. `upload`
        # not yet provided, `pick_ds` not yet picked), surface that
        # inline rather than letting the picker silently render empty.
        # Distinguished from a hard error: stays out of `#ptr_error`.
        if (is.null(df)) {
          pending <- pending_data_source_keywords(resolution$value)
          if (length(pending) > 0L) {
            advisory <- shiny::div(
              class = "alert alert-warning",
              "Data source not yet provided for this shared picker: ",
              shiny::strong(paste(pending, collapse = ", ")),
              ". Fill it in to populate the column list."
            )
            return(shiny::tagList(advisory, picker))
          }
        }
        picker
      })
    })
  }
  invisible(NULL)
}

# Bind every formula-local shared `var(shared = "...")` consumer picker for
# one tree (or list of trees) at the supplied namespace, EXCLUDING any keys
# the host already owns (`host_owned_keys`). This is the single binder
# preamble: it assembles the per-key representative nodes (id forced to the
# canonical `shared_<key>` id, multi-param param clearing, `shared_label`)
# via `shared_consumer_representatives()` and then calls
# `ptr_bind_shared_consumer_uis()` exactly once. Both `ptr_make_app_server`
# (single-instance: `host_owned_keys = character(0)`, owns all) and
# `ptr_server` (embed: `host_owned_keys =` the coordinator's panel
# keys) route through here so there is one binder implementation and no
# double-write to any shared consumer output id (ADR 0005 partition).
ptr_bind_local_shared_consumers <- function(tree, output, input, ns,
                                            host_owned_keys = character(),
                                            ui_text = NULL,
                                            eval_env = parent.frame(),
                                            expr_check = TRUE,
                                            errors_rv = NULL,
                                            state = NULL,
                                            panel_sources = list(),
                                            ui_ns = ns,
                                            spec_seed = NULL) {
  resolutions <- ptr_resolve_shared_consumers(tree)
  keys <- setdiff(names(resolutions), host_owned_keys)
  if (length(keys) == 0L) return(invisible(NULL))
  resolutions <- resolutions[keys]
  representative_nodes <- shared_consumer_representatives(tree)[keys]
  ptr_bind_shared_consumer_uis(
    output = output, input = input, ns = ns,
    resolutions = resolutions,
    representative_nodes = representative_nodes,
    ui_text = ui_text,
    eval_env = eval_env,
    expr_check = expr_check,
    errors_rv = errors_rv,
    state = state,
    panel_sources = panel_sources,
    ui_ns = ui_ns,
    spec_seed = spec_seed
  )
  invisible(NULL)
}

# Keywords of every data-source placeholder reachable from `upstream`.
# Used by `ptr_bind_shared_consumer_uis()` to label the soft advisory it
# shows when a shared picker's upstream has an unresolved source.
pending_data_source_keywords <- function(upstream) {
  if (is.null(upstream)) return(character())
  keywords <- character()
  ptr_walk(upstream, function(n) {
    if (is_ptr_ph_data_source(n)) {
      keywords[[length(keywords) + 1L]] <<- n$keyword %||% "data source"
    }
  })
  unique(keywords)
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

# Dataset-name shortcut ids of every `ptr_ph_data_source` in a consumer's
# `node$upstream` (e.g. an `upload` at the head of the pipeline). Their text
# values must be in the substitute snapshot for the source to resolve to a
# symbol rather than prune away; the resolved frame itself is bound into
# `state$eval_env` by `ptr_setup_pipelines()`. (`collect_upstream_ids()`
# returns `node$id`; here we need `node$shortcut_id`, hence the bespoke
# walk.)
find_source_companion_ids_in_upstream <- function(upstream) {
  if (is.null(upstream)) return(character())
  ids <- character()
  ptr_walk(upstream, function(n) {
    if (is_ptr_ph_data_source(n) && !is.null(n$shortcut_id)) {
      ids[[length(ids) + 1L]] <<- n$shortcut_id
    }
  })
  unique(ids)
}

# Shortcut-less data sources (e.g. a `selectInput` chooser): for these,
# `substitute_walk.ptr_ph_data_source()` reads the source's *own* input id
# directly out of the snapshot, so we must seed `snapshot[[node$id]]` for
# the upstream to substitute past the head. Shortcut-driven sources are
# handled by `find_source_companion_ids_in_upstream()` (helper name
# preserved; see ADR 0025 §1 — the role label keyed on this walk stays
# `source_companion`).
find_source_self_ids_in_upstream <- function(upstream) {
  if (is.null(upstream)) return(character())
  ids <- character()
  ptr_walk(upstream, function(n) {
    if (is_ptr_ph_data_source(n) && is.null(n$shortcut_id) &&
        !is.null(n$id)) {
      ids[[length(ids) + 1L]] <<- n$id
    }
  })
  unique(ids)
}

# ---- public output bindings ----

# Internal output bindings. Attach the standard plot, error, and code
# outputs to the Shiny `output` object. Sole caller is `ptr_server_internal()`
# (via `ptr_setup_runtime()`), which calls all three unconditionally;
# each reads `state$runtime()`, populated only by the internal
# `ptr_setup_runtime()`. Not a public composition surface post-rewrite.

# Cache the most recent ok-runtime result on `state$last_ok_runtime` so
# that downstream consumers (`ptr_register_code`, `ptr_register_plot`)
# can fall back to the prior successful code panel + plot when the
# current `runtime()` is not-ok. Registered alongside the rest of the
# `ptr_register_*` family and called BEFORE `ptr_register_plot` so the
# cache observer fires first when `state$runtime()` updates (Shiny
# observers run in registration order under default priority). Takes
# `output` to match the family signature but writes nothing to it.
# Caches the full `res` (no field narrowing); the plan permits narrowing
# only if memory pressure surfaces — it has not.
ptr_register_last_ok_cache <- function(output, state) {
  ptr_validate_state(state)
  shiny::observe({
    res <- state$runtime()
    if (isTRUE(res$ok)) {
      state$last_ok_runtime(res)
    }
  })
  invisible(state)
}

ptr_register_plot <- function(output, state) {
  ptr_validate_state(state)
  output[[state$server_ns_fn("ptr_plot")]] <- shiny::renderPlot({
    res <- state$runtime()
    if (!is.null(res) && isTRUE(res$ok) && !is.null(res$plot)) {
      return(res$plot)
    }
    # Retain-on-error: when the current runtime is not-ok (or has no
    # plot), surface the prior successful plot so a transient
    # `validate_input` failure does NOT blank the screen. The error pane
    # already shows the diagnostic; the user keeps the comparison plot.
    last <- state$last_ok_runtime()
    if (!is.null(last) && !is.null(last$plot)) {
      return(last$plot)
    }
    # No prior ok-result — blank the device (matches legacy
    # graphics::plot.new()).
    graphics::plot.new()
    invisible(NULL)
  })
  invisible(state)
}

ptr_register_error <- function(output, state) {
  ptr_validate_state(state)
  output[[state$server_ns_fn("ptr_error")]] <- shiny::renderUI({
    res <- state$runtime()
    # `resolve_errors` is checked first: if an upload (or other source
    # resolver) aborted, the user should see THAT message, not the
    # downstream "object '<name>' not found" that bubbles up at eval time.
    resolve_errs <- unname(unlist(state$resolve_errors()))
    pipe_warns <- state$pipe_layer_warnings %||% character()
    # A successful draw clears the error pane. The unresolvable-shared-picker
    # advisory still surfaces inline on the shared widget itself
    # (`ptr_bind_shared_consumer_uis`), so dropping it here just suppresses
    # the stale stack-up in `#ptr_error` after the plot has rendered.
    # Static formula diagnostics (`pipe_layer_warnings`) persist even after a
    # successful draw -- the formula didn't change.
    if (!is.null(res) && isTRUE(res$ok) && length(resolve_errs) == 0L &&
        length(pipe_warns) == 0L) {
      return(NULL)
    }
    shared_errs <- state$shared_resolution_errors()
    runtime_msg <- if (!is.null(res) && !isTRUE(res$ok)) {
      cli::ansi_strip(res$error %||% "")
    } else NULL
    parts <- c(pipe_warns, resolve_errs, shared_errs,
               if (!is.null(runtime_msg)) runtime_msg)
    if (length(parts) == 0L) return(NULL)
    ptr_error_ui(paste(parts, collapse = "\n"))
  })
  invisible(state)
}

# Render the code-panel text for a runtime result + extras source list.
# Used by both `ptr_register_code` (reactive output) and
# `ptr_extract_code` (read accessor).
#
# `prologue` is the pre-formatted upload-prologue block produced by
# `emit_upload_prologue(state$active_uploads())` -- a string ending with
# a trailing newline so it concatenates cleanly with `base`, or "" when
# no upload-bound source is active. Plain text only; ggpaintr never
# parses/eval's the prologue. See ADR 0025 §4 / PLAN-04.
format_code_with_extras <- function(res, extras_exprs, prologue = "") {
  base <- if (is.null(res)) "" else (res$code_text %||% "")
  if (is.null(res) || !isTRUE(res$ok) || length(extras_exprs) == 0L) {
    return(paste0(prologue, base))
  }
  extra_text <- vapply(extras_exprs, function(e) {
    if (rlang::is_quosure(e)) rlang::quo_text(e) else
      paste(deparse(e), collapse = " ")
  }, character(1))
  body <- if (!nzchar(base)) {
    paste(extra_text, collapse = " +\n  ")
  } else {
    paste(c(base, extra_text), collapse = " +\n  ")
  }
  paste0(prologue, body)
}

ptr_register_code <- function(output, state) {
  ptr_validate_state(state)
  code_id <- state$server_ns_fn("ptr_code")
  mode_id <- state$server_ns_fn("ptr_code_mode")
  output[[code_id]] <- shiny::renderText({
    # ADR 0009 / PLAN-08 / ADR 0022: respect the code-mode toggle. Default
    # ("final" or NULL when the UI has no toggle) shows the substituted
    # code; "spec" shows a snapshot of current widget state as a sparse
    # `ptr_spec <- list(...)` block (the owner pairs it with their formula
    # source to reproduce the app at this state). The pre-ADR-0022
    # "preserve" choice (formula-with-placeholders round-trip + spec
    # underneath) was retired — its formula half could not honestly
    # reproduce apps using structural keywords (ppLayerOff, ppVerbSwitch),
    # and its audience (non-owners cloning the app) does not exist.
    # `ptr_render(..., preserve_placeholders = TRUE)` itself remains a
    # working internal function in R/paintr-render.R; it is no longer
    # called from the panel.
    session <- shiny::getDefaultReactiveDomain()
    mode <- if (is.null(session)) NULL else session$input[[mode_id]]
    if (identical(mode, "spec")) {
      # The spec reflects the current widget state (state$spec() is the
      # live snapshot the runtime updates on each successful Update click).
      # Empty spec -> show a single comment line so the panel is never
      # blank-and-confusing pre-Update.
      spec_text <- format_spec_for_panel(state$spec())
      if (!nzchar(spec_text)) {
        "# No overrides yet -- interact with the controls and click Update."
      } else {
        spec_text
      }
    } else {
      # Retain-on-error fallback for final-mode: when the current runtime
      # is not-ok and a prior ok-result is cached, render the cached
      # `code_text` so the code panel keeps the user's last successful
      # draw on screen while the error pane surfaces the new diagnostic.
      res <- state$runtime()
      last <- state$last_ok_runtime()
      chosen <- if (is.null(res) || isTRUE(res$ok) || is.null(last)) res else last
      # ADR 0025 §4 / PLAN-04: read the active-upload ledger reactively so
      # the prologue updates whenever the upload set changes (post-bind /
      # post-vacate). Reads identical to `ptr_extract_code()`'s isolate
      # read so the on-screen and extracted panel text agree.
      format_code_with_extras(
        chosen, state$extras_exprs(),
        prologue = emit_upload_prologue(state$active_uploads())
      )
    }
  })
  # The generated-code panel lives inside a mini-window that is hidden
  # (display:none) until the user opens it; without this, Shiny suspends the
  # output and the panel renders empty when first revealed.
  shiny::outputOptions(output, code_id, suspendWhenHidden = FALSE)
  invisible(state)
}

#' Extract Runtime Outputs From a `ptr_state`
#'
#' Read the latest plot object, error message, or generated code text from
#' the runtime result stored on a `ptr_state`. Use these to compose custom
#' UIs or to test the runtime in `shiny::testServer`.
#'
#' @param state A `ptr_state` from [ptr_init_state()].
#'
#' @return `ptr_extract_plot` returns a `ggplot` object (or `NULL` on
#'   failure); `ptr_extract_error` returns a string or `NULL`;
#'   `ptr_extract_code` returns a single string.
#'
#' @section Reactive contexts:
#' Each function wraps its read in [shiny::isolate()], so it works in both
#' reactive and non-reactive contexts and returns the current value without
#' establishing a reactive dependency.
#'
#' **Do not call these inside a `render*{}` block** if you want the output
#' to update when the plot rerenders. Because `isolate()` suppresses the
#' dependency on `state$runtime()`, the render block fires once on mount and
#' never again. Inside a reactive context, read `state$runtime()` directly —
#' that takes the dependency and re-fires on every *Update plot* click.
#' Reserve `ptr_extract_*` for non-reactive contexts: download handlers,
#' [shiny::testServer()] assertions, and one-shot reads outside any session.
#' @name ptr_extract
#' @examples
#' shiny::isolate({
#'   state <- ptr_init_state(
#'     "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"
#'   )
#'   ptr_extract_code(state)
#' })
#' @export
ptr_extract_plot  <- function(state) shiny::isolate(state$runtime())$plot

#' @rdname ptr_extract
#' @export
ptr_extract_error <- function(state) shiny::isolate(state$runtime())$error

#' @rdname ptr_extract
#' @export
ptr_extract_code  <- function(state) {
  shiny::isolate(format_code_with_extras(
    state$runtime(), state$extras_exprs(),
    prologue = emit_upload_prologue(state$active_uploads())
  ))
}

# ---- ptr_gg_extra ----

#' Add `ggplot2` Layers Programmatically
#'
#' Evaluate one or more `ggplot2` expressions and attach the results as
#' "extras" on the state. Extras are folded into the plot during the next
#' runtime cycle when `state$runtime()$ok` is `TRUE`. Eval failures leave
#' the existing extras untouched.
#'
#' @param state A `ptr_state` from [ptr_init_state()].
#' @param ... `ggplot2` layer expressions (e.g.
#'   `ptr_gg_extra(state, ggplot2::scale_x_log10(), theme_minimal())`).
#'   Captured unevaluated and stored as quosures, then evaluated in
#'   `state$eval_env`. Eval errors propagate and leave the existing
#'   extras untouched (atomic update).
#'
#' @return `state`, invisibly.
#' @examples
#' shiny::isolate({
#'   state <- ptr_init_state(
#'     "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"
#'   )
#'   state <- ptr_gg_extra(state, ggplot2::theme_minimal())
#'   ptr_extract_code(state)
#' })
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
