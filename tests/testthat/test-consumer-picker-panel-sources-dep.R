# ADR 0023 / PLAN-07: consumer-picker renderUI depends on panel-owned
# source reactives. Pins `ptr_bind_shared_consumer_uis()` and
# `ptr_bind_local_shared_consumers()` in R/paintr-server.R:
#
#   * Empty `panel_sources = list()` (the common case) is a no-op: the
#     renderUI body never calls `panel_sources[[...]]()`, so the existing
#     `state$resolved_sources` / `state$resolved_data` deps remain the
#     sole signal.
#   * When the consumer's resolved upstream source id sits in
#     `names(panel_sources)`, the renderUI takes a reactive dep on
#     `panel_sources[[sid]]()`. Transitioning the panel reactive
#     (NULL -> df, or df -> df') re-fires the renderUI.
#   * The `has_rendered` closure-flag (commit 401804c) is preserved
#     verbatim: first render omits `selected`; subsequent renders pass
#     `seed %||% current %||% character(0)` so a user-emptied widget
#     stays empty. This file pins that invariant under populated
#     `panel_sources` so the new dep doesn't regress it.
#   * Host-scope invocation (`state = NULL`) with populated
#     `panel_sources` still wires the dep -- the new path replaces the
#     legacy "no state -> no upload re-fire" exclusion at host scope.
#
# Test mechanism: spy on `invoke_build_ui` via `local_mocked_bindings`
# (the binder's terminal call before flipping `has_rendered = TRUE`).
# Each invocation = one renderUI evaluation. The renderUI is forced to
# evaluate by reading `output[[id]]` after each `flushReact()`; in
# testServer that pull schedules the reactive endpoint to run.


# Helper: build the bare common bits each scenario needs.
build_panel_consumer_setup <- function(formula) {
  tree <- ggpaintr:::ptr_translate(formula)
  resolutions <- ggpaintr:::ptr_resolve_shared_consumers(tree)
  rep_nodes <- ggpaintr:::shared_consumer_representatives(tree)
  list(
    tree = tree,
    resolutions = resolutions,
    rep_nodes = rep_nodes,
    out_id = ggpaintr:::placeholder_output_id(
      ggpaintr:::canonical_shared_id("col")
    )
  )
}


# ---- Scenario 1: empty panel_sources is a no-op ---------------------------

test_that("empty panel_sources is a no-op (renderUI body never calls panel_sources[[...]])", {
  s <- build_panel_consumer_setup(
    'ggplot(ppUpload(), aes(x = ppVar(shared = "col"))) + geom_point()'
  )
  # If the body ever touched a panel_sources entry that doesn't exist,
  # we'd see a "subscript out of bounds" error on `panel_sources[[sid]]()`.
  # A successful renderUI invocation with `panel_sources = list()` proves
  # the empty branch is a no-op.
  call_count <- 0L
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    call_count <<- call_count + 1L
    shiny::tags$div(id = node$id, class = "stub-picker")
  }
  testthat::local_mocked_bindings(
    invoke_build_ui = invoke_spy, .package = "ggpaintr"
  )
  server <- function(input, output, session) {
    ggpaintr:::ptr_bind_shared_consumer_uis(
      output = output, input = input, ns = identity,
      resolutions = s$resolutions,
      representative_nodes = s$rep_nodes,
      eval_env = globalenv()
      # panel_sources defaults to list()
    )
    # Force the renderUI to evaluate by mounting it with
    # suspendWhenHidden = FALSE.
    shiny::outputOptions(output, s$out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_gte(call_count, 1L)
  })
})


# ---- Scenario 2: panel-owned source dep wakes the consumer picker --------

test_that("panel_sources reactive transitioning NULL -> df re-fires the consumer renderUI", {
  s <- build_panel_consumer_setup(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )

  panel_rv <- shiny::reactiveVal(NULL)
  panel_sources <- list(shared_ds = shiny::reactive(panel_rv()))

  call_count <- 0L
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    call_count <<- call_count + 1L
    shiny::tags$div(id = node$id, class = "stub-picker")
  }
  testthat::local_mocked_bindings(
    invoke_build_ui = invoke_spy, .package = "ggpaintr"
  )

  server <- function(input, output, session) {
    ggpaintr:::ptr_bind_shared_consumer_uis(
      output = output, input = input, ns = identity,
      resolutions = s$resolutions,
      representative_nodes = s$rep_nodes,
      eval_env = globalenv(),
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, s$out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    session$flushReact()
    baseline <- call_count
    expect_gte(baseline, 1L)
    # Flip NULL -> df: renderUI re-fires because of the new
    # `panel_sources[[sid]]()` dep.
    panel_rv(mtcars)
    session$flushReact()
    expect_gt(call_count, baseline)
    # Flip df -> df': still re-fires.
    after_first <- call_count
    panel_rv(iris)
    session$flushReact()
    expect_gt(call_count, after_first)
  })
})


# ---- Scenario 3: has_rendered invariant -- first render omits `selected` -

test_that("has_rendered: first render omits `selected` even with panel_sources", {
  # On first render (has_rendered = FALSE) AND no spec_seed,
  # `selected_arg` resolves to NULL and is NOT added to `extra`. The
  # 401804c invariant: omit `selected` on first render.
  s <- build_panel_consumer_setup(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )

  panel_rv <- shiny::reactiveVal(mtcars)
  panel_sources <- list(shared_ds = shiny::reactive(panel_rv()))

  captured_extras <- list()
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    captured_extras[[length(captured_extras) + 1L]] <<- extra
    shiny::tags$div(id = node$id, class = "stub-picker")
  }
  testthat::local_mocked_bindings(
    invoke_build_ui = invoke_spy, .package = "ggpaintr"
  )

  server <- function(input, output, session) {
    ggpaintr:::ptr_bind_shared_consumer_uis(
      output = output, input = input, ns = identity,
      resolutions = s$resolutions,
      representative_nodes = s$rep_nodes,
      eval_env = globalenv(),
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, s$out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_gte(length(captured_extras), 1L)
    first_extra <- captured_extras[[1L]]
    expect_false("selected" %in% names(first_extra))
  })
})


# ---- Scenario 4: has_rendered invariant -- subsequent render preserves -
# ---- a user-emptied widget (selected = character(0))                  ---

test_that("has_rendered: subsequent renders pass current %||% character(0) -- user-emptied widget stays empty", {
  # Pins the 401804c invariant under populated panel_sources: once
  # has_rendered = TRUE, `selected_arg` = `seed %||% current %||%
  # character(0)`. With no seed and user-emptied widget
  # (input = character(0)), `selected_arg` must be character(0).
  s <- build_panel_consumer_setup(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )

  panel_rv <- shiny::reactiveVal(mtcars)
  panel_sources <- list(shared_ds = shiny::reactive(panel_rv()))

  captured_extras <- list()
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    captured_extras[[length(captured_extras) + 1L]] <<- extra
    shiny::tags$div(id = node$id, class = "stub-picker")
  }
  testthat::local_mocked_bindings(
    invoke_build_ui = invoke_spy, .package = "ggpaintr"
  )

  server <- function(input, output, session) {
    ggpaintr:::ptr_bind_shared_consumer_uis(
      output = output, input = input, ns = identity,
      resolutions = s$resolutions,
      representative_nodes = s$rep_nodes,
      eval_env = globalenv(),
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, s$out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    # Give the panel-owned `ppUpload(shared = "ds")` source a companion name
    # so its frame actually binds and the consumer picker resolves a
    # POPULATED column list. Without this the upstream never resolves (no
    # binding name -> ptr_resolve_upstream returns NULL -> cols = character()),
    # and the test would only exercise the degenerate empty-picker regime
    # where the seed/current/default distinction is invisible. With cols
    # populated, `has_rendered` flips on the first populated render (no
    # default -> default_landed) and we genuinely test that a user-emptied
    # widget then keeps passing selected = character(0).
    session$setInputs(`shared_ds_shortcut` = "ds_df")
    session$flushReact()
    n_first <- length(captured_extras)
    expect_gte(n_first, 1L)
    # Sanity: the picker resolved real columns (mtcars), so the latch flipped.
    expect_gt(length(captured_extras[[n_first]]$cols), 0L)
    # User empties the picker.
    session$setInputs(`shared_col` = character(0))
    # Flip the panel reactive to re-fire the renderUI.
    panel_rv(iris)
    session$flushReact()
    expect_gt(length(captured_extras), n_first)
    last_extra <- captured_extras[[length(captured_extras)]]
    # has_rendered = TRUE (populated render happened) + user-emptied current
    # => selected_arg = seed %||% current %||% character(0) = character(0).
    expect_true("selected" %in% names(last_extra))
    expect_identical(last_extra$selected, character(0))
  })
})


# ---- Scenario 5: host-scope (state = NULL) with panel_sources wires dep -

test_that("host-scope invocation (state = NULL) with populated panel_sources still re-fires on panel updates", {
  # Mirrors Scenario 2 explicitly under `state = NULL`. The legacy code
  # required `state` to be present to register the consumer's data-source
  # dep; the new path takes the panel_sources dep regardless of `state`.
  s <- build_panel_consumer_setup(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )

  panel_rv <- shiny::reactiveVal(NULL)
  panel_sources <- list(shared_ds = shiny::reactive(panel_rv()))

  call_count <- 0L
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    call_count <<- call_count + 1L
    shiny::tags$div(id = node$id, class = "stub-picker")
  }
  testthat::local_mocked_bindings(
    invoke_build_ui = invoke_spy, .package = "ggpaintr"
  )

  server <- function(input, output, session) {
    ggpaintr:::ptr_bind_shared_consumer_uis(
      output = output, input = input, ns = identity,
      resolutions = s$resolutions,
      representative_nodes = s$rep_nodes,
      eval_env = globalenv(),
      state = NULL,
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, s$out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    session$flushReact()
    baseline <- call_count
    expect_gte(baseline, 1L)
    panel_rv(mtcars)
    session$flushReact()
    expect_gt(call_count, baseline)
  })
})


# ---- Scenario 6: ptr_bind_local_shared_consumers threads panel_sources --

test_that("ptr_bind_local_shared_consumers threads panel_sources into the binder", {
  s <- build_panel_consumer_setup(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )

  panel_rv <- shiny::reactiveVal(NULL)
  panel_sources <- list(shared_ds = shiny::reactive(panel_rv()))

  call_count <- 0L
  invoke_spy <- function(node, ui_text, layer_name, ns_fn, extra,
                         param_override = NULL, label_suffix = NULL,
                         label_override = NULL, ...) {
    call_count <<- call_count + 1L
    shiny::tags$div(id = node$id, class = "stub-picker")
  }
  testthat::local_mocked_bindings(
    invoke_build_ui = invoke_spy, .package = "ggpaintr"
  )

  server <- function(input, output, session) {
    ggpaintr:::ptr_bind_local_shared_consumers(
      tree = s$tree, output = output, input = input, ns = identity,
      eval_env = globalenv(),
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, s$out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    session$flushReact()
    baseline <- call_count
    expect_gte(baseline, 1L)
    panel_rv(mtcars)
    session$flushReact()
    expect_gt(call_count, baseline)
  })
})
