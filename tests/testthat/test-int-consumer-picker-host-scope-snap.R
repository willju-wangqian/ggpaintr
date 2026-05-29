# INT-2 (ADR 0023): the consumer-picker `renderUI` must populate at
# HOST scope (`state = NULL`) when a `var(shared = "...")` consumer reads
# `ppUpload(shared = "ds")` upstream and the panel owns that source.
#
# Two distinct gaps the patch closes:
#
#  GAP A — `ptr_bind_shared_consumer_uis()` renderUI body: the snap
#    block that reads upstream source companion / self ids into the
#    substitution snapshot, AND the env extension that binds the
#    panel-resolved df under the panel binding name, were both gated on
#    `!is.null(state)`. At host scope this left `snap` empty and the
#    substitute walk dropped the `ppUpload(shared)` node from the
#    consumer's upstream, so `ptr_resolve_upstream()` returned NULL and
#    the picker rendered with `cols = character()`. The patch hoists a
#    host-scope-only block that builds a temp env extending `eval_env`
#    with the panel-resolved df under the `panel_owned_binding_name()`
#    symbol and seeds the snap so the substitute walk emits that symbol.
#
#  GAP B — both `ptr_bind_local_shared_consumers()` call sites in
#    `R/paintr-app.R` (the single-instance preamble at line ~337 and the
#    embed binder at line ~1214) omitted `panel_sources =`, so even when
#    the host wired `state$panel_sources`, the binder defaulted to
#    `list()` and the renderUI's PLAN-07 dep loop never engaged. The
#    patch threads `panel_sources = state$panel_sources %||% list()`
#    through both sites.
#
# These tests pin the patch by asserting that `invoke_build_ui()`
# receives an `extra` with non-empty `cols` matching the panel-resolved
# frame, NOT merely that the renderUI fires. Asserting on `cols` is what
# would have failed at HEAD pre-patch (the PLAN-07 dep wakes the renderUI
# but `cols` stays `character()` at host scope).


# ---- GAP A: host-scope picker populates cols from panel-resolved df ----

test_that("host-scope (state=NULL) renderUI picker populates `cols` from panel-resolved frame", {
  tree <- ggpaintr:::ptr_translate(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )
  resolutions <- ggpaintr:::ptr_resolve_shared_consumers(tree)
  rep_nodes <- ggpaintr:::shared_consumer_representatives(tree)
  out_id <- ggpaintr:::consumer_output_id(
    ggpaintr:::canonical_shared_id("col")
  )

  panel_rv <- shiny::reactiveVal(NULL)
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
      resolutions = resolutions,
      representative_nodes = rep_nodes,
      eval_env = globalenv(),
      state = NULL,
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    # Seed the companion text input at host top-level (un-namespaced)
    # so `panel_owned_binding_name()` derives a valid R name. ppUpload's
    # companion convention is `<source_id>_name`; for this consumer
    # `shared_ds_shortcut` is what the panel's text widget would publish.
    session$setInputs(shared_ds_shortcut = "uploaded_ds")
    panel_rv(mtcars)
    session$flushReact()
    expect_gte(length(captured_extras), 1L)
    last_extra <- captured_extras[[length(captured_extras)]]
    # GAP-A regression assertion: cols are concretely populated from
    # the panel-resolved frame. Pre-patch this was `character(0)`.
    expect_identical(last_extra$cols, names(mtcars))
    expect_s3_class(last_extra$data, "data.frame")
    expect_identical(nrow(last_extra$data), nrow(mtcars))
  })
})


# ---- GAP A: host-scope picker re-fires + populates on df' --------------

test_that("host-scope renderUI re-fires AND re-populates cols when panel reactive flips df -> df'", {
  tree <- ggpaintr:::ptr_translate(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )
  resolutions <- ggpaintr:::ptr_resolve_shared_consumers(tree)
  rep_nodes <- ggpaintr:::shared_consumer_representatives(tree)
  out_id <- ggpaintr:::consumer_output_id(
    ggpaintr:::canonical_shared_id("col")
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
      resolutions = resolutions,
      representative_nodes = rep_nodes,
      eval_env = globalenv(),
      state = NULL,
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    session$setInputs(shared_ds_shortcut = "uploaded_ds")
    session$flushReact()
    n_first <- length(captured_extras)
    expect_gte(n_first, 1L)
    first_cols <- captured_extras[[n_first]]$cols
    expect_identical(first_cols, names(mtcars))
    # Flip to iris and confirm the last captured extra reflects iris cols.
    panel_rv(iris)
    session$flushReact()
    expect_gt(length(captured_extras), n_first)
    last_extra <- captured_extras[[length(captured_extras)]]
    expect_identical(last_extra$cols, names(iris))
  })
})


# ---- GAP A: panel df present + companion blank -> §3b canonical bind, picker
#            comes up POPULATED-unselected (ADR 0025 §3 caveat-2 + §3b) -------

test_that("host-scope: panel df present + companion blank => picker populated-unselected (canonical bind)", {
  # ADR 0025 §3 caveat-2 (campaign fix #2): an empty-shortcut SHARED upload
  # binds under the canonical auto-name (`panel_source_canonical_name()` =
  # node$auto_name %||% node$shared, here "ds"), NOT NULL. So with the panel
  # df present the consumer picker comes up POPULATED with that frame's columns
  # -- per §3b "the picker comes up populated but unselected." (Pre-fix the
  # blank companion returned NULL and the picker rendered empty cols; that is
  # the retired behavior this test used to assert.)
  tree <- ggpaintr:::ptr_translate(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )
  resolutions <- ggpaintr:::ptr_resolve_shared_consumers(tree)
  rep_nodes <- ggpaintr:::shared_consumer_representatives(tree)
  out_id <- ggpaintr:::consumer_output_id(
    ggpaintr:::canonical_shared_id("col")
  )

  panel_sources <- list(shared_ds = shiny::reactive(mtcars))

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
      resolutions = resolutions,
      representative_nodes = rep_nodes,
      eval_env = globalenv(),
      state = NULL,
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    # No companion seed -> empty-shortcut shared upload binds under the
    # canonical name "ds" (§3 caveat-2) -> the panel df (mtcars) resolves ->
    # the picker is offered that frame's columns, unselected (§3b).
    session$flushReact()
    expect_gte(length(captured_extras), 1L)
    last_extra <- captured_extras[[length(captured_extras)]]
    expect_identical(last_extra$cols, names(mtcars))
    # §3b: populated but NOT auto-selected.
    expect_length(last_extra$selected, 0L)
  })
})


# ---- GAP B: ptr_bind_local_shared_consumers threads panel_sources ------
# (symmetric pin of the per-instance call site; the binder must forward
# the arg verbatim and the renderUI dep on the panel reactive must wake.)

test_that("ptr_bind_local_shared_consumers (host-scope, no state) populates cols from panel_sources", {
  # End-to-end through the public binder helper (which both
  # `ptr_make_app_server` and `ptr_server_internal` use) under
  # `state = NULL`. Cols must come from the panel-resolved frame.
  tree <- ggpaintr:::ptr_translate(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "col"))) + geom_point()'
  )
  out_id <- ggpaintr:::consumer_output_id(
    ggpaintr:::canonical_shared_id("col")
  )

  panel_sources <- list(shared_ds = shiny::reactive(iris))

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
    ggpaintr:::ptr_bind_local_shared_consumers(
      tree = tree, output = output, input = input, ns = identity,
      eval_env = globalenv(),
      panel_sources = panel_sources
    )
    shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
  }
  shiny::testServer(server, {
    session$setInputs(shared_ds_shortcut = "uploaded_iris")
    session$flushReact()
    expect_gte(length(captured_extras), 1L)
    last_extra <- captured_extras[[length(captured_extras)]]
    expect_identical(last_extra$cols, names(iris))
  })
})
