# INT-3 (ADR 0023): two further gaps the consumer-picker `renderUI` body
# leaves open after INT-2 lands, both surfaced by PLAN-08's worked-example
# fixtures.
#
#  GAP-3A -- host-scope `default_arg` priming (worked example #2). The
#    panel-owned source carries `node$default` (a script-scope symbol like
#    `df_main` resolved by `try_bind_source_default_resolved()` at boot).
#    `panel_sources[[sid]]()` correctly returns the primed df, but the
#    INT-2 host-scope block in `ptr_bind_shared_consumer_uis()` calls
#    `panel_owned_binding_name()` with `shortcut_value = NULL` (the
#    companion textInput has not registered yet -- and never does at boot
#    because no upload occurred), so the helper returned NULL and the
#    `next` skipped the source. Snap stayed empty, the substitute walk
#    dropped the `ppUpload(shared)` node, and the picker rendered with
#    `cols = character()`. INT-3 fix: mirror the boot-race fallback in
#    `try_bind_source_default()` -- when the companion is blank/NULL,
#    fall back to `node$default` so the binding name resolves at boot.
#
#  GAP-3B -- per-instance shared consumer under a panel-owned source
#    (worked example #3). Formula-local `ppVar(shared='colA')` lives in
#    the per-instance module namespace; its picker renderUI runs inside
#    `moduleServer`, where `input` is the *module's namespaced* input
#    bag. `input[[cmp]]` resolves to `input[[<module>-<cmp>]]`, which
#    NEVER exists for a panel-owned companion id whose widget lives at
#    the host's top-level un-namespaced slot. So `snap[[cmp]]` stayed
#    NULL, the substitute walk dropped the upload node, and the picker
#    rendered empty even though the user typed the companion name.
#    INT-3 fix: route the panel-owned companion / self lookups through
#    `getDefaultReactiveDomain()$rootScope()$input[[cmp]]` so they reach
#    the un-namespaced top-level slot from inside the module.
#
# Both tests assert on the `extra$cols` the binder passes into
# `invoke_build_ui()`. Pre-patch, `cols` was `character()`; post-patch,
# it must be the panel-resolved frame's column names. Asserting on
# `cols` (not "renderUI fires") is what would have failed at HEAD --
# the renderUI fires either way because the PLAN-07 dep loop wakes it.


# ---- GAP-3A: host-scope `default_arg` priming populates picker --------

test_that("GAP-3A: host-scope picker populates from default-arg-primed panel source (no companion typed)", {
  tree <- ggpaintr:::ptr_translate(
    'ppUpload(df_main, shared = "ds") |> ggplot(aes(x = ppVar(shared = "col"), y = mpg)) + geom_point()'
  )
  resolutions <- ggpaintr:::ptr_resolve_shared_consumers(tree)
  rep_nodes   <- ggpaintr:::shared_consumer_representatives(tree)
  out_id      <- ggpaintr:::placeholder_output_id(
    ggpaintr:::canonical_shared_id("col")
  )

  # Simulate what `ptr_setup_panel_sources()`'s default-arg observer
  # publishes: a panel reactive returning the primed df, with the source
  # node carrying `default = "df_main"` (the script-scope symbol the
  # default-arg resolved to).
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
    # NB: NO `setInputs(shared_ds_shortcut = ...)` -- the companion
    # textInput hasn't registered (this is exactly the boot state for
    # the default-arg priming path). Pre-INT-3,
    # `panel_owned_binding_name()` would return NULL here and the
    # picker would stay empty; post-INT-3, the `node$default` fallback
    # supplies the binding name.
    session$flushReact()
    expect_gte(length(captured_extras), 1L)
    last_extra <- captured_extras[[length(captured_extras)]]
    expect_identical(last_extra$cols, names(mtcars))
    expect_s3_class(last_extra$data, "data.frame")
    expect_identical(nrow(last_extra$data), nrow(mtcars))
  })
})


test_that("GAP-3A unit: panel_owned_binding_name falls back to node$default when companion is blank", {
  # Direct unit on the helper: the NULL/empty companion path now consults
  # `node$default`. This nails the contract independent of the renderUI
  # plumbing and is what `ptr_setup_pipelines()`'s panel-owned branches
  # rely on to write a non-NULL key into `state$bound_names[[layer]]`
  # for default-arg priming.
  node <- list(
    id           = "shared_ds",
    shortcut_id = "shared_ds_shortcut",
    default      = "df_main"
  )
  # Companion blank => default wins.
  expect_identical(
    ggpaintr:::panel_owned_binding_name(node, entry = NULL, session = NULL,
                                        shortcut_value = NULL, df = mtcars),
    "df_main"
  )
  expect_identical(
    ggpaintr:::panel_owned_binding_name(node, entry = NULL, session = NULL,
                                        shortcut_value = "",   df = mtcars),
    "df_main"
  )
  # Companion present + valid => companion wins (default ignored).
  expect_identical(
    ggpaintr:::panel_owned_binding_name(node, entry = NULL, session = NULL,
                                        shortcut_value = "penguins",
                                        df = mtcars),
    "penguins"
  )
  # Companion blank AND default missing => NULL (no binding).
  node_no_default <- node; node_no_default$default <- NULL
  expect_null(
    ggpaintr:::panel_owned_binding_name(node_no_default, entry = NULL,
                                        session = NULL,
                                        shortcut_value = NULL, df = mtcars)
  )
  # Default not a valid R name => NULL.
  node_bad <- node; node_bad$default <- "1bad name"
  expect_null(
    ggpaintr:::panel_owned_binding_name(node_bad, entry = NULL, session = NULL,
                                        shortcut_value = NULL, df = mtcars)
  )
})


# ---- GAP-3B: per-instance picker reads root-scope panel companion -----

test_that("GAP-3B: per-instance (moduleServer) picker populates from panel-owned source via root-scope companion read", {
  # Mixed scope: panel-owned source (`shared_ds`) consumed by a
  # formula-local consumer (`colA`). The binder runs at module scope --
  # `input` is the module's namespaced bag. Pre-INT-3, `input[[cmp]]`
  # resolved to `input[["<module>-shared_ds_shortcut"]]` (NULL forever).
  # Post-INT-3 the panel-owned companion read uses rootScope()$input,
  # which sees the host top-level companion value.
  tree <- ggpaintr:::ptr_translate(
    'ggplot(ppUpload(shared = "ds"), aes(x = ppVar(shared = "colA"), y = body_mass_g)) + geom_point()'
  )
  resolutions <- ggpaintr:::ptr_resolve_shared_consumers(tree)
  rep_nodes   <- ggpaintr:::shared_consumer_representatives(tree)
  out_id      <- ggpaintr:::placeholder_output_id(
    ggpaintr:::canonical_shared_id("colA")
  )

  # Synthetic per-instance state mirroring what `ptr_init_state()`
  # produces for a bare-data-source layer (`bound_names[["ggplot"]]`,
  # `resolved_data[["ggplot"]]`) plus a populated eval_env (mimicking
  # the per-instance panel-owned branch in `ptr_setup_pipelines()` that
  # binds the resolved df under the binding name).
  eval_env <- new.env(parent = globalenv())
  assign("penguins_df", mtcars, envir = eval_env)  # stand-in frame
  state <- list(
    eval_env         = eval_env,
    resolved_data    = list(ggplot = shiny::reactive(mtcars)),
    resolved_sources = list(),
    bound_names      = list(
      ggplot = shiny::reactiveVal("penguins_df")
    ),
    spec_seed        = list()
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

  # Wrap the binder in a moduleServer so the renderUI body sees a
  # namespaced `input` bag -- the exact condition that exposes GAP-3B.
  inner_module <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      ggpaintr:::ptr_bind_shared_consumer_uis(
        output = output, input = input, ns = identity,
        resolutions = resolutions,
        representative_nodes = rep_nodes,
        eval_env = eval_env,
        state = state,
        panel_sources = panel_sources,
        ui_ns = session$ns
      )
      shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
    })
  }

  server <- function(input, output, session) {
    inner_module("p1")
  }

  shiny::testServer(server, {
    # Companion lives at host top-level; the module's `input` bag MUST
    # NOT see it directly. Pre-INT-3 the binder read `input[[cmp]]` in
    # the module bag (NULL); post-INT-3 it routes through rootScope().
    session$setInputs(shared_ds_shortcut = "penguins_df")
    session$flushReact()
    expect_gte(length(captured_extras), 1L)
    last_extra <- captured_extras[[length(captured_extras)]]
    expect_identical(last_extra$cols, names(mtcars))
    expect_s3_class(last_extra$data, "data.frame")
    expect_identical(nrow(last_extra$data), nrow(mtcars))
  })
})
