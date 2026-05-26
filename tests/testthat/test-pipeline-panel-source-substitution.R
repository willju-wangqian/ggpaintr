# ADR 0023 / PLAN-05: per-instance pipelines substitute panel-owned source
# reactives. Pins `ptr_setup_pipelines()` in R/paintr-server.R:
#
#   * Empty `state$panel_sources` (the common case) keeps the existing
#     per-instance input-watching observer path intact (no-op branch).
#   * For each source id `sid` in `names(state$panel_sources)`, both the
#     bare-data-source-layer loop and the pipeline-head source loop skip
#     the input-watching observer and instead install one
#     `shiny::observe({ df <- state$panel_sources[[sid]](); ... })` that
#     mirrors `df` into the per-instance slot
#     (`state$resolved_data[[layer]]` for the bare-layer case,
#     `state$resolved_sources[[sid]]` for the pipeline-head case) via
#     `bind_source_value()` -- so downstream consumer pickers and the
#     plot evaluator see one unified resolved view.
#   * Updating the panel reactive (incl. -> NULL) propagates to every
#     per-instance slot on the next flush; binding name derivation reads
#     the companion text-input value at the host's top-level
#     (`session$rootScope()$input[[node$companion_id]]`), mirroring the
#     name-derivation logic in `resolve_upload_source()`.
#
# Worked-example coverage:
#   * ADR #3 (positive) -- Mixed scope: panel-owned source, formula-local
#     consumers; both instances bind off the panel-resolved df.
#   * ADR #5 (positive) -- Pipeline-head ppUpload chained through
#     intermediate stages (dplyr::filter / mutate); the resolved df lands
#     under the companion's binding name in `state$eval_env` so the
#     intermediate stage's `.data` argument finds the panel-resolved df.


# ---- Scenario 1: empty panel_sources is a no-op (existing path runs) ----

test_that("empty panel_sources keeps the per-instance input-watching path", {
  # No `shared_state` -> `state$panel_sources` is `list()`. The existing
  # per-instance observer must still resolve the local input upload.
  captured <- NULL
  server <- function(input, output, session) {
    captured <<- ggpaintr::ptr_server(
      'ggplot(ppUpload(), aes(x = ppVar())) + geom_point()',
      "p1", envir = globalenv()
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_identical(captured$panel_sources, list())
    # Slot still NULL (no upload provided); critically no error from the
    # new branch having run.
    expect_null(shiny::isolate(captured$resolved_data$ggplot()))
  })
})


# ---- Scenario 2: panel-owned source id is read from state$panel_sources ----

test_that("panel-owned pipeline-head source reads from state$panel_sources, not input", {
  # Pipeline-head shape (an intermediate stage) keeps the source on the
  # `pipeline_source_ids` path (`state$resolved_sources[[sid]]`), which
  # the second loop in `ptr_setup_pipelines()` substitutes.
  panel_rv <- shiny::reactiveVal(mtcars)
  captured <- NULL
  server <- function(input, output, session) {
    ss <- ggpaintr:::new_ptr_shared_state(
      shared = list(col = shiny::reactiveVal("mpg")),
      draw_trigger = shiny::reactiveVal(1L),
      shared_resolutions = list(),
      shared_stage_enabled = list(),
      panel_sources = list(shared_ds = panel_rv)
    )
    captured <<- ggpaintr::ptr_server(
      'ppUpload(shared="ds") |> head(5) |> ggplot(aes(x = ppVar(shared="col"))) + geom_point()',
      "p1", envir = globalenv(),
      shared_state = ss,
      panel_sources = list(shared_ds = panel_rv)
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_identical(
      shiny::isolate(captured$resolved_sources$shared_ds()),
      mtcars
    )
  })
})

test_that("panel-owned bare-data-source layer reads from state$panel_sources, not input", {
  # Bare-data-source-layer shape -- `ggplot(ppUpload(shared="ds"), ...)`
  # routes through the FIRST loop (`is_bare_data_source_layer`), whose
  # slot lives at `state$resolved_data[[layer_name]]`. The PLAN-05 branch
  # must apply equally there.
  panel_rv <- shiny::reactiveVal(iris)
  captured <- NULL
  server <- function(input, output, session) {
    ss <- ggpaintr:::new_ptr_shared_state(
      shared = list(col = shiny::reactiveVal("Sepal.Length")),
      draw_trigger = shiny::reactiveVal(1L),
      shared_resolutions = list(),
      shared_stage_enabled = list(),
      panel_sources = list(shared_ds = panel_rv)
    )
    captured <<- ggpaintr::ptr_server(
      'ggplot(ppUpload(shared="ds"), aes(x = ppVar(shared="col"))) + geom_point()',
      "p1", envir = globalenv(),
      shared_state = ss,
      panel_sources = list(shared_ds = panel_rv)
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_identical(
      shiny::isolate(captured$resolved_data$ggplot()),
      iris
    )
  })
})


# ---- Scenario 3: panel reactive updating to NULL clears the slot --------

test_that("panel source reactive updating to NULL clears the per-instance slot", {
  panel_rv <- shiny::reactiveVal(mtcars)
  captured <- NULL
  server <- function(input, output, session) {
    ss <- ggpaintr:::new_ptr_shared_state(
      shared = list(col = shiny::reactiveVal("mpg")),
      draw_trigger = shiny::reactiveVal(1L),
      shared_resolutions = list(),
      shared_stage_enabled = list(),
      panel_sources = list(shared_ds = panel_rv)
    )
    captured <<- ggpaintr::ptr_server(
      'ppUpload(shared="ds") |> head(5) |> ggplot(aes(x = ppVar(shared="col"))) + geom_point()',
      "p1", envir = globalenv(),
      shared_state = ss,
      panel_sources = list(shared_ds = panel_rv)
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_identical(
      shiny::isolate(captured$resolved_sources$shared_ds()),
      mtcars
    )
    # Clear the panel reactive -> per-instance slot clears on next flush.
    panel_rv(NULL)
    session$flushReact()
    expect_null(shiny::isolate(captured$resolved_sources$shared_ds()))
  })
})


# ---- Scenario 4 (ADR #3): two instances both bind off the panel df ------

test_that("two instances sharing one panel_sources entry both see the same frame", {
  # ADR worked example #3 (positive): Mixed scope -- one panel-owned
  # source, formula-local consumer pickers; both per-instance pipelines
  # bind off the SAME panel reactive's resolved df. Asserts value
  # equality at both instances (not "reactive defined").
  panel_rv <- shiny::reactiveVal(iris)
  ms <- new.env(parent = emptyenv())
  server <- function(input, output, session) {
    ss <- ggpaintr:::new_ptr_shared_state(
      shared = list(col = shiny::reactiveVal("Sepal.Length")),
      draw_trigger = shiny::reactiveVal(1L),
      shared_resolutions = list(),
      shared_stage_enabled = list(),
      panel_sources = list(shared_ds = panel_rv)
    )
    ms$p1 <- ggpaintr::ptr_server(
      'ppUpload(shared="ds") |> head(10) |> ggplot(aes(x = ppVar(shared="col"))) + geom_point()',
      "p1", envir = globalenv(),
      shared_state = ss,
      panel_sources = list(shared_ds = panel_rv)
    )
    ms$p2 <- ggpaintr::ptr_server(
      'ppUpload(shared="ds") |> head(10) |> ggplot(aes(x = ppVar(shared="col"))) + geom_col()',
      "p2", envir = globalenv(),
      shared_state = ss,
      panel_sources = list(shared_ds = panel_rv)
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    # Two distinct ptr_server instances exist.
    expect_false(identical(ms$p1, ms$p2))
    # Both see the panel-resolved df, value-equal to iris (not just "a reactive").
    v1 <- shiny::isolate(ms$p1$resolved_sources$shared_ds())
    v2 <- shiny::isolate(ms$p2$resolved_sources$shared_ds())
    expect_identical(v1, iris)
    expect_identical(v2, iris)
    expect_identical(v1, v2)
    # Changing the panel reactive propagates to BOTH instances on the next flush.
    panel_rv(mtcars)
    session$flushReact()
    expect_identical(
      shiny::isolate(ms$p1$resolved_sources$shared_ds()),
      mtcars
    )
    expect_identical(
      shiny::isolate(ms$p2$resolved_sources$shared_ds()),
      mtcars
    )
  })
})


# ---- Scenario 5 (ADR #5): pipeline-head ppUpload chained through stages -

test_that("ADR #5 -- pipeline-head ppUpload chained through dplyr::filter binds under companion name in eval_env", {
  # ADR worked example #5 (positive): the panel-owned ppUpload sits at
  # the head of a pipeline whose intermediate stage is `dplyr::filter`.
  # `substitute_walk.ptr_ph_data_source` will produce a *symbol* (the
  # companion's text value, e.g. `mtcars`) in the generated code, and
  # the chain `mtcars |> dplyr::filter(...)` evaluates against that
  # symbol's binding in `state$eval_env`. PLAN-05's substitution branch
  # must therefore: (a) write the panel-resolved df into the per-
  # instance `state$resolved_sources[[sid]]` slot (so the per-consumer
  # picker reactives invalidate), AND (b) bind the df under the
  # companion's name in `state$eval_env` so the intermediate stage
  # finds it.
  skip_if_not_installed("dplyr")
  panel_rv <- shiny::reactiveVal(mtcars)
  captured <- NULL
  server <- function(input, output, session) {
    ss <- ggpaintr:::new_ptr_shared_state(
      shared = list(col = shiny::reactiveVal("mpg")),
      draw_trigger = shiny::reactiveVal(1L),
      shared_resolutions = list(),
      shared_stage_enabled = list(),
      panel_sources = list(shared_ds = panel_rv)
    )
    captured <<- ggpaintr::ptr_server(
      'ppUpload(shared="ds") |> dplyr::filter(cyl > 4) |> ggplot(aes(x = ppVar(shared="col"))) + geom_histogram()',
      "p1", envir = globalenv(),
      shared_state = ss,
      panel_sources = list(shared_ds = panel_rv)
    )
  }
  shiny::testServer(server, {
    # Set the companion text input at TOP LEVEL (panel-owned widgets live
    # at top-level namespace, not under the per-instance prefix). The
    # PLAN-05 branch reads `session$rootScope()$input[[node$companion_id]]`,
    # i.e. `input$shared_ds_name`, to derive the binding name.
    session$setInputs(shared_ds_name = "mtcars")
    session$flushReact()
    # (a) panel-resolved df is mirrored into the per-instance slot.
    expect_identical(
      shiny::isolate(captured$resolved_sources$shared_ds()),
      mtcars
    )
    # (b) `state$bound_names` carries the companion name; `eval_env`
    # holds the same panel-resolved df under that symbol, so the
    # intermediate `mtcars |> dplyr::filter(cyl > 4)` stage evaluates
    # against the panel-resolved frame. The final dplyr-piped value
    # therefore matches `dplyr::filter(mtcars, cyl > 4)`.
    expect_identical(
      shiny::isolate(captured$bound_names$shared_ds()),
      "mtcars"
    )
    expect_true("mtcars" %in% ls(captured$eval_env))
    bound_df <- get("mtcars", envir = captured$eval_env)
    expect_identical(bound_df, mtcars)
    expected <- dplyr::filter(mtcars, cyl > 4)
    actual <- dplyr::filter(bound_df, cyl > 4)
    expect_identical(actual, expected)
  })
})


# ---- Scenario 6: panel-owned branch does NOT also wire the input observer ----

test_that("panel-owned source does NOT also watch the input slot (no double-write)", {
  # ADR 0023 §4 note: the substitution branch must NOT also wire the
  # input-watching observer for panel-owned ids -- "one widget, one
  # owner". Writing the (namespaced) per-instance input slot to a
  # non-NULL value MUST NOT cause the per-instance slot to be rebound
  # (the panel reactive remains the single source of truth).
  panel_rv <- shiny::reactiveVal(mtcars)
  captured <- NULL
  server <- function(input, output, session) {
    ss <- ggpaintr:::new_ptr_shared_state(
      shared = list(col = shiny::reactiveVal("mpg")),
      draw_trigger = shiny::reactiveVal(1L),
      shared_resolutions = list(),
      shared_stage_enabled = list(),
      panel_sources = list(shared_ds = panel_rv)
    )
    captured <<- ggpaintr::ptr_server(
      'ppUpload(shared="ds") |> head(5) |> ggplot(aes(x = ppVar(shared="col"))) + geom_point()',
      "p1", envir = globalenv(),
      shared_state = ss,
      panel_sources = list(shared_ds = panel_rv)
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_identical(
      shiny::isolate(captured$resolved_sources$shared_ds()),
      mtcars
    )
    # Write something into the per-instance (module-namespaced) input
    # slot for `shared_ds`. With the substitution branch in place there
    # is no observer reading this slot, so the panel-resolved value
    # MUST be unchanged after a flush.
    session$setInputs(`p1-shared_ds` = list(datapath = "no-such-file", name = "x.csv"))
    session$flushReact()
    expect_identical(
      shiny::isolate(captured$resolved_sources$shared_ds()),
      mtcars
    )
  })
})
