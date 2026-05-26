# ADR 0023 / PLAN-06 — partition-aware skip in ptr_setup_source_uis().
#
# Cut 1 of the shared-source end-to-end fix dropped a partition-blind
# `if (!is.null(s$shared)) next` skip, which had hidden formula-local
# shared sources too (Bug 2). Cut 2 reintroduces the skip in a
# partition-aware form: only ids the host owns
# (`state$panel_sources`, populated by `ptr_setup_panel_sources()`
# in Plan 04) are skipped per-instance; everything else falls
# through to the standard renderUI path.
#
# These tests pin three behaviors:
#
# 1. Panel-owned id (`shared_ds` in `state$panel_sources`) is NOT
#    assigned by the per-instance binder — `session$getOutput()`
#    returns NULL after a flush.
# 2. Formula-local shared id (NOT in `state$panel_sources`) IS
#    assigned per-instance (Cut-1 regression preserved).
# 3. Single-instance config (`shared_state = NULL`) yields
#    `state$panel_sources = list()`; the `%||% list()` fallback
#    makes the skip a no-op, the source UI is rendered per-instance.

.test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

test_that("PLAN-06 positive: panel-owned source id is skipped per-instance (output never assigned)", {
  # shared_state carries a panel_sources entry for `shared_ds`; that
  # marks the id as host-owned. The per-instance ptr_setup_source_uis
  # MUST skip it. Observable: `session$getOutput("<output_id>")` is
  # NULL after `flushReact()` -- no renderUI was registered.
  e <- .test_env()
  r1 <- shiny::reactiveVal(mtcars)
  bundle <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list(),
    panel_sources = list(shared_ds = r1)
  )
  validate_ptr_shared_state(bundle)

  server <- function(input, output, session) {
    session$userData$state <- ggpaintr:::ptr_server_internal(
      input, output, session,
      formula = "ggplot(mtcars, aes(mpg, hp)) + geom_point(data = ppUpload(shared='ds'))",
      envir = e,
      shared_state = bundle,
      auto_bind_shared = TRUE
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state

    # Confirm the bundle wiring landed: state$panel_sources includes
    # shared_ds (the precondition for the skip to fire).
    expect_true("shared_ds" %in% names(state$panel_sources))

    # The canonical source output id for `ppUpload(shared='ds')` is
    # `shared_ds_ui` (per source_output_id()). With the partition-aware
    # skip in place, the per-instance binder must NOT have assigned a
    # renderUI to that slot. shiny::MockShinySession$getOutput() errors
    # with "hasn't been defined yet" when the slot was never assigned --
    # that is the observable Then for the negative scenario.
    expect_error(
      session$getOutput("shared_ds_ui"),
      "hasn't been defined yet"
    )
  })
})

test_that("PLAN-06 negative R5: formula-local shared source (not in panel_sources) is rendered per-instance", {
  # The per-instance tree references `ppUpload(shared='ds_local')`, but
  # the panel_sources bundle carries a different id (`shared_other`).
  # `shared_ds_local` is therefore formula-local from the instance's
  # view -- it must NOT be skipped, and its source UI MUST be rendered
  # per-instance (asserted by a non-NULL getOutput() after flush).
  e <- .test_env()
  r1 <- shiny::reactiveVal(mtcars)
  bundle <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list(),
    panel_sources = list(shared_other = r1)
  )
  validate_ptr_shared_state(bundle)

  server <- function(input, output, session) {
    session$userData$state <- ggpaintr:::ptr_server_internal(
      input, output, session,
      formula = "ggplot(mtcars, aes(mpg, hp)) + geom_point(data = ppUpload(shared='ds_local'))",
      envir = e,
      shared_state = bundle,
      auto_bind_shared = TRUE
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state

    # Precondition: shared_ds_local is NOT in state$panel_sources --
    # it is formula-local from this instance's perspective.
    expect_false("shared_ds_local" %in% names(state$panel_sources))

    # The renderUI MUST have fired for the formula-local source -- the
    # per-instance binder is still the sole writer for it.
    out <- session$getOutput("shared_ds_local_ui")
    expect_false(is.null(out))
  })
})

test_that("PLAN-06 worked-example #4: single-instance (NULL bundle) renders shared source per-instance (Cut-1 preserved)", {
  # shared_state = NULL means no bundle was passed -- ptr_init_state's
  # default leaves state$panel_sources as list(). The %||% list()
  # fallback in the skip guard yields character() from names(), so the
  # skip never fires and the standard per-instance path runs unchanged.
  # Asserts the canonical Cut-1 fixture id (`shared_ds`) gets a
  # renderUI assigned at the per-instance binder.
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ggpaintr:::ptr_server_internal(
      input, output, session,
      formula = "ggplot(mtcars, aes(mpg, hp)) + geom_point(data = ppUpload(shared='ds'))",
      envir = e,
      auto_bind_shared = TRUE
      # shared_state intentionally omitted -- single-instance path.
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state

    # Precondition: single-instance => panel_sources is empty list.
    expect_identical(state$panel_sources, list())

    # And the per-instance renderUI fired for `shared_ds` -- the
    # Cut-1 fixture's invariant.
    out <- session$getOutput("shared_ds_ui")
    expect_false(is.null(out))
  })
})
