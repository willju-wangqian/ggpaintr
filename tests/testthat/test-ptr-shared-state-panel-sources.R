# ADR 0023 / PLAN-01: ptr_shared_state carries a panel_sources payload.
#
# Covers the bundle-shape contract (constructor / validator / printer) plus
# the per-instance propagation via the existing `do.call(ptr_init_state, ...)`
# channel in `ptr_server_internal()`. Sibling plans (04/05/07) build on this
# slot; this file pins the value-type behavior.

test_that("constructor stores supplied panel_sources reactives under canonical ids", {
  r1 <- shiny::reactiveVal(mtcars)
  state <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list(),
    panel_sources = list(shared_ds = r1)
  )
  expect_equal(names(state$panel_sources), c("shared_ds"))
  expect_identical(state$panel_sources$shared_ds, r1)
  expect_s3_class(state, "ptr_shared_state")
})

test_that("default constructor argument yields an empty panel_sources slot", {
  state <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list()
  )
  expect_identical(state$panel_sources, list())
})

test_that("validator rejects non-reactive values in panel_sources", {
  bad <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list()
  )
  # bypass the constructor's argument typing -- inject a non-reactive
  bad$panel_sources <- list(shared_ds = mtcars)
  expect_error(validate_ptr_shared_state(bad), "must be Shiny reactives")
})

test_that("validator rejects non-list panel_sources", {
  bad <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list()
  )
  bad$panel_sources <- "not-a-list"
  expect_error(
    validate_ptr_shared_state(bad),
    "must be a \\(possibly empty\\) named list of reactives"
  )
})

test_that("validator rejects duplicate / empty names in panel_sources", {
  r1 <- shiny::reactiveVal(mtcars)
  r2 <- shiny::reactiveVal(iris)
  bad <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list()
  )
  bad$panel_sources <- list(shared_ds = r1, shared_ds = r2)
  expect_error(validate_ptr_shared_state(bad), "unique non-empty names")
})

test_that("print method names the panel source keys", {
  r1 <- shiny::reactiveVal(mtcars)
  state <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list(),
    panel_sources = list(shared_ds = r1)
  )
  txt <- paste(utils::capture.output(print(state)), collapse = "\n")
  expect_true(grepl("panel source keys", txt, fixed = TRUE))
  expect_true(grepl("shared_ds", txt, fixed = TRUE))
})

test_that("print method emits <none> when panel_sources is empty", {
  state <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list()
  )
  txt <- paste(utils::capture.output(print(state)), collapse = "\n")
  expect_true(grepl("panel source keys", txt, fixed = TRUE))
  expect_true(grepl("<none>", txt, fixed = TRUE))
})

test_that("ADR worked example R4 (escape-hatch) -- embedder-supplied reactive validates", {
  # R4 at the bundle level: an embedder may construct shared_state manually
  # (the escape-hatch noted in ADR 0023 worked example #R4) and pass
  # `panel_sources = list(shared_ds = <reactive>)`; the bundle must accept
  # this without raising. This is the validation-accepts-it half of the R4
  # contract; sibling plans cover the downstream wiring.
  my_df <- shiny::reactiveVal(mtcars)
  state <- list(
    shared = list(),
    draw_trigger = NULL,
    shared_resolutions = list(),
    shared_stage_enabled = list(),
    panel_sources = list(shared_ds = my_df)
  )
  class(state) <- c("ptr_shared_state", "list")
  expect_silent(validate_ptr_shared_state(state))
  expect_identical(validate_ptr_shared_state(state)$panel_sources$shared_ds, my_df)
})

test_that("ptr_init_state() accepts panel_sources arg and stores it on state", {
  r1 <- shiny::reactiveVal(mtcars)
  state <- ptr_init_state(
    "ggplot(mtcars, aes(mpg, hp)) + geom_point()",
    envir = globalenv(),
    panel_sources = list(shared_ds = r1)
  )
  expect_identical(state$panel_sources, list(shared_ds = r1))
  # Smoke-check that no other observable shape changed: a few canonical
  # fields are still present and the class chain is intact.
  expect_s3_class(state, "ptr_state")
  expect_true(is.environment(state$eval_env))
  expect_true(shiny::is.reactive(state$tree))
})

test_that("bundle panel_sources propagates to state via existing do.call channel", {
  # Scenario 8: shared_state -> dots -> ptr_init_state via the `do.call`
  # site in `ptr_server_internal()`. Exercising the projection block this
  # plan added: `if (is.null(dots$panel_sources)) dots$panel_sources <-
  # shared_state$panel_sources`. The do.call call site is unchanged --
  # `panel_sources` rides the existing `dots` list.
  r1 <- shiny::reactiveVal(mtcars)
  bundle <- new_ptr_shared_state(
    shared = list(), draw_trigger = NULL,
    shared_resolutions = list(),
    panel_sources = list(shared_ds = r1)
  )
  validate_ptr_shared_state(bundle)
  captured_state <- NULL
  server <- function(input, output, session) {
    captured_state <<- ggpaintr:::ptr_server_internal(
      input, output, session,
      formula = "ggplot(mtcars, aes(mpg, hp)) + geom_point()",
      envir = globalenv(),
      shared_state = bundle
    )
  }
  shiny::testServer(server, {})
  expect_identical(captured_state$panel_sources$shared_ds, r1)
  expect_identical(
    captured_state$panel_sources$shared_ds,
    bundle$panel_sources$shared_ds
  )
})
