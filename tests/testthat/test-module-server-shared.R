# Contract tests for `ptr_module_server(..., shared_state = ...)`. Covers
# the new convenience path plus the abort/warn pre-flight checks the plan
# specifies (dev/plans/shared-ui-multi-instance.html, "Success criteria").

test_that("M-SHR.1 aborts when formula has shared key but shared_state is NULL", {
  expect_error(
    {
      server <- function(input, output, session) {
        ptr_module_server(
          "plot_1",
          'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
        )
      }
      shiny::testServer(server, {})
    },
    "shared_state = NULL"
  )
})

test_that("M-SHR.2 escape hatch: passing shared= via ... bypasses the abort", {
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  )
  expect_silent({
    server <- function(input, output, session) {
      state <- ptr_shared_server(formulas, envir = globalenv())
      ptr_module_server(
        "plot_1", formulas[[1]],
        envir = globalenv(),
        shared = state$shared,
        shared_resolutions = state$shared_resolutions,
        draw_trigger = state$draw_trigger
      )
    }
    shiny::testServer(server, {})
  })
})

test_that("M-SHR.3 shared_state non-ptr_shared_state value aborts", {
  expect_error(
    {
      server <- function(input, output, session) {
        ptr_module_server(
          "plot_1",
          'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
          shared_state = list(shared = list(), draw_trigger = NULL)
        )
      }
      shiny::testServer(server, {})
    },
    "ptr_shared_state"
  )
})

test_that("M-SHR.4 warns when a formula key is missing from shared_state$shared", {
  # Plot 1 declares `var(shared = "col")` AND `num(shared = "a")`; the
  # shared state covers only "col". `ptr_module_server()` must surface the
  # gap with a cli warning, then keep going.
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point(alpha = num(shared = "a"))'
  )
  expect_warning(
    {
      server <- function(input, output, session) {
        # Build a state that omits "a" by sharing a custom one-key reactive
        # list. `ptr_shared_server()` itself wouldn't omit, so we build
        # the state by hand to exercise the gap detection.
        partial <- structure(
          list(
            shared = list(col = shiny::reactive(NULL)),
            draw_trigger = NULL,
            shared_resolutions = list()
          ),
          class = c("ptr_shared_state", "list")
        )
        ptr_module_server(
          "plot_1", formulas[[1]],
          envir = globalenv(),
          shared_state = partial
        )
      }
      shiny::testServer(server, {})
    },
    "not covered"
  )
})

test_that("M-SHR.5 shared_state convenience path delegates to ptr_server cleanly", {
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  )
  # No abort, no warning -- happy-path wiring.
  expect_silent({
    server <- function(input, output, session) {
      state <- ptr_shared_server(formulas, envir = globalenv())
      ptr_module_server(
        "plot_1", formulas[[1]],
        envir = globalenv(),
        shared_state = state
      )
    }
    shiny::testServer(server, {})
  })
})
