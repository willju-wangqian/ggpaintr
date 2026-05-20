# Contract tests for `ptr_server(..., shared_state = ...)`. Covers
# the convenience path plus the warn pre-flight check. Post Step 02
# (#P2): with a `ptr_shared_state` supplied, a formula key missing from the
# bundle is formula-local BY CONSTRUCTION (the coordinator never omits a
# panel key) -- the module self-binds it silently. The "not in the shared
# list" heads-up now fires ONLY on the raw escape-hatch path (a partial
# `shared =` passed without a `ptr_shared_state`).
#
# Single-instance fix: bare `ptr_server()` on a `shared = "..."` formula
# with no coordinator wiring self-binds every declared key under its own
# namespace (matches `ptr_app()`'s `auto_bind_shared = TRUE` path). The
# coordinator trio is only needed for cross-formula coordination.

test_that("M-SHR.1 bare ptr_server() self-binds formula-local shared keys", {
  # No shared_state, no shared = ..., one formula with one shared key
  # reused across slots. Per the single-instance contract this must
  # construct silently and bind the key under the module's own namespace.
  expect_silent({
    server <- function(input, output, session) {
      ptr_server(
        'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg - var(shared = "col"))) + geom_point()',
        "plot_1"
      )
    }
    shiny::testServer(server, {})
  })
})

test_that("M-SHR.2 escape hatch: passing shared= via ... bypasses the abort", {
  obj <- ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  ))
  expect_silent({
    server <- function(input, output, session) {
      state <- ptr_shared_server(obj, envir = globalenv())
      ptr_server(
        'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
        "plot_1",
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
        ptr_server(
          'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
          "plot_1",
          shared_state = list(shared = list(), draw_trigger = NULL)
        )
      }
      shiny::testServer(server, {})
    },
    "ptr_shared_state"
  )
})

test_that("M-SHR.4 shared_state path is silent for a formula-local key", {
  # f1 declares cross-formula "col" AND formula-local "a"; f2 only "col".
  # The coordinator's bundle covers "col" only -- "a" is formula-local by
  # construction, so the module self-binds it WITHOUT a warning.
  f1 <- 'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point(alpha = num(shared = "a"))'
  f2 <- 'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  obj <- ptr_shared(c(f1, f2))
  expect_silent({
    server <- function(input, output, session) {
      state <- ptr_shared_server(obj, envir = globalenv())
      ptr_server(f1, "plot_1", envir = globalenv(), shared_state = state)
    }
    shiny::testServer(server, {})
  })
})

test_that("M-SHR.4b raw escape-hatch partial shared= warns (reworded)", {
  # No `ptr_shared_state`: caller hand-passes a partial `shared =`. We
  # cannot tell a deliberately-local key from a forgotten one, so a
  # reworded heads-up fires and the key is bound locally.
  f1 <- 'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point(alpha = num(shared = "a"))'
  expect_warning(
    {
      server <- function(input, output, session) {
        ptr_server(
          f1, "plot_1",
          envir = globalenv(),
          shared = list(col = shiny::reactive(NULL))
        )
      }
      shiny::testServer(server, {})
    },
    "absent from the supplied"
  )
})

test_that("M-SHR.5 shared_state convenience path delegates to ptr_server_internal cleanly", {
  obj <- ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  ))
  # No abort, no warning -- happy-path wiring.
  expect_silent({
    server <- function(input, output, session) {
      state <- ptr_shared_server(obj, envir = globalenv())
      ptr_server(
        'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
        "plot_1",
        envir = globalenv(),
        shared_state = state
      )
    }
    shiny::testServer(server, {})
  })
})
