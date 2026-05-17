# Server-side tests for `ptr_shared_server()`. Post Step 01/02 it takes a
# `ptr_shared_spec` from `ptr_shared()` (not raw formulas) and owns ONLY
# the cross-formula (panel) keys -- formula-local keys are bound by each
# module. `shiny::testServer()` installs the reactive domain the function
# needs.

test_that("S-SRV.1 ptr_shared() errors when no formula declares a shared key", {
  expect_error(
    ptr_shared("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"),
    "declare no `shared"
  )
})

test_that("S-SRV.2 aborts when called outside a reactive domain", {
  obj <- ptr_shared(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()'
  )
  expect_error(
    ptr_shared_server(obj),
    "no default reactive domain"
  )
})

test_that("S-SRV.2b aborts when not given a ptr_shared_spec", {
  expect_error(
    {
      server <- function(input, output, session) {
        ptr_shared_server(
          'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()'
        )
      }
      shiny::testServer(server, {})
    },
    "ptr_shared_spec"
  )
})

test_that("S-SRV.3 returns a ptr_shared_state with the expected shape", {
  obj <- ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  ))
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  state <- NULL
  shiny::testServer(server, {
    expect_s3_class(state, "ptr_shared_state")
    expect_true(is.list(state$shared))
    # "col" is cross-formula -> panel-owned, in the bundle.
    expect_true("col" %in% names(state$shared))
    expect_true(shiny::is.reactive(state$shared$col))
    expect_true(shiny::is.reactive(state$draw_trigger))
    expect_true(is.list(state$shared_resolutions))
    expect_true("col" %in% names(state$shared_resolutions))
  })
})

test_that("S-SRV.4 single-formula path produces draw_trigger = NULL", {
  obj <- ptr_shared(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  )
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    expect_null(state$draw_trigger)
  })
})

test_that("S-SRV.5 shared override seeds a panel key until the picker is set", {
  # Override only applies to panel (cross-formula) keys -- those are the
  # ones `ptr_shared_server()` owns. Both formulas use "k" and "m" so both
  # are panel keys; the override for "k" wins while its input is unset.
  obj <- ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = var(shared = "m"))) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "k"), y = var(shared = "m"))) + geom_line()'
  ))
  state <- NULL
  server <- function(input, output, session) {
    my_rv <- shiny::reactive("custom-value")
    state <<- ptr_shared_server(
      obj, envir = globalenv(),
      shared = list(k = my_rv)
    )
  }
  shiny::testServer(server, {
    expect_equal(shiny::isolate(state$shared$k()), "custom-value")
    # auto reactive for the non-overridden panel key "m" still reads input.
    expect_true(shiny::is.reactive(state$shared$m))
  })
})

test_that("S-SRV.6 draw_trigger override wins over the auto button reactive", {
  obj <- ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "k"), y = hp)) + geom_line()'
  ))
  state <- NULL
  server <- function(input, output, session) {
    my_btn <- shiny::reactive(42L)
    state <<- ptr_shared_server(
      obj, envir = globalenv(),
      draw_trigger = my_btn
    )
  }
  shiny::testServer(server, {
    expect_equal(shiny::isolate(state$draw_trigger()), 42L)
  })
})

test_that("S-SRV.7 override referencing an unknown key aborts", {
  obj <- ptr_shared(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  )
  expect_error(
    {
      server <- function(input, output, session) {
        my_rv <- shiny::reactive(1)
        ptr_shared_server(obj, envir = globalenv(), shared = list(nope = my_rv))
      }
      shiny::testServer(server, {})
    },
    "not used in any plot formula"
  )
})

test_that("S-SRV.8 non-reactive override is rejected", {
  obj <- ptr_shared(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  )
  expect_error(
    {
      server <- function(input, output, session) {
        ptr_shared_server(obj, envir = globalenv(), shared = list(col = "x"))
      }
      shiny::testServer(server, {})
    },
    "must be Shiny reactives"
  )
})

test_that("S-SRV.9 print.ptr_shared_state surfaces keys", {
  obj <- ptr_shared(c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  ))
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    out <- utils::capture.output(print(state))
    expect_true(any(grepl("ptr_shared_state", out)))
    expect_true(any(grepl("col", out)))
  })
})
