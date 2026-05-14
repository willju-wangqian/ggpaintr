# Server-side tests for `ptr_shared_server()`. Uses shiny::testServer()
# to install a reactive domain so the function can pull session/input/output
# from `getDefaultReactiveDomain()`.

test_that("S-SRV.1 errors when no formula declares a shared key", {
  expect_error(
    {
      server <- function(input, output, session) {
        ptr_shared_server("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
      }
      shiny::testServer(server, {})
    },
    "declare no `shared"
  )
})

test_that("S-SRV.2 aborts when called outside a reactive domain", {
  expect_error(
    ptr_shared_server(
      'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()'
    ),
    "no default reactive domain"
  )
})

test_that("S-SRV.3 returns a ptr_shared_state with the expected shape", {
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  )
  server <- function(input, output, session) {
    state <<- ptr_shared_server(formulas, envir = globalenv())
  }
  state <- NULL
  shiny::testServer(server, {
    expect_s3_class(state, "ptr_shared_state")
    expect_true(is.list(state$shared))
    expect_true("col" %in% names(state$shared))
    expect_true(shiny::is.reactive(state$shared$col))
    expect_true(shiny::is.reactive(state$draw_trigger))
    expect_true(is.list(state$shared_resolutions))
    expect_true("col" %in% names(state$shared_resolutions))
  })
})

test_that("S-SRV.4 single-formula path produces draw_trigger = NULL", {
  formulas <-
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(formulas, envir = globalenv())
  }
  shiny::testServer(server, {
    expect_null(state$draw_trigger)
  })
})

test_that("S-SRV.5 shared override replaces auto reactive for that key", {
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "a"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "b"), y = hp)) + geom_line()'
  )
  state <- NULL
  server <- function(input, output, session) {
    my_rv <- shiny::reactive("custom-value")
    state <<- ptr_shared_server(
      formulas, envir = globalenv(),
      shared = list(a = my_rv)
    )
  }
  shiny::testServer(server, {
    expect_equal(shiny::isolate(state$shared$a()), "custom-value")
    # auto reactive for `b` reads input[[shared_b]] -- still a reactive
    expect_true(shiny::is.reactive(state$shared$b))
  })
})

test_that("S-SRV.6 draw_trigger override wins over the auto button reactive", {
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "k"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "k"), y = hp)) + geom_line()'
  )
  state <- NULL
  server <- function(input, output, session) {
    my_btn <- shiny::reactive(42L)
    state <<- ptr_shared_server(
      formulas, envir = globalenv(),
      draw_trigger = my_btn
    )
  }
  shiny::testServer(server, {
    expect_equal(shiny::isolate(state$draw_trigger()), 42L)
  })
})

test_that("S-SRV.7 override referencing an unknown key aborts", {
  formulas <-
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  expect_error(
    {
      server <- function(input, output, session) {
        my_rv <- shiny::reactive(1)
        ptr_shared_server(
          formulas, envir = globalenv(),
          shared = list(nope = my_rv)
        )
      }
      shiny::testServer(server, {})
    },
    "not used in any plot formula"
  )
})

test_that("S-SRV.8 non-reactive override is rejected", {
  formulas <-
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()'
  expect_error(
    {
      server <- function(input, output, session) {
        ptr_shared_server(
          formulas, envir = globalenv(),
          shared = list(col = "not a reactive")
        )
      }
      shiny::testServer(server, {})
    },
    "must be Shiny reactives"
  )
})

test_that("S-SRV.9 print.ptr_shared_state surfaces keys", {
  formulas <- c(
    'ggplot(mtcars, aes(x = var(shared = "col"), y = mpg)) + geom_point()',
    'ggplot(mtcars, aes(x = var(shared = "col"), y = hp)) + geom_line()'
  )
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(formulas, envir = globalenv())
  }
  shiny::testServer(server, {
    out <- utils::capture.output(print(state))
    expect_true(any(grepl("ptr_shared_state", out)))
    expect_true(any(grepl("col", out)))
  })
})
