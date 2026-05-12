# Phase 2 — render/error feedback regressions.
#   2.1 a failed render blanks the plot canvas (no stale plot lingers)
#   2.2 runtime error messages carry the legacy stage prefix

.feedback_env <- function() list2env(list(mtcars = mtcars), parent = globalenv())

# Set an input whose id is only known at runtime (server ns = NULL here, so
# the raw placeholder id is the input name).
.set_named_input <- function(session, id, value) {
  do.call(session$setInputs, stats::setNames(list(value), id))
}

.expr_input_id <- function(state) {
  spec <- state$input_spec
  spec$input_id[spec$keyword == "expr"][[1]]
}

test_that("2.1 a failed render blanks the plot canvas", {
  e <- .feedback_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + facet_wrap(expr)",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    expr_id <- .expr_input_id(state)

    blanked <- new.env(parent = emptyenv())
    blanked$n <- 0L
    testthat::local_mocked_bindings(
      plot.new = function(...) blanked$n <- blanked$n + 1L,
      .package = "graphics"
    )

    .set_named_input(session, expr_id, "1 +")  # parse error -> render fails
    session$setInputs(ptr_update_plot = 1L)
    session$flushReact()
    res <- state$runtime()
    expect_false(isTRUE(res$ok))
    try(output$ptr_plot, silent = TRUE)  # force the renderPlot to run
    expect_gt(blanked$n, 0L)
  })
})

test_that("2.2 substitute-stage errors carry 'Input error: ' (not double-applied)", {
  e <- .feedback_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + facet_wrap(expr)",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    expr_id <- .expr_input_id(state)

    .set_named_input(session, expr_id, "a; b")  # two expressions
    session$setInputs(ptr_update_plot = 1L)
    session$flushReact()
    res <- state$runtime()
    expect_false(isTRUE(res$ok))
    expect_equal(res$stage, "complete")
    expect_true(startsWith(res$error, "Input error: "))
    expect_false(startsWith(res$error, "Input error: Input error: "))
    expect_equal(ptr_extract_error(state), res$error)
  })
})

test_that("2.2 a plot/eval-stage failure carries the 'Plot error: ' prefix", {
  e <- .feedback_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg)) + geom_point(aes(y = no_such_column))",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(ptr_update_plot = 1L)
    session$flushReact()
    res <- state$runtime()
    expect_false(isTRUE(res$ok))
    expect_equal(res$stage, "plot")
    expect_true(startsWith(res$error, "Plot error: "))
  })
})

test_that("ptr_error_ui returns NULL for blank input; ptr_extract_error NULL when ok", {
  expect_null(ptr_error_ui(""))
  expect_null(ptr_error_ui("   "))
  expect_null(ptr_error_ui(NULL))
  e <- .feedback_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(ptr_update_plot = 1L)
    session$flushReact()
    expect_null(ptr_extract_error(state))
  })
})

test_that("ptr_format_runtime_message stamps + is idempotent", {
  expect_equal(ptr_format_runtime_message("complete", "boom"), "Input error: boom")
  expect_equal(ptr_format_runtime_message("plot", "boom"), "Plot error: boom")
  expect_equal(
    ptr_format_runtime_message("complete", "Input error: boom"),
    "Input error: boom"
  )
  expect_null(ptr_format_runtime_message("complete", NULL))
})
