# Tests for `ptr_server_state_v2`, `ptr_server_v2` observers, and the
# `ptr_extract_*_v2` accessors. Reactivity tests use `shiny::testServer`.

.server_test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- Pure state shape ----

test_that("ptr_server_state_v2 returns a typed-state structure", {
  state <- ptr_server_state_v2(
    "ggplot(mtcars) + geom_point()",
    envir = .server_test_env()
  )
  expect_s3_class(state, "ptr_state_v2")
  expect_s3_class(state, "ptr_state")
  expect_true(is.function(state$tree))  # reactiveVal is a function
})

test_that("ptr_server_state_v2 resolves checkbox_defaults", {
  state <- ptr_server_state_v2(
    "ggplot(mtcars) + geom_point() + geom_smooth()",
    checkbox_defaults = list(geom_smooth = FALSE),
    envir = .server_test_env()
  )
  expect_equal(state$checkbox_defaults[["geom_point"]], TRUE)
  expect_equal(state$checkbox_defaults[["geom_smooth"]], FALSE)
})

test_that("ptr_server_state_v2 builds resolved_data slots only for pipeline layers", {
  state <- ptr_server_state_v2(
    "mtcars |> head(num) |> ggplot() + geom_point()",
    envir = .server_test_env()
  )
  expect_named(state$resolved_data, "ggplot")
  # geom_point has no pipeline → no resolved_data slot
})

test_that("ptr_server_state_v2 with no pipeline layers has empty resolved_data", {
  state <- ptr_server_state_v2(
    "ggplot(mtcars) + geom_point()",
    envir = .server_test_env()
  )
  expect_equal(length(state$resolved_data), 0L)
})

test_that("ptr_server_state_v2 rejects non-function ns", {
  expect_error(
    ptr_server_state_v2("ggplot(mtcars)", ns = "module1"),
    "namespace function"
  )
})

# ---- testServer integration: pipeline observers ----

test_that("P12.1 / G6.1 — initial cache seeded via trim-to-root", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_v2(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    cached <- state$resolved_data[["ggplot"]]()
    expect_s3_class(cached, "data.frame")
    # Per G6.1 trim-to-root: missing num drops the entire head() stage
    # (positional missing escalates), leaving `mtcars` itself.
    expect_equal(nrow(cached), nrow(mtcars))
  })
})

test_that("P12.2 — Update Data click refreshes cache from current inputs", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_v2(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = mpg, y = hp))",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    tree <- shiny::isolate(state$tree())
    num_id <- find_nodes(tree, is_ptr_ph_value)[[1L]]$id

    args <- list()
    args[[num_id]] <- 3L
    args[["ggplot_update_data"]] <- 1L
    do.call(session$setInputs, args)

    cached <- state$resolved_data[["ggplot"]]()
    expect_equal(nrow(cached), 3L)
  })
})

test_that("P12.3 / P12.4 — stale flag flips on input divergence and back on click", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_v2(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = mpg, y = hp))",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    tree <- shiny::isolate(state$tree())
    num_id <- find_nodes(tree, is_ptr_ph_value)[[1L]]$id

    expect_false(state$is_stale_env[["ggplot"]]())

    do.call(session$setInputs, stats::setNames(list(7L), num_id))
    expect_true(state$is_stale_env[["ggplot"]]())

    args <- list()
    args[[num_id]] <- 7L
    args[["ggplot_update_data"]] <- 1L
    do.call(session$setInputs, args)
    expect_false(state$is_stale_env[["ggplot"]]())
  })
})

test_that("P12.7 — unresolvable pipeline leaves cache untouched on click", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_v2(
      input, output, session,
      "nonexistent_xyz |> head(num) |> ggplot()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    expect_null(state$resolved_data[["ggplot"]]())
    args <- list()
    tree <- shiny::isolate(state$tree())
    num_id <- find_nodes(tree, is_ptr_ph_value)[[1L]]$id
    args[[num_id]] <- 3L
    args[["ggplot_update_data"]] <- 1L
    do.call(session$setInputs, args)
    expect_null(state$resolved_data[["ggplot"]]())
  })
})

# ---- runtime ----

test_that("runtime observer populates state$runtime with code_text and plot", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_v2(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(.dummy = 1)  # force a flush so observe() runs
    res <- state$runtime()
    expect_true(isTRUE(res$ok), info = res$error %||% "ok")
    expect_match(res$code_text, "ggplot")
    expect_s3_class(res$plot, "ggplot")
  })
})

# ---- ptr_gg_extra_v2 ----

test_that("P12.10 ptr_gg_extra_v2 captures extras", {
  e <- .server_test_env()
  state <- ptr_server_state_v2("ggplot(mtcars) + geom_point()", envir = e)
  shiny::isolate({
    ptr_gg_extra_v2(state, list(quote(ggplot2::scale_x_log10())))
    expect_equal(length(state$extras()), 1L)
  })
})

test_that("P12.11 ptr_gg_extra_v2 no-op on empty list", {
  e <- .server_test_env()
  state <- ptr_server_state_v2("ggplot(mtcars) + geom_point()", envir = e)
  shiny::isolate({
    ptr_gg_extra_v2(state, list())
    expect_equal(length(state$extras()), 0L)
  })
})

test_that("P12.12 ptr_gg_extra_v2 leaves extras untouched on eval failure", {
  e <- .server_test_env()
  state <- ptr_server_state_v2("ggplot(mtcars) + geom_point()", envir = e)
  shiny::isolate({
    expect_error(
      ptr_gg_extra_v2(state, list(quote(does_not_exist_xyz()))),
      "does_not_exist_xyz"
    )
    expect_equal(length(state$extras()), 0L)
  })
})

# ---- extractors ----

test_that("ptr_extract_*_v2 surface the runtime fields", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_v2(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(.dummy = 1)
    expect_s3_class(ptr_extract_plot_v2(state), "ggplot")
    expect_match(ptr_extract_code_v2(state), "geom_point")
    expect_null(ptr_extract_error_v2(state))
  })
})
