# Tests for `ptr_server_state`, `ptr_server` observers, and the
# `ptr_extract_*` accessors. Reactivity tests use `shiny::testServer`.

.server_test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- Pure state shape ----

test_that("ptr_server_state returns a typed-state structure", {
  state <- ptr_server_state(
    "ggplot(mtcars) + geom_point()",
    envir = .server_test_env()
  )
  expect_s3_class(state, "ptr_state")
  expect_s3_class(state, "ptr_state")
  expect_true(is.function(state$tree))  # reactiveVal is a function
})

test_that("ptr_server_state resolves checkbox_defaults", {
  state <- ptr_server_state(
    "ggplot(mtcars) + geom_point() + geom_smooth()",
    checkbox_defaults = list(geom_smooth = FALSE),
    envir = .server_test_env()
  )
  expect_equal(state$checkbox_defaults[["geom_point"]], TRUE)
  expect_equal(state$checkbox_defaults[["geom_smooth"]], FALSE)
})

test_that("ptr_server_state builds resolved_data slots only for pipeline layers", {
  state <- ptr_server_state(
    "mtcars |> head(num) |> ggplot() + geom_point()",
    envir = .server_test_env()
  )
  expect_named(state$resolved_data, "ggplot")
  # geom_point has no pipeline → no resolved_data slot
})

test_that("ptr_server_state with no pipeline layers has empty resolved_data", {
  state <- ptr_server_state(
    "ggplot(mtcars) + geom_point()",
    envir = .server_test_env()
  )
  expect_equal(length(state$resolved_data), 0L)
})

test_that("ptr_server_state rejects non-function ns", {
  expect_error(
    ptr_server_state("ggplot(mtcars)", ns = "module1"),
    "namespace function"
  )
})

# ---- testServer integration: pipeline observers ----

test_that("P12.1 — initial cache seeded via trim-to-root", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    cached <- state$resolved_data[["ggplot"]]()
    expect_s3_class(cached, "data.frame")
    # Per relaxed P9 (P12.1): missing num drops the arg, head() survives empty.
    # head(mtcars) defaults to n = 6.
    expect_equal(nrow(cached), 6L)
  })
})

test_that("P12.2 — Update Data click refreshes cache from current inputs", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
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
    session$userData$state <- ptr_server(
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
    session$userData$state <- ptr_server(
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
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(ptr_update_plot = 1L)  # gated on Update Plot click
    res <- state$runtime()
    expect_true(isTRUE(res$ok), info = res$error %||% "ok")
    expect_match(res$code_text, "ggplot")
    expect_s3_class(res$plot, "ggplot")
  })
})

# ---- ptr_gg_extra ----

test_that("P12.10 ptr_gg_extra captures extras", {
  e <- .server_test_env()
  state <- ptr_server_state("ggplot(mtcars) + geom_point()", envir = e)
  shiny::isolate({
    ptr_gg_extra(state, ggplot2::scale_x_log10())
    expect_equal(length(state$extras()), 1L)
  })
})

test_that("P12.11 ptr_gg_extra no-op when no args", {
  e <- .server_test_env()
  state <- ptr_server_state("ggplot(mtcars) + geom_point()", envir = e)
  shiny::isolate({
    ptr_gg_extra(state)
    expect_equal(length(state$extras()), 0L)
  })
})

test_that("P12.12 ptr_gg_extra leaves extras untouched on eval failure", {
  e <- .server_test_env()
  state <- ptr_server_state("ggplot(mtcars) + geom_point()", envir = e)
  shiny::isolate({
    expect_error(
      ptr_gg_extra(state, does_not_exist_xyz()),
      "does_not_exist_xyz"
    )
    expect_equal(length(state$extras()), 0L)
  })
})

# ---- extractors ----

test_that("ptr_extract_* surface the runtime fields", {
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(ptr_update_plot = 1L)  # gated on Update Plot click
    expect_s3_class(ptr_extract_plot(state), "ggplot")
    expect_match(ptr_extract_code(state), "geom_point")
    expect_null(ptr_extract_error(state))
  })
})

test_that("runtime is gated: no eval until Update Plot click (BDD G11.12)", {
  # Spec G11.12: plot is not rendered until the user clicks Update Plot.
  # Specifically, no expression evaluation happens at all on first launch
  # — `state$runtime()` stays NULL until the trigger fires, so the plot,
  # code, and error outputs all render blank.
  e <- .server_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    expect_null(state$runtime())

    # Picking vars without clicking Update Plot must still leave runtime NULL.
    session$setInputs(ggplot_1_1_var_NA = "mpg", ggplot_1_2_var_NA = "hp")
    session$flushReact()
    expect_null(state$runtime())

    # First click fires the runtime.
    session$setInputs(ptr_update_plot = 1L)
    session$flushReact()
    res <- state$runtime()
    expect_true(isTRUE(res$ok))
    expect_match(res$code_text, "aes\\(x = mpg, y = hp\\)")
  })
})

test_that("inject_resolved_data swaps data_arg for the cached frame (spec L105)", {
  # Pruned tree: ggplot(data = mtcars) layer + geom_point() layer. After
  # injection with a 3-row cache for the ggplot layer, that layer's
  # data_arg is a ptr_literal carrying the cached frame; geom_point has
  # no cache entry and is left intact.
  r <- ptr_translate("ggplot(data = mtcars, aes(x = mpg)) + geom_point()")
  r <- ptr_classify_data(r)
  r <- ptr_assign_ids(r)
  r <- ptr_shared_bind(r)
  pruned <- ptr_prune(ptr_substitute(r, input_snapshot = list()))

  fake_state <- list(
    resolved_data = list(ggplot = function() utils::head(mtcars, 3))
  )
  injected <- inject_resolved_data(pruned, fake_state)

  ggplot_layer <- injected$layers[[1L]]
  expect_s3_class(ggplot_layer$data_arg, "ptr_literal")
  expect_s3_class(ggplot_layer$data_arg$expr, "data.frame")
  expect_equal(nrow(ggplot_layer$data_arg$expr), 3L)

  # Layers without a cache entry are untouched.
  geom_layer <- injected$layers[[2L]]
  expect_null(geom_layer$data_arg)
})
