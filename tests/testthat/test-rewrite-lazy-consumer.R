# Tests for the lazy-consumer-resolve refactor (spec
# `lazy-consumer-resolve.md`). Covers:
#   - D6 build_ui contract: `(node, cols, data)`
#   - D2 per-consumer reactive cache: deps + cascade across upstream consumers
#   - D8 producer-debounce auto-flip via `record_eval_time`

.lc_test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- D6: build_ui receives (node, cols, data) ----

test_that("D6 ptr_define_placeholder_consumer accepts (node, cols, data)", {
  ptr_registry_clear()
  on.exit(suppressWarnings(ptr_register_builtins()), add = TRUE)
  expect_silent({
    ptr_define_placeholder_consumer(
      keyword = "lc1",
      build_ui = function(node, cols, data, ...) NULL,
      resolve_expr = function(value, node, ...) rlang::sym(value)
    )
  })
})

test_that("D6 build_ui without `data` arg and no `...` is rejected", {
  ptr_registry_clear()
  on.exit(suppressWarnings(ptr_register_builtins()), add = TRUE)
  expect_error(
    ptr_define_placeholder_consumer(
      keyword = "lc2",
      build_ui = function(node, cols) NULL,  # no `...`, missing `data`
      resolve_expr = function(value, node, ...) rlang::sym(value)
    ),
    "must accept argument\\(s\\): data"
  )
})

# ---- D6: runtime threads `data` to build_ui ----

test_that("D6 build_ui receives the resolved upstream data frame", {
  captured <- new.env(parent = emptyenv())
  captured$data <- NULL
  captured$cols <- NULL

  suppressWarnings({
    ptr_define_placeholder_consumer(
      keyword = "captured_col",
      build_ui = function(node, cols, data, ...) {
        captured$data <- data
        captured$cols <- cols
        shinyWidgets::pickerInput(node$id, choices = cols)
      },
      resolve_expr = function(value, node, ...) rlang::sym(value)
    )
  })
  on.exit(suppressWarnings(ptr_register_builtins()), add = TRUE)

  e <- .lc_test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(x = captured_col))",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    tree <- shiny::isolate(state$tree())
    consumer <- find_nodes(tree, is_ptr_ph_data_consumer)[[1L]]
    output_id <- consumer_output_id(consumer$id)
    # Force the renderUI to run by reading its output.
    output[[output_id]]
    session$flushReact()
    expect_s3_class(captured$data, "data.frame")
    expect_setequal(captured$cols, names(mtcars))
    expect_equal(nrow(captured$data), nrow(mtcars))
  })
})

# ---- D2: per-consumer cache deps ----

test_that("D2 find_consumer_ids_in_upstream walks producer subtrees", {
  r <- ptr_translate("mtcars |> dplyr::select(var) |> dplyr::select(var)")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  # The downstream consumer's upstream contains the upstream consumer.
  expect_length(consumers, 2L)
  # Order may follow tree walk; pick the one whose upstream contains the other.
  ids <- lapply(consumers, function(c) find_consumer_ids_in_upstream(c$upstream))
  has_one <- vapply(ids, function(x) length(x) == 1L, logical(1))
  has_zero <- vapply(ids, function(x) length(x) == 0L, logical(1))
  expect_true(any(has_one))   # downstream var sees upstream var
  expect_true(any(has_zero))  # upstream var sees no upstream consumer
})

test_that("D2 find_producer_ids_in_upstream picks ptr_ph_value", {
  r <- ptr_translate("mtcars |> dplyr::filter(num > 0) |> dplyr::select(var)")
  consumer <- find_nodes(r, is_ptr_ph_data_consumer)[[1L]]
  pids <- find_producer_ids_in_upstream(consumer$upstream)
  expect_length(pids, 1L)
})

# ---- D8: record_eval_time flip-flop ----

.fake_state <- function(mode = "auto", initial_ms = 0L) {
  s <- list(
    producer_debounce_mode = mode,
    producer_debounce_ms = shiny::reactiveVal(as.integer(initial_ms)),
    producer_perf_env = new.env(parent = emptyenv())
  )
  s$producer_perf_env$slow_count <- 0L
  s$producer_perf_env$fast_count <- 0L
  s$producer_perf_env$first_eval_done <- FALSE
  s
}

test_that("D8 first eval is excluded from auto-flip", {
  s <- .fake_state()
  shiny::isolate({
    record_eval_time(s, 1000)  # would normally trigger; first eval skipped
    expect_equal(s$producer_debounce_ms(), 0L)
    expect_true(s$producer_perf_env$first_eval_done)
    expect_equal(s$producer_perf_env$slow_count, 0L)
  })
})

test_that("D8 three consecutive slow evals flip debounce on", {
  s <- .fake_state()
  shiny::isolate({
    record_eval_time(s, 0)  # first eval excluded
    record_eval_time(s, 200)  # slow_count = 1
    record_eval_time(s, 200)  # slow_count = 2
    expect_equal(s$producer_debounce_ms(), 0L)
    record_eval_time(s, 200)  # slow_count = 3 → flip on
    expect_equal(s$producer_debounce_ms(), 300L)
    expect_equal(s$producer_perf_env$slow_count, 0L)
    expect_equal(s$producer_perf_env$fast_count, 0L)
  })
})

test_that("D8 a fast eval mid-streak resets slow_count", {
  s <- .fake_state()
  shiny::isolate({
    record_eval_time(s, 0)
    record_eval_time(s, 200)  # slow_count = 1
    record_eval_time(s, 200)  # slow_count = 2
    record_eval_time(s, 50)   # fast → slow_count = 0
    expect_equal(s$producer_perf_env$slow_count, 0L)
    record_eval_time(s, 200)  # slow_count = 1 again
    expect_equal(s$producer_debounce_ms(), 0L)
  })
})

test_that("D8 five consecutive fast evals flip debounce off", {
  s <- .fake_state(initial_ms = 300L)
  s$producer_perf_env$first_eval_done <- TRUE  # skip cold-start
  shiny::isolate({
    for (i in 1:4) record_eval_time(s, 50)
    expect_equal(s$producer_debounce_ms(), 300L)
    record_eval_time(s, 50)  # 5th fast → flip off
    expect_equal(s$producer_debounce_ms(), 0L)
    expect_equal(s$producer_perf_env$fast_count, 0L)
  })
})

test_that("D8 a slow eval mid-streak resets fast_count", {
  s <- .fake_state(initial_ms = 300L)
  s$producer_perf_env$first_eval_done <- TRUE
  shiny::isolate({
    for (i in 1:4) record_eval_time(s, 50)
    record_eval_time(s, 200)  # slow → fast_count = 0; still on
    expect_equal(s$producer_debounce_ms(), 300L)
    expect_equal(s$producer_perf_env$fast_count, 0L)
  })
})

test_that("D8 manual mode skips auto-flip", {
  s <- .fake_state(mode = "manual", initial_ms = 0L)
  shiny::isolate({
    for (i in 1:10) record_eval_time(s, 1000)
    expect_equal(s$producer_debounce_ms(), 0L)
    expect_equal(s$producer_perf_env$slow_count, 0L)  # never incremented
  })
})

# ---- ptr_init_state validates producer_debounce_ms ----

test_that("ptr_init_state rejects negative producer_debounce_ms", {
  expect_error(
    ptr_init_state(
      "ggplot(mtcars)",
      envir = .lc_test_env(),
      producer_debounce_ms = -1
    ),
    "non-negative"
  )
})

test_that("ptr_init_state with NULL producer_debounce_ms enables auto mode", {
  state <- ptr_init_state(
    "ggplot(mtcars)",
    envir = .lc_test_env()
  )
  expect_identical(state$producer_debounce_mode, "auto")
  expect_equal(shiny::isolate(state$producer_debounce_ms()), 0L)
})

test_that("ptr_init_state with positive producer_debounce_ms uses manual mode", {
  state <- ptr_init_state(
    "ggplot(mtcars)",
    envir = .lc_test_env(),
    producer_debounce_ms = 500
  )
  expect_identical(state$producer_debounce_mode, "manual")
  expect_equal(shiny::isolate(state$producer_debounce_ms()), 500L)
})
