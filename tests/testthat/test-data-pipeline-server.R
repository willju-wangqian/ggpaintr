make_pipeline_server_env <- function() {
  env <- new.env(parent = .GlobalEnv)
  env$mtcars <- datasets::mtcars
  env
}

# ---------------------------------------------------------------------------
# Initial seed and click behavior via shiny::testServer
# ---------------------------------------------------------------------------

test_that("data-pipeline cache is seeded at app start with the trim-to-root frame", {
  pipeline_env <- make_pipeline_server_env()

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()",
      envir = pipeline_env
    )
  }

  shiny::testServer(server_wrapper, {
    state <- session$userData$ptr_state
    cached <- state$resolved_data[["ggplot"]]()
    expect_s3_class(cached, "data.frame")
    expect_identical(nrow(cached), 6L) # head(mtcars) default
    expect_identical(names(cached), names(datasets::mtcars))
  })
})

test_that("clicking Update Data refreshes the cache from current input values", {
  pipeline_env <- make_pipeline_server_env()

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()",
      envir = pipeline_env
    )
  }

  shiny::testServer(server_wrapper, {
    state <- session$userData$ptr_state
    obj <- shiny::isolate(state$obj())
    num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
    btn_id <- ptr_update_data_input_id("ggplot")

    args <- list()
    args[[num_id]] <- 3L
    args[[btn_id]] <- 1
    do.call(session$setInputs, args)

    cached <- state$resolved_data[["ggplot"]]()
    expect_s3_class(cached, "data.frame")
    expect_identical(nrow(cached), 3L)
  })
})

test_that("stale flag flips TRUE when inputs diverge and back to FALSE on click", {
  pipeline_env <- make_pipeline_server_env()

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()",
      envir = pipeline_env
    )
  }

  shiny::testServer(server_wrapper, {
    state <- session$userData$ptr_state
    obj <- shiny::isolate(state$obj())
    num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
    btn_id <- ptr_update_data_input_id("ggplot")

    expect_false(state$is_stale_env[["ggplot"]]())

    args <- list()
    args[[num_id]] <- 3L
    args[[btn_id]] <- 1
    do.call(session$setInputs, args)
    expect_false(state$is_stale_env[["ggplot"]]())

    args2 <- list()
    args2[[num_id]] <- 5L
    do.call(session$setInputs, args2)
    expect_true(state$is_stale_env[["ggplot"]]())

    args3 <- list()
    args3[[btn_id]] <- 2
    do.call(session$setInputs, args3)
    expect_false(state$is_stale_env[["ggplot"]]())
  })
})

# ---------------------------------------------------------------------------
# var dropdowns reflect the cached frame
# ---------------------------------------------------------------------------

test_that("ptr_build_var_column_map prefers cached data when resolved_data is supplied", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  eval_env <- make_pipeline_server_env()

  cached_frame <- data.frame(only_one = 1:3, only_two = 4:6)
  resolved_data <- list(ggplot = shiny::reactiveVal(cached_frame))

  cmap <- shiny::isolate(
    ptr_build_var_column_map(
      obj,
      input = list(),
      context = context,
      eval_env = eval_env,
      resolved_data = resolved_data
    )
  )

  expect_true("ggplot" %in% names(cmap))
  expect_true(cmap[["ggplot"]]$has_data)
  expect_setequal(cmap[["ggplot"]]$columns, c("only_one", "only_two"))
})

test_that("ptr_build_var_column_map falls back to live resolution when cache is NULL", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  eval_env <- make_pipeline_server_env()

  resolved_data <- list(ggplot = shiny::reactiveVal(NULL))

  cmap <- shiny::isolate(
    ptr_build_var_column_map(
      obj,
      input = list(),
      context = context,
      eval_env = eval_env,
      resolved_data = resolved_data
    )
  )

  expect_true("ggplot" %in% names(cmap))
  expect_setequal(cmap[["ggplot"]]$columns, names(datasets::mtcars))
})

# ---------------------------------------------------------------------------
# Failure path
# ---------------------------------------------------------------------------

test_that("update click on an unresolvable pipeline leaves the cache untouched", {
  pipeline_env <- make_pipeline_server_env()

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "ggpaintr_nonexistent_xyz |> head(num) |> ggplot(aes(x = var)) + geom_point()",
      envir = pipeline_env
    )
  }

  suppressWarnings(shiny::testServer(server_wrapper, {
    state <- session$userData$ptr_state
    obj <- shiny::isolate(state$obj())
    num_id <- obj$data_pipeline_info[["ggplot"]]$placeholder_ids[[1]]
    btn_id <- ptr_update_data_input_id("ggplot")

    expect_null(state$resolved_data[["ggplot"]]())

    args <- list()
    args[[num_id]] <- 3L
    args[[btn_id]] <- 1
    do.call(session$setInputs, args)

    expect_null(state$resolved_data[["ggplot"]]())
  }))
})

# ---------------------------------------------------------------------------
# UI: data-tab UI emits the custom-message handler script
# ---------------------------------------------------------------------------

test_that("ptr_get_data_tab_ui includes the ptr_set_class custom-message handler", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()"
  )
  rendered <- paste(as.character(ptr_get_data_tab_ui(obj)), collapse = "\n")
  expect_match(rendered, "ptr_set_class", fixed = TRUE)
  expect_match(rendered, "addCustomMessageHandler", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# Regression: clicking Update Data must not wipe the user's var-dropdown
# selection. The picker is rebuilt when the cache changes; the previously
# selected column should be retained as long as it still exists in the
# refreshed columns.
# ---------------------------------------------------------------------------

test_that("generate_ui_var preserves the previously selected column on rebuild", {
  rendered_default <- as.character(generate_ui_var(
    data_var = c("mpg", "cyl", "disp"),
    id = "x",
    param = "x"
  ))
  rendered_default <- paste(rendered_default, collapse = "\n")
  expect_false(grepl("selected[^>]*>cyl<", rendered_default))

  rendered_kept <- as.character(generate_ui_var(
    data_var = c("mpg", "cyl", "disp"),
    id = "x",
    param = "x",
    selected = "cyl"
  ))
  rendered_kept <- paste(rendered_kept, collapse = "\n")
  expect_match(rendered_kept, "selected[^>]*>cyl<")
})

test_that("generate_ui_var drops a stale selection that no longer exists in the columns", {
  rendered <- as.character(generate_ui_var(
    data_var = c("mpg", "cyl"),
    id = "x",
    param = "x",
    selected = "Sepal.Length"
  ))
  rendered <- paste(rendered, collapse = "\n")
  expect_false(grepl("selected[^>]*>Sepal\\.Length<", rendered))
  expect_false(grepl("selected[^>]*>(mpg|cyl)<", rendered))
})
