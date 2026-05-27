# ADR 0025 §3 / PLAN-02 — BDD scenarios 5 & 6: capture the `name`
# argument passed to `bind_source_value()` from `resolve_upload_source()`
# under two conditions:
#   1. shortcut textbox empty + file_info non-NULL → name == node$auto_name
#   2. shortcut textbox = "my_name" + file_info non-NULL → name == "my_name"
#
# Uses `testthat::local_mocked_bindings(.package = "ggpaintr", ...)`
# rather than `assignInNamespace()` per project memory mock-internal-calls
# (pkgload::load_all breaks assignInNamespace).

fake_state <- function() {
  env <- new.env(parent = emptyenv())
  bound <- new.env(parent = emptyenv())
  errors_val <- list()
  list(
    eval_env = env,
    bound_names = bound,
    resolve_errors = function(new_val) {
      if (missing(new_val)) errors_val else {
        errors_val <<- new_val
        invisible(errors_val)
      }
    }
  )
}

make_slot <- function() {
  v <- NULL
  function(new_val) {
    if (missing(new_val)) v else { v <<- new_val; invisible(v) }
  }
}

test_that("empty shortcut textbox → bind_source_value name = node$auto_name", {
  captured <- list()
  testthat::local_mocked_bindings(
    .package = "ggpaintr",
    bind_source_value = function(state, key, name, df, slot) {
      captured$key <<- key
      captured$name <<- name
      captured$df <<- df
      slot(df)
      invisible(NULL)
    }
  )

  st <- fake_state()
  st$bound_names[["k"]] <- make_slot()
  slot <- make_slot()
  node <- list(
    keyword = "ppUpload",
    shortcut_id = "ggplot_1_0_ppUpload_NA_shortcut",
    auto_name = "ggplot_1_0_ppUpload_NA",
    default = NULL
  )
  entry <- list(
    resolve_data = function(file_info, node) datasets::mtcars
  )

  ggpaintr:::resolve_upload_source(
    input_slot     = list(datapath = tempfile(fileext = ".csv"),
                          name     = "x.csv"),
    shortcut_slot = list(present = TRUE, value = ""),
    node           = node,
    entry          = entry,
    envir          = st$eval_env,
    state          = st,
    key            = "k",
    slot           = slot
  )

  expect_identical(captured$key, "k")
  expect_identical(captured$name, "ggplot_1_0_ppUpload_NA")
  expect_identical(captured$df, datasets::mtcars)
})

test_that("NULL shortcut textbox value → bind_source_value name = node$auto_name", {
  captured <- list()
  testthat::local_mocked_bindings(
    .package = "ggpaintr",
    bind_source_value = function(state, key, name, df, slot) {
      captured$name <<- name
      slot(df)
      invisible(NULL)
    }
  )
  st <- fake_state()
  st$bound_names[["k"]] <- make_slot()
  slot <- make_slot()
  node <- list(
    keyword = "ppUpload",
    shortcut_id = "any_shortcut",
    auto_name = "main_ds",
    default = NULL
  )
  entry <- list(
    resolve_data = function(file_info, node) datasets::iris
  )

  ggpaintr:::resolve_upload_source(
    input_slot     = list(datapath = tempfile(fileext = ".csv"),
                          name     = "x.csv"),
    shortcut_slot = list(present = TRUE, value = NULL),
    node           = node,
    entry          = entry,
    envir          = st$eval_env,
    state          = st,
    key            = "k",
    slot           = slot
  )
  expect_identical(captured$name, "main_ds")
})

test_that("non-empty shortcut textbox wins over node$auto_name", {
  captured <- list()
  testthat::local_mocked_bindings(
    .package = "ggpaintr",
    bind_source_value = function(state, key, name, df, slot) {
      captured$name <<- name
      slot(df)
      invisible(NULL)
    }
  )
  st <- fake_state()
  st$bound_names[["k"]] <- make_slot()
  slot <- make_slot()
  node <- list(
    keyword = "ppUpload",
    shortcut_id = "any_shortcut",
    auto_name = "ggplot_1_0_ppUpload_NA",
    default = NULL
  )
  entry <- list(
    resolve_data = function(file_info, node) datasets::mtcars
  )

  ggpaintr:::resolve_upload_source(
    input_slot     = list(datapath = tempfile(fileext = ".csv"),
                          name     = "x.csv"),
    shortcut_slot = list(present = TRUE, value = "my_name"),
    node           = node,
    entry          = entry,
    envir          = st$eval_env,
    state          = st,
    key            = "k",
    slot           = slot
  )
  expect_identical(captured$name, "my_name")
})
