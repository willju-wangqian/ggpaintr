# Focused unit tests for `resolve_upload_source()` — the internal helper
# extracted from the two near-identical observer bodies in
# `ptr_setup_pipelines()` (ADR 0023 §3, PLAN-02). Pure refactor: these
# tests pin the helper's contract at the boundary so the third caller
# (PLAN-04 host-scope panel sources) can land without redundant work,
# and so a future format addition (Parquet, Arrow) cannot accidentally
# diverge between bare-data and pipeline-head call sites.
#
# The integration tests (test-shared-source-rendering.R,
# test-upload-companion-autofill.R, every browser fixture that uploads)
# remain the load-bearing check that the refactor is behaviour-preserving
# end-to-end; these tests narrowly verify the four control-flow branches
# the plan calls out.

# ---- test fixtures -------------------------------------------------------

# A stand-in for `state` that exposes only what `resolve_upload_source()`
# touches: `eval_env`, `bound_names[[key]]`, and `resolve_errors()`.
fake_state <- function(eval_env = new.env(parent = emptyenv())) {
  bound <- list()
  errors <- list()
  list(
    eval_env = eval_env,
    bound_names = local({
      env <- new.env(parent = emptyenv())
      env
    }),
    resolve_errors = local({
      val <- list()
      function(new_val) {
        if (missing(new_val)) val else {
          val <<- new_val
          invisible(val)
        }
      }
    })
  )
}

# A stand-in for `state$resolved_data[[key]]` / `state$resolved_sources[[key]]`:
# a reactiveVal-shaped getter/setter that records the last value the helper
# pushed into it.
make_slot <- function() {
  store <- list(value = NULL, set_count = 0L, last = NULL)
  function(new_val) {
    if (missing(new_val)) {
      store$value
    } else {
      store$value <<- new_val
      store$set_count <<- store$set_count + 1L
      store$last <<- new_val
      invisible(new_val)
    }
  }
}

# A stand-in for `state$bound_names[[key]]`: same getter/setter shape.
make_name_slot <- function() {
  store <- list(value = NULL)
  function(new_val) {
    if (missing(new_val)) store$value else {
      store$value <<- new_val
      invisible(new_val)
    }
  }
}

# Wire a state + slot pair and pre-attach the named bound_names slot under
# `key`, mirroring what `ptr_init_state()` does at boot.
wire <- function(key, eval_env = new.env(parent = emptyenv())) {
  st <- fake_state(eval_env = eval_env)
  st$bound_names[[key]] <- make_name_slot()
  list(state = st, slot = make_slot())
}

# ---- BDD Scenario 1 — helper exists and is internal ---------------------

test_that("ggpaintr:::resolve_upload_source has the documented formals", {
  fn <- ggpaintr:::resolve_upload_source
  expect_true(is.function(fn))
  expect_identical(
    names(formals(fn)),
    c("input_slot", "companion_slot", "node", "entry", "envir",
      "state", "key", "slot")
  )
})

# ---- BDD Scenario 2 — NULL file_info + resolvable default-arg ----------

test_that("NULL file_info with resolvable default-arg binds the resolved frame", {
  env <- new.env(parent = emptyenv())
  env$df_main <- mtcars
  w <- wire("layerA", eval_env = env)

  node <- list(default = "df_main")
  entry <- list(resolve_data = function(file_info, node) stop("unreachable"))

  ggpaintr:::resolve_upload_source(
    input_slot     = NULL,
    companion_slot = list(present = TRUE, value = ""),
    node           = node,
    entry          = entry,
    envir          = env,
    state          = w$state,
    key            = "layerA",
    slot           = w$slot
  )

  # bind_source_value() path: eval_env gets the frame under `df_main`,
  # the bound_names slot records the name, the resolved-data slot
  # receives the frame.
  expect_identical(w$state$bound_names[["layerA"]](), "df_main")
  expect_identical(get("df_main", envir = env, inherits = FALSE), mtcars)
  expect_identical(w$slot(), mtcars)
})

# ---- BDD Scenario 3 — populated file_info dispatches resolve_data -----

test_that("populated file_info dispatches entry$resolve_data and binds result", {
  env <- new.env(parent = emptyenv())
  w <- wire("layerB", eval_env = env)

  node <- list(default = NULL)
  entry <- list(
    resolve_data = function(file_info, node) {
      # mimic ppUpload's resolver: read the file path, return penguins
      datasets::iris  # any data.frame works; using iris to be distinctive
    }
  )

  ggpaintr:::resolve_upload_source(
    input_slot     = list(datapath = tempfile(fileext = ".csv"),
                          name     = "iris.csv"),
    companion_slot = list(present = TRUE, value = "iris_df"),
    node           = node,
    entry          = entry,
    envir          = env,
    state          = w$state,
    key            = "layerB",
    slot           = w$slot
  )

  expect_identical(w$state$bound_names[["layerB"]](), "iris_df")
  expect_identical(get("iris_df", envir = env, inherits = FALSE), datasets::iris)
  expect_identical(w$slot(), datasets::iris)
  # resolve_errors was cleared (no entry under our key)
  expect_null(w$state$resolve_errors()[["layerB"]])
})

# ---- BDD Scenario 4 — resolve_data error surfaces via set_resolve_error --

test_that("resolve_data error surfaces via set_resolve_error and clears the slot", {
  env <- new.env(parent = emptyenv())
  w <- wire("layerC", eval_env = env)

  node <- list(default = NULL)
  entry <- list(
    resolve_data = function(file_info, node) stop("boom")
  )

  ggpaintr:::resolve_upload_source(
    input_slot     = list(datapath = tempfile(fileext = ".csv"),
                          name     = "x.csv"),
    companion_slot = list(present = TRUE, value = "x"),
    node           = node,
    entry          = entry,
    envir          = env,
    state          = w$state,
    key            = "layerC",
    slot           = w$slot
  )

  errs <- w$state$resolve_errors()
  expect_true("layerC" %in% names(errs))
  expect_match(errs[["layerC"]], "boom", fixed = TRUE)
  # slot was set to NULL via bind_source_value(df = NULL, name = "x"):
  # bind_source_value only assigns when both df and name are non-NULL,
  # so eval_env stays empty, but slot(df) fires unconditionally → NULL.
  expect_null(w$slot())
  expect_false(exists("x", envir = env, inherits = FALSE))
})

# ---- BDD Scenario 5 — invalid binding name yields NULL name ------------

test_that("invalid binding name (non-syntactic via make.names) leaves eval_env untouched", {
  env <- new.env(parent = emptyenv())
  w <- wire("layerD", eval_env = env)

  node <- list(default = NULL)
  entry <- list(
    resolve_data = function(file_info, node) mtcars
  )

  ggpaintr:::resolve_upload_source(
    input_slot     = list(datapath = tempfile(fileext = ".csv"),
                          name     = "x.csv"),
    companion_slot = list(present = TRUE, value = "1 not a name"),
    node           = node,
    entry          = entry,
    envir          = env,
    state          = w$state,
    key            = "layerD",
    slot           = w$slot
  )

  # bind_source_value(name = NULL, df = mtcars): the guard
  # `if (!is.null(df) && !is.null(name))` short-circuits, so eval_env
  # gets nothing AND bound_names stays unset, but slot(df) still fires
  # so downstream `req()` chains can halt cleanly.
  expect_null(w$state$bound_names[["layerD"]]())
  expect_identical(ls(envir = env), character(0))
  expect_identical(w$slot(), mtcars)
})
