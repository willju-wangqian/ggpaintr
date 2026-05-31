# test-vacate-on-empty-scope.R -- ADR 0025 §7 / PLAN-05 (A1) scope discipline.
#
# `vacate_source_binding()` MUST be scoped to `state$eval_env` only -- the
# `rm()` uses `inherits = FALSE` and the `exists()` probe uses the same
# guard. Caller-env bindings reached via the parent chain must be left
# intact (otherwise vacating a stale `mtcars` binding would also wipe the
# user's `globalenv()$mtcars`). Unit-level test of the predicate -- no
# Shiny session, just a synthetic `state` with the same shape `ptr_init_state()`
# would produce for `bound_names` / `eval_env` / `active_uploads`.

make_state <- function(parent_env, key, prior_name = NULL) {
  ee <- new.env(parent = parent_env)
  bn <- stats::setNames(list(shiny::reactiveVal(prior_name)), key)
  au <- shiny::reactiveVal(stats::setNames(list(), character()))
  list(
    eval_env = ee,
    bound_names = bn,
    active_uploads = au
  )
}

test_that("vacate removes only the eval_env binding, never the caller-env shadow", {
  # Caller env (e.g. globalenv()) holds `mtcars`. The synthetic state's
  # `eval_env` is a child of caller env that also holds a `mtcars` shadow
  # from a prior upload bind. After vacate, the eval_env shadow is gone
  # but the caller env mtcars is intact and reachable via the parent
  # chain.
  caller_env <- new.env(parent = emptyenv())
  caller_env$mtcars <- datasets::mtcars  # baseline -- must survive vacate
  state <- make_state(caller_env, key = "demo_key", prior_name = "mtcars")
  # Simulate the prior-resolve assign that bind_source_value() would have
  # performed: a tweaked frame, distinguishable from the caller-env one.
  assign("mtcars", data.frame(x = 1:3), envir = state$eval_env)

  # Sanity: pre-vacate, both surfaces are populated.
  expect_true(exists("mtcars", envir = state$eval_env, inherits = FALSE),
              label = "eval_env shadow present pre-vacate")
  expect_true(exists("mtcars", envir = caller_env, inherits = FALSE),
              label = "caller env mtcars present pre-vacate")
  expect_equal(shiny::isolate(state$bound_names[["demo_key"]]()), "mtcars",
               label = "bound_names tracks the prior name pre-vacate")

  ggpaintr:::vacate_source_binding(state, "demo_key")

  # Post-vacate: eval_env shadow gone, caller env mtcars intact.
  expect_false(exists("mtcars", envir = state$eval_env, inherits = FALSE),
               label = "eval_env shadow removed by vacate")
  expect_true(exists("mtcars", envir = caller_env, inherits = FALSE),
              label = "caller env mtcars UNTOUCHED by vacate")
  # The caller env's mtcars is still reachable from eval_env via the parent
  # chain (inherits = TRUE).
  expect_true(exists("mtcars", envir = state$eval_env, inherits = TRUE),
              label = "caller env mtcars still reachable through parent chain")
  # `bound_names[[key]]` reset to NULL.
  expect_null(shiny::isolate(state$bound_names[["demo_key"]]()))
})

test_that("vacate is a no-op when no prior name was bound", {
  caller_env <- new.env(parent = emptyenv())
  state <- make_state(caller_env, key = "demo_key", prior_name = NULL)
  # No assign() -- eval_env has nothing.
  expect_silent(ggpaintr:::vacate_source_binding(state, "demo_key"))
  expect_null(shiny::isolate(state$bound_names[["demo_key"]]()))
})

test_that("vacate is a no-op when the prior name does not exist in eval_env", {
  caller_env <- new.env(parent = emptyenv())
  state <- make_state(caller_env, key = "demo_key", prior_name = "ghost")
  # bound_names points at "ghost" but no assign() happened.
  expect_silent(ggpaintr:::vacate_source_binding(state, "demo_key"))
  expect_null(shiny::isolate(state$bound_names[["demo_key"]]()))
})

test_that("vacate skips rm() on a non-syntactic prior name (safety guard)", {
  # `make.names(prior) != prior` -> the guard short-circuits before exists()
  # / rm(), so even if a non-syntactic name somehow ended up in bound_names
  # (it should not -- bind_source_value's writers enforce make.names()),
  # vacate does not error and still clears the bound_names slot.
  caller_env <- new.env(parent = emptyenv())
  state <- make_state(caller_env, key = "demo_key", prior_name = "1bad name")
  expect_silent(ggpaintr:::vacate_source_binding(state, "demo_key"))
  expect_null(shiny::isolate(state$bound_names[["demo_key"]]()))
})

test_that("vacate clears active_uploads[[key]] (delegates to clear_active_upload)", {
  caller_env <- new.env(parent = emptyenv())
  state <- make_state(caller_env, key = "demo_key", prior_name = NULL)
  # Seed an active_uploads entry for the key.
  state$active_uploads(list(demo_key = list(
    auto_name = "demo_key", file_name = "mtcars.csv", ext = "csv"
  )))
  expect_equal(names(shiny::isolate(state$active_uploads())), "demo_key",
               label = "active_uploads seeded pre-vacate")

  ggpaintr:::vacate_source_binding(state, "demo_key")

  expect_equal(length(shiny::isolate(state$active_uploads())), 0L,
               label = "active_uploads cleared by vacate")
})
