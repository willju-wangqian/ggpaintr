# PLAN-01 (Follow-up Finding 2) — `last_ok_runtime` cache for
# validate_input retain-on-error.
#
# Contract: when a `validate_input` hook returns a non-TRUE string the
# runtime resolves with `ok = FALSE`. Prior to this plan the code panel
# and plot panel atomically blanked. Post-plan: a single-slot
# `state$last_ok_runtime` reactiveVal holds the most recent ok-runtime
# result; `ptr_register_code` / `ptr_register_plot` fall back to it on
# fail; the error pane keeps surfacing the new diagnostic.

# ---- helpers ---------------------------------------------------------------

.lor_unique_kw <- function() {
  paste0("ppRetainTest_", as.integer(Sys.time()), "_",
         sample.int(.Machine$integer.max, 1L))
}

# Register a value-role placeholder whose validate_input gates on
# numeric in [0, 1]. The build_ui hook is a plain `numericInput` so
# `testServer`'s `session$setInputs(<id> = <value>)` writes the raw R
# value into the snapshot.
.lor_register_retain_kw <- function(kw) {
  ggpaintr:::ptr_define_placeholder_value(
    keyword = kw,
    build_ui = function(node, label = NULL, ...) {
      shiny::numericInput(node$id, label %||% node$keyword, value = 0.5)
    },
    resolve_expr = function(value, node, ...) value,
    validate_session_input = function(value, ctx) {
      if (is.numeric(value) && length(value) == 1L &&
          !is.na(value) && value >= 0 && value <= 1) {
        TRUE
      } else {
        "must be in [0,1]"
      }
    }
  )
}

# Spin up a session-private envir for ptr_init_state — mirrors the
# pattern in `test-rewrite-disable.R::.test_env()`.
.lor_env <- function() {
  e <- new.env(parent = globalenv())
  e$mtcars <- mtcars
  e
}

# Build a `ptr_server_internal`-backed server for a formula containing
# the `kw` value placeholder. The plot panel renders `mtcars$mpg` (a
# constant ggplot) so success is binary on the validate_input gate, not
# on the value's semantics.
.lor_server <- function(formula, envir) {
  function(input, output, session) {
    session$userData$state <- ggpaintr:::ptr_server_internal(
      input, output, session,
      formula,
      envir = envir
    )
  }
}

# ---- success criterion #1: cache field exists and is NULL initially ------

test_that("last_ok_runtime: ptr_init_state seeds state$last_ok_runtime as a NULL reactiveVal", {
  kw <- .lor_unique_kw()
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  .lor_register_retain_kw(kw)
  formula <- paste0(
    "ggplot(mtcars, aes(mpg, wt)) + geom_point() + ",
    "labs(subtitle = paste0('p=', ", kw, "))"
  )
  state <- ggpaintr:::ptr_init_state(formula, envir = .lor_env())
  expect_true(is.function(state$last_ok_runtime))
  expect_null(shiny::isolate(state$last_ok_runtime()))
})

# ---- success criterion #2: validate_state recognizes the new field -------

test_that("last_ok_runtime: ptr_validate_state aborts when last_ok_runtime is missing", {
  kw <- .lor_unique_kw()
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  .lor_register_retain_kw(kw)
  formula <- paste0("ggplot(mtcars, aes(mpg, wt)) + geom_point() + labs(subtitle = paste0('p=', ", kw, "))")
  state <- ggpaintr:::ptr_init_state(formula, envir = .lor_env())
  # Strip the field and confirm ptr_validate_state flags it.
  broken <- state
  broken$last_ok_runtime <- NULL
  expect_error(
    ggpaintr:::ptr_validate_state(broken),
    regexp = "last_ok_runtime",
    fixed = TRUE
  )
})

# ---- success criteria #3-#7: end-to-end cache + fallback via testServer ---

test_that("last_ok_runtime: first ok draw populates cache; validate_input failure preserves it; recovery replaces it", {
  kw <- .lor_unique_kw()
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  .lor_register_retain_kw(kw)
  formula <- paste0(
    "ggplot(mtcars, aes(mpg, wt)) + geom_point() + ",
    "labs(subtitle = paste0('p=', ", kw, "))"
  )
  e <- .lor_env()
  server <- .lor_server(formula, e)

  shiny::testServer(server, {
    state <- session$userData$state
    # Boot: no draw yet → cache empty.
    expect_null(state$last_ok_runtime())

    # Locate the placeholder's raw input id from the registry-derived
    # input_spec (node ids are layer-scoped auto-generated strings, NOT
    # the bare keyword — see `ptr_runtime_input_spec()`).
    ph_row <- state$input_spec[state$input_spec$role == "placeholder" &
                                state$input_spec$keyword == kw, , drop = FALSE]
    ph_id <- ph_row$input_id[1]

    # Seed the placeholder widget with a valid value (the renderUI
    # `numericInput` default 0.5 is only realized after build_ui runs
    # in a live browser; in testServer we set it explicitly).
    args <- list(0.5); names(args) <- ph_id
    do.call(session$setInputs, args)

    # First successful Draw click.
    session$setInputs(ptr_update_plot = 1)
    res_ok <- state$runtime()
    expect_true(isTRUE(res_ok$ok))
    cached_ok <- state$last_ok_runtime()
    expect_identical(cached_ok$code_text, res_ok$code_text)

    # Drive a validate_input failure (1.5 is out of [0,1]). Cache must
    # NOT change; the runtime must report ok = FALSE; the code-panel
    # renderer must reuse the cached code_text — NOT "".
    prior_code_text <- cached_ok$code_text
    args <- list(1.5); names(args) <- ph_id
    do.call(session$setInputs, args)
    session$setInputs(ptr_update_plot = 2)
    res_fail <- state$runtime()
    expect_false(isTRUE(res_fail$ok))
    cached_after_fail <- state$last_ok_runtime()
    expect_identical(cached_after_fail$code_text, prior_code_text)

    # Read the code output via the runtime + fallback path that
    # ptr_register_code installs. The renderer formats via
    # format_code_with_extras over the chosen res; on fail with prior
    # cache it MUST choose the cached res.
    chosen <- if (is.null(res_fail) || isTRUE(res_fail$ok) ||
                   is.null(cached_after_fail)) {
      res_fail
    } else {
      cached_after_fail
    }
    code_panel <- ggpaintr:::format_code_with_extras(
      chosen, state$extras_exprs()
    )
    expect_identical(code_panel, prior_code_text)

    # Recovery: fix the value to a valid number; cache REPLACES with new
    # ok-result. The new code_text differs from the prior cache iff
    # the new subtitle differs (0.75 vs 0.5 → different).
    args <- list(0.75); names(args) <- ph_id
    do.call(session$setInputs, args)
    session$setInputs(ptr_update_plot = 3)
    res_recovered <- state$runtime()
    expect_true(isTRUE(res_recovered$ok))
    cached_after_recovery <- state$last_ok_runtime()
    expect_identical(cached_after_recovery$code_text,
                     res_recovered$code_text)
  })
})

test_that("last_ok_runtime: consecutive validate_input failures preserve the cache", {
  kw <- .lor_unique_kw()
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  .lor_register_retain_kw(kw)
  formula <- paste0(
    "ggplot(mtcars, aes(mpg, wt)) + geom_point() + ",
    "labs(subtitle = paste0('p=', ", kw, "))"
  )
  server <- .lor_server(formula, .lor_env())

  shiny::testServer(server, {
    state <- session$userData$state
    ph_row <- state$input_spec[state$input_spec$role == "placeholder" &
                                state$input_spec$keyword == kw, , drop = FALSE]
    ph_id <- ph_row$input_id[1]
    set_ph <- function(v) {
      args <- list(v); names(args) <- ph_id
      do.call(session$setInputs, args)
    }

    # Seed: one ok-draw to populate the cache.
    set_ph(0.5)
    session$setInputs(ptr_update_plot = 1)
    expect_true(isTRUE(state$runtime()$ok))
    seeded <- state$last_ok_runtime()

    # First failure.
    set_ph(-0.1)
    session$setInputs(ptr_update_plot = 2)
    expect_false(isTRUE(state$runtime()$ok))
    expect_identical(state$last_ok_runtime()$code_text, seeded$code_text)

    # Second consecutive failure with a different out-of-range value.
    set_ph(2.5)
    session$setInputs(ptr_update_plot = 3)
    expect_false(isTRUE(state$runtime()$ok))
    expect_identical(state$last_ok_runtime()$code_text, seeded$code_text)
  })
})

# ---- success criterion #8: no prior ok-result + immediate fail -----------

test_that("last_ok_runtime: immediate failure on first draw leaves cache NULL", {
  kw <- .lor_unique_kw()
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  .lor_register_retain_kw(kw)
  formula <- paste0(
    "ggplot(mtcars, aes(mpg, wt)) + geom_point() + ",
    "labs(subtitle = paste0('p=', ", kw, "))"
  )
  server <- .lor_server(formula, .lor_env())

  shiny::testServer(server, {
    state <- session$userData$state
    ph_row <- state$input_spec[state$input_spec$role == "placeholder" &
                                state$input_spec$keyword == kw, , drop = FALSE]
    ph_id <- ph_row$input_id[1]
    # Drive a validate_input failure on the FIRST draw, no prior ok.
    args <- list(5); names(args) <- ph_id
    do.call(session$setInputs, args)
    session$setInputs(ptr_update_plot = 1)
    res_fail <- state$runtime()
    expect_false(isTRUE(res_fail$ok))
    expect_null(state$last_ok_runtime())
    # With no cache, the code-panel fallback emits "" (same as today).
    chosen <- if (is.null(res_fail) || isTRUE(res_fail$ok) ||
                   is.null(state$last_ok_runtime())) {
      res_fail
    } else {
      state$last_ok_runtime()
    }
    code_panel <- ggpaintr:::format_code_with_extras(
      chosen, state$extras_exprs()
    )
    # `format_code_with_extras()` returns "" for a non-ok res with no
    # extras — matching the no-prior-ok behavior the plan preserves.
    expect_identical(code_panel, "")
  })
})

# ---- plot fallback: cached res$plot survives a fail ----------------------

test_that("last_ok_runtime: plot fallback returns the cached plot object after a validate_input failure", {
  kw <- .lor_unique_kw()
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  .lor_register_retain_kw(kw)
  formula <- paste0(
    "ggplot(mtcars, aes(mpg, wt)) + geom_point() + ",
    "labs(subtitle = paste0('p=', ", kw, "))"
  )
  server <- .lor_server(formula, .lor_env())

  shiny::testServer(server, {
    state <- session$userData$state
    ph_row <- state$input_spec[state$input_spec$role == "placeholder" &
                                state$input_spec$keyword == kw, , drop = FALSE]
    ph_id <- ph_row$input_id[1]
    set_ph <- function(v) {
      args <- list(v); names(args) <- ph_id
      do.call(session$setInputs, args)
    }
    set_ph(0.5)
    session$setInputs(ptr_update_plot = 1)
    res_ok <- state$runtime()
    expect_true(isTRUE(res_ok$ok))
    cached_plot <- state$last_ok_runtime()$plot
    expect_false(is.null(cached_plot))
    # Drive a failure; cache$plot must equal the prior ok's plot.
    set_ph(9)
    session$setInputs(ptr_update_plot = 2)
    expect_false(isTRUE(state$runtime()$ok))
    expect_identical(state$last_ok_runtime()$plot, cached_plot)
  })
})
