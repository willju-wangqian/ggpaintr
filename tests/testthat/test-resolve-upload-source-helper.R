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
  # ADR 0025 item #7: `file_reset` (default FALSE) added so the resolver can
  # treat the fileInput as absent when its display was re-rendered, without
  # depending on the unreliable server-side input value.
  expect_identical(
    names(formals(fn)),
    c("input_slot", "shortcut_slot", "node", "entry", "envir",
      "state", "key", "slot", "file_reset")
  )
  expect_identical(formals(fn)$file_reset, FALSE)
})

# ---- BDD Scenarios 2/3/4 — MERGED into J2 journey on 2026-05-28 ---------
# resolve-upload-source-helper.R:90 (NULL file_info default-arg)
#   -> covered by test-upload-clears-stale-cache.R:21-22 (boot-time picker
#      cols seed from mtcars only if try_bind_source_default_resolved fired).
# resolve-upload-source-helper.R:119 (populated file_info dispatch)
#   -> covered by test-prologue-csv-upload.R (prologue LHS = auto_name).
# resolve-upload-source-helper.R:152 (resolve_data error path)
#   -> covered by test-j2-prologue-csv-upload-journey.R stage 2 (new
#      fixture j2-custom-source-error registers ppFailingSource whose
#      resolve_data throws "boom"; #ptr_error surfaces the message).
# v9 routing: dev/audit/audit-test-fidelity-v9-j2-browser-faithfulness-
# 2026-05-28-0027.html

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
    shortcut_slot = list(present = TRUE, value = "1 not a name"),
    node           = node,
    entry          = entry,
    envir          = env,
    state          = w$state,
    key            = "layerD",
    slot           = w$slot
  )

  # ADR 0025 §2/§3: a non-empty shortcut ("1 not a name") makes the textbox
  # the live affordance, so resolve_upload_source nulls the lingering file
  # (file_info <- NULL) and takes the env-load path -- entry$resolve_data is
  # never called. The typed name is non-syntactic and not present in eval_env,
  # so nothing binds: eval_env is untouched, bound_names stays unset, and the
  # slot vacates to NULL (the eval-side validator is what aborts loudly on a
  # draw for an invalid name). Pre-ADR-0025 the file was kept and slot(mtcars)
  # fired with name=NULL; that name-override-with-file path is retired.
  expect_null(w$state$bound_names[["layerD"]]())
  expect_identical(ls(envir = env), character(0))
  expect_null(w$slot())
})
