# ADR 0024 — source companion as data-loading entry point. Promotes the
# companion textInput beside ppUpload's fileInput (and any custom source's
# input emitted at node$shortcut_id) from "name override for an uploaded
# frame" to "typed-in shortcut for loading a data.frame from envir."
#
# Pre-ADR-0024 the binder helpers
#   try_bind_source_default        (R/paintr-server.R:945)
#   try_bind_source_default_resolved (R/paintr-server.R:1069)
# short-circuited on `if (is.null(node$default)) return(FALSE)`, rejecting
# a typed-in OR spec-seeded companion value when the placeholder had no
# `default=`. ADR 0024 lifts the guard (preserving the existing
# default-fallback behavior) and adds a structured error surface via
# set_resolve_error so a typed name that fails to resolve goes to the
# inline error pane instead of disappearing silently.

# ---- Scenario A: user types a valid name; ppVar populates ----

test_that("adr0024: typed companion name binds env frame without default", {
  app <- boot_vignette_app("adr24-companion-typed")
  app$wait_for_idle(timeout = 15 * 1000)

  # Companion starts blank (no default, no spec). ppVar picker is empty.
  expect_equal(
    app$get_value(input = "ggplot_0_ppUpload_NA_shortcut") %||% "",
    "",
    label = "companion starts blank with no default + no spec"
  )

  # User types "mtcars". The input-bound textInput sends the change; the
  # bare-data-source observer at R/paintr-server.R:1149-1164 invalidates,
  # calls resolve_upload_source -> file_info NULL ->
  # try_bind_source_default_resolved (post ADR 0024) -> binds mtcars.
  set_input(app, "ggplot_0_ppUpload_NA_shortcut", "mtcars")
  app$wait_for_idle(timeout = 15 * 1000)

  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "cyl")
  expect_no_inline_error(app, "ptr_error")
})

# ---- Scenario C: typed name that doesn't resolve surfaces inline error ----

test_that("adr0024: typed name not resolving to a data frame surfaces inline error", {
  app <- boot_vignette_app("adr24-companion-typed")
  app$wait_for_idle(timeout = 15 * 1000)

  # Type a name that does NOT resolve to a data.frame in envir. The
  # error pane #ptr_error is a renderUI that depends on `state$runtime()`
  # — it only re-renders on a draw click (see ptr_register_error @
  # R/paintr-server.R:2672-2700 + the .claude/rules/testing.md
  # "ggpaintr only re-renders on the Update/Draw click" note). set_input
  # writes the resolve error into `state$resolve_errors` synchronously
  # but the pane reflects it only after Update/Draw fires.
  set_input(app, "ggplot_0_ppUpload_NA_shortcut", "fooberry")
  # `ptr_register_error()`'s renderUI body reads `state$resolve_errors()`,
  # so the structured error written inside the resolve observer re-renders
  # the pane WITHOUT needing a draw click. wait_for_idle can flag the
  # consumer-side req() block as instability while the error pane content
  # is already settled, so poll on the actual content.
  app$wait_for_js(
    paste0("(function(){var e=document.getElementById('ptr_error');",
           "return !!e && e.innerHTML.indexOf('fooberry')!==-1;})()"),
    timeout = 15 * 1000
  )

  err_html <- app$get_html("#ptr_error") %||% ""
  expect_match(err_html, "fooberry", fixed = TRUE,
               label = "inline error names the unresolved binding")
  expect_match(err_html, "not found", fixed = TRUE,
               label = "inline error explains the unresolved binding")

  # Recovery: typing a valid name clears the error (resolve observer
  # calls `set_resolve_error(NULL)` at L1014 before retrying the bind).
  set_input(app, "ggplot_0_ppUpload_NA_shortcut", "mtcars")
  app$wait_for_js(
    paste0("(function(){var e=document.getElementById('ptr_error');",
           "return !!e && e.innerHTML.indexOf('ptr-alert--error')===-1;})()"),
    timeout = 15 * 1000
  )
  expect_no_inline_error(app, "ptr_error")
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "cyl")
})

# ---- Scenario B: spec= override for a no-default source ----

test_that("adr0024: spec= for a source_companion id binds env frame without default", {
  app <- boot_vignette_app("adr24-companion-spec-no-default")
  app$wait_for_idle(timeout = 15 * 1000)

  # The companion text input lands "mtcars" at boot via the FINDING #8 fix
  # (apply_spec_at_boot no longer marks source_companion rows in `seeded`,
  # so the onFlushed updateTextInput dispatch runs).
  expect_equal(
    app$get_value(input = "ggplot_0_ppUpload_NA_shortcut"),
    "mtcars",
    label = "companion id seeded at boot from spec= (no default on node)"
  )
  # And the binder picks it up post-ADR-0024.
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "cyl")
  expect_no_inline_error(app, "ptr_error")
})

# ---- Scenario D: shared-section variant ----

test_that("adr0024: shared-section ppUpload companion entry point honored at boot", {
  app <- boot_vignette_app("adr24-companion-shared-spec-no-default")
  app$wait_for_idle(timeout = 15 * 1000)

  # Shared-section canonical companion id is `shared_<k>_name` (no module
  # ns prefix at host scope). spec= dispatches the same source_companion
  # role; the bind path is the per-instance pipeline-head loop running
  # inside the formula's moduleServer (single-formula = formula-local).
  expect_equal(
    app$get_value(input = "shared_ds_shortcut"),
    "mtcars",
    label = "shared companion id seeded at boot from spec="
  )
  expect_picker_populated(app, "shared_col", "cyl")
  expect_no_inline_error(app, "ptr_error")
})

# ---- Unit-level: try_bind_source_default_resolved widened guard ----

test_that("adr0024: try_bind_source_default_resolved binds without default when companion is set", {
  # Build a stub state with eval_env carrying mtcars; bind_source_value
  # writes assign-before-signal — state$eval_env gets the symbol AND
  # state$bound_names()[[key]] fires the reactive. We drive the helper
  # directly with a plain reactiveVal slot.
  eval_env <- new.env(parent = emptyenv())
  assign("mtcars", mtcars, envir = eval_env)

  resolve_errors_store <- shiny::reactiveVal(stats::setNames(list(), character()))
  # `state$bound_names` is a NAMED LIST of reactiveVals (one per layer
  # name / source id), built by ptr_init_state at R/paintr-server.R:187-190.
  # bind_source_value() at L877 calls `state$bound_names[[key]](name)`,
  # so each key needs its own reactiveVal slot.
  bound_names_store <- list(k = shiny::reactiveVal(NULL),
                            k2 = shiny::reactiveVal(NULL),
                            k3 = shiny::reactiveVal(NULL),
                            k4 = shiny::reactiveVal(NULL))

  state <- list(
    eval_env       = eval_env,
    resolve_errors = resolve_errors_store,
    bound_names    = bound_names_store
  )
  slot <- shiny::reactiveVal(NULL)
  node <- list(default = NULL, keyword = "ppUpload")
  entry <- ggpaintr:::ptr_registry_lookup("ppUpload")

  # Wrap in isolate so reactiveVal writes inside bind_source_value don't
  # require a real reactive context.
  ok <- shiny::isolate(
    ggpaintr:::try_bind_source_default_resolved(
      state, key = "k", node = node,
      has_shortcut = TRUE, shortcut_value = "mtcars",
      entry = entry, slot = slot
    )
  )
  expect_true(ok, label = "post-ADR-0024 bind succeeds with companion-only (no default)")
  expect_true(is.data.frame(shiny::isolate(slot())))
  expect_equal(nrow(shiny::isolate(slot())), nrow(mtcars))
})

test_that("adr0024: try_bind_source_default_resolved records error when name not found", {
  eval_env <- new.env(parent = emptyenv())
  # mtcars NOT in env (parent is emptyenv, inherits=TRUE walks empty chain)
  resolve_errors_store <- shiny::reactiveVal(stats::setNames(list(), character()))
  bound_names_store   <- shiny::reactiveVal(stats::setNames(list(), character()))
  state <- list(
    eval_env       = eval_env,
    resolve_errors = resolve_errors_store,
    bound_names    = bound_names_store
  )
  slot <- shiny::reactiveVal(NULL)
  node <- list(default = NULL, keyword = "ppUpload")
  entry <- ggpaintr:::ptr_registry_lookup("ppUpload")

  ok <- shiny::isolate(
    ggpaintr:::try_bind_source_default_resolved(
      state, key = "k2", node = node,
      has_shortcut = TRUE, shortcut_value = "fooberry",
      entry = entry, slot = slot
    )
  )
  expect_false(ok)
  errs <- shiny::isolate(resolve_errors_store())
  expect_true("k2" %in% names(errs))
  expect_match(errs[["k2"]], "fooberry", fixed = TRUE)
  expect_match(errs[["k2"]], "not found", fixed = TRUE)
})

test_that("adr0024: try_bind_source_default_resolved records error when name is not a data frame", {
  eval_env <- new.env(parent = emptyenv())
  assign("not_a_df", 1:10, envir = eval_env)
  resolve_errors_store <- shiny::reactiveVal(stats::setNames(list(), character()))
  bound_names_store   <- shiny::reactiveVal(stats::setNames(list(), character()))
  state <- list(
    eval_env       = eval_env,
    resolve_errors = resolve_errors_store,
    bound_names    = bound_names_store
  )
  slot <- shiny::reactiveVal(NULL)
  node <- list(default = NULL, keyword = "ppUpload")
  entry <- ggpaintr:::ptr_registry_lookup("ppUpload")

  ok <- shiny::isolate(
    ggpaintr:::try_bind_source_default_resolved(
      state, key = "k3", node = node,
      has_shortcut = TRUE, shortcut_value = "not_a_df",
      entry = entry, slot = slot
    )
  )
  expect_false(ok)
  errs <- shiny::isolate(resolve_errors_store())
  expect_true("k3" %in% names(errs))
  expect_match(errs[["k3"]], "not_a_df", fixed = TRUE)
  expect_match(errs[["k3"]], "not a data frame", fixed = TRUE)
})

test_that("adr0024: try_bind_source_default_resolved still bails when neither default nor companion are set", {
  eval_env <- new.env(parent = emptyenv())
  resolve_errors_store <- shiny::reactiveVal(stats::setNames(list(), character()))
  bound_names_store   <- shiny::reactiveVal(stats::setNames(list(), character()))
  state <- list(
    eval_env       = eval_env,
    resolve_errors = resolve_errors_store,
    bound_names    = bound_names_store
  )
  slot <- shiny::reactiveVal(NULL)
  node <- list(default = NULL, keyword = "ppUpload")
  entry <- ggpaintr:::ptr_registry_lookup("ppUpload")

  ok <- shiny::isolate(
    ggpaintr:::try_bind_source_default_resolved(
      state, key = "k4", node = node,
      has_shortcut = TRUE, shortcut_value = "",
      entry = entry, slot = slot
    )
  )
  expect_false(ok)
  # No error: nothing to attempt -> nothing to error about (silent bail
  # is the right semantics here; the widget is just unconfigured).
  errs <- shiny::isolate(resolve_errors_store())
  expect_false("k4" %in% names(errs))
})
