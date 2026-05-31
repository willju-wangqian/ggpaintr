# ADR 0024 — source companion as data-loading entry point. Promotes the
# companion textInput beside ppUpload's fileInput (and any custom source's
# input emitted at node$shortcut_id) from "name override for an uploaded
# frame" to "typed-in shortcut for loading a data.frame from envir."
#
# Pre-ADR-0024 the binder helpers
#   try_bind_source_default        (R/paintr-server.R:1106)
#   try_bind_source_default_resolved (R/paintr-server.R:1323)
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
  # input-bound observer fires; resolve_upload_source (R/paintr-server.R:1214)
  # sees file_info NULL ->
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
  # R/paintr-server.R:3128 + the .claude/rules/testing.md
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
  # calls `set_resolve_error(NULL)` at R/paintr-server.R:1220 before retrying the bind).
  set_input(app, "ggplot_0_ppUpload_NA_shortcut", "mtcars")
  app$wait_for_js(
    paste0("(function(){var e=document.getElementById('ptr_error');",
           "return !!e && e.innerHTML.indexOf('ptr-alert--error')===-1;})()"),
    timeout = 15 * 1000
  )
  expect_no_inline_error(app, "ptr_error")
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "cyl")
})

# ---- Scenario C2: typed name resolves but isn't a data frame ----
# Covers the "Object 'X' is not a data frame." branch in
# try_bind_source_default_resolved that test-adr0024-companion-entry-point.R:151
# pinned at the unit level. `letters` is a base-R character vector;
# get(name, envir = eval_env, inherits = TRUE) resolves it via baseenv on
# the search chain, but the is.data.frame() guard fails so the resolve
# observer records `Object 'letters' is not a data frame.` into
# state$resolve_errors -- the same renderUI path #ptr_error pane re-renders
# off (see Scenario C above for the mechanism).

test_that("adr0024: typed name resolving to a non-data-frame surfaces inline error", {
  app <- boot_vignette_app("adr24-companion-typed")
  app$wait_for_idle(timeout = 15 * 1000)

  set_input(app, "ggplot_0_ppUpload_NA_shortcut", "letters")
  app$wait_for_js(
    paste0("(function(){var e=document.getElementById('ptr_error');",
           "return !!e && e.innerHTML.indexOf('letters')!==-1;})()"),
    timeout = 15 * 1000
  )

  err_html <- app$get_html("#ptr_error") %||% ""
  expect_match(err_html, "letters", fixed = TRUE,
               label = "inline error names the typed non-df binding")
  expect_match(err_html, "not a data frame", fixed = TRUE,
               label = "inline error explains the not-a-data-frame branch")
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
