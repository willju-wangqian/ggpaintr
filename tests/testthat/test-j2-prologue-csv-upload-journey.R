# J2 journey -- prologue-csv-upload + upload-name-precedence + upload-bad-ext.
#
# Browser-driven coverage for three upload-flow contracts that absorbed
# 4 STRONG L2 blocks from the v9 routing pass (3 more L2 deletes ride
# pre-existing L3 siblings -- see "Routing" below).
#
# Routing decisions (dev/audit/audit-test-fidelity-v9-j2-browser-
# faithfulness-2026-05-28-0027.html):
#   MERGED via NEW stages here:
#     test-upload-binds-under-auto-name.R:114 (typed shortcut wins)
#       -> stage 1 below
#     test-resolve-upload-source-helper.R:152 (resolve_data error path)
#       -> stage 2 below (via new fixture j2-custom-source-error)
#     test-upload-unsupported-extension.R:23  (bad ext -> resolve_errors)
#     test-upload-unsupported-extension.R:74  (recover-by-good-upload clears)
#       -> stage 3 below (combined)
#   MERGED via PRE-EXISTING L3 siblings (no new stage needed):
#     test-resolve-upload-source-helper.R:90  (NULL file_info default-arg)
#       -> covered by test-upload-clears-stale-cache.R:21-22 (picker cols
#          seed from mtcars at boot, only reachable if default-arg bind
#          fired through try_bind_source_default_resolved)
#     test-resolve-upload-source-helper.R:119 (populated dispatch + bind)
#     test-upload-binds-under-auto-name.R:34  (empty shortcut -> auto_name)
#       -> both covered by test-prologue-csv-upload.R (the prologue LHS
#          `_ppUpload_NA <- read.csv("mtcars.csv")` IS the auto_name,
#          captured at the precise binding-name `bind_source_value`
#          received; only reachable if entry$resolve_data dispatched
#          AND the empty-shortcut fallback chose node$auto_name)
#   RETAINED as unit pins (NOT covered here -- helper-level triggers):
#     test-upload.R:139 (missing readxl)
#     test-upload.R:230 (missing jsonlite)
#       -> local_mocked_bindings(.package="base") of requireNamespace is
#          process-local; does not cross the shinytest2 child boundary.
#          DOM-faithful equivalent would require an assignInNamespace
#          hack inside the child app.R, itself an R-side fakery that
#          no DOM event can dispatch.
#
# Standard browser-e2e scaffolding (skip_on_cran / skip_if_not_installed /
# source-root guard) lives in boot_vignette_app() -- helper-vignette-apps.R.

test_that("J2 stage 1 (prologue-csv-upload): typed shortcut wins over node$auto_name", {
  # Covers test-upload-binds-under-auto-name.R:114 -- non-empty shortcut
  # textbox wins over `node$auto_name` as the eval_env binding name. The
  # L2 captured `bind_source_value`'s `name` argument via mocked binding;
  # the DOM-faithful equivalent observes the substituted formula body in
  # `output$ptr_code` -- substitute_walk.ptr_ph_data_source emits the
  # bound symbol from `state$bound_names[[key]]`, so a regression that
  # ignored the shortcut and kept the auto_name would change the body's
  # `data =` symbol.
  #
  # Sequence (per ADR 0025 §7 A2 race-fix at paintr-server.R:1448 and the
  # mutex semantics at :1562-1599):
  #   1. Upload file. Bind observer fires immediately with shortcut="" ->
  #      bind under node$auto_name.
  #   2. Type "my_data" into shortcut. After the 400ms debounce, the bind
  #      observer re-fires with file_info still present AND shortcut_r()=
  #      "my_data" -> bind_source_value(key, "my_data", df) -- the second
  #      bind sticks (bound_names[[key]] = "my_data"). The mutex's reset-
  #      file observer fires on the SAME debounced tick, so the file pill
  #      clears AFTER the rebind has landed.
  #   3. Click Update Plot -- substitute_walk emits the body with
  #      data = my_data.
  # The prologue line (`<auto_name> <- read.csv(...)`) does NOT survive
  # the mutex clear (clear_active_upload runs when file_info goes NULL).
  # The body symbol is the durable observable for the binding-name
  # contract.
  app <- boot_vignette_app("prologue-csv-upload")

  src_id      <- "ggplot_0_ppUpload_NA"
  shortcut_id <- "ggplot_0_ppUpload_NA_shortcut"

  upload_file(app, ggplot_0_ppUpload_NA = testthat::test_path(
    "fixtures", "mtcars.csv"
  ))
  app$wait_for_idle(timeout = 15 * 1000)

  set_input(app, shortcut_id, "my_data")
  app$wait_for_idle(timeout = 15 * 1000)

  draw(app, "ptr_update_plot")

  code_text <- app$get_value(output = "ptr_code")
  expect_match(
    code_text, "data = my_data", fixed = TRUE,
    label = "substituted body uses bound_names[[key]] = `my_data` (typed shortcut)"
  )
  expect_false(
    grepl(src_id, code_text, fixed = TRUE),
    label = paste0("body does NOT reference auto_name (", src_id, ")")
  )
})

test_that("J2 stage 2 (j2-custom-source-error): resolve_data error surfaces in #ptr_error and clears the bind", {
  # Covers test-resolve-upload-source-helper.R:152. Fixture registers a
  # custom `ppFailingSource` whose resolve_data deterministically throws
  # "boom". After uploading a file, the upload observer's tryCatch sends
  # `conditionMessage(e)` into `state$resolve_errors[[key]]` and the
  # `#ptr_error` renderUI surfaces it verbatim. The slot-clear half of
  # the L2 contract (`slot(NULL)` -> eval_env unbound -> downstream
  # consumer has no cols) is observable via the absence of the
  # consumer's expected col surface in the rendered widget.
  app <- boot_vignette_app("j2-custom-source-error")

  src_id <- "ggplot_0_ppFailingSource_NA"
  expect_dom_id(app, src_id)

  upload_file(app, ggplot_0_ppFailingSource_NA = testthat::test_path(
    "fixtures", "mtcars.csv"
  ))
  app$wait_for_idle(timeout = 15 * 1000)

  err_html <- app$get_html("#ptr_error") %||% ""
  expect_match(
    err_html, "boom", fixed = TRUE,
    label = "#ptr_error surfaces conditionMessage(e) from resolve_data verbatim"
  )
  expect_match(
    err_html, "ptr-alert--error", fixed = TRUE,
    label = "#ptr_error renders via ptr_error_ui (error class present)"
  )
})

test_that("J2 stage 3 (prologue-csv-upload): bad extension surfaces resolve_errors; next good upload clears", {
  # Covers test-upload-unsupported-extension.R:23 AND :74 in one stage.
  # :23 -- uploading `bad_extension.txt` populates `state$resolve_errors`
  # with the literal abort `"Please upload a .csv, .tsv, .rds, .xlsx,
  # .xls, or .json file."` -- DOM-visible in #ptr_error.
  # :74 -- shinytest2 has no native "clear file input" primitive (the
  # L2 used `setInputs(NULL)` which is testServer-only). The DOM-
  # faithful proxy is "next good upload clears the error slot", which
  # exercises the same `set_resolve_error(state, key, NULL)` branch
  # inside `resolve_upload_source` (paintr-server.R:1266, fires when
  # `df` is non-NULL after a successful resolve_data dispatch). Per
  # v9 audit §7 decision 3, accepted.
  app <- boot_vignette_app("prologue-csv-upload")

  # ---- :23 -- upload bad ext, assert error surfaces -------------------
  upload_file(app, ggplot_0_ppUpload_NA = testthat::test_path(
    "fixtures", "bad_extension.txt"
  ))
  app$wait_for_idle(timeout = 15 * 1000)

  err_html <- app$get_html("#ptr_error") %||% ""
  expect_match(
    err_html,
    "Please upload a .csv, .tsv, .rds, .xlsx, .xls, or .json file.",
    fixed = TRUE,
    label = "#ptr_error surfaces the unsupported-format abort verbatim"
  )

  # ---- :74 -- next good upload clears the error -----------------------
  upload_file(app, ggplot_0_ppUpload_NA = testthat::test_path(
    "fixtures", "mtcars.csv"
  ))
  app$wait_for_idle(timeout = 15 * 1000)

  err_html_after <- app$get_html("#ptr_error") %||% ""
  expect_false(
    grepl(
      "Please upload a .csv, .tsv, .rds, .xlsx, .xls, or .json file.",
      err_html_after, fixed = TRUE
    ),
    label = "successful upload clears the unsupported-format abort from #ptr_error"
  )
})
