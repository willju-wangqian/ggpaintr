# test-vacate-on-empty.R -- ADR 0025 §7 / PLAN-05 (A1) vacate-on-empty.
#
# Two layers of coverage:
#  (1) Unit test on `vacate_source_binding()` directly — proves the BDD
#      `Then` clauses (state$bound_names NULL, eval_env binding removed,
#      state$active_uploads cleared) by driving the helper with a hand-
#      built state. Avoids the shinytest2 wrapped-namespace fragility.
#  (2) Browser-level observable test — proves the no-file branch reaches
#      vacate AND the cleared state flows to user-visible surfaces (code
#      panel prologue line disappears after clearing the shortcut box).

test_that("vacate_source_binding() clears bound_names, eval_env, and active_uploads", {
  # Hand-build a state shape matching what ptr_server_internal builds.
  eval_env <- new.env(parent = emptyenv())
  eval_env$my_data <- data.frame(x = 1:3)
  bn_slot <- shiny::reactiveVal("my_data")
  state <- list(
    eval_env = eval_env,
    bound_names = list(my_key = bn_slot),
    active_uploads = shiny::reactiveVal(
      stats::setNames(list(list(auto_name = "my_data", file_name = "f.csv",
                                ext = "csv")), "my_key")
    )
  )

  # Pre-vacate sanity.
  expect_true(exists("my_data", envir = eval_env, inherits = FALSE))
  expect_identical(shiny::isolate(bn_slot()), "my_data")
  expect_equal(names(shiny::isolate(state$active_uploads())), "my_key")

  # Drive the helper.
  shiny::isolate(ggpaintr:::vacate_source_binding(state, "my_key"))

  # (1) eval_env binding rm'd.
  expect_false(exists("my_data", envir = eval_env, inherits = FALSE))
  # (2) bound_names reset to NULL.
  expect_null(shiny::isolate(bn_slot()))
  # (3) active_uploads slot cleared.
  expect_equal(length(shiny::isolate(state$active_uploads())), 0L)
})

test_that("vacate_source_binding() is idempotent on an already-vacated key", {
  state <- list(
    eval_env = new.env(parent = emptyenv()),
    bound_names = list(),  # key absent entirely
    active_uploads = shiny::reactiveVal(stats::setNames(list(), character()))
  )
  # Should not error; should not raise; should return invisible NULL.
  expect_silent(shiny::isolate(ggpaintr:::vacate_source_binding(state, "absent")))
})

test_that("clearing shortcut textbox drops the code-panel prologue line (observable end-to-end)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("vacate-on-empty")

  src_id      <- "ggplot_0_ppUpload_NA"
  shortcut_id <- "ggplot_0_ppUpload_NA_shortcut"
  expect_dom_id(app, src_id)
  expect_dom_id(app, shortcut_id)

  # Step 1: upload a csv. resolve_upload_source binds + registers
  # active_uploads + (on draw) emits a prologue line.
  csv_path <- testthat::test_path("fixtures", "mtcars.csv")
  upload_file(app, ggplot_0_ppUpload_NA = csv_path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  code_after_upload <- app$get_value(output = "ptr_code")
  expect_match(
    code_after_upload,
    "^[A-Za-z0-9_.]+ <- read\\.csv\\(",
    label = "code panel leads with a prologue line after upload"
  )

  # Step 2: type a non-empty name into the shortcut textbox. ADR 0025
  # item #7: this is the shortcut rising edge -> the source uiOutput
  # re-renders (the file pill clears) AND the `state$source_file_reset`
  # flag for this source is set TRUE. resolve_upload_source then fires
  # with the shortcut active ("zzz"), so file_info is forced NULL; the
  # prior active_uploads entry is cleared and try_bind_source_default
  # fails to resolve "zzz" -- so no prologue, but no vacate yet (the
  # textbox is non-empty).
  set_input(app, shortcut_id, "zzz")
  app$wait_for_idle(timeout = 15 * 1000)
  app$wait_for_idle(timeout = 15 * 1000)

  # Step 3: clear the shortcut textbox to "". The `file_reset` flag is
  # still TRUE (no new file picked since the re-render), so
  # resolve_upload_source treats file_info as NULL even though the stale
  # server-side fileInput value persists; shortcut is now empty ->
  # shortcut_is_empty TRUE -> vacate_source_binding() fires.
  set_input(app, shortcut_id, "")
  app$wait_for_idle(timeout = 15 * 1000)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  # Code panel has NO prologue line: the active_uploads slot was cleared
  # by vacate_source_binding(). This is the load-bearing observable
  # downstream of the mechanism the BDD `Then` clauses pin in state.
  code_after_vacate <- app$get_value(output = "ptr_code")
  expect_true(
    is.character(code_after_vacate) && length(code_after_vacate) == 1L,
    label = "code panel returns a single string post-vacate"
  )
  expect_false(
    grepl("read\\.csv\\(", code_after_vacate),
    label = "code panel has no read.csv() prologue line after clearing textbox"
  )
})
