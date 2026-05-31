# test-shared-host-source-reset.R -- ADR 0025 item #7 FOLLOW-UP.
#
# #7 made the source-shortcut a framework-owned STATIC textInput and re-renders
# only the fileInput on the shortcut's rising edge (empty -> non-empty),
# clearing the stale filename pill, with a `source_file_reset` flag driving the
# env-load gate + vacate-on-empty. That landed for the SINGLE-INSTANCE source
# path (`ptr_setup_source_uis`) only. This file is the regression for porting
# the same machinery to the multi-instance shared-coordinator HOST source path
# (`ptr_setup_panel_sources`, R/paintr-shared-ui.R) and the attended host
# consumer clear (`ptr_bind_shared_consumer_uis`, host scope state = NULL).
#
# Harness note (plan dev/plans/0025-item7-followup-host-shared-consumer-clear,
# §7): the fileInput filename PILL is rendered client-side by the browser's
# native file widget; chromote/CDP get_html cannot observe it. P1 (pill clear)
# is therefore eyeballed, NOT asserted here. What IS harness-observable -- and
# carried by this file -- is the data side: the env-load gate, the vacate, and
# the consumer-picker clear/suppress.
#
# Protocol: project memory `shinytest2-appdir-pkgload` + helper-vignette-apps.R
# (`boot_vignette_app` = skip guards + GGP_PKG + AppDriver + wait_for_idle +
# defer). Never app$get_values(). set_input/upload_file wait for the binding;
# wait_for_idle after every set. The shortcut rising edge + the resolve path
# are debounced 400ms, so `type_shortcut()` sleeps past the window then re-idles
# (same ordering caution as test-shared-source-panel-multi-instance.R #4).

# Set the host shortcut textbox and settle past the 400ms rising-edge / resolve
# debounce so the bump + file_reset + re-resolve have all fired before asserting.
type_shortcut <- function(app, id, value) {
  set_input(app, id, value)
  Sys.sleep(0.6)
  app$wait_for_idle(timeout = 15 * 1000)
}

# ---------------------------------------------------------------------------
# P3 env-load gate (host): a typed shortcut resolves the env frame at host
# scope -- the picker offers the typed dataset's columns. (Regression guard:
# green before & after; the shortcut_active branch already forces env-load, so
# this protects that invariant while the rest of the file changes the path.)
# ---------------------------------------------------------------------------
test_that("host: typing an env-frame shortcut resolves it (env-load gate)", {
  app <- boot_vignette_app("adr25-host-shared-source-reset")

  expect_dom_id(app, "shared_ds")
  expect_dom_id(app, "shared_ds_shortcut")

  type_shortcut(app, "shared_ds_shortcut", "typed_ds")
  # `grp` is unique to typed_ds -> proves the env frame loaded at host scope.
  expect_picker_populated(app, "shared_col", "grp")
})

# ---------------------------------------------------------------------------
# P3 vacate (host): upload -> type a shortcut over it -> CLEAR the shortcut.
# With the file_reset flag wired, clearing the shortcut vacates the source; the
# stale lingering fileInput value is ignored. WITHOUT it (baseline) the cleared
# shortcut falls back to the still-present uploaded file and the picker
# resurrects the upload's columns -- the discriminating assertion below.
# ---------------------------------------------------------------------------
test_that("host: clearing the shortcut after a typed-over upload vacates the source", {
  app <- boot_vignette_app("adr25-host-shared-source-reset")
  csv <- test_path("fixtures", "penguins.csv")

  upload_file(app, shared_ds = csv)
  app$wait_for_idle(timeout = 15 * 1000)
  # `species` is penguins-only -> proves the upload resolved.
  expect_picker_populated(app, "shared_col", "species")

  # Type a real env frame over the upload (env-load gate forces the typed
  # source; the mutex blanks nothing here since we type, not pick).
  type_shortcut(app, "shared_ds_shortcut", "typed_ds")
  expect_picker_populated(app, "shared_col", "grp")

  # Clear the shortcut to empty. file_reset holds -> file_info stays NULL ->
  # vacate-on-empty. Baseline (no flag): file_info reverts to the stale penguins
  # upload and `species` reappears.
  type_shortcut(app, "shared_ds_shortcut", "")
  shared_col_html <- app$get_html("#shared_col") %||% ""
  expect_false(
    grepl("species", shared_col_html, fixed = TRUE),
    label = "after clearing the shortcut the vacated source no longer offers the stale upload column 'species'"
  )
})

# ---------------------------------------------------------------------------
# S-N1 (attended consumer clear): type env frame -> select an OVERLAPPING
# column -> upload a CSV that also has that column. The host consumer picker
# must CLEAR (new source = new work), surviving the upload's trailing
# double-render. Baseline rides the stale pick (probe-verified). Overlapping
# column (body_mass_g) is the load-bearing proxy-trap guard.
# ---------------------------------------------------------------------------
test_that("host: uploading over a typed source clears the consumer picker (overlapping column)", {
  app <- boot_vignette_app("adr25-host-shared-source-reset")
  csv <- test_path("fixtures", "penguins.csv")

  type_shortcut(app, "shared_ds_shortcut", "typed_ds")
  expect_picker_populated(app, "shared_col", "body_mass_g")

  set_input(app, "shared_col", "body_mass_g")
  app$wait_for_idle(timeout = 10 * 1000)
  expect_equal(app$get_value(input = "shared_col"), "body_mass_g",
               label = "pre-upload pick is body_mass_g")

  upload_file(app, shared_ds = csv)
  app$wait_for_idle(timeout = 15 * 1000)
  # Settle past the resolve debounce + the trailing double-render.
  Sys.sleep(0.5)
  app$wait_for_idle(timeout = 15 * 1000)

  sel <- app$get_value(input = "shared_col")
  expect_true(
    is.null(sel) || length(sel) == 0L || identical(sel, ""),
    label = paste0("host consumer picker cleared after upload-over-typed ",
                   "(got: ", deparse(sel), "); body_mass_g exists in BOTH ",
                   "datasets so this is not a selectInput auto-drop")
  )

  # The picker still offers the new dataset's columns (cleared != broken) and
  # both plots still draw once the user re-picks.
  expect_picker_populated(app, "shared_col", "species")
  set_input(app, "shared_col", "bill_length_mm")
  draw(app, "p1-ptr_update_plot")
  draw(app, "p2-ptr_update_plot")
  expect_no_inline_error(app, "p1-ptr_error")
  expect_no_inline_error(app, "p2-ptr_error")
})

# ---------------------------------------------------------------------------
# S-N2 (vacate-suppress): after a column is selected over a typed source,
# clearing the source (P3 vacate) leaves no stale pick on the picker.
# ---------------------------------------------------------------------------
test_that("host: vacating the source leaves no stale consumer pick", {
  app <- boot_vignette_app("adr25-host-shared-source-reset")

  type_shortcut(app, "shared_ds_shortcut", "typed_ds")
  expect_picker_populated(app, "shared_col", "body_mass_g")
  set_input(app, "shared_col", "body_mass_g")
  app$wait_for_idle(timeout = 10 * 1000)
  expect_equal(app$get_value(input = "shared_col"), "body_mass_g")

  type_shortcut(app, "shared_ds_shortcut", "")  # vacate
  sel <- app$get_value(input = "shared_col")
  expect_true(
    is.null(sel) || length(sel) == 0L || identical(sel, ""),
    label = paste0("vacated source suppresses the stale consumer pick (got: ",
                   deparse(sel), ")")
  )
})

# ---------------------------------------------------------------------------
# W1 (single-instance guard): the single-instance source path
# (`ptr_setup_source_uis` / `ptr_setup_consumer_uis`) is untouched by this
# change (the host edits are in `ptr_setup_panel_sources` and the host-scope,
# state==NULL branch of `ptr_bind_shared_consumer_uis`). Re-assert the
# verified single-instance upload-clear here so a regression surfaces in THIS
# file's gate, not only the full suite. Re-uses the known-good
# adr25-upload-default-clear fixture (ppUpload(mtcars) + ppVar default `mpg`).
# ---------------------------------------------------------------------------
test_that("W1: single-instance upload-clear is unchanged", {
  app <- boot_vignette_app("adr25-upload-default-clear")

  # Boot over the env-default frame seeds the formula default `mpg`.
  expect_input_eventually(app, "geom_point_1_1_ppVar_NA", "mpg")

  # A real upload is new data -> the boot default must not carry.
  up <- test_path("fixtures", "vignette-apps", "adr25-upload-default-clear",
                  "up.csv")
  upload_file(app, geom_point_0_ppUpload_NA = up)
  app$wait_for_idle(timeout = 15 * 1000)
  after <- app$get_value(input = "geom_point_1_1_ppVar_NA")
  expect_true(
    is.null(after) || length(after) == 0L || identical(after, ""),
    label = paste0("single-instance consumer picker cleared after upload ",
                   "(got: ", deparse(after), ")")
  )
})
