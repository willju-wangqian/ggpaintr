# test-adr25-shortcut-env-load-drops-upload.R
#
# Regression for the ADR 0025 §2/§3 "shortcut is the live affordance, upload is
# dropped" contract -- the fix for the F3 eval-env pollution that the §7-A2
# debounce race reincarnated.
#
# The live bug (pre-fix, on the frontier): upload a file to a ppUpload source,
# then TYPE a real env-frame name into its shortcut textbox. The resolver saw
# `file=PRESENT` and `shortcut="<name>"` in the same flush and `assign()`ed the
# UPLOADED frame into eval_env under `<name>` (pollution); the later no-file
# re-resolve then `get()`'d the polluted frame instead of the env frame. So the
# pickers stayed on the uploaded columns and the env frame never loaded.
#
# Fixed by gating `resolve_upload_source()` (R/paintr-server.R): a non-empty
# shortcut means the textbox was last-touched (the Q3-B mutex blanks it on every
# upload), so the lingering file is ignored and the env-load path runs. The
# eval side (`substitute_walk.ptr_ph_data_source`) keys off the same
# shortcut-non-empty condition, so bind and eval stay consistent.
#
# Covers BOTH scopes:
#   * non-shared layer-data ppUpload  (super-2b fixture, geom_rug)
#   * panel-owned shared ppUpload     (shared-source-panel-multi fixture)
# plus the empty-shortcut upload binding under the canonical auto-name
# (ADR 0025 §3 shared rows; the panel-owned path-4 caveat) and the typo path
# (a non-existent name surfaces an error and drops the upload, never clobbers
# silently with the uploaded data).

# ---------------------------------------------------------------------------
# Non-shared: upload penguins -> type a real env frame (iris) -> iris loads
# ---------------------------------------------------------------------------
test_that("non-shared: typing a real env-frame shortcut after an upload loads the env frame, drops the upload", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_super_app("super-2b-customsource-splice")
  # Boot-tail: wait for the layer-data fileInput's Shiny binding to wire up
  # before uploading (renderUI-vs-bindAll race, project memory
  # `shinytest2-boot-tail-race-wait-for-idle`).
  for (i in seq_len(30)) {
    h <- app$get_html("#geom_rug_0_ppUpload_NA") %||% ""
    if (grepl("shiny-bound-input", h, fixed = TRUE)) break
    app$wait_for_idle(timeout = 2000)
  }

  app$upload_file(geom_rug_0_ppUpload_NA = testthat::test_path("fixtures", "penguins.csv"))
  app$wait_for_idle(timeout = 15 * 1000)
  # `iris` is always attached (datasets); ppSample's resolver also makes it
  # resolvable. Type it into the layer's shortcut textbox.
  set_sentinel(app, "geom_rug_0_ppUpload_NA_shortcut", "iris")
  app$wait_for_idle(timeout = 15 * 1000)
  set_input(app, "geom_rug_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)

  px <- app$get_html("#geom_rug_1_1_ppVar_NA") %||% ""
  # iris loaded: its columns are offered.
  expect_true(grepl("Sepal.Length", px, fixed = TRUE),
              label = "geom_rug picker offers iris column 'Sepal.Length'")
  # the uploaded penguins frame was dropped, NOT bound under 'iris'.
  expect_false(grepl("bill_length_mm", px, fixed = TRUE),
               label = "geom_rug picker does NOT offer penguins column 'bill_length_mm'")
})

# ---------------------------------------------------------------------------
# Shared/panel-owned: upload penguins -> type a real env frame (mtcars)
# ---------------------------------------------------------------------------
test_that("panel-owned shared: typing a real env-frame shortcut after an upload loads the env frame, drops the upload", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "panel-owned env-load e2e needs the package source root; absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "shared-source-panel-multi")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir, name = "adr25-shared-env-load-drops-upload",
      load_timeout = 60 * 1000, timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 25 * 1000)

  app$upload_file(shared_ds = test_path("fixtures", "penguins.csv"))
  app$wait_for_idle(timeout = 15 * 1000)
  # `mtcars` is always attached; type it into the panel's shared shortcut.
  set_input(app, "shared_ds_shortcut", "mtcars")
  # The shared mutex reads the raw (un-debounced) shortcut input; a brief
  # settle keeps the assertion off the boot/clear tail.
  Sys.sleep(0.6)
  app$wait_for_idle(timeout = 15 * 1000)

  h <- app$get_html("#shared_col") %||% ""
  expect_true(grepl("mpg", h, fixed = TRUE),
              label = "#shared_col offers mtcars column 'mpg'")
  expect_true(grepl("cyl", h, fixed = TRUE),
              label = "#shared_col offers mtcars column 'cyl'")
  expect_false(grepl("bill_length_mm", h, fixed = TRUE),
               label = "#shared_col does NOT offer penguins column 'bill_length_mm'")
})

# ---------------------------------------------------------------------------
# Typo: a non-existent shortcut name surfaces an error and drops the upload
# (it must NOT silently keep the uploaded data).
# ---------------------------------------------------------------------------
test_that("panel-owned shared: a typo'd shortcut name surfaces an error and drops the upload (no silent clobber)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "panel-owned typo e2e needs the package source root; absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "shared-source-panel-multi")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir, name = "adr25-shared-typo-drops-upload",
      load_timeout = 60 * 1000, timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 25 * 1000)

  app$upload_file(shared_ds = test_path("fixtures", "penguins.csv"))
  app$wait_for_idle(timeout = 15 * 1000)
  set_input(app, "shared_ds_shortcut", "no_such_frame_xyz")
  Sys.sleep(0.6)
  app$wait_for_idle(timeout = 15 * 1000)

  h <- app$get_html("#shared_col") %||% ""
  # the uploaded penguins columns must NOT silently persist under the typo.
  expect_false(grepl("bill_length_mm", h, fixed = TRUE),
               label = "#shared_col does NOT silently keep the uploaded penguins data on a typo")
  # a structured "object not found" error surfaces somewhere in the panel.
  err <- tryCatch(
    paste(app$get_text(".shiny-output-error, #ptr_shared_errors, [id$=ptr_error]"),
          collapse = " | "),
    error = function(e) ""
  )
  expect_true(grepl("not found", err, fixed = TRUE),
              label = paste0("an 'object not found' error surfaces for the typo; actual=", err))
})

# ---------------------------------------------------------------------------
# Empty-shortcut upload binds under the CANONICAL auto-name (panel-owned
# path-4 / caveat-2): no name typed -> the uploaded columns populate the
# host-scope shared consumer picker.
# ---------------------------------------------------------------------------
test_that("panel-owned shared: an empty-shortcut upload binds under the canonical auto-name and populates the consumer picker", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "panel-owned canonical-bind e2e needs the package source root; absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "shared-source-panel-multi")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir, name = "adr25-shared-canonical-bind",
      load_timeout = 60 * 1000, timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 25 * 1000)

  # Upload and DO NOT type a shortcut -> ADR 0025 §3: the upload binds under
  # the canonical auto-name `ds`. Pre-fix (caveat-2) the panel-owned binder
  # returned NULL here (node$auto_name is unstamped on the per-instance /
  # consumer nodes) and the picker stayed empty.
  app$upload_file(shared_ds = test_path("fixtures", "penguins.csv"))
  app$wait_for_idle(timeout = 15 * 1000)

  h <- app$get_html("#shared_col") %||% ""
  for (col in c("species", "bill_length_mm", "body_mass_g")) {
    expect_true(grepl(col, h, fixed = TRUE),
                label = paste0("#shared_col offers uploaded column '", col, "'"))
  }
})
