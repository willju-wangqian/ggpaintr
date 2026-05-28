# test-adr15-consumer-binding.R — shinytest2 driver for ADR 0015 PLAN-01.
# Closes the UX defect where consumer pickers nested under a data-source
# placeholder (ppUpload, ppSample, …) silently failed to bind unless the
# user navigated to the layer's inner Controls subtab.
#
# Two failure paths the source fix covers:
#   (1) Non-shared layer-aes ppVar under a ppUpload layer-data: the
#       picker's renderUI was suspended on visibility behind the default
#       "Data" subtab (ptr_setup_consumer_uis pre-warm gate).
#   (2) Shared ppVar(shared = "<k>") under a ppUpload layer-data: the
#       shared binder's renderUI fired but its dep set missed
#       state$resolved_data, so resolved_data invalidations never
#       re-rendered the picker after the file landed.
#
# Both scenarios assert the picker is populated WITHOUT any
# set_inputs() call targeting an "_subtab" input. SC-9's third block
# pins the unrelated no-source-upstream pre-warm path so the new req()
# guard doesn't regress today's working behavior.

# Companion ids in the adr15-consumer-binding fixture:
#   - geom_point_0_ppUpload_NA           : the upload file-input
#   - geom_point_0_ppUpload_NA_shortcut      : the dataset-name companion
#   - geom_point_1_1_ppVar_NA            : non-shared ppVar(mpg) at x
#   - shared_v                           : shared ppVar(shared = "v") at y

test_that("adr15: non-shared ppVar under ppUpload populates after upload, no subtab nav", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "adr15 browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app_dir <- test_path("fixtures", "vignette-apps", "adr15-consumer-binding")
  csv_path <- file.path(app_dir, "sample.csv")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "adr15-non-shared",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$upload_file(geom_point_0_ppUpload_NA = csv_path)
  set_input(app, "geom_point_0_ppUpload_NA_shortcut", "df")
  app$wait_for_idle(timeout = 25 * 1000)

  expect_picker_populated(app, "geom_point_1_1_ppVar_NA", "mpg")
  expect_match(
    app$get_html("#geom_point_1_1_ppVar_NA_ui") %||% "", "mpg", fixed = TRUE
  )
  # Contract (ADR 0025): an uploaded data source is NEW data, so the
  # formula's positional default (`ppVar(mpg)`) does NOT auto-carry onto
  # it — the picker comes up POPULATED (mpg/wt are offered, asserted
  # above) but UNSELECTED, leaving the user to pick a column for the new
  # frame.
  #
  # Pre-ADR-0025 this picker auto-selected `mpg`, but only by COINCIDENCE:
  # the old auto-name was `node$default %||% node$id`, which for
  # `ppUpload(df)` equalled the typed dataset name `"df"`, so the uploaded
  # frame got bound under the symbol `"df"` and the old default-seed found
  # it. ADR 0025 binds the upload under a system `df_<hash(node$id)>`
  # instead (killing the `df_rug`-style leak), which breaks that
  # coincidence — so "no selection after upload" is now the correct,
  # intended outcome. (See test-auto-name-derivation.R for the auto-name
  # contract and the handoff note on the consumer-seeding follow-up.)
  sel <- app$get_value(input = "geom_point_1_1_ppVar_NA")
  expect_true(
    is.null(sel) || !nzchar(sel),
    label = "uploaded-source consumer picker is unselected (no default carry-over)"
  )
})

test_that("adr15: shared ppVar under ppUpload populates after upload, no subtab nav", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "adr15 browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app_dir <- test_path("fixtures", "vignette-apps", "adr15-consumer-binding")
  csv_path <- file.path(app_dir, "sample.csv")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "adr15-shared",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$upload_file(geom_point_0_ppUpload_NA = csv_path)
  set_input(app, "geom_point_0_ppUpload_NA_shortcut", "df")
  app$wait_for_idle(timeout = 25 * 1000)

  expect_picker_populated(app, "shared_v", "mpg")
  shared_ui <- app$get_html("#shared_v_ui") %||% ""
  expect_false(
    grepl("not yet provided", shared_ui, fixed = TRUE),
    label = "shared picker wrapper no longer shows the unresolved-source advisory"
  )
})

test_that("adr15: no-source upstream pre-warm still binds at boot (req() guard is no-op)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "adr15 browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app_dir <- test_path("fixtures", "vignette-apps", "adr15-no-source-prewarm")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "adr15-no-source-prewarm",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)

  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "mpg")
  # See comment on the sibling test above: the positional default
  # `ppVar(mpg)` must actually be the SELECTED value, not merely offered.
  expect_picker_selected(app, "ggplot_1_1_ppVar_NA", "mpg")
})
