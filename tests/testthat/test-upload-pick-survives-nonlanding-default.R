# Regression (2026-05-29): a consumer picker on a layer-data `ppUpload()` whose
# formula default is NOT a column of the user-loaded dataset must KEEP the
# user's column pick across an Update Plot click. The `has_rendered` latch in
# `consumer_seed_decision()` only flipped when the formula default landed (was a
# valid column) or a seed/current drove the choice; for an uploaded /
# shortcut-swapped frame whose schema omits the default, the latch never flipped
# and `boot_seed_selected()` discarded the user's pick on every later render --
# the picker blanked on every Update. See test-spec-seed-boot-only.R Seam 1c for
# the unit-level seam and the mechanism comment in R/paintr-server.R.

app_dir <- function() {
  test_path("fixtures", "vignette-apps", "upload-pick-survives-nonlanding-default")
}

test_that("layer-upload consumer keeps the user's pick across Update Plot when the default never lands", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir(), name = "upload-pick-survives-nonlanding-default", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)

  # Reveal the geom_rug layer's Data subtab so its shortcut + var pickers bind.
  app$set_inputs(ptr_layer_select = "geom_rug", wait_ = FALSE)
  app$set_inputs(geom_rug_subtab = "Data", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 1000)

  # Load `iris` via the upload shortcut (env-load). iris has no carb/gear, so
  # the formula defaults ppVar(carb)/ppVar(gear) cannot land.
  app$set_inputs(geom_rug_0_ppUpload_NA_shortcut = "iris", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 1000)

  # Picker comes up populated (iris columns) but unselected (default did not
  # land, no spec seed).
  expect_true(
    grepl("Sepal.Length", app$get_html("#geom_rug_1_1_ppVar_NA"), fixed = TRUE),
    label = "x picker is populated with the loaded dataset's columns"
  )

  # User picks valid iris columns.
  app$set_inputs(geom_rug_1_1_ppVar_NA = "Sepal.Length", wait_ = FALSE)
  app$set_inputs(geom_rug_1_2_ppVar_NA = "Sepal.Width", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 1000)

  # Click Update Plot -- the bug reset the pickers here.
  app$set_inputs(ptr_update_plot = "click", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 1000)

  expect_equal(
    app$get_value(input = "geom_rug_1_1_ppVar_NA"), "Sepal.Length",
    label = "x pick survives Update Plot"
  )
  expect_equal(
    app$get_value(input = "geom_rug_1_2_ppVar_NA"), "Sepal.Width",
    label = "y pick survives Update Plot"
  )
})
