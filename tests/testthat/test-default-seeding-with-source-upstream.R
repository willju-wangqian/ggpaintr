# Regression: `ppVar('<col>')` default seeds the picker at boot even when the
# upstream is a `ppUpload(<df>)` (source-headed pipeline). Pre-fix bug: the
# consumer renderUI's first `entry_reactive()` fire returned NULL because
# `substitute_walk.ptr_ph_data_source()` reads the companion textInput's value
# from the input snapshot, which is briefly NULL post-boot before the client
# reports the widget's `value=` back. That NULL fire flipped `has_rendered`,
# locking the next valid fire into the `seed %||% current %||% character(0)`
# branch -- `extra$selected = character(0)` suppressed `invoke_build_ui()`'s
# `node$default` injection, so the picker boot-selected nothing.

test_that("ppVar default seeds picker through ppUpload(df) (string form)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(!file.exists(file.path(pkg, "DESCRIPTION")), "needs source root")
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps",
                       "default-seeding-source-upstream")
  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir, name = "default-seeding-source-upstream",
    load_timeout = 60 * 1000, timeout = 30 * 1000
  ))
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 25 * 1000)

  # Picker boot value reflects `ppVar('hp')` default, not character(0).
  expect_equal(app$get_value(input = "ggplot_1_1_ppVar_NA"), "hp")
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "hp")
})

test_that("ppVar default seeds picker through ppUpload(df) (unquoted form)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(!file.exists(file.path(pkg, "DESCRIPTION")), "needs source root")
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps",
                       "default-seeding-source-upstream-unquoted")
  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir, name = "default-seeding-source-upstream-unquoted",
    load_timeout = 60 * 1000, timeout = 30 * 1000
  ))
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 25 * 1000)

  expect_equal(app$get_value(input = "ggplot_1_1_ppVar_NA"), "hp")
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "hp")
})

test_that("baseline: ppVar default seeds picker with bare-df upstream", {
  # Negative-baseline: pinning the always-working path forecloses the
  # misdiagnosis "ppVar default broken everywhere" if a future regression
  # returns and only affects the source-headed case.
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(!file.exists(file.path(pkg, "DESCRIPTION")), "needs source root")
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps",
                       "default-seeding-no-upstream-baseline")
  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir, name = "default-seeding-no-upstream-baseline",
    load_timeout = 60 * 1000, timeout = 30 * 1000
  ))
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 25 * 1000)

  expect_equal(app$get_value(input = "ggplot_1_1_ppVar_NA"), "hp")
})
