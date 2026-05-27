# test-source-default-fallback.R — regression for the source-default
# fallback fix. Companion to ADR 0015 PLAN-01 (req() source-ready gate):
# the gate must not halt when the source carries a `default_arg`-validated
# symbol that resolves via `state$eval_env`'s parent chain.
#
# Before the fix: `state$resolved_sources[[sid]]` and
# `state$bound_names[[sid]]` stayed NULL forever with no upload, so the
# consumer entry_reactive's `req()` halted, and the ppVar picker for
# `ppUpload(df_main)` rendered as an empty uiOutput at boot. Manually
# uploading a file made it work (the upload observer wrote both slots).
#
# After the fix: when no file is uploaded but the source has a `default`
# that resolves to a data.frame in `state$eval_env`'s parent chain, the
# source observer pre-binds via `bind_source_value()`, the gate passes,
# and the picker populates at boot.

test_that("pipeline-head ppUpload(df_main) populates ppVar picker at boot via default", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("dplyr")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "source-default-fallback browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "source-default-fallback")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "source-default-fallback",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)

  # ppVar(mpg) picker for the pipeline-head ppUpload(df_main) should be
  # populated with mtcars's columns at boot — no upload performed.
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "mpg")
  expect_picker_populated(app, "ggplot_1_2_ppVar_NA", "hp")
  # AND the positional defaults `ppVar(mpg)` / `ppVar(hp)` are actually
  # SELECTED, not merely offered. `_populated` is a presence proxy on the
  # options list and was the bug-class that hid the `aes(y = ppVar(adj))`
  # derived-column regression (2026-05-27).
  expect_picker_selected(app, "ggplot_1_1_ppVar_NA", "mpg")
  expect_picker_selected(app, "ggplot_1_2_ppVar_NA", "hp")
})
