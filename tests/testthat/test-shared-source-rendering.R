# test-shared-source-rendering.R — regression for Cut 1 of the
# shared-source end-to-end fix.
#
# Bug 2 (single-instance shared source): `ppUpload(shared='ds')` rendered
# only its empty `uiOutput("shared_ds_ui")` container — the per-instance
# `ptr_setup_source_uis()` (R/paintr-server.R) had a partition-blind
# `if (!is.null(s$shared)) next` skip, so no renderUI was ever assigned
# and the fileInput never appeared. Cut 1 drops the skip; the rewritten
# shared-source node (canonical id `shared_ds`) flows through the normal
# per-instance renderUI path. Cut 2 will reintroduce a *partition-aware*
# skip once panel-owned source ids exist in the bundle.
#
# Combined-default test exercises the interaction with the source-default
# fallback fix (eyeball-play a3d1636 / `try_bind_source_default`): once
# the skip is gone, the pipeline observer fires for the shared source at
# boot and the default symbol (`df_main` → mtcars) primes
# `state$resolved_sources` so the consumer picker populates without an
# upload click.

test_that("single-instance ppUpload(shared='ds') renders the fileInput at boot", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "shared-source-rendering browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "shared-source-rendering-bare")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "shared-source-rendering-bare",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)

  # The shared-source fileInput at the canonical id must be in the DOM —
  # the container alone (`#shared_ds_ui`) is not proof; Bug 2 left that
  # container present and empty. Asserting the inner fileInput is what
  # this test exists to protect.
  expect_dom_id(app, "shared_ds")
})

test_that("ppUpload(df_main, shared='ds') populates downstream picker at boot via default-arg fallback", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "shared-source-rendering browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "shared-source-rendering-default")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "shared-source-rendering-default",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)

  # Source renders.
  expect_dom_id(app, "shared_ds")
  # Default-arg fallback primes the picker with mtcars columns at boot.
  expect_picker_populated(app, "shared_a", "mpg")
})
