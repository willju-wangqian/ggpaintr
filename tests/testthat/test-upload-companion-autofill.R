# test-upload-companion-autofill.R — Cut 1.5 regression. When the user
# uploads a new file into a `ppUpload(default)` widget, the companion
# "Optional dataset name" textInput must auto-update to the uploaded
# file's basename (sans extension), so:
#   1. Generated code references the right dataset name.
#   2. `bind_source_value(state, key, name = <companion>, df = <uploaded>)`
#      doesn't bind the new frame under the *old* (stale-default) name.
#
# Pre-fix behavior: companion = "mtcars" at boot (from `node$default`);
# uploading penguins.csv left companion at "mtcars"; generated code
# claimed `mtcars |> ggplot(...)` while the bound frame was actually
# penguins. The fix calls `updateTextInput()` from the pipeline-head /
# bare-data source observers whenever `file_info` is non-NULL.

test_that("ppUpload(default) companion auto-updates to uploaded file's basename", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "upload-companion-autofill browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "upload-companion-autofill")
  csv_path <- file.path(app_dir, "penguins.csv")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "upload-companion-autofill",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)

  # At boot: companion seeded with the default-arg symbol name.
  initial_companion <- app$get_value(input = "ggplot_0_ppUpload_NA_name")
  expect_equal(initial_companion, "mtcars")

  # Upload penguins.csv — companion must auto-update to "penguins" so
  # the generated code's binding name matches the actual dataset.
  app$upload_file(ggplot_0_ppUpload_NA = csv_path)
  app$wait_for_idle(timeout = 25 * 1000)

  post_upload_companion <- app$get_value(input = "ggplot_0_ppUpload_NA_name")
  expect_equal(post_upload_companion, "penguins")
})
