# Regression: `state$upstream_cache` returning a stale resolve after a
# source rebind. See fixture `upload-clears-stale-cache/app.R` for the
# narrative; in short, the cache is keyed on the deparsed substituted
# expression only, so a re-upload (which changes the eval_env binding but
# not the substituted symbol) used to short-circuit on the prior result.
# Fix lives in `bind_source_value()` -- it now drops the upstream cache
# whenever a binding changes, so the next consumer fire re-resolves
# against the current eval_env.

test_that("upload re-resolves consumers; pickers don't keep stale columns", {
  app <- boot_vignette_app("upload-clears-stale-cache")

  # ---- Boot baseline: cols come from mtcars (via parent.frame lookup) ---
  # Click into the layer's Controls subtab so the in-aes ppVar pickers
  # bind their renderUI (per project memory `shinytest2-appdir-pkgload`).
  app$set_inputs(ggplot_subtab = "Controls", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 1000)
  expect_equal(app$get_value(input = "ggplot_1_1_ppVar_NA"), "mpg")
  expect_equal(app$get_value(input = "ggplot_1_2_ppVar_NA"), "cyl")

  # ---- Upload a CSV with DISJOINT columns -----------------------------
  csv <- testthat::test_path("fixtures", "vignette-apps",
                             "upload-clears-stale-cache", "penguins5.csv")
  upload_file(app, ggplot_0_ppUpload_NA = csv)
  app$wait_for_idle(timeout = 25 * 1000)

  # ---- The non-shared x/y pickers MUST NOT still hold mtcars cols -----
  # Pre-fix observation: both pickers retained "mpg" / "cyl" because the
  # upstream_cache returned the mtcars-shaped resolve under the same key.
  # Post-fix: the cache is dropped on every `bind_source_value` call so
  # the next consumer fire re-resolves against eval_env[["df_main"]] =
  # penguins; mtcars cols intersect to character(0); pickers clear.
  for (id in c("ggplot_1_1_ppVar_NA", "ggplot_1_2_ppVar_NA")) {
    val <- app$get_value(input = id)
    testthat::expect_false(
      identical(val, "mpg") || identical(val, "cyl"),
      label = paste0(
        "picker '", id, "' must not retain the pre-upload mtcars ",
        "selection after a disjoint-cols upload; saw ", deparse(val)
      )
    )
  }
})
