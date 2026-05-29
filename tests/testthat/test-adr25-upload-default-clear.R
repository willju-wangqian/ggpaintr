# ADR 0025 contract (ii), user-locked 2026-05-28: a consumer's formula default
# seeds at boot over the source's env-default frame, but a NEW post-boot source
# (a file upload, or a shortcut rename to a different dataset) is new work and
# CLEARS the picker — the default does not carry, nor does a stale prior pick.
#
# This is the regression the adr15 fixture could NOT catch: adr15 uses
# `ppUpload(df)` where `df` is not an env frame (no boot data), so it never
# exercised the boot-default-seeds path (case A). Here `ppUpload(mtcars)`
# resolves `mtcars` at boot via the shortcut, so the boot seed is observable.

app_dir <- function() {
  test_path("fixtures", "vignette-apps", "adr25-upload-default-clear")
}

test_that("adr25: env-default boot seeds the formula default; a new upload clears it", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "adr25 browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir(), name = "adr25-upload-default-clear", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)

  # Case A (must NOT regress): the source resolves the env frame `mtcars` at
  # boot, so the consumer's formula default `mpg` is selected.
  expect_equal(
    app$get_value(input = "geom_point_1_1_ppVar_NA"), "mpg",
    label = "boot over env-default frame seeds the formula default (mpg)"
  )

  # Case C (the bug): a real upload is new data — the boot default must not
  # carry onto it. Picker stays populated but unselected.
  app$upload_file(
    geom_point_0_ppUpload_NA = file.path(app_dir(), "up.csv"),
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 20 * 1000)
  after <- app$get_value(input = "geom_point_1_1_ppVar_NA")
  expect_true(
    is.null(after) || length(after) == 0L || identical(after, ""),
    label = "post-upload consumer picker is cleared (formula default does not carry)"
  )
})
