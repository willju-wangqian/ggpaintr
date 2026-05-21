# test-adr10-ppupload-name-browser.R — shinytest2 driver for the
# adr10-ppupload-name fixture. Covers the browser-level BDD scenarios of
# PLAN-03 (ADR 0010): companion textInput seeded at boot, preserve-mode
# round-trip to ppUpload(<name>), plot renders via caller-env auto-resolve
# without any file upload, and re-editing the companion to "iris" updates
# the preserve-mode code panel.
#
# Gate pattern + boot scaffolding follow CLAUDE.md "Browser e2e (shinytest2)
# — hard-won gotchas": app-dir + pkgload::load_all in the fixture, never
# get_values(), set placeholder inputs with wait_=FALSE then explicit Draw.

test_that("adr10-ppupload-name: companion textInput seeded, preserve-mode round-trips, plot auto-resolves", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "adr10 browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app_dir <- test_path("fixtures", "vignette-apps", "adr10-ppupload-name")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "adr10-ppupload-name",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  # Discover the companion id from the translated tree (same convention used
  # in app.R's formula): `ppUpload(penguins) |> dplyr::filter(...) |>
  # ggplot(aes(x = ppVar(bill_length_mm), y = ppVar(bill_depth_mm))) + geom_point()`.
  # The first ppUpload data-source node's companion_id is
  # paste0(node$id, "_name") = "ggplot_1_ppUpload_NA_name".
  companion_id <- "ggplot_1_ppUpload_NA_name"

  # Sanity: standard ptr_app() DOM landmarks are present.
  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_code")
  expect_dom_id(app, "ptr_code_mode")
  expect_dom_id(app, companion_id)

  # (a) Companion textInput is seeded with "penguins" at boot (PLAN-01).
  expect_equal(
    app$get_value(input = companion_id), "penguins",
    label = "companion textInput is seeded with bareword from formula"
  )

  # Drive the first render. ppVar pickers live under the layer's Data
  # subtab; their renderUI is suspended until shown -- but for THIS test the
  # var pickers are not user-driven, they're picked from the formula
  # bareword `bill_length_mm` / `bill_depth_mm` (the ppVar(default = <sym>)
  # path established in ADR 0009). A bare Draw click is enough.
  app$click("ptr_update_plot")
  app$wait_for_idle(timeout = 25 * 1000)

  # (c) Plot output is non-empty -- auto-resolve found `penguins` via the
  # eval_env parent chain without any file upload (Clarification C3).
  plot_html <- app$get_html("#ptr_plot")
  expect_true(
    !is.null(plot_html) && nzchar(plot_html),
    label = "plot output container is present after Draw"
  )
  expect_match(plot_html, "<img", fixed = TRUE)

  # (b) Switch to preserve mode and assert the code panel contains
  # ppUpload(penguins) (PLAN-02 round-trip end-to-end).
  app$set_inputs(ptr_code_mode = "preserve", wait_ = TRUE, timeout_ = 10000)
  app$wait_for_idle(timeout = 25 * 1000)
  code_preserve <- app$get_value(output = "ptr_code")
  expect_true(
    grepl("ppUpload(penguins)", code_preserve, fixed = TRUE),
    label = "preserve-mode code contains ppUpload(penguins)"
  )

  # (d) Edit the companion to "iris" and redraw; preserve mode should now
  # show ppUpload(iris) and NOT ppUpload(penguins). Switch back to final
  # then preserve to flush the renderer.
  app$set_inputs(ptr_code_mode = "final", wait_ = TRUE, timeout_ = 10000)
  app$wait_for_idle(timeout = 25 * 1000)
  set_input(app, companion_id, "iris")
  app$click("ptr_update_plot")
  app$wait_for_idle(timeout = 25 * 1000)
  app$set_inputs(ptr_code_mode = "preserve", wait_ = TRUE, timeout_ = 10000)
  app$wait_for_idle(timeout = 25 * 1000)
  code_preserve2 <- app$get_value(output = "ptr_code")
  expect_true(
    grepl("ppUpload(iris)", code_preserve2, fixed = TRUE),
    label = "after re-edit, preserve-mode code contains ppUpload(iris)"
  )
  expect_false(
    grepl("ppUpload(penguins)", code_preserve2, fixed = TRUE),
    label = "after re-edit, preserve-mode code no longer mentions penguins"
  )
})
