test_that("ptr_app_bslib returns a shiny.appobj using public API", {
  skip_if_not_installed("bslib")

  app <- ptr_app_bslib(
    "ggplot(data = mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  )

  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource()))
})

test_that("ptr_app_bslib accepts a custom bslib theme", {
  skip_if_not_installed("bslib")

  custom_theme <- bslib::bs_theme(version = 5, bootswatch = "darkly")
  app <- ptr_app_bslib(
    "ggplot(data = mtcars, aes(x = ppVar, y = ppVar)) + geom_point()",
    theme = custom_theme
  )

  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_bslib self-binds a single-formula shared placeholder", {
  # Single-formula `ppVar(shared = "...")` coordination works through the
  # bare wrapper: ptr_server() self-binds every declared key under the
  # module's own namespace (matches ptr_app()'s auto_bind_shared path).
  # Documented in vignette("ggpaintr-customization").
  #
  # Browser test (replaces a prior `expect_silent(testServer(...flushReact()))`
  # no-crash proxy that could not fail when self-binding regressed). The shared
  # key `v` drives BOTH ppVar consumers (alpha + size). "wt" is a column literal
  # nowhere in the formula text, so its presence in the generated code can only
  # come from the inline shared widget propagating; asserting it lands in BOTH
  # aes slots proves the two consumers coordinate off ONE widget.
  skip_on_cran()
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("bslib-single-shared")

  set_input(app, "ptr-shared_v", "wt")
  draw(app, "ptr-ptr_update_plot")
  expect_no_inline_error(app, "ptr-ptr_error")

  # The inline shared picker is populated (a real bound widget, not an empty
  # uiOutput) and offers the chosen column.
  expect_picker_populated(app, "ptr-shared_v", "wt")

  # Coordination: one shared widget drove both consumers -> "wt" reaches the
  # alpha slot AND the size slot of the generated code.
  code <- app$get_value(output = "ptr-ptr_code")
  expect_match(code, "alpha = wt", fixed = TRUE)
  expect_match(code, "size = wt", fixed = TRUE)
})
