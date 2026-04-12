test_that("ptr_app_bslib returns a shiny.appobj using public API", {
  skip_if_not_installed("bslib")

  app <- ptr_app_bslib(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource()))
})

test_that("ptr_app_bslib accepts a custom bslib theme and title", {
  skip_if_not_installed("bslib")

  custom_theme <- bslib::bs_theme(version = 5, bootswatch = "darkly")
  app <- ptr_app_bslib(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    theme = custom_theme,
    title = "Custom"
  )

  expect_s3_class(app, "shiny.appobj")
})
