test_that("ptr_app_bslib returns a shiny.appobj using public API", {
  skip_if_not_installed("bslib")

  app <- ptr_app_bslib(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  expect_s3_class(app, "shiny.appobj")
  expect_true(is.function(app$serverFuncSource()))
})

test_that("ptr_app_bslib accepts a custom bslib theme", {
  skip_if_not_installed("bslib")

  custom_theme <- bslib::bs_theme(version = 5, bootswatch = "darkly")
  app <- ptr_app_bslib(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    theme = custom_theme
  )

  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_bslib self-binds a single-formula shared placeholder", {
  skip_if_not_installed("bslib")

  # Single-formula `var(shared = "...")` coordination now works through
  # the bare wrapper: ptr_server() self-binds every declared key under
  # the module's own namespace (matches ptr_app()'s auto_bind_shared
  # path). Documented in vignette("ggpaintr-customization").
  app <- ptr_app_bslib(
    paste0(
      "ggplot(data = mtcars, aes(alpha = var(shared = \"v\"))) + ",
      "geom_point(aes(size = var(shared = \"v\")))"
    ),
    envir = list2env(list(mtcars = mtcars), parent = globalenv())
  )
  expect_s3_class(app, "shiny.appobj")

  expect_silent(
    shiny::testServer(app$serverFuncSource(), {
      session$flushReact()
    })
  )
})
