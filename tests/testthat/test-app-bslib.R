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

test_that("ptr_app_bslib aborts when given a single-formula shared placeholder (public-API limit)", {
  skip_if_not_installed("bslib")

  # Single-formula `var(shared = "...")` coordination is not expressible on
  # the public wrapper API today; the auto-built shared widgets ptr_app()
  # provides require an internal helper. The recommended path for this
  # case is `ptr_app()` itself. Documented in
  # `vignette("ggpaintr-customization")` under "Writing your own wrapper".
  app <- ptr_app_bslib(
    paste0(
      "ggplot(data = mtcars, aes(alpha = var(shared = \"v\"))) + ",
      "geom_point(aes(size = var(shared = \"v\")))"
    ),
    envir = list2env(list(mtcars = mtcars), parent = globalenv())
  )
  expect_s3_class(app, "shiny.appobj")

  expect_error(
    shiny::testServer(app$serverFuncSource(), {
      session$flushReact()
    }),
    "shared placeholder"
  )
})
