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

test_that("ptr_app_bslib renders a shared section + binds shared widgets", {
  skip_if_not_installed("bslib")

  app <- ptr_app_bslib(
    paste0(
      "ggplot(data = mtcars, aes(alpha = var(shared = \"v\"))) + ",
      "geom_point(aes(size = var(shared = \"v\")))"
    ),
    envir = list2env(list(mtcars = mtcars), parent = globalenv())
  )
  expect_s3_class(app, "shiny.appobj")

  # The server binds the shared `var` picker (server-rendered) with the
  # combined multi-param label.
  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    rendered <- output$`shared_v_ui`
    html_str <- if (is.list(rendered) && !is.null(rendered$html)) {
      rendered$html
    } else {
      as.character(rendered)
    }
    expect_match(html_str, "Pick a column for: alpha, size", fixed = TRUE)
  })
})
