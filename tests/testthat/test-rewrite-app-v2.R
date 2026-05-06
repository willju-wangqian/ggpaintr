.app_test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- ptr_app_v2 ----

test_that("ptr_app_v2 returns a shinyApp object", {
  app <- ptr_app_v2(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_components_v2 returns a UI tag and server function", {
  parts <- ptr_app_components_v2(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  expect_true(inherits(parts$ui, "shiny.tag") || inherits(parts$ui, "shiny.tag.list"))
  expect_true(is.function(parts$server))
})

test_that("ptr_app_components_v2 UI emits expected output ids", {
  parts <- ptr_app_components_v2(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  rendered <- as.character(parts$ui)
  expect_match(rendered, "ptr_plot")
  expect_match(rendered, "ptr_code")
  expect_match(rendered, "ptr_error")
  expect_match(rendered, "ptr_layer_select")
})

test_that("ptr_app_components_v2 server wires runtime end-to-end", {
  parts <- ptr_app_components_v2(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  shiny::testServer(parts$server, {
    session$setInputs(.dummy = 1)
    res <- session$userData$state %||% NULL
    # ptr_server_v2 returns state but moduleServer/closure swallows it; access
    # the reactive output instead.
    expect_match(output$ptr_code, "geom_point")
  })
})

# ---- module variants — namespacing isolation (E6) ----

test_that("ptr_module_server_v2 ids are namespaced by id", {
  ui <- ptr_module_ui_v2(
    "m1",
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"
  )
  rendered <- as.character(ui)
  expect_match(rendered, "m1-ptr_plot")
  expect_match(rendered, "m1-ptr_layer_select")
})

test_that("two ptr_module_server_v2 instances do not collide", {
  ui_a <- ptr_module_ui_v2("a",
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  ui_b <- ptr_module_ui_v2("b",
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  expect_no_match(as.character(ui_a), "b-ptr_plot")
  expect_no_match(as.character(ui_b), "a-ptr_plot")
})
