.app_test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- ptr_app ----

test_that("ptr_app returns a shinyApp object", {
  app <- ptr_app(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_components returns a UI tag and server function", {
  parts <- ptr_app_components(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  expect_true(inherits(parts$ui, "shiny.tag") || inherits(parts$ui, "shiny.tag.list"))
  expect_true(is.function(parts$server))
})

test_that("ptr_app_components UI emits expected output ids", {
  parts <- ptr_app_components(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  rendered <- as.character(parts$ui)
  expect_match(rendered, "ptr_plot")
  expect_match(rendered, "ptr_code")
  expect_match(rendered, "ptr_error")
  expect_match(rendered, "ptr_layer_select")
})

test_that("ptr_app_components server wires runtime end-to-end", {
  parts <- ptr_app_components(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .app_test_env()
  )
  shiny::testServer(parts$server, {
    session$setInputs(.dummy = 1)
    res <- session$userData$state %||% NULL
    # ptr_server returns state but moduleServer/closure swallows it; access
    # the reactive output instead.
    expect_match(output$ptr_code, "geom_point")
  })
})

# ---- module variants — namespacing isolation (E6) ----

test_that("ptr_module_server ids are namespaced by id", {
  ui <- ptr_module_ui(
    "m1",
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"
  )
  rendered <- as.character(ui)
  expect_match(rendered, "m1-ptr_plot")
  expect_match(rendered, "m1-ptr_layer_select")
})

test_that("two ptr_module_server instances do not collide", {
  ui_a <- ptr_module_ui("a",
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  ui_b <- ptr_module_ui("b",
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  expect_no_match(as.character(ui_a), "b-ptr_plot")
  expect_no_match(as.character(ui_b), "a-ptr_plot")
})

# ---- ptr_app_grid (BDD P12.16) ----

test_that("ptr_app_grid returns a shiny app object", {
  app <- ptr_app_grid(
    plots = list(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "sz"))',
      'ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point(size = num(shared = "sz"))'
    ),
    shared_ui = list(
      sz = function(id) shiny::sliderInput(id, "Size", 1, 10, value = 3)
    ),
    envir = .app_test_env()
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_grid_components UI contains shared widget id and per-plot module ids", {
  parts <- ptr_app_grid_components(
    plots = list(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "sz"))',
      'ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point(size = num(shared = "sz"))'
    ),
    shared_ui = list(
      sz = function(id) shiny::sliderInput(id, "Size", 1, 10, value = 3)
    ),
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "id=\"sz\"", fixed = TRUE)
  expect_match(ui_html, "plot_1-", fixed = TRUE)
  expect_match(ui_html, "plot_2-", fixed = TRUE)
})

test_that("ptr_app_grid works with no shared controls", {
  app <- ptr_app_grid(
    plots = list("ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point()"),
    shared_ui = list(),
    envir = .app_test_env()
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_grid rejects empty plots list", {
  expect_error(
    ptr_app_grid(plots = list(), shared_ui = list()),
    "length\\(plots\\)"
  )
})

test_that("ptr_app_grid rejects non-string plot entries", {
  expect_error(
    ptr_app_grid(plots = list(42), shared_ui = list()),
    "is_string"
  )
})

test_that("ptr_app_grid rejects shared_ui without unique non-empty names", {
  expect_error(
    ptr_app_grid(
      plots = list("ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))"),
      shared_ui = list(function(id) shiny::sliderInput(id, "x", 1, 10, 5)),
      envir = .app_test_env()
    ),
    "unique non-empty names"
  )
})

test_that("ptr_app_grid rejects non-function shared_ui entries", {
  expect_error(
    ptr_app_grid(
      plots = list("ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))"),
      shared_ui = list(sz = "not a function"),
      envir = .app_test_env()
    ),
    "must be a function"
  )
})

test_that("ptr_app_grid_components UI includes the draw-all button", {
  parts <- ptr_app_grid_components(
    plots = list("ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))"),
    shared_ui = list(),
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "ptr_grid_draw_all", fixed = TRUE)
})

test_that("ptr_app_grid_components draw-all button label is configurable", {
  parts <- ptr_app_grid_components(
    plots = list("ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))"),
    shared_ui = list(),
    draw_all_label = "Refresh everything",
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "Refresh everything", fixed = TRUE)
})
