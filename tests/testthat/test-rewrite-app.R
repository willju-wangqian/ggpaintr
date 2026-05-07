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

test_that("minimal var-using example: pickers populated from literal `data =`", {
  # Regression for the empty-dropdown bug. The documented minimal example
  # `aes(x = var, y = var)` with `data = mtcars` should yield two pickers,
  # each populated with the columns of `mtcars`. The static UI emits an
  # empty `uiOutput` per consumer; the server's renderUI fills it on the
  # first reactive flush.
  parts <- ptr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "id=\"ggplot_1_1_var_NA_ui\"", fixed = TRUE)
  expect_match(ui_html, "id=\"ggplot_1_2_var_NA_ui\"", fixed = TRUE)

  shiny::testServer(parts$server, {
    session$setInputs(.dummy = 1)
    x_picker <- output$`ggplot_1_1_var_NA_ui`$html
    y_picker <- output$`ggplot_1_2_var_NA_ui`$html
    # both pickers are shinyWidgets selectpickers populated with mtcars cols
    expect_match(x_picker, "selectpicker")
    expect_match(y_picker, "selectpicker")
    for (col in c("mpg", "cyl", "hp")) {
      expect_match(x_picker, col)
      expect_match(y_picker, col)
    }
  })
})

test_that("var picker renders with no default selection", {
  # Regression: shinyWidgets::pickerInput defaulted to the first choice,
  # so a fresh app launched with `aes(x = mpg, y = mpg)` (var pickers
  # silently bound to the first column) instead of waiting for the user.
  parts <- ptr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    envir = .app_test_env()
  )
  shiny::testServer(parts$server, {
    session$setInputs(.dummy = 1)
    x_picker <- output$`ggplot_1_1_var_NA_ui`$html
    # The rendered <select> must not pre-select any option.
    # `selected="selected"` is the only HTML form for an option default;
    # `data-none-selected-text=` is unrelated and should be present.
    expect_false(grepl('selected="selected"', x_picker, fixed = TRUE))
    expect_match(x_picker, "data-none-selected-text=", fixed = TRUE)
  })
})

test_that("ptr_app emits an Update Plot trigger button", {
  # Spec L142 + BDD G11.12: standalone `ptr_app` carries an Update Plot
  # button so the plot is gated behind an explicit click rather than
  # re-rendering on every keystroke.
  parts <- ptr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    envir = .app_test_env()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, 'id="ptr_update_plot"', fixed = TRUE)
})

test_that("layer-select picker drives the hidden tabset", {
  # Regression: the layer picker (`ptr_layer_select`) was rendered but never
  # wired to `updateTabsetPanel(ptr_layer_tabset)`, so picker changes left
  # the visible panel unchanged. A placeholder-free layer like geom_point()
  # exposed this most visibly: switching to it appeared to do nothing.
  parts <- ptr_app_components(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    envir = .app_test_env()
  )

  spy <- new.env(parent = emptyenv())
  spy$calls <- list()
  trace(
    shiny::updateTabsetPanel,
    tracer = bquote({
      .spy <- .(spy)
      .spy$calls[[length(.spy$calls) + 1L]] <- list(
        inputId = inputId, selected = selected
      )
    }),
    print = FALSE
  )
  on.exit(untrace(shiny::updateTabsetPanel), add = TRUE)

  shiny::testServer(parts$server, {
    session$setInputs(ptr_layer_select = "geom_point")
    session$flushReact()
    session$setInputs(ptr_layer_select = "ggplot")
    session$flushReact()
  })

  selecteds <- vapply(spy$calls, function(c) c$selected, character(1))
  inputIds  <- vapply(spy$calls, function(c) c$inputId,  character(1))
  expect_true(all(inputIds == "ptr_layer_tabset"))
  expect_true(all(c("geom_point", "ggplot") %in% selecteds))
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
