test_that("call-form num placeholder parses with shared metadata", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  num_metas <- Filter(function(m) m$keyword == "num", metas)
  expect_length(num_metas, 1L)
  expect_identical(num_metas[[1]]$shared, "size_filter")
})

test_that("bare-symbol placeholder still parses with NULL shared", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num)"
  )
  metas <- ptr_flatten_placeholder_map(obj)
  num_metas <- Filter(function(m) m$keyword == "num", metas)
  expect_length(num_metas, 1L)
  expect_null(num_metas[[1]]$shared)
})

test_that("call-form var and text placeholders parse with shared metadata", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = var(shared = "x_axis"), y = mpg)) + labs(title = text(shared = "plot_title"))'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  shared_names <- vapply(metas, function(m) m$shared %||% NA_character_, character(1))
  keywords <- vapply(metas, function(m) m$keyword, character(1))
  expect_true("x_axis" %in% shared_names)
  expect_true("plot_title" %in% shared_names)
  expect_identical(unname(shared_names[keywords == "var"])[1], "x_axis")
  expect_identical(unname(shared_names[keywords == "text"])[1], "plot_title")
})

test_that("call-form placeholder with no args is equivalent to bare symbol", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num())"
  )
  metas <- ptr_flatten_placeholder_map(obj)
  num_metas <- Filter(function(m) m$keyword == "num", metas)
  expect_length(num_metas, 1L)
  expect_null(num_metas[[1]]$shared)
})

test_that("positional arg in call-form placeholder is rejected", {
  expect_error(
    ptr_parse_formula(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num("size_filter"))'
    ),
    "Positional arguments are not allowed"
  )
})

test_that("unknown arg in call-form placeholder is rejected", {
  expect_error(
    ptr_parse_formula(
      "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(min = 0))"
    ),
    "unknown argument"
  )
})

test_that("non-string shared value is rejected", {
  expect_error(
    ptr_parse_formula(
      "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = 42))"
    ),
    "must be a single non-empty string"
  )
})

test_that("empty shared string is rejected", {
  expect_error(
    ptr_parse_formula(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = ""))'
    ),
    "must be a single non-empty string"
  )
})

test_that("call-form placeholder substitutes correctly at runtime", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  spec <- ptr_runtime_input_spec(obj)
  num_id <- spec$input_id[spec$keyword == "num" & !is.na(spec$keyword)][1]
  input <- list()
  input[["geom_point_checkbox"]] <- TRUE
  input[[num_id]] <- 3
  res <- ptr_complete_expr(obj, input)
  expect_true(grepl("size = 3", res$code_text, fixed = TRUE))
  expect_false(grepl("shared", res$code_text, fixed = TRUE))
})

test_that("call-form expr placeholder is exempt from safety walker", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + expr(shared = "extras")'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  expr_metas <- Filter(function(m) m$keyword == "expr", metas)
  expect_length(expr_metas, 1L)
  expect_identical(expr_metas[[1]]$shared, "extras")
})

test_that("multiple placeholders can share the same name within one formula", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = var(shared = "axis"), y = var(shared = "axis"))) + geom_point()'
  )
  metas <- ptr_flatten_placeholder_map(obj)
  shared_axis <- Filter(function(m) identical(m$shared, "axis"), metas)
  expect_length(shared_axis, 2L)
})

test_that("ptr_runtime_input_spec exposes shared column", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = var, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  spec <- ptr_runtime_input_spec(obj)
  expect_true("shared" %in% names(spec))

  num_row <- spec[spec$keyword == "num" & !is.na(spec$keyword), , drop = FALSE]
  var_row <- spec[spec$keyword == "var" & !is.na(spec$keyword), , drop = FALSE]
  checkbox_row <- spec[spec$role == "layer_checkbox", , drop = FALSE]

  expect_identical(num_row$shared, "size_filter")
  expect_true(is.na(var_row$shared))
  expect_true(all(is.na(checkbox_row$shared)))
})

test_that("ptr_runtime_input_spec shared column is NA for unannotated formulas", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  spec <- ptr_runtime_input_spec(obj)
  expect_true("shared" %in% names(spec))
  expect_true(all(is.na(spec$shared)))
})

test_that("ptr_runtime_input_spec carries shared onto upload_name companion row", {
  obj <- ptr_parse_formula(
    'ggplot(data = upload(shared = "ds")) + geom_point(aes(x = var, y = var))'
  )
  spec <- ptr_runtime_input_spec(obj)
  upload_rows <- spec[spec$keyword == "upload" & !is.na(spec$keyword), , drop = FALSE]
  expect_true(nrow(upload_rows) >= 2L)
  expect_true(all(upload_rows$shared == "ds"))
})

test_that("shared binding overrides placeholder resolution at runtime", {
  shiny::isolate({
    obj <- ptr_parse_formula(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
    )
    bindings <- list(size_filter = shiny::reactiveVal(7))
    input <- list("geom_point_checkbox" = TRUE)
    res <- ptr_complete_expr(obj, input, shared_bindings = bindings)
    expect_true(grepl("size = 7", res$code_text, fixed = TRUE))
  })
})

test_that("missing shared binding falls back to NULL (placeholder resolves as missing)", {
  shiny::isolate({
    obj <- ptr_parse_formula(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
    )
    input <- list("geom_point_checkbox" = TRUE)
    res <- ptr_complete_expr(obj, input, shared_bindings = list())
    expect_false(grepl("size = ", res$code_text, fixed = TRUE))
  })
})

test_that("ptr_validate_shared_bindings accepts NULL and empty list", {
  expect_identical(ptr_validate_shared_bindings(NULL), list())
  expect_identical(ptr_validate_shared_bindings(list()), list())
})

test_that("ptr_validate_shared_bindings rejects non-list", {
  expect_error(ptr_validate_shared_bindings("nope"), "must be a named list")
})

test_that("ptr_validate_shared_bindings requires names", {
  rv <- shiny::reactiveVal(1)
  expect_error(
    ptr_validate_shared_bindings(list(rv)),
    "non-empty names"
  )
})

test_that("ptr_validate_shared_bindings rejects duplicate names", {
  rv <- shiny::reactiveVal(1)
  expect_error(
    ptr_validate_shared_bindings(list(a = rv, a = rv)),
    "non-empty names"
  )
})

test_that("ptr_validate_shared_bindings rejects non-reactive values", {
  expect_error(
    ptr_validate_shared_bindings(list(a = 42)),
    "must be Shiny reactives"
  )
})

test_that("UI list omits widgets for shared placeholders", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  ui_list <- ptr_build_ui_list(obj)
  geom_ui <- ui_list[["geom_point"]]
  placeholder_ids <- setdiff(names(geom_ui), "geom_point_checkbox")
  for (id in placeholder_ids) {
    expect_null(geom_ui[[id]])
  }
})

test_that("UI list still renders widgets for non-shared placeholders alongside shared ones", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "size_filter"), alpha = num)'
  )
  ui_list <- ptr_build_ui_list(obj)
  geom_ui <- ui_list[["geom_point"]]
  placeholder_ids <- setdiff(names(geom_ui), "geom_point_checkbox")
  rendered <- vapply(placeholder_ids, function(id) !is.null(geom_ui[[id]]), logical(1))
  expect_equal(sum(rendered), 1L)
})

test_that("ptr_app_grid returns a shiny app object", {
  app <- ptr_app_grid(
    plots = list(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "sz"))',
      'ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point(size = num(shared = "sz"))'
    ),
    shared_ui = list(
      sz = function(id) shiny::sliderInput(id, "Size", 1, 10, value = 3)
    )
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_app_grid UI contains shared widget id and per-plot module ids", {
  parts <- ptr_app_grid_components(
    plots = list(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point(size = num(shared = "sz"))',
      'ggplot(data = mtcars, aes(x = hp, y = mpg)) + geom_point(size = num(shared = "sz"))'
    ),
    shared_ui = list(
      sz = function(id) shiny::sliderInput(id, "Size", 1, 10, value = 3)
    )
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "id=\"sz\"", fixed = TRUE)
  expect_match(ui_html, "plot_1-", fixed = TRUE)
  expect_match(ui_html, "plot_2-", fixed = TRUE)
})

test_that("ptr_app_grid works with no shared controls", {
  app <- ptr_app_grid(
    plots = list("ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point()"),
    shared_ui = list()
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
      shared_ui = list(function(id) shiny::sliderInput(id, "x", 1, 10, 5))
    ),
    "unique non-empty names"
  )
})

test_that("ptr_app_grid rejects non-function shared_ui entries", {
  expect_error(
    ptr_app_grid(
      plots = list("ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))"),
      shared_ui = list(sz = "not a function")
    ),
    "must be a function"
  )
})

test_that("ptr_app_grid UI includes the draw-all button", {
  parts <- ptr_app_grid_components(
    plots = list("ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))"),
    shared_ui = list()
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "ptr_grid_draw_all", fixed = TRUE)
})

test_that("ptr_app_grid draw-all button label is configurable", {
  parts <- ptr_app_grid_components(
    plots = list("ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))"),
    shared_ui = list(),
    draw_all_label = "Refresh everything"
  )
  ui_html <- as.character(parts$ui)
  expect_match(ui_html, "Refresh everything", fixed = TRUE)
})

test_that("ptr_server_state stores draw_trigger when provided", {
  rv <- shiny::reactiveVal(0)
  state <- ptr_server_state(
    "ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))",
    draw_trigger = rv
  )
  expect_identical(state$draw_trigger, rv)
})

test_that("ptr_server_state rejects non-reactive draw_trigger", {
  expect_error(
    ptr_server_state(
      "ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))",
      draw_trigger = 42
    ),
    "must be a Shiny reactive"
  )
})

test_that("ptr_server_state accepts NULL draw_trigger", {
  state <- ptr_server_state(
    "ggplot(data = mtcars) + geom_point(aes(x = wt, y = mpg))",
    draw_trigger = NULL
  )
  expect_null(state$draw_trigger)
})
