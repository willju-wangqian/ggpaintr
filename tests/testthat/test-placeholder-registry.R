placeholder_ui_text <- function(ui) {
  paste(as.character(ui), collapse = "\n")
}

test_that("placeholder constructor validates keyword names and required hooks", {
  expect_error(
    ptr_define_placeholder(
      keyword = "bad keyword",
      build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
      resolve_expr = function(value, meta, context) rlang::expr(x)
    ),
    "syntactic placeholder name"
  )

  expect_error(
    ptr_define_placeholder(
      keyword = "date",
      build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
      resolve_expr = 1
    ),
    "resolve_expr must be a function"
  )
})

test_that("placeholder registries reject duplicate keywords and pass through unchanged", {
  date_placeholder <- make_test_date_placeholder()

  expect_error(
    ptr_merge_placeholders(list(date_placeholder, date_placeholder)),
    "duplicated keywords"
  )

  registry <- ptr_merge_placeholders(list(date = date_placeholder))
  expect_identical(ptr_merge_placeholders(registry), registry)
  expect_true("date" %in% names(registry))
})

test_that("custom placeholders are parsed into placeholder metadata and runtime output", {
  registry <- ptr_merge_placeholders(
    list(date = make_test_date_placeholder())
  )
  obj <- ptr_parse_formula(test_date_formula, placeholders = registry)

  expect_true("geom_vline_2" %in% names(obj$placeholder_map$geom_vline))
  expect_identical(obj$placeholder_map$geom_vline[["geom_vline_2"]]$keyword, "date")
  expect_s3_class(obj$placeholders, "ptr_define_placeholder_registry")
  expect_true("date" %in% names(obj$custom_placeholders))

  runtime <- ptr_exec(
    obj,
    list(
      "geom_line_checkbox" = TRUE,
      "geom_vline_checkbox" = TRUE,
      "geom_vline_2" = as.Date("2020-01-02")
    ),
    envir = test_sales_env()
  )

  expect_true(runtime$ok)
  expect_match(runtime$code_text, 'as.Date\\("2020-01-02"\\)')
  expect_s3_class(runtime$plot, "ggplot")
})

test_that("custom placeholders participate in copy rules and generated UI", {
  registry <- ptr_merge_placeholders(
    list(date = make_test_date_placeholder())
  )
  rules <- ptr_merge_ui_text(
    list(
      defaults = list(date = list(label = "Choose any date")),
      params = list(
        xintercept = list(date = list(label = "Reference date"))
      ),
      layers = list(
        geom_vline = list(
          date = list(
            xintercept = list(help = "Pick a cutoff date.")
          )
        )
      )
    ),
    placeholders = registry
  )

  resolved <- ptr_resolve_ui_text(
    "control",
    keyword = "date",
    layer_name = "geom_vline",
    param = "xintercept",
    ui_text = rules
  )
  obj <- ptr_parse_formula(test_date_formula, placeholders = registry)
  ui <- ptr_get_tab_ui(obj, ui_text = rules)
  ui_text <- placeholder_ui_text(ui)

  expect_identical(resolved$label, "Reference date")
  expect_identical(resolved$help, "Pick a cutoff date.")
  expect_match(ui_text, "Reference date", fixed = TRUE)
  expect_match(ui_text, "Pick a cutoff date.", fixed = TRUE)
})

test_that("custom placeholders work through ggpaintr wrappers", {
  registry <- ptr_merge_placeholders(
    list(date = make_test_date_placeholder())
  )
  app <- ptr_app(
    test_date_formula,
    envir = test_sales_env(),
    placeholders = registry
  )

  expect_s3_class(app, "shiny.appobj")

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input,
      output,
      session,
      test_date_formula,
      envir = test_sales_env(),
      placeholders = registry
    )
  }

  shiny::testServer(server_wrapper, {
    expect_s3_class(session$userData$ptr_state$placeholders, "ptr_define_placeholder_registry")
    expect_true("date" %in% names(session$userData$ptr_state$custom_placeholders))

    session$setInputs(
      "geom_line_checkbox" = TRUE,
      "geom_vline_checkbox" = TRUE,
      "geom_vline_2" = as.Date("2020-01-02"),
      draw = 1
    )

    runtime_result <- session$userData$ptr_state$runtime()
    expect_true(runtime_result$ok)
    expect_match(runtime_result$code_text, 'as.Date\\("2020-01-02"\\)')
  })
})
