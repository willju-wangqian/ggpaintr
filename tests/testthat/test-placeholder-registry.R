placeholder_ui_text <- function(ui) {
  paste(as.character(ui), collapse = "\n")
}

test_that("placeholder constructor validates keyword names and required hooks", {
  expect_error(
    ggpaintr_placeholder(
      keyword = "bad keyword",
      build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
      resolve_expr = function(value, meta, context) rlang::expr(x)
    ),
    "syntactic placeholder name"
  )

  expect_error(
    ggpaintr_placeholder(
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
    ggpaintr_effective_placeholders(list(date_placeholder, date_placeholder)),
    "duplicated keywords"
  )

  registry <- ggpaintr_effective_placeholders(list(date = date_placeholder))
  expect_identical(ggpaintr_effective_placeholders(registry), registry)
  expect_true("date" %in% names(registry))
})

test_that("custom placeholders are parsed into placeholder metadata and runtime output", {
  registry <- ggpaintr_effective_placeholders(
    list(date = make_test_date_placeholder())
  )
  obj <- ggpaintr_formula(test_date_formula, placeholders = registry)

  expect_true("geom_vline+2" %in% names(obj$placeholder_map$geom_vline))
  expect_identical(obj$placeholder_map$geom_vline[["geom_vline+2"]]$keyword, "date")
  expect_s3_class(obj$placeholders, "ggpaintr_placeholder_registry")
  expect_true("date" %in% names(obj$custom_placeholders))

  runtime <- ggpaintr_build_runtime(
    obj,
    list(
      "geom_line+checkbox" = TRUE,
      "geom_vline+checkbox" = TRUE,
      "geom_vline+2" = as.Date("2020-01-02")
    ),
    envir = test_sales_env()
  )

  expect_true(runtime$ok)
  expect_match(runtime$code_text, 'as.Date\\("2020-01-02"\\)')
  expect_s3_class(runtime$plot, "ggplot")
})

test_that("custom placeholders participate in copy rules and generated UI", {
  registry <- ggpaintr_effective_placeholders(
    list(date = make_test_date_placeholder())
  )
  rules <- ggpaintr_effective_copy_rules(
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

  resolved <- ggpaintr_resolve_copy(
    "control",
    keyword = "date",
    layer_name = "geom_vline",
    param = "xintercept",
    copy_rules = rules
  )
  obj <- ggpaintr_formula(test_date_formula, placeholders = registry)
  ui <- ggpaintr_get_tab_ui(obj, copy_rules = rules)
  ui_text <- placeholder_ui_text(ui)

  expect_identical(resolved$label, "Reference date")
  expect_identical(resolved$help, "Pick a cutoff date.")
  expect_match(ui_text, "Reference date", fixed = TRUE)
  expect_match(ui_text, "Pick a cutoff date.", fixed = TRUE)
})

test_that("custom placeholders work through ggpaintr wrappers", {
  registry <- ggpaintr_effective_placeholders(
    list(date = make_test_date_placeholder())
  )
  app <- ggpaintr_app(
    test_date_formula,
    envir = test_sales_env(),
    placeholders = registry
  )

  expect_s3_class(app, "shiny.appobj")

  server_wrapper <- function(input, output, session) {
    session$userData$ggpaintr_state <- ggpaintr_server(
      input,
      output,
      session,
      test_date_formula,
      envir = test_sales_env(),
      placeholders = registry
    )
  }

  shiny::testServer(server_wrapper, {
    expect_s3_class(session$userData$ggpaintr_state$placeholders, "ggpaintr_placeholder_registry")
    expect_true("date" %in% names(session$userData$ggpaintr_state$custom_placeholders))

    session$setInputs(
      "geom_line+checkbox" = TRUE,
      "geom_vline+checkbox" = TRUE,
      "geom_vline+2" = as.Date("2020-01-02"),
      draw = 1
    )

    runtime_result <- session$userData$ggpaintr_state$runtime()
    expect_true(runtime_result$ok)
    expect_match(runtime_result$code_text, 'as.Date\\("2020-01-02"\\)')
  })
})
