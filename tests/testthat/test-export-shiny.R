test_that("generate_shiny writes a syntactically valid app script", {
  obj <- paintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(obj, list(), out_file, style = FALSE)

  expect_true(file.exists(out_file))
  expect_no_error(parse(file = out_file))

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "input_formula <- ")
  expect_match(app_text, "ui <- fluidPage\\(")
  expect_match(app_text, "server <- function\\(input, output, session\\)")
  expect_match(app_text, "paintr_state <- ggpaintr_server\\(input, output, session, input_formula\\)")
  expect_match(app_text, "shinyApp\\(ui, server\\)")
  expect_no_match(app_text, "app\\$ui")
  expect_no_match(app_text, "app\\$server")
})

test_that("generate_shiny preserves upload-aware runtime code", {
  obj <- paintr_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(obj, list(), out_file, style = FALSE)

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "ggplot\\(data = upload")
  expect_match(app_text, "ggpaintr_server\\(input, output, session, input_formula\\)")
})

test_that("generate_shiny preserves the formula text in the exported app", {
  formula_text <- paste(
    "",
    "ggplot(data = upload, aes(x = var, y = var)) +",
    "  geom_point(size = num) +",
    "  labs(title = text)",
    sep = "\n"
  )

  obj <- paintr_formula(formula_text)
  out_file <- tempfile(fileext = ".R")

  generate_shiny(obj, list(), out_file, style = FALSE)

  app_expr <- parse(file = out_file)
  input_formula_expr <- NULL

  for (expr in app_expr) {
    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "input_formula")) {
      input_formula_expr <- expr
      break
    }
  }

  expect_false(is.null(input_formula_expr))
  expect_identical(eval(input_formula_expr[[3]]), formula_text)
})

test_that("ggpaintr_app returns a shiny app object", {
  app <- ggpaintr_app(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  expect_s3_class(app, "shiny.appobj")
})

test_that("ggpaintr_server is exported in NAMESPACE", {
  namespace_text <- paste(
    readLines(testthat::test_path("..", "..", "NAMESPACE")),
    collapse = "\n"
  )

  expect_match(namespace_text, "export\\(ggpaintr_server\\)")
})

test_that("ggpaintr_server returns reusable state before and after a successful draw", {
  server_wrapper <- function(input, output, session) {
    session$userData$paintr_state <- ggpaintr_server(
      input,
      output,
      session,
      paste(
        "ggplot(data = iris, aes(x = var, y = var)) +",
        "geom_point() +",
        "facet_wrap(expr)"
      )
    )
  }

  shiny::testServer(server_wrapper, {
    expect_type(session$userData$paintr_state, "list")
    expect_true(is.function(session$userData$paintr_state$runtime))
    expect_true(is.function(session$userData$paintr_state$obj))
    expect_null(session$userData$paintr_state$runtime())
    expect_s3_class(session$userData$paintr_state$obj(), "paintr_obj")

    session$setInputs(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "facet_wrap+2" = "~ Species",
      "geom_point+checkbox" = TRUE,
      "facet_wrap+checkbox" = TRUE,
      draw = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
    expect_true(runtime_result$ok)
    expect_null(runtime_result$message)
    expect_s3_class(runtime_result$plot, "ggplot")
    expect_match(runtime_result$code_text, "facet_wrap\\(~Species\\)")
  })
})

test_that("ggpaintr_server stores structured failure state after draw", {
  server_wrapper <- function(input, output, session) {
    session$userData$paintr_state <- ggpaintr_server(
      input,
      output,
      session,
      "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
    )
  }

  shiny::testServer(server_wrapper, {
    expect_null(session$userData$paintr_state$runtime())

    session$setInputs(
      "geom_point+checkbox" = TRUE,
      draw = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
    expect_false(runtime_result$ok)
    expect_identical(runtime_result$stage, "plot")
    expect_null(runtime_result$plot)
    expect_match(runtime_result$message, "^Plot error:")
    expect_match(runtime_result$message, "unknown_object")
    expect_match(runtime_result$code_text, "ggplot\\(data = unknown_object")
  })
})
