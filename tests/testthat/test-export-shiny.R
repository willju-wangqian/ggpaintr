source_exported_app <- function(path, envir = new.env(parent = baseenv())) {
  source(path, local = envir)$value
}

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
  expect_match(app_text, "copy_rules <- NULL", fixed = TRUE)
  expect_match(app_text, "Replace NULL with a named list", fixed = TRUE)
  expect_match(app_text, "placeholders <- NULL", fixed = TRUE)
  expect_match(
    app_text,
    "title_copy <- paintr_resolve_copy\\(\"title\", copy_rules = copy_rules\\)"
  )
  expect_match(
    app_text,
    "draw_copy <- paintr_resolve_copy\\(\"draw_button\", copy_rules = copy_rules\\)"
  )
  expect_match(
    app_text,
    "export_copy <- paintr_resolve_copy\\(\"export_button\", copy_rules = copy_rules\\)"
  )
  expect_match(app_text, "ui <- fluidPage\\(")
  expect_match(app_text, "titlePanel\\(title_copy\\$label\\)")
  expect_match(app_text, "actionButton\\(\"draw\", draw_copy\\$label\\)")
  expect_match(app_text, "downloadButton\\(\"shinyExport\", export_copy\\$label\\)")
  expect_match(app_text, "server <- function\\(input, output, session\\)")
  expect_match(
    app_text,
    paste0(
      "paintr_state <- ggpaintr_server\\(",
      "input, output, session, input_formula, copy_rules = copy_rules, ",
      "placeholders = placeholders\\)"
    )
  )
  expect_match(app_text, "shinyApp\\(ui, server\\)")
  expect_no_match(app_text, "copy_rules <- list\\(")
  expect_no_match(app_text, "custom_copy_rules <- list\\(")
  expect_no_match(app_text, "custom_placeholders <- list\\(")
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
  expect_match(
    app_text,
    paste0(
      "ggpaintr_server\\(",
      "input, output, session, input_formula, copy_rules = copy_rules, ",
      "placeholders = placeholders\\)"
    )
  )
})

test_that("generate_shiny writes multiline formulas as multiline source", {
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

  app_lines <- readLines(out_file)
  input_formula_index <- grep("^input_formula <- ", app_lines)
  copy_rules_index <- grep("^copy_rules <- NULL$", app_lines)

  expect_identical(length(input_formula_index), 1L)
  expect_identical(length(copy_rules_index), 1L)
  expect_identical(app_lines[[input_formula_index]], "input_formula <- \"")
  expect_identical(app_lines[[input_formula_index + 1]], "ggplot(data = upload, aes(x = var, y = var)) +")
  expect_false(any(grepl("\\\\n", app_lines[input_formula_index:(copy_rules_index - 1)])))

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

test_that("generate_shiny preserves quotes and backslashes in exported formulas", {
  formula_text <- paste(
    "",
    "ggplot(data = mtcars, aes(x = var, y = var)) +",
    "  labs(title = \"A \\\"quote\\\"\", caption = \"C:\\\\temp\")",
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

test_that("generate_shiny exported apps execute end to end for static formulas", {
  obj <- paintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + labs(title = text)"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(obj, list(), out_file, style = FALSE)

  export_env <- new.env(parent = environment())
  exported_app <- source_exported_app(out_file, envir = export_env)
  server_wrapper <- function(input, output, session) {
    session$userData$paintr_state <- export_env$server(input, output, session)
  }

  expect_s3_class(exported_app, "shiny.appobj")
  expect_true(is.function(export_env$server))

  shiny::testServer(server_wrapper, {
    session$setInputs(
      "ggplot+3+2" = "mpg",
      "ggplot+3+3" = "disp",
      "labs+2" = "Exported plot",
      "geom_point+checkbox" = TRUE,
      "labs+checkbox" = TRUE,
      draw = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
    expect_true(runtime_result$ok)
    expect_null(runtime_result$message)
    expect_s3_class(runtime_result$plot, "ggplot")
    expect_match(runtime_result$code_text, "labs\\(title = \"Exported plot\"\\)")
  })
})

test_that("generate_shiny exported apps execute upload formulas end to end", {
  obj <- paintr_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(obj, list(), out_file, style = FALSE)

  export_env <- new.env(parent = environment())
  exported_app <- source_exported_app(out_file, envir = export_env)
  server_wrapper <- function(input, output, session) {
    session$userData$paintr_state <- export_env$server(input, output, session)
  }

  expect_s3_class(exported_app, "shiny.appobj")

  shiny::testServer(server_wrapper, {
    session$setInputs(
      "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
      "ggplot+2+name" = "",
      "ggplot+3+2" = "x",
      "ggplot+3+3" = "y",
      "geom_point+checkbox" = TRUE,
      draw = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
    expect_true(runtime_result$ok)
    expect_null(runtime_result$message)
    expect_s3_class(runtime_result$plot, "ggplot")
    expect_match(runtime_result$code_text, "ggplot\\(data = simple_numeric")
  })
})

test_that("generate_shiny writes compact custom copy rules for exported apps", {
  obj <- paintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(
    obj,
    list(),
    out_file,
    style = FALSE,
    copy_rules = list(
      shell = list(
        title = list(label = "Exploratory Plot Builder"),
        draw_button = list(label = "Render plot")
      ),
      params = list(
        x = list(var = list(label = "Pick the field for the x-axis"))
      )
    )
  )

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "custom_copy_rules <- ")
  expect_match(
    app_text,
    "copy_rules <- paintr_effective_copy_rules\\(custom_copy_rules\\)"
  )
  expect_match(app_text, "Exploratory Plot Builder", fixed = TRUE)
  expect_match(app_text, "Render plot", fixed = TRUE)
  expect_match(app_text, "Pick the field for the x-axis", fixed = TRUE)
  expect_no_match(app_text, "Choose the y-axis column", fixed = TRUE)
  expect_no_match(app_text, "Optional dataset name", fixed = TRUE)
  expect_match(
    app_text,
    "title_copy <- paintr_resolve_copy\\(\"title\", copy_rules = copy_rules\\)"
  )
  expect_match(
    app_text,
    paste0(
      "ggpaintr_server\\(",
      "input, output, session, input_formula, copy_rules = copy_rules, ",
      "placeholders = placeholders\\)"
    )
  )

  app_expr <- parse(file = out_file)
  custom_copy_rules_expr <- NULL
  copy_rules_expr <- NULL

  for (expr in app_expr) {
    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "custom_copy_rules")) {
      custom_copy_rules_expr <- expr
    }

    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "copy_rules")) {
      copy_rules_expr <- expr
    }
  }

  expect_false(is.null(custom_copy_rules_expr))
  expect_false(is.null(copy_rules_expr))

  export_env <- new.env(parent = environment())
  eval(custom_copy_rules_expr, envir = export_env)
  eval(copy_rules_expr, envir = export_env)

  exported_custom_rules <- export_env$custom_copy_rules
  exported_rules <- export_env$copy_rules
  expect_identical(
    exported_custom_rules,
    list(
      shell = list(
        title = list(label = "Exploratory Plot Builder"),
        draw_button = list(label = "Render plot")
      ),
      params = list(
        x = list(var = list(label = "Pick the field for the x-axis"))
      )
    )
  )
  expect_identical(exported_rules$shell$title$label, "Exploratory Plot Builder")
  expect_identical(exported_rules$shell$draw_button$label, "Render plot")
  expect_identical(exported_rules$params$x$var$label, "Pick the field for the x-axis")
})

test_that("generate_shiny omits concrete copy rules when effective rules match defaults", {
  obj <- paintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  generate_shiny(
    obj,
    list(),
    out_file,
    style = FALSE,
    copy_rules = list(
      shell = list(
        title = list(label = "ggpaintr Plot Builder")
      )
    )
  )

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "copy_rules <- NULL", fixed = TRUE)
  expect_match(app_text, "placeholders <- NULL", fixed = TRUE)
  expect_no_match(app_text, "copy_rules <- list\\(")
  expect_no_match(app_text, "custom_copy_rules <- list\\(")
})

test_that("generate_shiny writes compact custom placeholders for exported apps", {
  registry <- ggpaintr_effective_placeholders(
    list(date = make_test_date_placeholder())
  )
  obj <- paintr_formula(test_date_formula, placeholders = registry)
  out_file <- tempfile(fileext = ".R")

  generate_shiny(
    obj,
    list(),
    out_file,
    style = FALSE,
    placeholders = registry
  )

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "custom_placeholders <- ")
  expect_match(
    app_text,
    "placeholders <- ggpaintr_effective_placeholders\\(custom_placeholders\\)"
  )
  expect_match(app_text, "keyword = \"date\"", fixed = TRUE)
  expect_match(
    app_text,
    paste0(
      "ggpaintr_server\\(",
      "input, output, session, input_formula, copy_rules = copy_rules, ",
      "placeholders = placeholders\\)"
    )
  )
  expect_no_match(app_text, "custom_placeholders <- list\\(var = ")

  app_expr <- parse(file = out_file)
  custom_placeholders_expr <- NULL
  placeholders_expr <- NULL

  for (expr in app_expr) {
    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "custom_placeholders")) {
      custom_placeholders_expr <- expr
    }

    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "placeholders")) {
      placeholders_expr <- expr
    }
  }

  expect_false(is.null(custom_placeholders_expr))
  expect_false(is.null(placeholders_expr))

  export_env <- new.env(parent = environment())
  eval(custom_placeholders_expr, envir = export_env)
  eval(placeholders_expr, envir = export_env)

  expect_true("date" %in% names(export_env$custom_placeholders))
  expect_s3_class(export_env$custom_placeholders$date, "ggpaintr_placeholder")
  expect_s3_class(export_env$placeholders, "ggpaintr_placeholder_registry")
  expect_true("date" %in% names(export_env$placeholders))
})

test_that("generate_shiny errors when custom placeholders are not exportable", {
  registry <- ggpaintr_effective_placeholders(
    list(date = make_non_inline_date_placeholder())
  )
  obj <- paintr_formula(test_date_formula, placeholders = registry)
  out_file <- tempfile(fileext = ".R")

  expect_error(
    generate_shiny(
      obj,
      list(),
      out_file,
      style = FALSE,
      placeholders = registry
    ),
    "must define build_ui inline"
  )
})

test_that("generate_shiny compacts pre-merged copy rules before export", {
  obj <- paintr_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")
  merged_rules <- paintr_effective_copy_rules(
    list(
      shell = list(
        title = list(label = "Exploratory Plot Builder")
      ),
      params = list(
        colour = list(var = list(label = "Choose a colour column"))
      )
    )
  )

  generate_shiny(
    obj,
    list(),
    out_file,
    style = FALSE,
    copy_rules = merged_rules
  )

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "custom_copy_rules <- ")
  expect_match(
    app_text,
    "copy_rules <- paintr_effective_copy_rules\\(custom_copy_rules\\)"
  )
  expect_no_match(app_text, "ggpaintr Plot Builder", fixed = TRUE)
  expect_no_match(app_text, "Update plot", fixed = TRUE)
  expect_no_match(app_text, "Choose the x-axis column", fixed = TRUE)
  expect_match(app_text, "Choose a colour column", fixed = TRUE)
  expect_match(app_text, "color", fixed = TRUE)

  app_expr <- parse(file = out_file)
  custom_copy_rules_expr <- NULL
  for (expr in app_expr) {
    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "custom_copy_rules")) {
      custom_copy_rules_expr <- expr
      break
    }
  }

  expect_false(is.null(custom_copy_rules_expr))
  exported_custom_rules <- eval(custom_copy_rules_expr[[3]])
  expect_identical(
    exported_custom_rules,
    list(
      shell = list(
        title = list(label = "Exploratory Plot Builder")
      ),
      params = list(
        color = list(var = list(label = "Choose a colour column"))
      )
    )
  )
})

test_that("ggpaintr_server is exported in NAMESPACE", {
  expect_true("ggpaintr_server" %in% getNamespaceExports("ggpaintr"))
})

test_that("paintr_resolve_copy is exported in NAMESPACE", {
  installed_export <- "paintr_resolve_copy" %in% getNamespaceExports("ggpaintr")
  repo_export <- FALSE

  namespace_candidates <- c(
    file.path(getwd(), "NAMESPACE"),
    testthat::test_path("..", "..", "NAMESPACE"),
    testthat::test_path("..", "NAMESPACE")
  )
  namespace_path <- namespace_candidates[file.exists(namespace_candidates)][1]

  if (!is.na(namespace_path) && nzchar(namespace_path)) {
    namespace_lines <- readLines(namespace_path)
    repo_export <- any(grepl("^export\\(paintr_resolve_copy\\)$", namespace_lines))
  }

  expect_true(installed_export || repo_export)
})

test_that("paintr_effective_copy_rules is exported in NAMESPACE", {
  installed_export <- "paintr_effective_copy_rules" %in% getNamespaceExports("ggpaintr")
  repo_export <- FALSE

  namespace_candidates <- c(
    file.path(getwd(), "NAMESPACE"),
    testthat::test_path("..", "..", "NAMESPACE"),
    testthat::test_path("..", "NAMESPACE")
  )
  namespace_path <- namespace_candidates[file.exists(namespace_candidates)][1]

  if (!is.na(namespace_path) && nzchar(namespace_path)) {
    namespace_lines <- readLines(namespace_path)
    repo_export <- any(grepl("^export\\(paintr_effective_copy_rules\\)$", namespace_lines))
  }

  expect_true(installed_export || repo_export)
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
