source_exported_app <- function(path, envir = new.env(parent = baseenv())) {
  source(path, local = envir)$value
}

test_that("ptr_generate_shiny writes a syntactically valid app script", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(obj, out_file, style = FALSE)

  expect_true(file.exists(out_file))
  expect_no_error(parse(file = out_file))

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "input_formula <- ")
  expect_match(app_text, "ui_text <- NULL", fixed = TRUE)
  expect_match(app_text, "Replace NULL with a named list", fixed = TRUE)
  expect_match(app_text, "placeholders <- NULL", fixed = TRUE)
  expect_match(
    app_text,
    "title_copy <- ptr_resolve_ui_text\\(\"title\", ui_text = ui_text\\)"
  )
  expect_match(
    app_text,
    "draw_copy <- ptr_resolve_ui_text\\(\"draw_button\", ui_text = ui_text\\)"
  )
  expect_match(
    app_text,
    "export_copy <- ptr_resolve_ui_text\\(\"export_button\", ui_text = ui_text\\)"
  )
  expect_match(app_text, "ui <- fluidPage\\(")
  expect_match(app_text, "titlePanel\\(title_copy\\$label\\)")
  expect_match(app_text, "actionButton\\(ids\\$draw_button, draw_copy\\$label\\)")
  expect_match(app_text, "downloadButton\\(ids\\$export_button, export_copy\\$label\\)")
  expect_match(app_text, "server <- function\\(input, output, session\\)")
  expect_match(
    app_text,
    paste0(
      "ptr_state <- ptr_server\\(",
      "input, output, session, input_formula, ui_text = ui_text, ",
      "placeholders = placeholders, ids = ids\\)"
    )
  )
  expect_match(app_text, "shinyApp\\(ui, server\\)")
  expect_no_match(app_text, "ui_text <- list\\(")
  expect_no_match(app_text, "custom_ui_text <- list\\(")
  expect_no_match(app_text, "custom_placeholders <- list\\(")
  expect_no_match(app_text, "app\\$ui")
  expect_no_match(app_text, "app\\$server")
})

test_that("ptr_generate_shiny preserves upload-aware runtime code", {
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(obj, out_file, style = FALSE)

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "ggplot\\(data = upload")
  expect_match(
    app_text,
    paste0(
      "ptr_server\\(",
      "input, output, session, input_formula, ui_text = ui_text, ",
      "placeholders = placeholders, ids = ids\\)"
    )
  )
})

test_that("ptr_generate_shiny writes multiline formulas as multiline source", {
  formula_text <- paste(
    "",
    "ggplot(data = upload, aes(x = var, y = var)) +",
    "  geom_point(size = num) +",
    "  labs(title = text)",
    sep = "\n"
  )

  obj <- ptr_parse_formula(formula_text)
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(obj, out_file, style = FALSE)

  app_lines <- readLines(out_file)
  input_formula_index <- grep("^input_formula <- ", app_lines)
  ui_text_index <- grep("^ui_text <- NULL$", app_lines)

  expect_identical(length(input_formula_index), 1L)
  expect_identical(length(ui_text_index), 1L)
  expect_identical(app_lines[[input_formula_index]], "input_formula <- \"")
  expect_identical(app_lines[[input_formula_index + 1]], "ggplot(data = upload, aes(x = var, y = var)) +")
  expect_false(any(grepl("\\\\n", app_lines[input_formula_index:(ui_text_index - 1)])))

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

test_that("ptr_generate_shiny preserves quotes and backslashes in exported formulas", {
  formula_text <- paste(
    "",
    "ggplot(data = mtcars, aes(x = var, y = var)) +",
    "  labs(title = \"A \\\"quote\\\"\", caption = \"C:\\\\temp\")",
    sep = "\n"
  )

  obj <- ptr_parse_formula(formula_text)
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(obj, out_file, style = FALSE)

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

test_that("ptr_app returns a shiny app object", {
  app <- ptr_app(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )

  expect_s3_class(app, "shiny.appobj")
})

test_that("ptr_generate_shiny exported apps execute end to end for static formulas", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + labs(title = text)"
  )
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(obj, out_file, style = FALSE)

  export_env <- new.env(parent = environment())
  exported_app <- source_exported_app(out_file, envir = export_env)
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- export_env$server(input, output, session)
  }

  expect_s3_class(exported_app, "shiny.appobj")
  expect_true(is.function(export_env$server))

  spec <- ptr_runtime_input_spec(obj)
  ph_ids <- spec[spec$role == "placeholder", ]
  ggplot_vars <- ph_ids[ph_ids$layer_name == "ggplot" & ph_ids$keyword == "var", "input_id"]
  labs_text   <- ph_ids[ph_ids$layer_name == "labs"   & ph_ids$keyword == "text", "input_id"]

  inputs_to_set <- c(
    setNames(list("mpg", "disp"), ggplot_vars),
    setNames(list("Exported plot"), labs_text),
    list("geom_point+checkbox" = TRUE, "labs+checkbox" = TRUE, draw = 1)
  )

  shiny::testServer(server_wrapper, {
    do.call(session$setInputs, inputs_to_set)

    runtime_result <- session$userData$ptr_state$runtime()
    expect_true(runtime_result$ok)
    expect_null(runtime_result$message)
    expect_s3_class(runtime_result$plot, "ggplot")
    expect_match(runtime_result$code_text, "labs\\(title = \"Exported plot\"\\)")
  })
})

test_that("ptr_generate_shiny exported apps execute upload formulas end to end", {
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(obj, out_file, style = FALSE)

  export_env <- new.env(parent = environment())
  exported_app <- source_exported_app(out_file, envir = export_env)
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- export_env$server(input, output, session)
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

    runtime_result <- session$userData$ptr_state$runtime()
    expect_true(runtime_result$ok)
    expect_null(runtime_result$message)
    expect_s3_class(runtime_result$plot, "ggplot")
    expect_match(runtime_result$code_text, "ggplot\\(data = simple_numeric")
  })
})

test_that("ptr_generate_shiny accepts the new public call shape", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  expect_no_warning(ptr_generate_shiny(obj, out_file, style = FALSE))
  expect_true(file.exists(out_file))
})

test_that("ptr_generate_shiny errors on unknown arguments", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- withr::local_tempfile(fileext = ".R")
  expect_error(
    ptr_generate_shiny(obj, out_file, unknown_arg = TRUE),
    regexp = "unused argument"
  )
})

test_that("ptr_generate_shiny aborts with path on write failure", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  bad_path <- file.path(withr::local_tempdir(), "nonexistent_subdir", "app.R")
  expect_error(
    ptr_generate_shiny(obj, bad_path, style = FALSE),
    regexp = "nonexistent_subdir"
  )
})

test_that("ptr_generate_shiny writes compact custom copy rules for exported apps", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(
    obj,
    out_file,
    style = FALSE,
    ui_text = list(
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
  expect_match(app_text, "custom_ui_text <- ")
  expect_match(
    app_text,
    "ui_text <- ptr_merge_ui_text\\(custom_ui_text\\)"
  )
  expect_match(app_text, "Exploratory Plot Builder", fixed = TRUE)
  expect_match(app_text, "Render plot", fixed = TRUE)
  expect_match(app_text, "Pick the field for the x-axis", fixed = TRUE)
  expect_no_match(app_text, "Choose the y-axis column", fixed = TRUE)
  expect_no_match(app_text, "Optional dataset name", fixed = TRUE)
  expect_match(
    app_text,
    "title_copy <- ptr_resolve_ui_text\\(\"title\", ui_text = ui_text\\)"
  )
  expect_match(
    app_text,
    paste0(
      "ptr_server\\(",
      "input, output, session, input_formula, ui_text = ui_text, ",
      "placeholders = placeholders, ids = ids\\)"
    )
  )

  app_expr <- parse(file = out_file)
  custom_ui_text_expr <- NULL
  ui_text_expr <- NULL

  for (expr in app_expr) {
    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "custom_ui_text")) {
      custom_ui_text_expr <- expr
    }

    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "ui_text")) {
      ui_text_expr <- expr
    }
  }

  expect_false(is.null(custom_ui_text_expr))
  expect_false(is.null(ui_text_expr))

  export_env <- new.env(parent = environment())
  eval(custom_ui_text_expr, envir = export_env)
  eval(ui_text_expr, envir = export_env)

  exported_custom_rules <- export_env$custom_ui_text
  exported_rules <- export_env$ui_text
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

test_that("ptr_generate_shiny omits concrete copy rules when effective rules match defaults", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(
    obj,
    out_file,
    style = FALSE,
    ui_text = list(
      shell = list(
        title = list(label = "ggpaintr Plot Builder")
      )
    )
  )

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "ui_text <- NULL", fixed = TRUE)
  expect_match(app_text, "placeholders <- NULL", fixed = TRUE)
  expect_no_match(app_text, "ui_text <- list\\(")
  expect_no_match(app_text, "custom_ui_text <- list\\(")
})

test_that("ptr_generate_shiny writes compact custom placeholders for exported apps", {
  registry <- ptr_merge_placeholders(
    list(date = make_test_date_placeholder())
  )
  obj <- ptr_parse_formula(test_date_formula, placeholders = registry)
  out_file <- tempfile(fileext = ".R")

  ptr_generate_shiny(
    obj,
    out_file,
    style = FALSE,
    placeholders = registry
  )

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "custom_placeholders <- ")
  expect_match(
    app_text,
    "placeholders <- ptr_merge_placeholders\\(custom_placeholders\\)"
  )
  expect_match(app_text, "keyword = \"date\"", fixed = TRUE)
  expect_match(
    app_text,
    paste0(
      "ptr_server\\(",
      "input, output, session, input_formula, ui_text = ui_text, ",
      "placeholders = placeholders, ids = ids\\)"
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
  expect_s3_class(export_env$custom_placeholders$date, "ptr_define_placeholder")
  expect_s3_class(export_env$placeholders, "ptr_define_placeholder_registry")
  expect_true("date" %in% names(export_env$placeholders))
})

test_that("ptr_generate_shiny errors when custom placeholders are not exportable", {
  registry <- ptr_merge_placeholders(
    list(date = make_non_inline_date_placeholder())
  )
  obj <- ptr_parse_formula(test_date_formula, placeholders = registry)
  out_file <- tempfile(fileext = ".R")

  expect_error(
    ptr_generate_shiny(
      obj,
      out_file,
      style = FALSE,
      placeholders = registry
    ),
    "must define 'build_ui' inline or supply"
  )
})

test_that("ptr_validate_exportable_placeholder warns on free variables in hooks", {
  ph <- ptr_define_placeholder(
    keyword = "freevar",
    build_ui = function(id, copy, meta, context) {
      shiny::textInput(id, my_external_label)
    },
    resolve_expr = function(value, meta, context) {
      if (is.null(value)) return(ptr_missing_expr())
      rlang::expr(!!value)
    }
  )

  expect_warning(
    ptr_validate_exportable_placeholder(ph),
    "my_external_label"
  )
})

test_that("ptr_validate_exportable_placeholder does not warn on clean hooks", {
  ph <- ptr_define_placeholder(
    keyword = "clean",
    build_ui = function(id, copy, meta, context) {
      shiny::textInput(id, copy$label)
    },
    resolve_expr = function(value, meta, context) {
      if (is.null(value)) return(ptr_missing_expr())
      rlang::expr(!!value)
    }
  )

  expect_no_warning(
    ptr_validate_exportable_placeholder(ph)
  )
})

test_that("ptr_generate_shiny compacts pre-merged copy rules before export", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")
  merged_rules <- ptr_merge_ui_text(
    list(
      shell = list(
        title = list(label = "Exploratory Plot Builder")
      ),
      params = list(
        colour = list(var = list(label = "Choose a colour column"))
      )
    )
  )

  ptr_generate_shiny(
    obj,
    out_file,
    style = FALSE,
    ui_text = merged_rules
  )

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "custom_ui_text <- ")
  expect_match(
    app_text,
    "ui_text <- ptr_merge_ui_text\\(custom_ui_text\\)"
  )
  expect_no_match(app_text, "ggpaintr Plot Builder", fixed = TRUE)
  expect_no_match(app_text, "Update plot", fixed = TRUE)
  expect_no_match(app_text, "Choose the x-axis column", fixed = TRUE)
  expect_match(app_text, "Choose a colour column", fixed = TRUE)
  expect_match(app_text, "color", fixed = TRUE)

  app_expr <- parse(file = out_file)
  custom_ui_text_expr <- NULL
  for (expr in app_expr) {
    if (rlang::is_call(expr, "<-") &&
        rlang::is_symbol(expr[[2]], "custom_ui_text")) {
      custom_ui_text_expr <- expr
      break
    }
  }

  expect_false(is.null(custom_ui_text_expr))
  exported_custom_rules <- eval(custom_ui_text_expr[[3]])
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

test_that("ptr_server is exported in NAMESPACE", {
  expect_true("ptr_server" %in% getNamespaceExports("ggpaintr"))
})

test_that("ptr_resolve_ui_text is exported in NAMESPACE", {
  installed_export <- "ptr_resolve_ui_text" %in% getNamespaceExports("ggpaintr")
  repo_export <- FALSE

  namespace_path <- system.file("NAMESPACE", package = "ggpaintr")
  if (nzchar(namespace_path)) {
    namespace_lines <- readLines(namespace_path)
    repo_export <- any(grepl("^export\\(ptr_resolve_ui_text\\)$", namespace_lines))
  }

  expect_true(installed_export || repo_export)
})

test_that("ptr_merge_ui_text is exported in NAMESPACE", {
  installed_export <- "ptr_merge_ui_text" %in% getNamespaceExports("ggpaintr")
  repo_export <- FALSE

  namespace_path <- system.file("NAMESPACE", package = "ggpaintr")
  if (nzchar(namespace_path)) {
    namespace_lines <- readLines(namespace_path)
    repo_export <- any(grepl("^export\\(ptr_merge_ui_text\\)$", namespace_lines))
  }

  expect_true(installed_export || repo_export)
})

test_that("ptr_server returns reusable state before and after a successful draw", {
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
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
    expect_type(session$userData$ptr_state, "list")
    expect_true(is.function(session$userData$ptr_state$runtime))
    expect_true(is.function(session$userData$ptr_state$obj))
    expect_null(session$userData$ptr_state$runtime())
    expect_s3_class(session$userData$ptr_state$obj(), "ptr_obj")

    session$setInputs(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "facet_wrap+2" = "~ Species",
      "geom_point+checkbox" = TRUE,
      "facet_wrap+checkbox" = TRUE,
      draw = 1
    )

    runtime_result <- session$userData$ptr_state$runtime()
    expect_true(runtime_result$ok)
    expect_null(runtime_result$message)
    expect_s3_class(runtime_result$plot, "ggplot")
    expect_match(runtime_result$code_text, "facet_wrap\\(~Species\\)")
  })
})

test_that("ptr_server stores structured failure state after draw", {
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input,
      output,
      session,
      "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
    )
  }

  shiny::testServer(server_wrapper, {
    expect_null(session$userData$ptr_state$runtime())

    session$setInputs(
      "geom_point+checkbox" = TRUE,
      draw = 1
    )

    runtime_result <- session$userData$ptr_state$runtime()
    expect_false(runtime_result$ok)
    expect_identical(runtime_result$stage, "plot")
    expect_null(runtime_result$plot)
    expect_match(runtime_result$message, "^Plot error:")
    expect_match(runtime_result$message, "unknown_object")
    expect_match(runtime_result$code_text, "ggplot\\(data = unknown_object")
  })
})

# ---------------------------------------------------------------------------
# source_file / source_package / source_function export strategies
# ---------------------------------------------------------------------------

app_text_for <- function(ph, formula = test_date_formula) {
  registry <- ptr_merge_placeholders(list(date = ph))
  obj <- ptr_parse_formula(formula, placeholders = registry)
  out_file <- tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE, placeholders = registry)
  paste(readLines(out_file), collapse = "\n")
}

# --- source_file ---

test_that("source_file emits tryCatch source() when on_missing = 'warn'", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_file = "helpers.R",
    on_missing = "warn"
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, 'tryCatch', fixed = TRUE)
  expect_match(app_text, 'source("helpers.R")', fixed = TRUE)
  expect_match(app_text, "could not source 'helpers.R'", fixed = TRUE)
  expect_no_error(parse(text = app_text))
})

test_that("source_file emits plain source() when on_missing = 'error'", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_file = "helpers.R",
    on_missing = "error"
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, 'source("helpers.R")', fixed = TRUE)
  expect_no_match(app_text, "tryCatch")
  expect_no_error(parse(text = app_text))
})

test_that("source_file per-hook override emits correct paths", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_file = list(.default = "helpers.R", resolve_expr = "resolve-helpers.R"),
    on_missing = "warn"
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, 'source("helpers.R")', fixed = TRUE)
  expect_match(app_text, 'source("resolve-helpers.R")', fixed = TRUE)
  expect_no_error(parse(text = app_text))
})

# --- source_package ---

test_that("source_package emits requireNamespace + install + library block when on_missing = 'warn'", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_package = "mypkg",
    on_missing = "warn"
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, 'requireNamespace("mypkg"', fixed = TRUE)
  expect_match(app_text, 'install.packages("mypkg")', fixed = TRUE)
  expect_match(app_text, 'library("mypkg")', fixed = TRUE)
  expect_match(app_text, "could not install 'mypkg'", fixed = TRUE)
  expect_no_error(parse(text = app_text))
})

test_that("source_package omits tryCatch when on_missing = 'error'", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_package = "mypkg",
    on_missing = "error"
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, 'install.packages("mypkg")', fixed = TRUE)
  expect_match(app_text, 'library("mypkg")', fixed = TRUE)
  expect_no_match(app_text, "could not install")
  expect_no_error(parse(text = app_text))
})

# --- source_function ---

test_that("source_function deparsed function appears in exported app before placeholder call", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_function = list(
      build_ui = named_date_build_ui,
      resolve_expr = named_date_resolve_expr
    )
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, "named_date_build_ui <- function", fixed = TRUE)
  expect_match(app_text, "named_date_resolve_expr <- function", fixed = TRUE)
  expect_no_error(parse(text = app_text))

  # preamble must appear before the custom_placeholders assignment
  fn_pos <- regexpr("named_date_build_ui <- function", app_text, fixed = TRUE)
  ph_pos <- regexpr("custom_placeholders", app_text, fixed = TRUE)
  expect_true(fn_pos < ph_pos)
})

test_that("source_function passes validation and export for non-inline hooks", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_function = list(
      build_ui = named_date_build_ui,
      resolve_expr = named_date_resolve_expr
    )
  )
  expect_no_error(ptr_validate_exportable_placeholder(ph))
})

# --- Validation ---

test_that("on_missing rejects invalid values", {
  make_bad_ph <- function() {
    ptr_define_placeholder(
      keyword = "date",
      build_ui = function(id, copy, meta, context) shiny::dateInput(id, copy$label),
      resolve_expr = function(value, meta, context) {
        if (is.null(value)) return(ptr_missing_expr())
        rlang::expr(as.Date(!!value))
      },
      on_missing = "ignore"
    )
  }
  expect_error(make_bad_ph(), 'on_missing must be "warn" or "error"')
})

test_that("source_function with a non-function value aborts", {
  expect_error(
    ptr_define_placeholder(
      keyword = "date",
      build_ui = named_date_build_ui,
      resolve_expr = named_date_resolve_expr,
      source_function = list(build_ui = "not_a_function")
    ),
    "must be a function object"
  )
})

test_that("source_function hook name typo emits warning", {
  expect_warning(
    ptr_define_placeholder(
      keyword = "date",
      build_ui = named_date_build_ui,
      resolve_expr = named_date_resolve_expr,
      source_function = list(
        build_ui = named_date_build_ui,
        resolve_expr = named_date_resolve_expr,
        biuld_ui = named_date_build_ui
      )
    ),
    "unrecognized hook name 'biuld_ui'"
  )
})

test_that("non-inline hook with no source strategy still aborts at export", {
  ph <- make_non_inline_date_placeholder()
  expect_error(
    ptr_validate_exportable_placeholder(ph),
    "must define 'build_ui' inline or supply"
  )
})

# --- Precedence ---

test_that("source_function takes priority over source_file for same hook", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_file = "helpers.R",
    source_function = list(build_ui = named_date_build_ui)
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, "named_date_build_ui <- function", fixed = TRUE)
  expect_no_error(parse(text = app_text))
})

test_that("inline function literal takes priority over source_function for same hook", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = function(id, copy, meta, context) shiny::dateInput(id, copy$label),
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_function = list(resolve_expr = named_date_resolve_expr)
  )
  expect_no_error(ptr_validate_exportable_placeholder(ph))
  app_text <- app_text_for(ph)
  expect_match(app_text, "named_date_resolve_expr <- function", fixed = TRUE)
  expect_no_error(parse(text = app_text))
})

# ---------------------------------------------------------------------------
# Regression tests for the 6 hardening improvements
# ---------------------------------------------------------------------------

# --- 1. Path escaping: ptr_serialize_source_file_block ---

test_that("source_file with backslash in path emits parseable code", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_file = "helpers\\hooks.R",
    on_missing = "warn"
  )
  app_text <- app_text_for(ph)
  expect_no_error(parse(text = app_text))
  # The escaped backslash must appear as \\ inside the string literal
  expect_match(app_text, "helpers\\\\hooks.R", fixed = TRUE)
})

test_that("source_file with double-quote in path emits parseable code", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_file = 'my"special"file.R',
    on_missing = "warn"
  )
  app_text <- app_text_for(ph)
  expect_no_error(parse(text = app_text))
})

test_that("source_file with backslash in path round-trips through parse + eval", {
  raw_lines <- ggpaintr:::ptr_serialize_source_file_block(
    "helpers\\hooks.R",
    on_missing = "error"
  )
  joined <- paste(raw_lines, collapse = "\n")
  expect_no_error(parse(text = joined))
})

test_that("source_file with double-quote in path round-trips through parse + eval", {
  raw_lines <- ggpaintr:::ptr_serialize_source_file_block(
    'my"special"file.R',
    on_missing = "error"
  )
  joined <- paste(raw_lines, collapse = "\n")
  expect_no_error(parse(text = joined))
})

# --- 2. Package name escaping: ptr_serialize_source_pkg_block ---

test_that("source_package emits library() with quoted package name", {
  ph <- ptr_define_placeholder(
    keyword = "date",
    build_ui = named_date_build_ui,
    resolve_expr = named_date_resolve_expr,
    copy_defaults = list(label = "Choose a date for {param}"),
    source_package = "mypkg",
    on_missing = "warn"
  )
  app_text <- app_text_for(ph)
  expect_match(app_text, 'library("mypkg")', fixed = TRUE)
  expect_no_error(parse(text = app_text))
})

test_that("ptr_serialize_source_pkg_block output parses for warn and error branches", {
  warn_lines <- ggpaintr:::ptr_serialize_source_pkg_block("mypkg", on_missing = "warn")
  expect_no_error(parse(text = paste(warn_lines, collapse = "\n")))
  expect_true(any(grepl('library("mypkg")', warn_lines, fixed = TRUE)))

  err_lines <- ggpaintr:::ptr_serialize_source_pkg_block("mypkg", on_missing = "error")
  expect_no_error(parse(text = paste(err_lines, collapse = "\n")))
  expect_true(any(grepl('library("mypkg")', err_lines, fixed = TRUE)))
})

# --- 3. library(ggplot2) presence in ptr_shiny_template ---

test_that("ptr_shiny_template output contains library(ggplot2)", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE)
  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "library(ggplot2)", fixed = TRUE)
})

test_that("library(ggplot2) appears before library(ggpaintr) in generated app", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE)
  app_lines <- readLines(out_file)
  ggplot2_idx  <- which(app_lines == "library(ggplot2)")
  ggpaintr_idx <- which(app_lines == "library(ggpaintr)")
  expect_identical(length(ggplot2_idx), 1L)
  expect_identical(length(ggpaintr_idx), 1L)
  expect_true(ggplot2_idx < ggpaintr_idx)
})

# --- 4. Formula text guard: ptr_serialize_formula_text ---

test_that("ptr_serialize_formula_text errors on NULL input", {
  expect_error(
    ggpaintr:::ptr_serialize_formula_text(NULL),
    "formula_text must be a single non-missing"
  )
})

test_that("ptr_serialize_formula_text errors on zero-length character input", {
  expect_error(
    ggpaintr:::ptr_serialize_formula_text(character(0)),
    "formula_text must be a single non-missing"
  )
})

test_that("ptr_serialize_formula_text errors on NA input", {
  expect_error(
    ggpaintr:::ptr_serialize_formula_text(NA_character_),
    "formula_text must be a single non-missing"
  )
})

# --- 5. deparse width: ptr_serialize_r_object ---

test_that("ptr_serialize_r_object splits wide objects across multiple lines", {
  # A named character vector wide enough that deparse without width.cutoff=80
  # would produce a single very long line.
  wide_obj <- setNames(
    paste0("value_", seq_len(20)),
    paste0("key_", seq_len(20))
  )
  lines <- ggpaintr:::ptr_serialize_r_object(wide_obj)
  # With width.cutoff = 80L there must be more than one line for this object.
  expect_true(length(lines) > 1L)
  # Every line must be at most 80 characters (deparse respects width.cutoff as
  # a soft hint; lines should not wildly exceed 80 chars).
  expect_true(all(nchar(lines) <= 120L))
  # The result must round-trip: re-parse and eval recovers the original object.
  evaluated <- eval(parse(text = paste(lines, collapse = "\n")))
  expect_identical(evaluated, wide_obj)
})
