ext_ui_text <- function(ui) {
  paste(as.character(ui), collapse = "\n")
}

test_that("ptr_build_ids validates defaults and rejects invalid registries", {
  ids <- ptr_build_ids()

  expect_s3_class(ids, "ptr_build_ids")
  expect_identical(
    unclass(ids),
    list(
      control_panel = "controlPanel",
      draw_button = "draw",
      export_button = "shinyExport",
      plot_output = "outputPlot",
      error_output = "outputError",
      code_output = "outputCode"
    )
  )

  expect_error(
    ptr_build_ids(draw_button = ""),
    "single non-empty string"
  )
  expect_error(
    ptr_build_ids(draw_button = "shared", export_button = "shared"),
    "must be unique"
  )
  expect_error(
    ptr_server_state(
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      ids = list(draw_button = "draw")
    ),
    "missing required entries"
  )
})

test_that("optional UI helpers use resolved copy and custom ids", {
  ids <- ptr_build_ids(
    control_panel = "custom_controls",
    draw_button = "render_plot",
    export_button = "download_app",
    plot_output = "main_plot",
    error_output = "main_error",
    code_output = "main_code"
  )
  controls_ui <- ptr_input_ui(
    ids = ids,
    ui_text = list(
      shell = list(
        draw_button = list(label = "Render plot"),
        export_button = list(label = "Download generated app")
      )
    )
  )
  outputs_ui <- ptr_output_ui(ids = ids)

  expect_match(ext_ui_text(controls_ui), "custom_controls", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "render_plot", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "download_app", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "Render plot", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "Download generated app", fixed = TRUE)

  expect_match(ext_ui_text(outputs_ui), "main_plot", fixed = TRUE)
  expect_match(ext_ui_text(outputs_ui), "main_error", fixed = TRUE)
  expect_match(ext_ui_text(outputs_ui), "main_code", fixed = TRUE)
})

test_that("ptr_build_app_ui uses custom ids in rendered HTML", {
  custom_ids <- ptr_build_ids(
    control_panel = "myPanel",
    draw_button = "myDraw",
    export_button = "myExport",
    plot_output = "myPlot",
    error_output = "myError",
    code_output = "myCode"
  )
  ui <- ptr_build_app_ui("Test", "Draw", "Export", ids = custom_ids)
  html <- ext_ui_text(ui)

  expect_match(html, "myPanel", fixed = TRUE)
  expect_match(html, "myDraw", fixed = TRUE)
  expect_match(html, "myExport", fixed = TRUE)
  expect_match(html, "myPlot", fixed = TRUE)
  expect_match(html, "myError", fixed = TRUE)
  expect_match(html, "myCode", fixed = TRUE)
})

test_that("ptr_build_app_ui defaults match ptr_build_ids defaults", {
  ui <- ptr_build_app_ui("Test", "Draw", "Export")
  html <- ext_ui_text(ui)

  expect_match(html, "controlPanel", fixed = TRUE)
  expect_match(html, "\"draw\"", fixed = TRUE)
  expect_match(html, "outputPlot", fixed = TRUE)
})

test_that("value helpers expose plot, code, and default error UI", {
  obj_success <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
  )
  runtime_success <- ptr_exec(
    obj_success,
    list(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "geom_point+checkbox" = TRUE
    )
  )

  plot_obj <- ptr_extract_plot(runtime_success)
  themed_plot <- plot_obj + ggplot2::theme_minimal()

  expect_s3_class(plot_obj, "ggplot")
  expect_s3_class(themed_plot, "ggplot")
  expect_match(ptr_extract_code(runtime_success), "Sepal.Length")
  expect_null(ptr_extract_error(runtime_success))

  obj_failure <- ptr_parse_formula(
    "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
  )
  runtime_failure <- ptr_exec(
    obj_failure,
    list("geom_point+checkbox" = TRUE)
  )

  expect_null(ptr_extract_plot(runtime_failure))
  expect_match(ptr_extract_code(runtime_failure), "unknown_object")
  expect_match(
    ext_ui_text(ptr_extract_error(runtime_failure)),
    "Plot error:",
    fixed = TRUE
  )

  expect_null(ptr_extract_plot(NULL))
  expect_null(ptr_extract_code(NULL))
  expect_null(ptr_extract_error(NULL))
})

test_that("bind helpers support custom ids inside an existing app server", {
  ids <- ptr_build_ids(
    control_panel = "customPanel",
    draw_button = "runPlot",
    export_button = "downloadApp",
    plot_output = "mainPlot",
    error_output = "mainError",
    code_output = "mainCode"
  )

  server_wrapper <- function(input, output, session) {
    ptr_state <- ptr_server_state(
      "ggplot(data = iris, aes(x = var, y = var)) + geom_point()",
      ids = ids
    )

    ptr_setup_controls(input, output, ptr_state, ids = ids)
    ptr_register_draw(input, ptr_state, ids = ids)
    ptr_register_export(output, ptr_state, ids = ids)
    ptr_register_plot(output, ptr_state, ids = ids)
    ptr_register_error(output, ptr_state, ids = ids)
    ptr_register_code(output, ptr_state, ids = ids)

    session$userData$ptr_state <- ptr_state
  }

  shiny::testServer(server_wrapper, {
    expect_s3_class(session$userData$ptr_state, "ptr_state")
    expect_s3_class(session$userData$ptr_state$ids, "ptr_build_ids")
    expect_type(output$customPanel, "list")

    session$setInputs(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "geom_point+checkbox" = TRUE,
      runPlot = 1
    )

    runtime_result <- session$userData$ptr_state$runtime()
    expect_true(runtime_result$ok)
    expect_true(is.list(output$mainPlot))
    expect_null(output$mainError)
    expect_match(output$mainCode, "Sepal.Length")
    expect_match(output$downloadApp, "ggpaintr-app[.]R")
  })
})

test_that("bind helpers expose error and code output with custom ids on failure", {
  ids <- ptr_build_ids(
    control_panel = "customPanel",
    draw_button = "runPlot",
    export_button = "downloadApp",
    plot_output = "mainPlot",
    error_output = "mainError",
    code_output = "mainCode"
  )

  server_wrapper <- function(input, output, session) {
    ptr_state <- ptr_server_state(
      "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()",
      ids = ids
    )

    ptr_setup_controls(input, output, ptr_state, ids = ids)
    ptr_register_draw(input, ptr_state, ids = ids)
    ptr_register_error(output, ptr_state, ids = ids)
    ptr_register_code(output, ptr_state, ids = ids)

    session$userData$ptr_state <- ptr_state
  }

  shiny::testServer(server_wrapper, {
    session$setInputs(
      "geom_point+checkbox" = TRUE,
      runPlot = 1
    )

    runtime_result <- session$userData$ptr_state$runtime()
    expect_false(runtime_result$ok)
    expect_match(output$mainError$html, "Plot error:", fixed = TRUE)
    expect_match(output$mainError$html, "unknown_object")
    expect_match(output$mainCode, "ggplot(data = unknown_object", fixed = TRUE)
  })
})

test_that("new extensibility helpers are exported in NAMESPACE", {
  installed_exports <- tryCatch(
    getNamespaceExports("ggpaintr"),
    error = function(e) character()
  )

  namespace_candidates <- c(
    file.path(getwd(), "NAMESPACE"),
    testthat::test_path("..", "..", "NAMESPACE"),
    testthat::test_path("..", "NAMESPACE")
  )
  namespace_matches <- namespace_candidates[file.exists(namespace_candidates)]
  namespace_lines <- character()

  if (length(namespace_matches) > 0) {
    namespace_lines <- readLines(namespace_matches[[1]])
  }

  expected_exports <- c(
    "ptr_define_placeholder",
    "ptr_merge_placeholders",
    "ptr_missing_expr",
    "ptr_normalize_column_names",
    "ptr_build_ids",
    "ptr_server_state",
    "ptr_setup_controls",
    "ptr_register_draw",
    "ptr_register_export",
    "ptr_register_plot",
    "ptr_register_error",
    "ptr_register_code",
    "ptr_extract_plot",
    "ptr_extract_error",
    "ptr_extract_code",
    "ptr_input_ui",
    "ptr_output_ui"
  )

  for (export_name in expected_exports) {
    expect_true(
      export_name %in% installed_exports ||
        any(grepl(paste0("^export\\(", export_name, "\\)$"), namespace_lines)),
      info = export_name
    )
  }
})
