ext_ui_text <- function(ui) {
  paste(as.character(ui), collapse = "\n")
}

test_that("ggpaintr_ids validates defaults and rejects invalid registries", {
  ids <- ggpaintr_ids()

  expect_s3_class(ids, "ggpaintr_ids")
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
    ggpaintr_ids(draw_button = ""),
    "single non-empty string"
  )
  expect_error(
    ggpaintr_ids(draw_button = "shared", export_button = "shared"),
    "must be unique"
  )
  expect_error(
    ggpaintr_server_state(
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      ids = list(draw_button = "draw")
    ),
    "missing required entries"
  )
})

test_that("optional UI helpers use resolved copy and custom ids", {
  ids <- ggpaintr_ids(
    control_panel = "custom_controls",
    draw_button = "render_plot",
    export_button = "download_app",
    plot_output = "main_plot",
    error_output = "main_error",
    code_output = "main_code"
  )
  controls_ui <- ggpaintr_controls_ui(
    ids = ids,
    copy_rules = list(
      shell = list(
        draw_button = list(label = "Render plot"),
        export_button = list(label = "Download generated app")
      )
    )
  )
  outputs_ui <- ggpaintr_outputs_ui(ids = ids)

  expect_match(ext_ui_text(controls_ui), "custom_controls", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "render_plot", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "download_app", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "Render plot", fixed = TRUE)
  expect_match(ext_ui_text(controls_ui), "Download generated app", fixed = TRUE)

  expect_match(ext_ui_text(outputs_ui), "main_plot", fixed = TRUE)
  expect_match(ext_ui_text(outputs_ui), "main_error", fixed = TRUE)
  expect_match(ext_ui_text(outputs_ui), "main_code", fixed = TRUE)
})

test_that("value helpers expose plot, code, and default error UI", {
  obj_success <- paintr_formula(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
  )
  runtime_success <- paintr_build_runtime(
    obj_success,
    list(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "geom_point+checkbox" = TRUE
    )
  )

  plot_obj <- ggpaintr_plot_value(runtime_success)
  themed_plot <- plot_obj + ggplot2::theme_minimal()

  expect_s3_class(plot_obj, "ggplot")
  expect_s3_class(themed_plot, "ggplot")
  expect_match(ggpaintr_code_value(runtime_success), "Sepal.Length")
  expect_null(ggpaintr_error_value(runtime_success))

  obj_failure <- paintr_formula(
    "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"
  )
  runtime_failure <- paintr_build_runtime(
    obj_failure,
    list("geom_point+checkbox" = TRUE)
  )

  expect_null(ggpaintr_plot_value(runtime_failure))
  expect_match(ggpaintr_code_value(runtime_failure), "unknown_object")
  expect_match(
    ext_ui_text(ggpaintr_error_value(runtime_failure)),
    "Plot error:",
    fixed = TRUE
  )

  expect_null(ggpaintr_plot_value(NULL))
  expect_null(ggpaintr_code_value(NULL))
  expect_null(ggpaintr_error_value(NULL))
})

test_that("bind helpers support custom ids inside an existing app server", {
  ids <- ggpaintr_ids(
    control_panel = "customPanel",
    draw_button = "runPlot",
    export_button = "downloadApp",
    plot_output = "mainPlot",
    error_output = "mainError",
    code_output = "mainCode"
  )

  server_wrapper <- function(input, output, session) {
    paintr_state <- ggpaintr_server_state(
      "ggplot(data = iris, aes(x = var, y = var)) + geom_point()",
      ids = ids
    )

    ggpaintr_bind_control_panel(input, output, paintr_state, ids = ids)
    ggpaintr_bind_draw(input, paintr_state, ids = ids)
    ggpaintr_bind_export(output, paintr_state, ids = ids)
    ggpaintr_bind_plot(output, paintr_state, ids = ids)
    ggpaintr_bind_error(output, paintr_state, ids = ids)
    ggpaintr_bind_code(output, paintr_state, ids = ids)

    session$userData$paintr_state <- paintr_state
  }

  shiny::testServer(server_wrapper, {
    expect_s3_class(session$userData$paintr_state, "ggpaintr_state")
    expect_s3_class(session$userData$paintr_state$ids, "ggpaintr_ids")
    expect_type(output$customPanel, "list")

    session$setInputs(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "geom_point+checkbox" = TRUE,
      runPlot = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
    expect_true(runtime_result$ok)
    expect_true(is.list(output$mainPlot))
    expect_null(output$mainError)
    expect_match(output$mainCode, "Sepal.Length")
    expect_match(output$downloadApp, "ggpaintr-app[.]R")
  })
})

test_that("bind helpers expose error and code output with custom ids on failure", {
  ids <- ggpaintr_ids(
    control_panel = "customPanel",
    draw_button = "runPlot",
    export_button = "downloadApp",
    plot_output = "mainPlot",
    error_output = "mainError",
    code_output = "mainCode"
  )

  server_wrapper <- function(input, output, session) {
    paintr_state <- ggpaintr_server_state(
      "ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()",
      ids = ids
    )

    ggpaintr_bind_control_panel(input, output, paintr_state, ids = ids)
    ggpaintr_bind_draw(input, paintr_state, ids = ids)
    ggpaintr_bind_error(output, paintr_state, ids = ids)
    ggpaintr_bind_code(output, paintr_state, ids = ids)

    session$userData$paintr_state <- paintr_state
  }

  shiny::testServer(server_wrapper, {
    session$setInputs(
      "geom_point+checkbox" = TRUE,
      runPlot = 1
    )

    runtime_result <- session$userData$paintr_state$runtime()
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
    "ggpaintr_placeholder",
    "ggpaintr_effective_placeholders",
    "ggpaintr_missing_expr",
    "ggpaintr_normalize_column_names",
    "ggpaintr_ids",
    "ggpaintr_server_state",
    "ggpaintr_bind_control_panel",
    "ggpaintr_bind_draw",
    "ggpaintr_bind_export",
    "ggpaintr_bind_plot",
    "ggpaintr_bind_error",
    "ggpaintr_bind_code",
    "ggpaintr_plot_value",
    "ggpaintr_error_value",
    "ggpaintr_code_value",
    "ggpaintr_controls_ui",
    "ggpaintr_outputs_ui"
  )

  for (export_name in expected_exports) {
    expect_true(
      export_name %in% installed_exports ||
        any(grepl(paste0("^export\\(", export_name, "\\)$"), namespace_lines)),
      info = export_name
    )
  }
})
