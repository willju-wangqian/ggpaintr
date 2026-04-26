test_that("ptr_module_ui emits namespaced top-level ids", {
  ui <- ptr_module_ui("plot1")
  rendered <- paste(as.character(ui), collapse = "\n")

  expect_match(rendered, 'id="plot1-controlPanel"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-draw"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-outputPlot"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-outputError"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-outputCode"', fixed = TRUE)
})

test_that("ptr_module_server returns namespaced ptr_state", {
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_module_server(
      "plot1",
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
    )
  }

  shiny::testServer(server_wrapper, {
    expect_s3_class(session$userData$ptr_state, "ptr_state")
    expect_equal(session$userData$ptr_state$ids$control_panel, "plot1-controlPanel")
    expect_equal(session$userData$ptr_state$ids$draw_button, "plot1-draw")
    expect_equal(session$userData$ptr_state$ids$plot_output, "plot1-outputPlot")
    expect_equal(session$userData$ptr_state$ids$error_output, "plot1-outputError")
    expect_equal(session$userData$ptr_state$ids$code_output, "plot1-outputCode")
  })
})

test_that("ptr_module_server passes through customization arguments", {
  registry <- ptr_merge_placeholders(
    list(date = make_test_date_placeholder())
  )
  ui_text <- list(
    shell = list(draw_button = list(label = "Render module plot"))
  )

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_module_server(
      "plot1",
      test_date_formula,
      envir = test_sales_env(),
      ui_text = ui_text,
      placeholders = registry,
      checkbox_defaults = list(geom_vline = FALSE),
      expr_check = FALSE
    )
  }

  shiny::testServer(server_wrapper, {
    state <- session$userData$ptr_state

    expect_s3_class(state$placeholders, "ptr_define_placeholder_registry")
    expect_true("date" %in% names(state$custom_placeholders))
    expect_equal(state$raw_ui_text, ui_text)
    expect_equal(state$checkbox_defaults[["geom_vline"]], FALSE)
    expect_false(state$expr_check)
  })
})
