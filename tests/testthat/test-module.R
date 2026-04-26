test_that("ptr_module_ui emits namespaced top-level ids", {
  ui <- ptr_module_ui("plot1")
  rendered <- paste(as.character(ui), collapse = "\n")

  expect_match(rendered, 'id="plot1-controlPanel"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-draw"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-outputPlot"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-outputError"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-outputCode"', fixed = TRUE)
})

test_that("ptr_module_server returns module-scoped ptr_state", {
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_module_server(
      "plot1",
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
    )
  }

  shiny::testServer(server_wrapper, {
    expect_s3_class(session$userData$ptr_state, "ptr_state")
    expect_equal(session$userData$ptr_state$ids$control_panel, "controlPanel")
    expect_equal(session$userData$ptr_state$ids$draw_button, "draw")
    expect_equal(session$userData$ptr_state$ids$plot_output, "outputPlot")
    expect_equal(session$userData$ptr_state$ids$error_output, "outputError")
    expect_equal(session$userData$ptr_state$ids$code_output, "outputCode")
    expect_equal(
      session$userData$ptr_state$ui_placeholder_ns_fn("controlPanel"),
      "plot1-controlPanel"
    )
    expect_equal(
      session$userData$ptr_state$ui_checkbox_ns_fn("geom_point_checkbox"),
      "plot1-geom_point_checkbox"
    )
  })
})

test_that("ptr_get_tab_ui can namespace dynamic module controls separately from runtime ids", {
  state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  obj <- shiny::isolate(state$obj())
  rendered <- paste(
    as.character(
      ptr_get_tab_ui(
        obj,
        ns_fn = shiny::NS("plot1"),
        checkbox_ns_fn = shiny::NS("plot1")
      )
    ),
    collapse = "\n"
  )

  expect_match(rendered, 'id="plot1-var-ggplot_3_2"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-var-ggplot_3_3"', fixed = TRUE)
  expect_match(rendered, 'id="plot1-geom_point_checkbox"', fixed = TRUE)
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
