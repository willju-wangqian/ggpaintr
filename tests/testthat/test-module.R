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
    state <- session$userData$ptr_state

    expect_s3_class(state, "ptr_state")
    expect_equal(state$raw_ids$control_panel, "controlPanel")
    expect_equal(state$ui_ids$control_panel, "plot1-controlPanel")
    expect_equal(state$server_ids$control_panel, "controlPanel")
    expect_equal(state$ids, state$server_ids)
    expect_equal(state$ui_ns_fn("draw"), "plot1-draw")
    expect_equal(state$server_ns_fn("draw"), "draw")
  })
})

test_that("ptr_get_layer_switcher_ui can namespace dynamic module controls separately from runtime ids", {
  state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  obj <- shiny::isolate(state$obj())
  rendered <- paste(
    as.character(
      ptr_get_layer_switcher_ui(
        obj,
        ns_fn = shiny::NS("plot1")
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

test_that("ptr_module_server resolves namespaced browser inputs through module-local runtime ids", {
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_module_server(
      "plot1",
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
    )
  }

  shiny::testServer(server_wrapper, {
    session$setInputs(
      "plot1-ggplot_3_2" = "mpg",
      "plot1-ggplot_3_3" = "wt",
      "plot1-geom_point_checkbox" = TRUE,
      "plot1-draw" = 1
    )

    result <- session$userData$ptr_state$runtime()
    expect_true(result$ok)
    expect_match(result$code_text, "aes\\(x = mpg, y = wt\\)")
  })
})
