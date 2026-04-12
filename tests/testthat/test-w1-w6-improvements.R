# Tests for improvements W1-W6 introduced in:
#   R/paintr-placeholders.R, R/paintr-utils.R, R/paintr-export.R, R/paintr-app.R

# =============================================================================
# W1: ptr_resolve_num_expr - NULL/NA guard order
# =============================================================================

test_that("ptr_resolve_num_expr returns ptr_missing_expr for NULL without warning", {
  expect_no_warning(
    result <- ptr_resolve_num_expr(NULL, meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for NA", {
  result <- ptr_resolve_num_expr(NA_real_, meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for NA integer", {
  result <- ptr_resolve_num_expr(NA_integer_, meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns a numeric expression for a valid number", {
  result <- ptr_resolve_num_expr(3.14, meta = list(), context = list())
  expect_true(is.numeric(eval(result)))
  expect_equal(eval(result), 3.14)
})

test_that("ptr_resolve_num_expr NULL check does not invoke is.na on NULL", {
  # Before W1, is.na(NULL) returns logical(0) which is falsy, making the guard
  # fail silently. This test confirms NULL short-circuits before is.na.
  expect_no_error(
    ptr_resolve_num_expr(NULL, meta = list(), context = list())
  )
})

# =============================================================================
# W2: ptr_bind_var_ui_impl - silent skip when has_data is FALSE
# =============================================================================

test_that("formulas with var and no data silently skip during UI preparation", {
  obj <- ptr_parse_formula(unsupported_use_cases$no_data_for_var$formula)
  output <- list2env(list(), parent = emptyenv())
  # Must not error or abort — previously aborted inside a tryCatch
  expect_no_error(
    register_var_ui_outputs(list(), output, obj)
  )
})

test_that("var UI skips silently for layer with no data object", {
  # A layer-level var with no data= argument provides no column info
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length)) + geom_point(aes(colour = var))"
  )
  output <- list2env(list(), parent = emptyenv())
  expect_no_error(
    register_var_ui_outputs(list(), output, obj, envir = globalenv())
  )
})

# =============================================================================
# W3: ptr_normalize_placeholders - seen_keywords initialized as character(0)
# =============================================================================

test_that("ptr_normalize_placeholders with a single valid placeholder has no empty-string contamination", {
  ph <- make_test_date_placeholder()
  result <- ptr_normalize_placeholders(list(date = ph))
  expect_length(result, 1)
  expect_named(result, "date")
  # No empty strings should appear as names
  expect_false("" %in% names(result))
})

test_that("ptr_normalize_placeholders with multiple distinct placeholders stores all", {
  ph_date <- make_test_date_placeholder()
  ph_text <- ptr_define_placeholder(
    keyword = "text",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) {
      if (is.null(value) || identical(value, "")) return(ptr_missing_expr())
      rlang::expr(!!value)
    }
  )
  result <- ptr_normalize_placeholders(list(date = ph_date, text = ph_text))
  expect_length(result, 2)
  expect_named(result, c("date", "text"))
  expect_false(any(names(result) == ""))
})

test_that("ptr_normalize_placeholders detects duplicate keywords without false positives", {
  ph_date1 <- make_test_date_placeholder()
  ph_date2 <- make_test_date_placeholder()
  expect_error(
    ptr_normalize_placeholders(list(date = ph_date1, date = ph_date2)),
    "duplicated keywords"
  )
})

test_that("ptr_normalize_placeholders returns empty list for NULL input", {
  expect_equal(ptr_normalize_placeholders(NULL), list())
})

# =============================================================================
# W5: handle_duplicate_names - O(n) environment-based deduplication
# =============================================================================

test_that("handle_duplicate_names leaves a vector with no duplicates unchanged", {
  x <- c("a", "b", "c")
  expect_equal(handle_duplicate_names(x), c("a", "b", "c"))
})

test_that("handle_duplicate_names keeps first occurrence, suffixes second with -2", {
  result <- handle_duplicate_names(c("a", "b", "a"))
  expect_equal(result, c("a", "b", "a-2"))
})

test_that("handle_duplicate_names suffixes three identical names correctly", {
  result <- handle_duplicate_names(c("x", "x", "x"))
  expect_equal(result, c("x", "x-2", "x-3"))
})

test_that("handle_duplicate_names handles multiple independent duplicated groups", {
  result <- handle_duplicate_names(c("a", "b", "a", "b", "a"))
  expect_equal(result, c("a", "b", "a-2", "b-2", "a-3"))
})

test_that("handle_duplicate_names returns a single-element vector unchanged", {
  expect_equal(handle_duplicate_names(c("z")), c("z"))
})

test_that("handle_duplicate_names returns empty character vector unchanged", {
  expect_equal(handle_duplicate_names(character(0)), character(0))
})

test_that("handle_duplicate_names does not modify the first occurrence's name", {
  result <- handle_duplicate_names(c("geom_point", "geom_point"))
  expect_equal(result[[1]], "geom_point")
  expect_equal(result[[2]], "geom_point-2")
})

# =============================================================================
# W6: ids param in ptr_generate_shiny, ptr_serialize_ids, ptr_server
# =============================================================================

test_that("ptr_serialize_ids serializes default ids correctly", {
  ids <- ptr_build_ids()
  result <- ptr_serialize_ids(ids)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_match(result, "^ids <- ptr_build_ids\\(", fixed = FALSE)
  expect_match(result, 'control_panel = "controlPanel"', fixed = TRUE)
  expect_match(result, 'draw_button = "draw"', fixed = TRUE)
  expect_match(result, 'export_button = "shinyExport"', fixed = TRUE)
  expect_match(result, 'plot_output = "outputPlot"', fixed = TRUE)
  expect_match(result, 'error_output = "outputError"', fixed = TRUE)
  expect_match(result, 'code_output = "outputCode"', fixed = TRUE)
})

test_that("ptr_serialize_ids serializes custom ids", {
  ids <- ptr_build_ids(
    control_panel = "myControls",
    draw_button = "runPlot"
  )
  result <- ptr_serialize_ids(ids)
  expect_match(result, 'control_panel = "myControls"', fixed = TRUE)
  expect_match(result, 'draw_button = "runPlot"', fixed = TRUE)
})

test_that("ptr_serialize_ids escapes special characters in id values", {
  # Build ids with a value containing a double quote would fail ptr_validate_ids
  # (non-empty string), so test backslash instead
  ids <- structure(
    list(
      control_panel = "panel\\one",
      draw_button = "draw",
      export_button = "export",
      plot_output = "plot",
      error_output = "error",
      code_output = "code"
    ),
    class = c("ptr_build_ids", "list")
  )
  result <- ptr_serialize_ids(ids)
  # backslash should be escaped as \\
  expect_match(result, "panel\\\\one", fixed = TRUE)
})

test_that("ptr_generate_shiny with default ids emits ptr_build_ids() call", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- withr::local_tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE)

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "ids <- ptr_build_ids(", fixed = TRUE)
  expect_match(app_text, 'control_panel = "controlPanel"', fixed = TRUE)
  expect_no_error(parse(file = out_file))
})

test_that("ptr_generate_shiny with custom ids serializes custom values into template", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  custom_ids <- ptr_build_ids(
    control_panel = "myPanel",
    draw_button = "runIt",
    export_button = "saveIt",
    plot_output = "thePlot",
    error_output = "theError",
    code_output = "theCode"
  )
  out_file <- withr::local_tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE, ids = custom_ids)

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, 'control_panel = "myPanel"', fixed = TRUE)
  expect_match(app_text, 'draw_button = "runIt"', fixed = TRUE)
  expect_match(app_text, 'export_button = "saveIt"', fixed = TRUE)
  expect_match(app_text, 'plot_output = "thePlot"', fixed = TRUE)
  expect_match(app_text, 'error_output = "theError"', fixed = TRUE)
  expect_match(app_text, 'code_output = "theCode"', fixed = TRUE)
  # Template references should use ids$ field access
  expect_match(app_text, "ids$control_panel", fixed = TRUE)
  expect_match(app_text, "ids$draw_button", fixed = TRUE)
  expect_no_error(parse(file = out_file))
})

test_that("ptr_generate_shiny template references ids fields via ids$ syntax", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- withr::local_tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE)

  app_text <- paste(readLines(out_file), collapse = "\n")
  # All template UI/server wiring must go through ids$ not hardcoded strings
  expect_match(app_text, "uiOutput(ids$control_panel)", fixed = TRUE)
  expect_match(app_text, "actionButton(ids$draw_button", fixed = TRUE)
  expect_match(app_text, "downloadButton(ids$export_button", fixed = TRUE)
  expect_match(app_text, "plotOutput(ids$plot_output)", fixed = TRUE)
})

test_that("ptr_server accepts ids param without error", {
  server_wrapper <- function(input, output, session) {
    custom_ids <- ptr_build_ids(
      control_panel = "myPanel",
      draw_button = "myDraw",
      export_button = "myExport",
      plot_output = "myPlot",
      error_output = "myError",
      code_output = "myCode"
    )
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      ids = custom_ids
    )
  }

  shiny::testServer(server_wrapper, {
    expect_type(session$userData$ptr_state, "list")
    expect_s3_class(session$userData$ptr_state, "ptr_state")
    expect_equal(session$userData$ptr_state$ids$control_panel, "myPanel")
    expect_equal(session$userData$ptr_state$ids$draw_button, "myDraw")
  })
})

test_that("ptr_server with default ids has standard id values in state", {
  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- ptr_server(
      input, output, session,
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
    )
  }

  shiny::testServer(server_wrapper, {
    expect_equal(session$userData$ptr_state$ids$control_panel, "controlPanel")
    expect_equal(session$userData$ptr_state$ids$draw_button, "draw")
  })
})

# =============================================================================
# Fix A: numeric(0) / length-0 guard in ptr_resolve_num_expr
# =============================================================================

test_that("ptr_resolve_num_expr returns ptr_missing_expr for numeric(0) without warning", {
  expect_no_warning(
    result <- ptr_resolve_num_expr(numeric(0), meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for integer(0) without warning", {
  expect_no_warning(
    result <- ptr_resolve_num_expr(integer(0), meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("ptr_resolve_num_expr returns ptr_missing_expr for character(0) without error", {
  expect_no_error(
    result <- ptr_resolve_num_expr(character(0), meta = list(), context = list())
  )
  expect_s3_class(result, "ptr_missing_expr")
})

# =============================================================================
# Fix B: ptr_shiny_template emits ptr_server(..., ids = ids) as single literal
# =============================================================================

test_that("ptr_generate_shiny template contains ptr_server call with ids = ids", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- withr::local_tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE)

  app_text <- paste(readLines(out_file), collapse = "\n")
  expect_match(app_text, "ptr_server(", fixed = TRUE)
  expect_match(app_text, "ids = ids", fixed = TRUE)
})

test_that("ptr_generate_shiny exported app with custom ids executes end to end", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  custom_ids <- ptr_build_ids(
    control_panel = "myPanel",
    draw_button = "myDraw",
    export_button = "myExport",
    plot_output = "myPlot",
    error_output = "myError",
    code_output = "myCode"
  )
  out_file <- withr::local_tempfile(fileext = ".R")
  ptr_generate_shiny(obj, out_file, style = FALSE, ids = custom_ids)

  export_env <- new.env(parent = environment())
  exported_app <- source(out_file, local = export_env)$value
  expect_s3_class(exported_app, "shiny.appobj")

  server_wrapper <- function(input, output, session) {
    session$userData$ptr_state <- export_env$server(input, output, session)
  }

  spec <- ptr_runtime_input_spec(obj)
  ph_ids <- spec[spec$role == "placeholder", ]
  var_ids <- ph_ids[ph_ids$layer_name == "ggplot" & ph_ids$keyword == "var", "input_id"]

  shiny::testServer(server_wrapper, {
    do.call(session$setInputs, c(
      setNames(list("mpg", "disp"), var_ids),
      list("geom_point+checkbox" = TRUE, myDraw = 1)
    ))
    runtime_result <- session$userData$ptr_state$runtime()
    expect_true(runtime_result$ok)
    expect_s3_class(runtime_result$plot, "ggplot")
  })
})
