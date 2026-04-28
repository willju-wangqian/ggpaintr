# Exercises the data-aware custom-placeholder pattern documented in
# ?ptr_define_placeholder: an external author registers a `numvar` keyword
# whose choices come from the active layer dataset (numeric columns only),
# using the public helpers ptr_resolve_layer_data() and ptr_ns_id().

make_numvar_placeholder <- function() {
  ptr_define_placeholder(
    keyword = "numvar",
    build_ui = function(id, copy, meta, context) {
      shiny::uiOutput(paste0(id, "_container"))
    },
    bind_ui = function(input, output, metas, context) {
      for (meta in metas) {
        local({
          m <- meta
          layer_data <- ptr_resolve_layer_data(
            context$ptr_obj, m$layer_name, input, context, context$eval_env
          )
          choices <- if (isTRUE(layer_data$has_data) &&
                         is.data.frame(layer_data$data)) {
            df <- layer_data$data
            names(df)[vapply(df, is.numeric, logical(1))]
          } else {
            character()
          }
          input_id  <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL), m$id)
          output_id <- ptr_ns_id(
            context$ui_ns_fn %||% shiny::NS(NULL),
            paste0(m$id, "_container")
          )
          output[[output_id]] <- shiny::renderUI({
            shiny::selectInput(
              input_id,
              paste("Numeric column for", m$param),
              choices = choices
            )
          })
        })
      }
      invisible(NULL)
    },
    resolve_expr = function(value, meta, context) {
      if (is.null(value) || identical(value, "")) {
        return(ptr_missing_expr())
      }
      rlang::sym(value)
    }
  )
}

# ---------------------------------------------------------------------------
# resolve_expr: empty value -> missing; non-empty -> column symbol
# ---------------------------------------------------------------------------

test_that("numvar resolve_expr returns missing for empty input", {
  numvar <- make_numvar_placeholder()
  expect_true(ptr_is_missing_expr(numvar$resolve_expr(NULL, list(), list())))
  expect_true(ptr_is_missing_expr(numvar$resolve_expr("",  list(), list())))
})

test_that("numvar resolve_expr converts a chosen column name to a symbol", {
  numvar <- make_numvar_placeholder()
  out <- numvar$resolve_expr("Sepal.Length", list(), list())
  expect_identical(out, rlang::sym("Sepal.Length"))
})

# ---------------------------------------------------------------------------
# bind_ui: registers a renderUI per meta, choices populated from layer data
# ---------------------------------------------------------------------------

test_that("numvar bind_ui populates choices from the layer dataset", {
  numvar <- make_numvar_placeholder()
  registry <- ptr_merge_placeholders(list(numvar = numvar))
  obj <- ptr_parse_formula(
    "ggplot(iris, aes(x = numvar, y = numvar)) + geom_point()",
    placeholders = registry
  )

  metas <- ptr_flatten_placeholder_map(obj, keyword = "numvar")
  expect_length(metas, 2L)

  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  context$ns_fn <- shiny::NS(NULL)
  context$ui_ns_fn <- shiny::NS(NULL)
  context$eval_env <- new.env(parent = .GlobalEnv)

  output <- new.env(parent = emptyenv())
  spec <- registry[["numvar"]]
  result <- spec$bind_ui(input = list(), output = output, metas = metas, context = context)

  expect_null(result)

  # An output handler should have been registered under <id>_container for each meta
  for (m in metas) {
    expect_true(paste0(m$id, "_container") %in% names(output))
  }
})

test_that("numvar bind_ui falls back to empty choices when layer data is unavailable", {
  numvar <- make_numvar_placeholder()
  registry <- ptr_merge_placeholders(list(numvar = numvar))
  obj <- ptr_parse_formula(
    "ggplot(no_such_df, aes(x = numvar, y = numvar)) + geom_point()",
    placeholders = registry
  )

  metas <- ptr_flatten_placeholder_map(obj, keyword = "numvar")
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  context$ns_fn <- shiny::NS(NULL)
  context$ui_ns_fn <- shiny::NS(NULL)
  context$eval_env <- new.env(parent = baseenv())

  output <- new.env(parent = emptyenv())
  spec <- registry[["numvar"]]
  expect_no_error(
    spec$bind_ui(input = list(), output = output, metas = metas, context = context)
  )
})

# ---------------------------------------------------------------------------
# Namespacing: ptr_ns_id round-trips ids through a module namespace
# ---------------------------------------------------------------------------

test_that("ptr_ns_id namespaces ids through a Shiny module namespace", {
  ns <- shiny::NS("mod1")
  expect_identical(ptr_ns_id(ns, "ggplot_3_2"), "mod1-ggplot_3_2")
  expect_identical(ptr_ns_id(shiny::NS(NULL), "ggplot_3_2"), "ggplot_3_2")
})

# ---------------------------------------------------------------------------
# End-to-end: ptr_bind_placeholder_ui invokes the custom bind_ui
# ---------------------------------------------------------------------------

test_that("ptr_bind_placeholder_ui drives a custom data-aware bind_ui", {
  numvar <- make_numvar_placeholder()
  registry <- ptr_merge_placeholders(list(numvar = numvar))
  obj <- ptr_parse_formula(
    "ggplot(iris, aes(x = numvar, y = numvar)) + geom_point()",
    placeholders = registry
  )

  output <- new.env(parent = emptyenv())
  ptr_bind_placeholder_ui(
    input = list(),
    output = output,
    ptr_obj = obj,
    eval_env = new.env(parent = .GlobalEnv)
  )

  metas <- ptr_flatten_placeholder_map(obj, keyword = "numvar")
  expect_length(metas, 2L)
  for (m in metas) {
    expect_true(paste0(m$id, "_container") %in% names(output))
  }
})
