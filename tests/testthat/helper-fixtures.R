library(assertthat)
library(ggplot2)
library(purrr)
library(rlang)
library(shiny)
library(shinyWidgets)
library(stringr)

source(testthat::test_path("..", "..", "R", "paintr2_func.R"), local = TRUE)
source(testthat::test_path("..", "..", "R", "ui_function.R"), local = TRUE)

fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

mock_upload_input <- function(path, name = basename(path), type = "") {
  data.frame(
    name = name,
    size = file.info(path)$size,
    type = type,
    datapath = path,
    stringsAsFactors = FALSE
  )
}

set_layer_checkboxes <- function(input, paintr_obj, value = TRUE) {
  for (layer_name in names(paintr_obj$expr_list)) {
    if (identical(layer_name, "ggplot")) {
      next
    }

    input[[paste0(layer_name, "+checkbox")]] <- value
  }

  input
}

supported_use_cases <- list(
  basic_scatter = list(
    formula = "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    envir = globalenv(),
    input = list(
      "ggplot+3+2" = "mpg",
      "ggplot+3+3" = "disp",
      "geom_point+checkbox" = TRUE
    )
  ),
  scatter_with_labels = list(
    formula = paste(
      "ggplot(data = iris, aes(x = var, y = var)) +",
      "geom_point(size = num) +",
      "labs(title = text, x = text, y = text)"
    ),
    envir = globalenv(),
    input = list(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "geom_point+2" = 2,
      "labs+2" = "Iris scatter",
      "labs+3" = "Sepal length",
      "labs+4" = "Sepal width",
      "geom_point+checkbox" = TRUE,
      "labs+checkbox" = TRUE
    )
  ),
  facet_expr = list(
    formula = "ggplot(data = iris, aes(x = var, y = var)) + geom_point() + facet_wrap(expr)",
    envir = globalenv(),
    input = list(
      "ggplot+3+2" = "Sepal.Length",
      "ggplot+3+3" = "Sepal.Width",
      "facet_wrap+2" = "~ Species",
      "geom_point+checkbox" = TRUE,
      "facet_wrap+checkbox" = TRUE
    )
  ),
  upload_global = list(
    formula = "ggplot(data = upload, aes(x = var, y = var)) + geom_point()",
    envir = globalenv(),
    input = list(
      "ggplot+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
      "ggplot+2+name" = "",
      "ggplot+3+2" = "x",
      "ggplot+3+3" = "y",
      "geom_point+checkbox" = TRUE
    )
  ),
  upload_layer = list(
    formula = paste(
      "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +",
      "geom_point(data = upload, aes(x = var, y = var))"
    ),
    envir = globalenv(),
    input = list(
      "geom_point+2" = mock_upload_input(fixture_path("simple_numeric.csv"), "layer-data.csv"),
      "geom_point+2+name" = "layer_data",
      "geom_point+3+2" = "x",
      "geom_point+3+3" = "y",
      "geom_point+checkbox" = TRUE
    )
  )
)

unsupported_use_cases <- list(
  multiple_expressions = list(
    formula = "ggplot(data = mtcars, aes(x = mpg, y = disp)); geom_point()",
    stage = "parse"
  ),
  no_data_for_var = list(
    formula = "ggplot(aes(x = var, y = var)) + geom_point()",
    stage = "ui"
  ),
  unknown_data_object = list(
    formula = "ggplot(data = unknown_object, aes(x = var, y = var)) + geom_point()",
    stage = "ui"
  ),
  malformed_expr_input = list(
    formula = "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)",
    stage = "complete"
  )
)
