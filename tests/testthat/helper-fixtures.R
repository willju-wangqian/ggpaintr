library(assertthat)
library(ggplot2)
library(purrr)
library(rlang)
library(shiny)
library(shinyWidgets)

r_files <- sort(list.files(
  testthat::test_path("..", "..", "R"),
  pattern = "[.]R$",
  full.names = TRUE
))
for (r_file in r_files) {
  source(r_file, local = TRUE)
}

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

set_layer_checkboxes <- function(input, ptr_obj, value = TRUE) {
  for (layer_name in names(ptr_obj$expr_list)) {
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
      "ggplot_3_2" = "mpg",
      "ggplot_3_3" = "disp",
      "geom_point_checkbox" = TRUE
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
      "ggplot_3_2" = "Sepal.Length",
      "ggplot_3_3" = "Sepal.Width",
      "geom_point_2" = 2,
      "labs_2" = "Iris scatter",
      "labs_3" = "Sepal length",
      "labs_4" = "Sepal width",
      "geom_point_checkbox" = TRUE,
      "labs_checkbox" = TRUE
    )
  ),
  facet_expr = list(
    formula = "ggplot(data = iris, aes(x = var, y = var)) + geom_point() + facet_wrap(expr)",
    envir = globalenv(),
    input = list(
      "ggplot_3_2" = "Sepal.Length",
      "ggplot_3_3" = "Sepal.Width",
      "facet_wrap_2" = "~ Species",
      "geom_point_checkbox" = TRUE,
      "facet_wrap_checkbox" = TRUE
    )
  ),
  upload_global = list(
    formula = "ggplot(data = upload, aes(x = var, y = var)) + geom_point()",
    envir = globalenv(),
    input = list(
      "ggplot_2" = mock_upload_input(fixture_path("simple_numeric.csv"), "simple numeric.csv"),
      "ggplot_2_name" = "",
      "ggplot_3_2" = "x",
      "ggplot_3_3" = "y",
      "geom_point_checkbox" = TRUE
    )
  ),
  upload_layer = list(
    formula = paste(
      "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +",
      "geom_point(data = upload, aes(x = var, y = var))"
    ),
    envir = globalenv(),
    input = list(
      "geom_point_2" = mock_upload_input(fixture_path("simple_numeric.csv"), "layer-data.csv"),
      "geom_point_2_name" = "layer_data",
      "geom_point_3_2" = "x",
      "geom_point_3_3" = "y",
      "geom_point_checkbox" = TRUE
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
    stage = "plot"
  ),
  malformed_expr_input = list(
    formula = "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + facet_wrap(expr)",
    stage = "complete"
  )
)
