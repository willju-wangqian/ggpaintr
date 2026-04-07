#' Build the Standalone App Template
#'
#' @return A character vector containing the exported app template.
#' @keywords internal
get_shiny_template <- function() {
  c(
    "library(ggpaintr)",
    "library(shiny)",
    "library(shinyWidgets)",
    "",
    "input_formula <- $formula_text$",
    "",
    "ui <- fluidPage(",
    "  titlePanel(\"ggpaintr demo\"),",
    "  sidebarLayout(",
    "    sidebarPanel(",
    "      # Modify or add controls here.",
    "      uiOutput(\"controlPanel\"),",
    "      actionButton(\"draw\", \"click to draw the plot\"),",
    "      downloadButton(\"shinyExport\", \"export the shiny app\")",
    "    ),",
    "    mainPanel(",
    "      # Modify or add outputs here.",
    "      plotOutput(\"outputPlot\"),",
    "      uiOutput(\"outputError\"),",
    "      verbatimTextOutput(\"outputCode\")",
    "    )",
    "  )",
    ")",
    "",
    "server <- function(input, output, session) {",
    "  paintr_state <- ggpaintr_server(input, output, session, input_formula)",
    "",
    "  # Add custom observers or outputs below.",
    "  # observe({",
    "  #   runtime_result <- paintr_state$runtime()",
    "  #   if (!is.null(runtime_result) && isTRUE(runtime_result$ok)) {",
    "  #     message(runtime_result$code_text)",
    "  #   }",
    "  # })",
    "}",
    "",
    "shinyApp(ui, server)"
  )
}

#' Generate a Standalone Shiny App Script
#'
#' Generate a single-file Shiny app that uses the same `ggpaintr` runtime helpers
#' as the package.
#'
#' @param paintr_obj A `paintr_obj`.
#' @param var_ui Deprecated legacy argument retained for backward compatibility.
#'   It is ignored.
#' @param output_file Path to the generated `.R` script.
#' @param style Whether to style the generated file with `styler` when available.
#'
#' @return Invisibly returns `output_file`.
#' @examples
#' obj <- paintr_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' out_file <- tempfile(fileext = ".R")
#' generate_shiny(obj, list(), out_file, style = FALSE)
#' file.exists(out_file)
#' @export
generate_shiny <- function(paintr_obj, var_ui, output_file, style = TRUE) {
  shiny_text <- get_shiny_template()
  formula_text <- paintr_obj$formula_text
  formula_text <- gsub("\\\\", "\\\\\\\\", formula_text)
  formula_text <- gsub("\"", "\\\\\"", formula_text)
  formula_text <- paste0("\"", formula_text, "\"")
  shiny_text <- stringr::str_replace(
    shiny_text,
    "\\$formula_text\\$",
    function(...) formula_text
  )

  writeLines(shiny_text, output_file)
  if (isTRUE(style) && requireNamespace("styler", quietly = TRUE)) {
    styler::style_file(output_file)
  }

  invisible(output_file)
}
