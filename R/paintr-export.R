#' Serialize an R Object as Source Text
#'
#' @param x An R object.
#'
#' @return A character vector.
#' @keywords internal
paintr_serialize_r_object <- function(x) {
  utils::capture.output(dput(x))
}

#' Build the Standalone App Template
#'
#' @param formula_text A single formula string.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A character vector containing the exported app template.
#' @keywords internal
get_shiny_template <- function(formula_text, copy_rules = NULL) {
  effective_copy_rules <- paintr_effective_copy_rules(copy_rules)
  title_copy <- paintr_resolve_copy("title", copy_rules = effective_copy_rules)
  draw_copy <- paintr_resolve_copy("draw_button", copy_rules = effective_copy_rules)
  export_copy <- paintr_resolve_copy("export_button", copy_rules = effective_copy_rules)

  formula_text_lines <- paintr_serialize_r_object(formula_text)
  formula_text_lines[1] <- paste0("input_formula <- ", formula_text_lines[1])

  copy_rules_lines <- paintr_serialize_r_object(unclass(effective_copy_rules))
  copy_rules_lines[1] <- paste0("copy_rules <- ", copy_rules_lines[1])

  c(
    "library(ggpaintr)",
    "library(shiny)",
    "library(shinyWidgets)",
    "",
    formula_text_lines,
    "",
    copy_rules_lines,
    "",
    "ui <- fluidPage(",
    paste0("  titlePanel(", paintr_serialize_r_object(title_copy$label), "),"),
    "  sidebarLayout(",
    "    sidebarPanel(",
    "      # Modify or add controls here.",
    "      uiOutput(\"controlPanel\"),",
    paste0(
      "      actionButton(\"draw\", ",
      paintr_serialize_r_object(draw_copy$label),
      "),"
    ),
    paste0(
      "      downloadButton(\"shinyExport\", ",
      paintr_serialize_r_object(export_copy$label),
      ")"
    ),
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
    paste(
      "  paintr_state <- ggpaintr_server(",
      "input, output, session, input_formula, copy_rules = copy_rules",
      ")",
      sep = ""
    ),
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
#' @param copy_rules Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
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
generate_shiny <- function(paintr_obj,
                           var_ui,
                           output_file,
                           style = TRUE,
                           copy_rules = NULL) {
  shiny_text <- get_shiny_template(paintr_obj$formula_text, copy_rules = copy_rules)

  writeLines(shiny_text, output_file)
  if (isTRUE(style) && requireNamespace("styler", quietly = TRUE)) {
    styler::style_file(output_file)
  }

  invisible(output_file)
}
