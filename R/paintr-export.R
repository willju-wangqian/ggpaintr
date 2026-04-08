#' Serialize an R Object as Source Text
#'
#' @param x An R object.
#'
#' @return A character vector.
#' @noRd
paintr_serialize_r_object <- function(x) {
  utils::capture.output(dput(x))
}

#' Escape One String Segment for a Template Literal
#'
#' @param x A character string.
#'
#' @return A single escaped string segment.
#' @noRd
paintr_escape_string_segment <- function(x) {
  encoded <- encodeString(x, quote = "\"")
  substring(encoded, 2, nchar(encoded) - 1)
}

#' Prefix Serialized Template Lines with an Assignment
#'
#' @param name An assignment target.
#' @param value_lines A character vector of serialized value lines.
#'
#' @return A character vector.
#' @noRd
paintr_prefix_assignment <- function(name, value_lines) {
  value_lines[1] <- paste0(name, " <- ", value_lines[1])
  value_lines
}

#' Serialize a Formula String for an Exported Template
#'
#' @param formula_text A single formula string.
#'
#' @return A character vector of source lines.
#' @noRd
paintr_serialize_formula_text <- function(formula_text) {
  formula_lines <- strsplit(formula_text, "\n", fixed = TRUE)[[1]]
  formula_lines <- vapply(
    formula_lines,
    paintr_escape_string_segment,
    character(1),
    USE.NAMES = FALSE
  )

  if (length(formula_lines) == 1) {
    return(paste0("\"", formula_lines, "\""))
  }

  formula_lines[1] <- paste0("\"", formula_lines[1])
  formula_lines[length(formula_lines)] <- paste0(formula_lines[length(formula_lines)], "\"")
  formula_lines
}

#' Serialize Copy Rules for an Exported Template
#'
#' @param copy_rules User-supplied or effective copy rules.
#' @param placeholders Placeholder definitions or a placeholder registry used
#'   to compact placeholder-specific copy overrides.
#'
#' @return A character vector of source lines.
#' @noRd
paintr_serialize_export_copy_rules <- function(copy_rules = NULL, placeholders = NULL) {
  default_header <- paste(
    "# Replace NULL with a named list to customize UI labels, help text, and",
    "placeholders."
  )
  compact_copy_rules <- paintr_compact_copy_rules(
    copy_rules,
    placeholders = placeholders
  )
  if (is.null(compact_copy_rules)) {
    return(c(default_header, "copy_rules <- NULL"))
  }

  custom_header <- paste(
    "# Edit custom_copy_rules to customize UI labels, help text, and",
    "placeholders."
  )
  custom_copy_rules_lines <- paintr_serialize_r_object(compact_copy_rules)
  custom_copy_rules_lines <- paintr_prefix_assignment(
    "custom_copy_rules",
    custom_copy_rules_lines
  )

  c(
    custom_header,
    custom_copy_rules_lines,
    "",
    "copy_rules <- paintr_effective_copy_rules(custom_copy_rules)"
  )
}

#' Serialize Custom Placeholder Definitions for an Exported Template
#'
#' @param placeholders Placeholder definitions or a placeholder registry.
#'
#' @return A character vector of source lines.
#' @noRd
paintr_serialize_export_placeholders <- function(placeholders = NULL) {
  custom_placeholders <- paintr_exportable_custom_placeholders(placeholders)
  default_header <- paste(
    "# Replace NULL with a named list of ggpaintr_placeholder() calls to",
    "register custom placeholder types."
  )

  if (length(custom_placeholders) == 0) {
    return(c(default_header, "placeholders <- NULL"))
  }

  custom_header <- paste(
    "# Edit custom_placeholders to register custom placeholder types for this",
    "exported app."
  )
  custom_placeholder_lines <- paintr_serialize_r_object(custom_placeholders)
  custom_placeholder_lines <- paintr_prefix_assignment(
    "custom_placeholders",
    custom_placeholder_lines
  )

  c(
    custom_header,
    custom_placeholder_lines,
    "",
    "placeholders <- ggpaintr_effective_placeholders(custom_placeholders)"
  )
}

#' Build the Standalone App Template
#'
#' @param formula_text A single formula string.
#' @param copy_rules Effective or user-supplied copy rules.
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return A character vector containing the exported app template.
#' @noRd
get_shiny_template <- function(formula_text,
                               copy_rules = NULL,
                               placeholders = NULL) {
  formula_text_lines <- paintr_prefix_assignment(
    "input_formula",
    paintr_serialize_formula_text(formula_text)
  )
  copy_rules_lines <- paintr_serialize_export_copy_rules(
    copy_rules,
    placeholders = placeholders
  )
  placeholder_lines <- paintr_serialize_export_placeholders(placeholders)

  c(
    "library(ggpaintr)",
    "library(shiny)",
    "library(shinyWidgets)",
    "",
    formula_text_lines,
    "",
    copy_rules_lines,
    "",
    placeholder_lines,
    "",
    "title_copy <- paintr_resolve_copy(\"title\", copy_rules = copy_rules)",
    "draw_copy <- paintr_resolve_copy(\"draw_button\", copy_rules = copy_rules)",
    "export_copy <- paintr_resolve_copy(\"export_button\", copy_rules = copy_rules)",
    "",
    "ui <- fluidPage(",
    "  titlePanel(title_copy$label),",
    "  sidebarLayout(",
    "    sidebarPanel(",
    "      # Modify or add controls here.",
    "      uiOutput(\"controlPanel\"),",
    "      actionButton(\"draw\", draw_copy$label),",
    "      downloadButton(\"shinyExport\", export_copy$label)",
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
      "input, output, session, input_formula, copy_rules = copy_rules, ",
      "placeholders = placeholders",
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
#' @param output_file Path to the generated `.R` script.
#' @param style Whether to style the generated file with `styler` when available.
#' @param copy_rules Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry. Exported custom placeholders must define their hooks
#'   inline inside `ggpaintr_placeholder()` so the generated app can stay
#'   standalone.
#' @param ... Backward-compatibility only. A deprecated legacy `var_ui` argument
#'   may still be supplied positionally or by name, but it is ignored.
#'
#' @return Invisibly returns `output_file`.
#' @examples
#' obj <- paintr_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' out_file <- tempfile(fileext = ".R")
#' generate_shiny(obj, out_file, style = FALSE)
#' file.exists(out_file)
#' @export
generate_shiny <- function(paintr_obj,
                           output_file,
                           ...,
                           style = TRUE,
                           copy_rules = NULL,
                           placeholders = NULL) {
  extra_args <- list(...)
  legacy_var_ui_supplied <- FALSE
  legacy_positional_supplied <- FALSE
  legacy_positional_args <- list()

  if ("var_ui" %in% names(extra_args)) {
    legacy_var_ui_supplied <- TRUE
    extra_args[["var_ui"]] <- NULL
  }

  extra_arg_names <- names(extra_args)
  if (is.null(extra_arg_names)) {
    extra_arg_names <- rep("", length(extra_args))
  }

  legacy_positional_args <- extra_args[extra_arg_names == ""]
  extra_args <- extra_args[extra_arg_names != ""]

  if (!is.character(output_file) || length(output_file) != 1 || is.na(output_file)) {
    legacy_var_ui_supplied <- TRUE

    if (length(legacy_positional_args) == 0) {
      stop("output_file must be a single non-missing string.", call. = FALSE)
    }

    output_file <- legacy_positional_args[[1]]
    legacy_positional_args <- legacy_positional_args[-1]
  }

  if (length(legacy_positional_args) > 0) {
    legacy_positional_supplied <- TRUE

    if (length(legacy_positional_args) >= 1) {
      style <- legacy_positional_args[[1]]
    }
    if (length(legacy_positional_args) >= 2) {
      copy_rules <- legacy_positional_args[[2]]
    }
    if (length(legacy_positional_args) >= 3) {
      placeholders <- legacy_positional_args[[3]]
    }
    if (length(legacy_positional_args) > 3) {
      stop("Too many legacy positional arguments supplied.", call. = FALSE)
    }
  }

  if (length(extra_args) > 0) {
    stop(
      "Unused arguments: ",
      paste(names(extra_args), collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.character(output_file) || length(output_file) != 1 || is.na(output_file)) {
    stop("output_file must be a single non-missing string.", call. = FALSE)
  }

  if (legacy_var_ui_supplied || legacy_positional_supplied) {
    warning_parts <- character()

    if (legacy_var_ui_supplied) {
      warning_parts <- c(warning_parts, "`var_ui` is deprecated and ignored.")
    }
    if (legacy_positional_supplied) {
      warning_parts <- c(
        warning_parts,
        "Pass additional arguments by name after `output_file`."
      )
    }

    warning(
      paste(
        c(warning_parts, "Call generate_shiny(paintr_obj, output_file, ...) instead."),
        collapse = " "
      ),
      call. = FALSE
    )
  }

  placeholder_registry <- if (is.null(placeholders)) {
    paintr_obj$placeholders
  } else {
    ggpaintr_effective_placeholders(placeholders)
  }
  shiny_text <- get_shiny_template(
    paintr_obj$formula_text,
    copy_rules = copy_rules,
    placeholders = placeholder_registry
  )

  writeLines(shiny_text, output_file)
  if (isTRUE(style) && requireNamespace("styler", quietly = TRUE)) {
    styler::style_file(output_file)
  }

  invisible(output_file)
}
