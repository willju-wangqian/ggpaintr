#' Serialize an R Object as Source Text
#'
#' @param x An R object.
#'
#' @return A character vector.
#' @noRd
ptr_serialize_r_object <- function(x) {
  utils::capture.output(dput(x))
}

#' Escape One String Segment for a Template Literal
#'
#' @param x A character string.
#'
#' @return A single escaped string segment.
#' @noRd
ptr_escape_string_segment <- function(x) {
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
ptr_prefix_assignment <- function(name, value_lines) {
  value_lines[1] <- paste0(name, " <- ", value_lines[1])
  value_lines
}

#' Serialize a Formula String for an Exported Template
#'
#' @param formula_text A single formula string.
#'
#' @return A character vector of source lines.
#' @noRd
ptr_serialize_formula_text <- function(formula_text) {
  formula_lines <- strsplit(formula_text, "\n", fixed = TRUE)[[1]]
  formula_lines <- vapply(
    formula_lines,
    ptr_escape_string_segment,
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
#' @param ui_text User-supplied or effective copy rules.
#' @param placeholders Placeholder definitions or a placeholder registry used
#'   to compact placeholder-specific copy overrides.
#'
#' @return A character vector of source lines.
#' @noRd
ptr_serialize_export_ui_text <- function(ui_text = NULL, placeholders = NULL) {
  default_header <- paste(
    "# Replace NULL with a named list to customize UI labels, help text, and",
    "placeholders."
  )
  compact_ui_text <- ptr_compact_ui_text(
    ui_text,
    placeholders = placeholders
  )
  if (is.null(compact_ui_text)) {
    return(c(default_header, "ui_text <- NULL"))
  }

  custom_header <- paste(
    "# Edit custom_ui_text to customize UI labels, help text, and",
    "placeholders."
  )
  custom_ui_text_lines <- ptr_serialize_r_object(compact_ui_text)
  custom_ui_text_lines <- ptr_prefix_assignment(
    "custom_ui_text",
    custom_ui_text_lines
  )

  c(
    custom_header,
    custom_ui_text_lines,
    "",
    "ui_text <- ptr_merge_ui_text(custom_ui_text)"
  )
}

#' Resolve the Source Strategy Value for One Hook
#'
#' @param param A scalar character or named list (with `.default` and hook keys).
#' @param hook_name A hook name string.
#'
#' @return A character string or `NULL`.
#' @noRd
ptr_resolve_source_param <- function(param, hook_name) {
  if (is.null(param)) return(NULL)
  if (is.character(param)) return(param)
  if (is.list(param)) {
    if (hook_name %in% names(param)) return(param[[hook_name]])
    if (".default" %in% names(param)) return(param[[".default"]])
  }
  NULL
}

#' Deparse a Named Function as an Assignment Block
#'
#' @param fn_name Character string — the name to assign.
#' @param fn_obj A function object.
#'
#' @return A character vector of source lines.
#' @noRd
ptr_serialize_source_fn_block <- function(fn_name, fn_obj) {
  fn_lines <- deparse(fn_obj)
  fn_lines[1] <- paste0(fn_name, " <- ", fn_lines[1])
  fn_lines
}

#' Emit a source() Call for an Exported App
#'
#' @param path Character string — the file path to source.
#' @param on_missing One of `"warn"` or `"error"`.
#'
#' @return A character vector of source lines.
#' @noRd
ptr_serialize_source_file_block <- function(path, on_missing = "warn") {
  if (identical(on_missing, "error")) {
    return(c(
      paste0("# Provide '", path, "' alongside this app.R."),
      paste0("source(\"", path, "\")")
    ))
  }
  c(
    paste0("# Provide '", path, "' alongside this app.R."),
    paste0("tryCatch("),
    paste0("  source(\"", path, "\"),"),
    paste0("  error = function(e) {"),
    paste0("    warning(\"ggpaintr: could not source '", path,
           "' -- custom placeholder hooks unavailable. \","),
    paste0("            \"Provide this file alongside app.R.\")"),
    paste0("  }"),
    paste0(")")
  )
}

#' Emit a library() Call With Install-on-Missing for an Exported App
#'
#' @param pkg Character string — the package name.
#' @param on_missing One of `"warn"` or `"error"`.
#'
#' @return A character vector of source lines.
#' @noRd
ptr_serialize_source_pkg_block <- function(pkg, on_missing = "warn") {
  if (identical(on_missing, "error")) {
    return(c(
      paste0("if (!requireNamespace(\"", pkg, "\", quietly = TRUE)) {"),
      paste0("  install.packages(\"", pkg, "\")"),
      paste0("}"),
      paste0("library(", pkg, ")")
    ))
  }
  c(
    paste0("if (!requireNamespace(\"", pkg, "\", quietly = TRUE)) {"),
    paste0("  tryCatch("),
    paste0("    install.packages(\"", pkg, "\"),"),
    paste0("    error = function(e) warning(\"ggpaintr: could not install '", pkg,
           "' -- custom placeholder hooks unavailable.\")"),
    paste0("  )"),
    paste0("}"),
    paste0("library(", pkg, ")")
  )
}

#' Build Preamble Code for Custom Placeholder Source Strategies
#'
#' Emits named function definitions (source_function), source() calls
#' (source_file), and library() calls (source_package) for all custom
#' placeholders that use non-inline hooks.
#'
#' @param placeholders Placeholder definitions or a placeholder registry.
#'
#' @return A character vector of source lines, or `character(0)` if none needed.
#' @importFrom rlang %||%
#' @noRd
ptr_serialize_placeholder_preamble <- function(placeholders = NULL) {
  registry <- ptr_merge_placeholders(placeholders)
  custom_phs <- registry$custom_placeholders

  if (length(custom_phs) == 0) return(character(0))

  hook_names <- c("build_ui", "resolve_expr", "resolve_input", "bind_ui", "prepare_eval_env")

  fn_blocks  <- list()    # symbol_name -> lines (deduplicated by original name)
  file_specs <- list()    # path -> on_missing (strictest wins)
  pkg_specs  <- list()    # pkg  -> on_missing (strictest wins)

  for (ph in custom_phs) {
    on_missing <- ph$on_missing %||% "warn"

    # source_function: recover original symbol names from definition_call, not hook names
    if (!is.null(ph$source_function)) {
      def_call <- ph$definition_call
      sf_expr <- def_call[["source_function"]]

      if (!is.null(sf_expr) && rlang::is_call(sf_expr, "list")) {
        sf_args <- as.list(sf_expr)[-1]
        for (hook_name in names(sf_args)) {
          sym_expr <- sf_args[[hook_name]]
          fn_obj   <- ph$source_function[[hook_name]]

          if (is.symbol(sym_expr) && !is.null(fn_obj)) {
            sym_name <- as.character(sym_expr)
            if (!sym_name %in% names(fn_blocks)) {
              fn_blocks[[sym_name]] <- ptr_serialize_source_fn_block(sym_name, fn_obj)
            }
          }
          # Inline function literals need no preamble — they are captured in definition_call
        }
      }
    }

    # source_file / source_package: collect per hook, deduplicate paths/pkgs
    for (hook in hook_names) {
      file_path <- ptr_resolve_source_param(ph$source_file, hook)
      if (!is.null(file_path)) {
        prev <- file_specs[[file_path]]
        file_specs[[file_path]] <- if (!is.null(prev) && identical(prev, "error")) "error" else on_missing
      }

      pkg_name <- ptr_resolve_source_param(ph$source_package, hook)
      if (!is.null(pkg_name)) {
        prev <- pkg_specs[[pkg_name]]
        pkg_specs[[pkg_name]] <- if (!is.null(prev) && identical(prev, "error")) "error" else on_missing
      }
    }
  }

  lines <- character(0)

  for (sym_name in names(fn_blocks)) {
    lines <- c(lines, fn_blocks[[sym_name]], "")
  }

  for (path in names(file_specs)) {
    lines <- c(lines, ptr_serialize_source_file_block(path, file_specs[[path]]), "")
  }

  for (pkg in names(pkg_specs)) {
    lines <- c(lines, ptr_serialize_source_pkg_block(pkg, pkg_specs[[pkg]]), "")
  }

  lines
}

#' Serialize Custom Placeholder Definitions for an Exported Template
#'
#' @param placeholders Placeholder definitions or a placeholder registry.
#'
#' @return A character vector of source lines.
#' @noRd
ptr_serialize_export_placeholders <- function(placeholders = NULL) {
  custom_placeholders <- ptr_exportable_custom_placeholders(placeholders)
  default_header <- paste(
    "# Replace NULL with a named list of ptr_define_placeholder() calls to",
    "register custom placeholder types."
  )

  if (length(custom_placeholders) == 0) {
    return(c(default_header, "placeholders <- NULL"))
  }

  custom_header <- paste(
    "# Edit custom_placeholders to register custom placeholder types for this",
    "exported app."
  )
  custom_placeholder_lines <- ptr_serialize_r_object(custom_placeholders)
  custom_placeholder_lines <- ptr_prefix_assignment(
    "custom_placeholders",
    custom_placeholder_lines
  )

  c(
    custom_header,
    custom_placeholder_lines,
    "",
    "placeholders <- ptr_merge_placeholders(custom_placeholders)"
  )
}

#' Serialize a ptr_build_ids Object to an R Assignment String
#'
#' @param ids A `ptr_build_ids` object.
#'
#' @return A single character string containing an R assignment expression.
#' @noRd
ptr_serialize_ids <- function(ids) {
  fields <- c(
    "control_panel", "draw_button", "export_button",
    "plot_output", "error_output", "code_output"
  )
  args <- paste(
    vapply(fields, function(f) {
      paste0(f, " = \"", ptr_escape_string_segment(ids[[f]]), "\"")
    }, character(1)),
    collapse = ", "
  )
  paste0("ids <- ptr_build_ids(", args, ")")
}

#' Build the Standalone App Template
#'
#' @param formula_text A single formula string.
#' @param ui_text Effective or user-supplied copy rules.
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return A character vector containing the exported app template.
#' @noRd
ptr_shiny_template <- function(formula_text,
                               ui_text = NULL,
                               placeholders = NULL,
                               ids = ptr_build_ids()) {
  formula_text_lines <- ptr_prefix_assignment(
    "input_formula",
    ptr_serialize_formula_text(formula_text)
  )
  ui_text_lines <- ptr_serialize_export_ui_text(
    ui_text,
    placeholders = placeholders
  )
  placeholder_lines <- ptr_serialize_export_placeholders(placeholders)

  preamble_lines <- ptr_serialize_placeholder_preamble(placeholders)

  c(
    "library(ggpaintr)",
    "library(shiny)",
    "library(shinyWidgets)",
    "",
    if (length(preamble_lines) > 0) c(preamble_lines, "") else character(0),
    formula_text_lines,
    "",
    ui_text_lines,
    "",
    placeholder_lines,
    "",
    ptr_serialize_ids(ids),
    "",
    "title_copy <- ptr_resolve_ui_text(\"title\", ui_text = ui_text)",
    "draw_copy <- ptr_resolve_ui_text(\"draw_button\", ui_text = ui_text)",
    "export_copy <- ptr_resolve_ui_text(\"export_button\", ui_text = ui_text)",
    "",
    "ui <- fluidPage(",
    "  titlePanel(title_copy$label),",
    "  sidebarLayout(",
    "    sidebarPanel(",
    "      # Modify or add controls here.",
    "      uiOutput(ids$control_panel),",
    "      actionButton(ids$draw_button, draw_copy$label),",
    "      downloadButton(ids$export_button, export_copy$label)",
    "    ),",
    "    mainPanel(",
    "      # Modify or add outputs here.",
    "      plotOutput(ids$plot_output),",
    "      uiOutput(ids$error_output),",
    "      verbatimTextOutput(ids$code_output)",
    "    )",
    "  )",
    ")",
    "",
    "server <- function(input, output, session) {",
    "  ptr_state <- ptr_server(input, output, session, input_formula, ui_text = ui_text, placeholders = placeholders, ids = ids)",
    "",
    "  # Add custom observers or outputs below.",
    "  # observe({",
    "  #   runtime_result <- ptr_state$runtime()",
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
#' @param ptr_obj A `ptr_obj`.
#' @param output_file Path to the generated `.R` script.
#' @param style Whether to style the generated file with `styler` when available.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry. Exported custom placeholders must define their hooks
#'   inline inside `ptr_define_placeholder()` so the generated app can stay
#'   standalone.
#' @param ids A `ptr_build_ids` object controlling the Shiny element IDs used in
#'   the generated app. Defaults to `ptr_build_ids()`.
#'
#' @return Invisibly returns `output_file`.
#' @examples
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' out_file <- tempfile(fileext = ".R")
#' ptr_generate_shiny(obj, out_file, style = FALSE)
#' file.exists(out_file)
#' @export
ptr_generate_shiny <- function(ptr_obj,
                           output_file,
                           style = TRUE,
                           ui_text = NULL,
                           placeholders = NULL,
                           ids = ptr_build_ids()) {
  if (!is.character(output_file) || length(output_file) != 1 || is.na(output_file)) {
    rlang::abort("output_file must be a single non-missing string.")
  }

  placeholder_registry <- if (is.null(placeholders)) {
    ptr_obj$placeholders
  } else {
    ptr_merge_placeholders(placeholders)
  }
  shiny_text <- ptr_shiny_template(
    ptr_obj$formula_text,
    ui_text = ui_text,
    placeholders = placeholder_registry,
    ids = ids
  )

  tryCatch(
    writeLines(shiny_text, output_file),
    error = function(e) {
      rlang::abort(
        paste0("Failed to write app script to '", output_file, "': ", conditionMessage(e))
      )
    }
  )
  if (isTRUE(style) && requireNamespace("styler", quietly = TRUE)) {
    styler::style_file(output_file)
  }

  invisible(output_file)
}
