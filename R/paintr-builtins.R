# Built-in placeholders registered against `.ptr_registry`.
#
# Wiring summary:
#   text  -> value     (textInput)
#   num   -> value     (numericInput)
#   expr  -> value     (textAreaInput; P8 wraps in ptr_user_expr)
#   var   -> consumer  (pickerInput; cols resolved from upstream)
#   upload-> source    (fileInput + companion textInput; resolve_data
#                       delegates to ptr_read_uploaded_data verbatim)
#
# UI hooks here are minimal but callable; the app/UI layer wires them up
# at the cutover phase.

# ---- text -------------------------------------------------------------------

ptr_builtin_text_build_ui <- function(node, label = NULL, ...) {
  shiny::textInput(
    inputId = node$id,
    label = label %||% "Enter a value",
    value = ""
  )
}

ptr_builtin_text_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L) {
    return(as.character(value))
  }
  strip_matched_quote_pair(value)
}

strip_matched_quote_pair <- function(s) {
  if (nchar(s) < 2L) return(s)
  first <- substr(s, 1L, 1L)
  last <- substr(s, nchar(s), nchar(s))
  if (first == last && first %in% c("'", "\"")) {
    return(substr(s, 2L, nchar(s) - 1L))
  }
  s
}

# ---- num --------------------------------------------------------------------

ptr_builtin_num_build_ui <- function(node, label = NULL, ...) {
  shiny::numericInput(
    inputId = node$id,
    label = label %||% "Enter a number",
    value = NA_real_
  )
}

ptr_builtin_num_resolve_expr <- function(value, node, ...) {
  as.numeric(value)
}

# ---- expr -------------------------------------------------------------------

ptr_builtin_expr_build_ui <- function(node, label = NULL, ...) {
  shiny::textAreaInput(
    inputId = node$id,
    label = label %||% "Enter an expression",
    value = ""
  )
}

ptr_builtin_expr_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    rlang::abort("expr placeholder requires exactly one expression as a string.")
  }
  exprs <- rlang::parse_exprs(value)
  if (length(exprs) != 1L) {
    rlang::abort("expr placeholder requires exactly one expression.")
  }
  exprs[[1]]
}

# ---- var --------------------------------------------------------------------

ptr_builtin_var_build_ui <- function(node, cols = character(), label = NULL, ...) {
  shinyWidgets::pickerInput(
    inputId = node$id,
    label = label %||% "Pick a column",
    choices = cols
  )
}

ptr_builtin_var_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    rlang::abort("var placeholder requires a single column name.")
  }
  rlang::sym(value)
}

ptr_builtin_var_validate_input <- function(value, upstream_cols) {
  if (length(value) != 1L) {
    return(paste0("Expected a single column; got ", length(value), "."))
  }
  if (!value %in% upstream_cols) {
    return(paste0("Column ", value, " is not in upstream."))
  }
  TRUE
}

# ---- upload -----------------------------------------------------------------

ptr_builtin_upload_build_ui <- function(node, label = NULL, ...) {
  shiny::tagList(
    shiny::fileInput(
      inputId = node$id,
      label = label %||% "Upload data"
    ),
    shiny::textInput(
      inputId = node$companion_id %||% paste0(node$id, "_name"),
      label = "Dataset name",
      value = ""
    )
  )
}

# Delegates to the existing reader in paintr-upload.R verbatim.
ptr_builtin_upload_resolve_data <- function(value, node, ...) {
  ptr_read_uploaded_data(value)
}

# ---- registration -----------------------------------------------------------

ptr_register_builtins <- function() {
  ptr_define_placeholder_value(
    keyword = "text",
    build_ui = ptr_builtin_text_build_ui,
    resolve_expr = ptr_builtin_text_resolve_expr,
    copy_defaults = list(label = "Enter a value for {param}")
  )
  ptr_define_placeholder_value(
    keyword = "num",
    build_ui = ptr_builtin_num_build_ui,
    resolve_expr = ptr_builtin_num_resolve_expr,
    copy_defaults = list(label = "Enter a number for {param}")
  )
  ptr_define_placeholder_value(
    keyword = "expr",
    build_ui = ptr_builtin_expr_build_ui,
    resolve_expr = ptr_builtin_expr_resolve_expr,
    copy_defaults = list(label = "Enter an expression for {param}")
  )
  ptr_define_placeholder_consumer(
    keyword = "var",
    build_ui = ptr_builtin_var_build_ui,
    resolve_expr = ptr_builtin_var_resolve_expr,
    validate_input = ptr_builtin_var_validate_input,
    copy_defaults = list(label = "Pick a column for {param}")
  )
  ptr_define_placeholder_source(
    keyword = "upload",
    build_ui = ptr_builtin_upload_build_ui,
    resolve_data = ptr_builtin_upload_resolve_data,
    companion_id_fn = ptr_upload_name_id,
    copy_defaults = list(label = "Upload data for {param}")
  )
  invisible(NULL)
}

.onLoad <- function(libname, pkgname) {
  ptr_register_builtins()
}

`%||%` <- function(a, b) if (is.null(a)) b else a
