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

# Wrap a control in a tagList with a helpText note when `help` is a
# non-empty string; otherwise return the control unchanged. Mirrors the
# legacy `ptr_attach_help()` from paintr-ui.R.
attach_help <- function(ui, help) {
  if (is.null(help) || !nzchar(trimws(help))) return(ui)
  shiny::tagList(ui, shiny::helpText(help))
}

# ---- text -------------------------------------------------------------------

ptr_builtin_text_build_ui <- function(node, label = NULL, copy = NULL,
                                      selected = NULL, ...) {
  # PLAN-07: seed initial value from `selected` (orchestrator passes
  # `node$default` here when no persisted input exists).
  initial <- if (is.null(selected) || length(selected) == 0L) {
    ""
  } else {
    as.character(selected)[[1L]]
  }
  attach_help(
    shiny::textInput(
      inputId = node$id,
      label = label %||% "Enter a value",
      value = initial,
      placeholder = copy$placeholder
    ),
    copy$help
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

ptr_builtin_num_build_ui <- function(node, label = NULL, copy = NULL,
                                     selected = NULL, ...) {
  # PLAN-07: seed initial value from `selected` (orchestrator passes
  # `node$default` here when no persisted input exists).
  initial <- if (is.null(selected) || length(selected) == 0L) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(selected[[1L]]))
  }
  control <- shiny::numericInput(
    inputId = node$id,
    label = label %||% "Enter a number",
    value = initial
  )
  has_initial <- !is.na(initial)
  # shiny::numericInput unconditionally renders `value="<as.character(initial)>"`,
  # which is the literal string "NA" when initial is NA_real_. Walk into the
  # rendered `<input>` and (a) overwrite value with a clean numeric (or empty
  # string when no seed), and (b) stamp `copy$empty_text` as the HTML placeholder
  # attribute (numericInput does not expose `placeholder`). Both edits live on
  # the same child so a single walk handles them.
  for (i in seq_along(control$children)) {
    child <- control$children[[i]]
    if (inherits(child, "shiny.tag") && identical(child$name, "input")) {
      if (!is.null(copy$empty_text) && nzchar(copy$empty_text)) {
        child$attribs$placeholder <- copy$empty_text
      }
      if (has_initial) {
        # Use formatC to keep integer-shaped seeds like 5 from rendering as "5.0".
        child$attribs$value <- formatC(initial, format = "fg",
                                       drop0trailing = TRUE)
      } else if (!is.null(copy$empty_text) && nzchar(copy$empty_text)) {
        child$attribs$value <- ""
      }
      control$children[[i]] <- child
      break
    }
  }
  attach_help(control, copy$help)
}

ptr_builtin_num_resolve_expr <- function(value, node, ...) {
  # `as.numeric("abc")` is `NA_real_` with a warning. Drop those cases via
  # NULL so substitute marks the arg as missing and prune deletes it,
  # rather than splicing `NA_real_` into the rendered code.
  out <- suppressWarnings(as.numeric(value))
  if (length(out) != 1L || is.na(out)) return(NULL)
  out
}

# ---- expr -------------------------------------------------------------------

ptr_builtin_expr_build_ui <- function(node, label = NULL, copy = NULL,
                                      selected = NULL, ...) {
  # PLAN-07: seed initial textarea value from `selected`. Strings render
  # verbatim; a language default (from `ptr_default_expression`) is
  # deparsed back to source text.
  initial <- if (is.null(selected) ||
                 (is.atomic(selected) && length(selected) == 0L)) {
    ""
  } else if (is.language(selected)) {
    paste(deparse(selected), collapse = "\n")
  } else {
    as.character(selected)[[1L]]
  }
  attach_help(
    shiny::textAreaInput(
      inputId = node$id,
      label = label %||% "Enter an expression",
      value = initial,
      placeholder = copy$placeholder
    ),
    copy$help
  )
}

ptr_builtin_expr_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    rlang::abort(paste0(
      "An `expr` input must contain a single R expression written as text ",
      "(for example: `factor(cyl)`)."
    ))
  }
  exprs <- tryCatch(
    rlang::parse_exprs(value),
    error = function(e) {
      rlang::abort(paste0(
        "expr placeholder: could not parse input as R expression: ",
        conditionMessage(e)
      ))
    }
  )
  if (length(exprs) != 1L) {
    rlang::abort(paste0(
      "expr placeholder: input must contain exactly one expression, but ",
      length(exprs), " were found."
    ))
  }
  exprs[[1]]
}

# ---- var --------------------------------------------------------------------

ptr_builtin_var_build_ui <- function(node, cols = character(),
                                     label = NULL, copy = NULL,
                                     selected = character(0), ...) {
  picker_label <- label %||% "Pick a column"
  none_text <- copy$empty_text %||% picker_label
  # Legacy paintr trick: `multiple = TRUE` + `maxOptions = 1L`. This buys
  # two real-browser behaviours that single-select pickerInput can't:
  #   1. No first-choice default at launch (input value starts as
  #      character(0), not the first column — verified in a probe app).
  #   2. Clicking the currently-selected column deselects it (the
  #      old single-select picker silently ignored the click).
  # `noneSelectedText` is the placeholder shown when nothing is picked.
  retained <- intersect(selected, cols)
  attach_help(
    shinyWidgets::pickerInput(
      inputId = node$id,
      label = picker_label,
      choices = cols,
      selected = retained,
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        noneSelectedText = none_text,
        maxOptions = 1L
      )
    ),
    copy$help
  )
}

ptr_builtin_var_resolve_expr <- function(value, node, ...) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) {
    rlang::abort("A `var` input must be a single column name.")
  }
  rlang::sym(value)
}

ptr_builtin_var_validate_input <- function(value, upstream_cols) {
  if (length(value) != 1L) {
    return(paste0("Expected a single column; got ", length(value), "."))
  }
  if (!value %in% upstream_cols) {
    return(paste0("Column `", value, "` is not in the data."))
  }
  TRUE
}

# ---- upload -----------------------------------------------------------------

# Kept in sync with the extensions `ptr_read_uploaded_data()` dispatches on.
ptr_upload_accept_formats <- function() {
  c(".csv", ".tsv", ".rds", ".xlsx", ".xls", ".json")
}

ptr_builtin_upload_build_ui <- function(node, label = NULL, copy = NULL,
                                        file_copy = NULL, name_copy = NULL,
                                        ...) {
  shiny::tagList(
    attach_help(
      shiny::fileInput(
        inputId = node$id,
        label = file_copy$label %||% label %||% "Choose a data file",
        accept = ptr_upload_accept_formats()
      ),
      file_copy$help
    ),
    attach_help(
      shiny::textInput(
        inputId = node$companion_id %||% paste0(node$id, "_name"),
        label = name_copy$label %||% "Optional dataset name",
        value = "",
        placeholder = name_copy$placeholder
      ),
      name_copy$help
    )
  )
}

# Delegates to the existing reader in paintr-upload.R verbatim.
ptr_builtin_upload_resolve_data <- function(value, node, ...) {
  ptr_read_uploaded_data(value)
}

# ---- registration -----------------------------------------------------------

# Keywords registered by `ptr_register_builtins()`. Maintained by hand --
# `ptr_clear_placeholder()` uses this to tell built-ins (which it refuses to
# drop) apart from user-registered placeholders.
ptr_builtin_keywords <- function() {
  c("text", "num", "expr", "var", "upload")
}

ptr_register_builtins <- function() {
  ptr_define_placeholder_value(
    keyword = "text",
    build_ui = ptr_builtin_text_build_ui,
    resolve_expr = ptr_builtin_text_resolve_expr,
    copy_defaults = list(
      label = "Enter a value for {param}",
      placeholder = "Plain text - quotes are added automatically"
    )
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
    copy_defaults = list(
      label = "Pick a column for {param}",
      empty_text = "Choose one column"
    )
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
  ptr_register_constant_fold_builtins()
}

`%||%` <- function(a, b) if (is.null(a)) b else a
