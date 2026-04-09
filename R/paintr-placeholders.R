#' Construct a Custom ggpaintr Placeholder
#'
#' Build one placeholder specification for use with
#' `ptr_merge_placeholders()`. Custom placeholders can define their own
#' UI control, runtime expression replacement, deferred UI binding, and
#' evaluation-environment preparation.
#'
#' @note **Export limitation:** To make a custom placeholder exportable through
#'   `ptr_generate_shiny()`, all hook functions (`build_ui`, `resolve_expr`, etc.)
#'   must be defined **inline** inside the `ptr_define_placeholder()` call.
#'   Functions defined elsewhere and passed by name (e.g.,
#'   `build_ui = my_function`) cannot be serialized into the standalone app file.
#'   This is a known limitation; a future release may lift this restriction.
#'
#' @param keyword A single syntactic placeholder name used inside the formula.
#' @param build_ui Function with signature `(id, copy, meta, context)` returning
#'   a Shiny UI control or placeholder.
#' @param resolve_expr Function with signature `(value, meta, context)`
#'   returning an R expression or `ptr_missing_expr()`.
#' @param resolve_input Optional function with signature
#'   `(input, id, meta, context)` returning the raw value to hand to
#'   `resolve_expr()`. Defaults to `input[[id]]`.
#' @param bind_ui Optional function with signature `(input, output, metas,
#'   context)` for registering deferred UI such as the built-in `var`
#'   placeholder.
#' @param prepare_eval_env Optional function with signature
#'   `(input, metas, eval_env, context)` returning an updated evaluation
#'   environment.
#' @param copy_defaults Optional named list with `label`, `help`,
#'   `placeholder`, and `empty_text`. Defaults to
#'   `list(label = "Enter a value for {param}")`.
#'
#' @return An object of class `ptr_define_placeholder`.
#' @examples
#' date_placeholder <- ptr_define_placeholder(
#'   keyword = "date",
#'   build_ui = function(id, copy, meta, context) {
#'     shiny::dateInput(id, copy$label)
#'   },
#'   resolve_expr = function(value, meta, context) {
#'     if (is.null(value) || identical(value, "")) {
#'       return(ptr_missing_expr())
#'     }
#'
#'     rlang::expr(as.Date(!!value))
#'   },
#'   copy_defaults = list(label = "Choose a date for {param}")
#' )
#' date_placeholder$keyword
#' @export
ptr_define_placeholder <- function(keyword,
                                 build_ui,
                                 resolve_expr,
                                 resolve_input = NULL,
                                 bind_ui = NULL,
                                 prepare_eval_env = NULL,
                                 copy_defaults = list(label = "Enter a value for {param}")) {
  placeholder <- structure(
    list(
      keyword = keyword,
      build_ui = build_ui,
      resolve_expr = resolve_expr,
      resolve_input = resolve_input,
      bind_ui = bind_ui,
      prepare_eval_env = prepare_eval_env,
      copy_defaults = copy_defaults,
      definition_call = match.call(expand.dots = FALSE)
    ),
    class = c("ptr_define_placeholder", "list")
  )

  ptr_validate_placeholder(placeholder)
  placeholder
}

#' Build the Effective Placeholder Registry for ggpaintr
#'
#' Combine the built-in placeholder definitions with optional custom
#' placeholders. Custom placeholders override built-in placeholders on keyword
#' collision.
#'
#' @param placeholders Either `NULL`, a named list of
#'   `ptr_define_placeholder` objects, or an existing
#'   `ptr_define_placeholder_registry`.
#'
#' @return An object of class `ptr_define_placeholder_registry`.
#' @examples
#' registry <- ptr_merge_placeholders()
#' all(c("var", "text", "num", "expr", "upload") %in% names(registry))
#'
#' date_placeholder <- ptr_define_placeholder(
#'   keyword = "date",
#'   build_ui = function(id, copy, meta, context) {
#'     shiny::dateInput(id, copy$label)
#'   },
#'   resolve_expr = function(value, meta, context) {
#'     if (is.null(value) || identical(value, "")) {
#'       return(ptr_missing_expr())
#'     }
#'
#'     rlang::expr(as.Date(!!value))
#'   }
#' )
#' custom_registry <- ptr_merge_placeholders(list(date = date_placeholder))
#' "date" %in% names(custom_registry)
#' @export
ptr_merge_placeholders <- function(placeholders = NULL) {
  if (inherits(placeholders, "ptr_define_placeholder_registry")) {
    return(placeholders)
  }

  custom_placeholders <- ptr_normalize_placeholders(placeholders)
  builtin_placeholders <- ptr_builtin_placeholders()
  registry_entries <- builtin_placeholders

  for (keyword in names(custom_placeholders)) {
    registry_entries[[keyword]] <- custom_placeholders[[keyword]]
  }

  structure(
    registry_entries,
    class = c("ptr_define_placeholder_registry", "list"),
    custom_placeholders = custom_placeholders
  )
}

#' Return the Sentinel for Removing a Placeholder Argument
#'
#' Placeholder resolvers should return `ptr_missing_expr()` when the target
#' argument should be removed from the completed expression.
#'
#' @return An object of class `ptr_missing_expr`.
#' @examples
#' inherits(ptr_missing_expr(), "ptr_missing_expr")
#' @export
ptr_missing_expr <- function() {
  structure(list(), class = "ptr_missing_expr")
}

#' Extract One Placeholder Registry Entry with `$`
#'
#' @param x A `ptr_define_placeholder_registry`.
#' @param name An entry name.
#'
#' @return A registry entry or the stored custom placeholder list.
#' @noRd
`$.ptr_define_placeholder_registry` <- function(x, name) {
  if (identical(name, "custom_placeholders")) {
    return(attr(x, "custom_placeholders"))
  }

  .subset2(unclass(x), name)
}

#' Extract One Placeholder Registry Entry with `[[`
#'
#' @param x A `ptr_define_placeholder_registry`.
#' @param i An index or name.
#' @param ... Unused.
#'
#' @return A registry entry or the stored custom placeholder list.
#' @noRd
`[[.ptr_define_placeholder_registry` <- function(x, i, ...) {
  if (is.character(i) && length(i) == 1 && identical(i, "custom_placeholders")) {
    return(attr(x, "custom_placeholders"))
  }

  NextMethod()
}

#' Validate One ggpaintr Placeholder Definition
#'
#' @param placeholder A placeholder object.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_placeholder <- function(placeholder) {
  required_names <- c(
    "keyword",
    "build_ui",
    "resolve_expr",
    "resolve_input",
    "bind_ui",
    "prepare_eval_env",
    "copy_defaults",
    "definition_call"
  )

  if (!inherits(placeholder, "ptr_define_placeholder")) {
    rlang::abort("placeholders must inherit from 'ptr_define_placeholder'.")
  }

  missing_names <- setdiff(required_names, names(placeholder))
  if (length(missing_names) > 0) {
    rlang::abort(paste0("placeholder is missing required entries: ", paste(missing_names, collapse = ", "), "."))
  }

  if (!is.character(placeholder$keyword) || length(placeholder$keyword) != 1) {
    rlang::abort("placeholder$keyword must be a single string.")
  }

  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", placeholder$keyword)) {
    rlang::abort("placeholder$keyword must be a syntactic placeholder name.")
  }

  if (!is.function(placeholder$build_ui)) {
    rlang::abort("placeholder$build_ui must be a function.")
  }

  if (!is.function(placeholder$resolve_expr)) {
    rlang::abort("placeholder$resolve_expr must be a function.")
  }

  optional_function_names <- c("resolve_input", "bind_ui", "prepare_eval_env")
  for (name in optional_function_names) {
    value <- placeholder[[name]]
    if (!is.null(value) && !is.function(value)) {
      rlang::abort(paste0("placeholder$", name, " must be NULL or a function."))
    }
  }

  expected_arity <- list(
    build_ui = 4L,
    resolve_expr = 3L,
    resolve_input = 4L,
    bind_ui = 4L,
    prepare_eval_env = 4L
  )
  for (hook_name in names(expected_arity)) {
    fn <- placeholder[[hook_name]]
    if (!is.null(fn) && is.function(fn)) {
      actual <- length(formals(fn))
      expected <- expected_arity[[hook_name]]
      if (actual != expected) {
        rlang::abort(paste0(
          "placeholder$", hook_name, " must accept ", expected,
          " arguments, but has ", actual, "."
        ))
      }
    }
  }

  if (is.null(placeholder$copy_defaults)) {
    placeholder$copy_defaults <- list()
  }

  if (!is.list(placeholder$copy_defaults) || is.null(names(placeholder$copy_defaults))) {
    rlang::abort("placeholder$copy_defaults must be a named list.")
  }

  unknown_copy_fields <- setdiff(names(placeholder$copy_defaults), ptr_ui_text_leaf_fields())
  if (length(unknown_copy_fields) > 0) {
    rlang::abort(paste0("placeholder$copy_defaults has unsupported fields: ", paste(sort(unknown_copy_fields), collapse = ", "), "."))
  }

  for (field_name in names(placeholder$copy_defaults)) {
    value <- placeholder$copy_defaults[[field_name]]
    if (!is.character(value) || length(value) != 1) {
      rlang::abort(paste0("placeholder$copy_defaults$", field_name, " must be a single string."))
    }
  }

  invisible(TRUE)
}

#' Normalize User-Supplied Placeholder Definitions
#'
#' @param placeholders Placeholder definitions or `NULL`.
#'
#' @return A named list of `ptr_define_placeholder` objects.
#' @noRd
ptr_normalize_placeholders <- function(placeholders = NULL) {
  if (is.null(placeholders)) {
    return(list())
  }

  if (!is.list(placeholders)) {
    rlang::abort("placeholders must be NULL or a named list.")
  }

  normalized <- list()
  seen_keywords <- character()

  for (i in seq_along(placeholders)) {
    placeholder <- placeholders[[i]]
    ptr_validate_placeholder(placeholder)

    keyword <- placeholder$keyword
    supplied_name <- names(placeholders)[i]
    if (!is.null(supplied_name) &&
        !identical(supplied_name, "") &&
        !identical(supplied_name, keyword)) {
      rlang::abort(paste0("placeholders name '", supplied_name, "' does not match placeholder keyword '", keyword, "'."))
    }

    if (keyword %in% seen_keywords) {
      rlang::abort(paste0("placeholders contains duplicated keywords: ", keyword, "."))
    }

    normalized[[keyword]] <- placeholder
    seen_keywords <- c(seen_keywords, keyword)
  }

  normalized
}

#' Return the Flattened Placeholder Metadata for a Parsed Formula
#'
#' @param ptr_obj A `ptr_obj`.
#' @param keyword Optional placeholder keyword filter.
#'
#' @return A list of metadata records.
#' @noRd
ptr_flatten_placeholder_map <- function(ptr_obj, keyword = NULL) {
  placeholder_map <- ptr_obj[["placeholder_map"]]
  if (is.null(placeholder_map) || length(placeholder_map) == 0) {
    return(list())
  }

  flattened <- do.call(c, lapply(placeholder_map, unname))
  if (is.null(keyword)) {
    return(flattened)
  }

  Filter(function(meta) identical(meta$keyword, keyword), flattened)
}

#' Build the Hook Context for Placeholder Operations
#'
#' @param ptr_obj A `ptr_obj`.
#' @param ui_text Optional copy rules.
#' @param envir Environment used to resolve local objects.
#'
#' @return A named list.
#' @noRd
ptr_define_placeholder_context <- function(ptr_obj,
                                       ui_text = NULL,
                                       envir = parent.frame()) {
  list(
    ptr_obj = ptr_obj,
    placeholders = ptr_obj$placeholders,
    ui_text = ptr_merge_ui_text(
      ui_text,
      placeholders = ptr_obj$placeholders
    ),
    envir = envir
  )
}

#' Return the Internal Missing-Expression Symbol
#'
#' @return A symbol.
#' @noRd
ptr_missing_expr_symbol <- function() {
  rlang::sym("_NULL_PLACEHOLDER")
}

#' Detect the Public Missing-Expression Sentinel
#'
#' @param x An object.
#'
#' @return A single logical value.
#' @noRd
ptr_is_missing_expr <- function(x) {
  inherits(x, "ptr_missing_expr")
}

#' Resolve One Placeholder Input Value
#'
#' @param spec A `ptr_define_placeholder`.
#' @param input A Shiny input-like object.
#' @param meta A metadata record.
#' @param context A placeholder context list.
#'
#' @return A raw input value.
#' @noRd
ptr_resolve_placeholder_input <- function(spec, input, meta, context) {
  if (is.null(spec$resolve_input)) {
    return(input[[meta$id]])
  }

  spec$resolve_input(input, meta$id, meta, context)
}

#' Resolve One Placeholder Expression Replacement
#'
#' @param spec A `ptr_define_placeholder`.
#' @param value A raw value.
#' @param meta A metadata record.
#' @param context A placeholder context list.
#'
#' @return An R object suitable for `expr_pluck<-`.
#' @noRd
ptr_resolve_placeholder_expr <- function(spec, value, meta, context) {
  resolved_expr <- spec$resolve_expr(value, meta, context)

  if (ptr_is_missing_expr(resolved_expr)) {
    return(ptr_missing_expr_symbol())
  }

  if (is.function(resolved_expr)) {
    rlang::abort(paste0("Placeholder '", meta$keyword, "' returned a function instead of an expression."))
  }

  resolved_expr
}

#' Register Deferred UI for Placeholder Specs
#'
#' @param input A Shiny input-like object.
#' @param output A Shiny output object.
#' @param ptr_obj A `ptr_obj`.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A named list of deferred UI controls.
#' @noRd
ptr_bind_placeholder_ui <- function(input,
                                       output,
                                       ptr_obj,
                                       envir = parent.frame(),
                                       ui_text = NULL) {
  context <- ptr_define_placeholder_context(
    ptr_obj,
    ui_text = ui_text,
    envir = envir
  )
  deferred_ui <- list()

  for (keyword in names(ptr_obj$placeholders)) {
    spec <- ptr_obj$placeholders[[keyword]]
    metas <- ptr_flatten_placeholder_map(ptr_obj, keyword = keyword)

    if (length(metas) == 0 || is.null(spec$bind_ui)) {
      next
    }

    bound_ui <- spec$bind_ui(input, output, metas, context)
    if (is.null(bound_ui)) {
      next
    }

    deferred_ui <- utils::modifyList(deferred_ui, bound_ui, keep.null = TRUE)
  }

  deferred_ui
}

#' Return Placeholder Copy Defaults
#'
#' @param spec A `ptr_define_placeholder`.
#'
#' @return A named list of copy defaults.
#' @noRd
ptr_define_placeholder_copy_defaults <- function(spec) {
  defaults <- spec$copy_defaults
  if (is.null(defaults)) {
    defaults <- list()
  }

  defaults
}

# TODO(future): lift the inline-only restriction by deparsing function bodies
# at export time, so hooks defined outside ptr_define_placeholder() can also be
# serialized into standalone apps.

#' Validate That a Placeholder Definition Can Be Exported
#'
#' @param placeholder A `ptr_define_placeholder`.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_exportable_placeholder <- function(placeholder) {
  if (is.null(placeholder$definition_call) ||
      !rlang::is_call(placeholder$definition_call, "ptr_define_placeholder")) {
    rlang::abort("Custom placeholders must be created with ptr_define_placeholder() to be exportable.")
  }

  inline_args <- c("build_ui", "resolve_expr", "resolve_input", "bind_ui", "prepare_eval_env")
  call_args <- as.list(placeholder$definition_call)[-1]

  for (arg_name in inline_args) {
    arg_expr <- call_args[[arg_name]]
    if (is.null(arg_expr) || identical(arg_expr, quote(NULL))) {
      next
    }

    if (!rlang::is_call(arg_expr, "function")) {
      rlang::abort(paste0("Custom placeholder '", placeholder$keyword, "' must define ", arg_name, " inline so exported apps stay standalone."))
    }

    ptr_check_free_variables(arg_expr, placeholder$keyword, arg_name)
  }

  invisible(TRUE)
}

#' Recursively Collect Free Symbol Names from an Expression
#'
#' Skips symbols accessed via `::`, `:::`, or `$` (namespace-qualified or
#' member-access symbols are not free variables).
#'
#' @param expr An R expression.
#'
#' @return A character vector of unique symbol names.
#' @noRd
ptr_collect_symbols <- function(expr) {
  if (is.symbol(expr)) return(as.character(expr))
  if (!is.call(expr)) return(character(0))

  fn <- expr[[1]]
  fn_name <- if (is.symbol(fn)) as.character(fn) else ""

  # Skip both sides of :: and ::: (namespace-qualified calls are safe).
  # Skip RHS of $ (member access is not a free variable).
  if (fn_name %in% c("::", ":::") && length(expr) == 3) {
    return(character(0))
  }
  if (fn_name == "$" && length(expr) == 3) {
    return(ptr_collect_symbols(expr[[2]]))
  }

  unique(unlist(lapply(as.list(expr), ptr_collect_symbols)))
}

#' Warn About Free Variables in an Inline Hook Function
#'
#' @param fn_expr A function call expression.
#' @param keyword The placeholder keyword.
#' @param hook_name The hook name.
#'
#' @return Invisibly returns `NULL`.
#' @noRd
ptr_check_free_variables <- function(fn_expr, keyword, hook_name) {
  fn_formals <- fn_expr[[2]]
  fn_body <- fn_expr[[3]]

  formal_names <- names(fn_formals)
  all_symbols <- ptr_collect_symbols(fn_body)

  # Known safe: formals, base R, ggpaintr namespace
  safe_names <- unique(c(
    formal_names,
    ls(baseenv()),
    ls(asNamespace("ggpaintr")),
    # R syntax not captured by baseenv()
    "if", "else", "for", "while", "repeat", "function", "return",
    "next", "break", "{", "(", "<-", "<<-", "=", "~",
    "!", "&", "|", "&&", "||", "+", "-", "*", "/", "^",
    "%%", "%/%", "%in%", "%>%", "|>",
    "<", ">", "<=", ">=", "==", "!=",
    "$", "[", "[[", ":", "::", ":::",
    "NULL", "TRUE", "FALSE", "NA", "NA_character_", "NA_real_", "NA_integer_",
    "T", "F"
  ))

  free_vars <- setdiff(all_symbols, safe_names)
  if (length(free_vars) > 0) {
    rlang::warn(paste0(
      "Custom placeholder '", keyword, "': hook '", hook_name,
      "' references names not in its formals or common packages: ",
      paste(free_vars, collapse = ", "),
      ". These may not be available in the exported app."
    ))
  }

  invisible(NULL)
}

#' Serialize Custom Placeholder Definitions for Export
#'
#' @param placeholders A placeholder registry or custom placeholder list.
#'
#' @return A named list of exportable placeholder definition calls.
#' @noRd
ptr_exportable_custom_placeholders <- function(placeholders = NULL) {
  registry <- ptr_merge_placeholders(placeholders)
  custom_placeholders <- registry$custom_placeholders

  if (length(custom_placeholders) == 0) {
    return(list())
  }

  for (keyword in names(custom_placeholders)) {
    ptr_validate_exportable_placeholder(custom_placeholders[[keyword]])
  }

  exportable <- lapply(custom_placeholders, `[[`, "definition_call")
  names(exportable) <- names(custom_placeholders)
  exportable
}

#' Build the Built-In Placeholder Registry
#'
#' @return A named list of built-in placeholders.
#' @noRd
ptr_builtin_placeholders <- function() {
  list(
    var = ptr_define_placeholder(
      keyword = "var",
      build_ui = ptr_build_var_placeholder_ui,
      resolve_expr = ptr_resolve_var_expr,
      bind_ui = ptr_bind_var_ui_impl,
      copy_defaults = list(
        label = "Choose a column for {param}",
        empty_text = "Choose one column"
      )
    ),
    text = ptr_define_placeholder(
      keyword = "text",
      build_ui = ptr_build_text_placeholder_ui,
      resolve_expr = ptr_resolve_text_expr,
      copy_defaults = list(label = "Enter text for {param}")
    ),
    num = ptr_define_placeholder(
      keyword = "num",
      build_ui = ptr_build_num_placeholder_ui,
      resolve_expr = ptr_resolve_num_expr,
      copy_defaults = list(label = "Enter a number for {param}")
    ),
    expr = ptr_define_placeholder(
      keyword = "expr",
      build_ui = ptr_build_expr_placeholder_ui,
      resolve_expr = ptr_resolve_expr_expr,
      copy_defaults = list(label = "Enter an expression for {param}")
    ),
    upload = ptr_define_placeholder(
      keyword = "upload",
      build_ui = ptr_build_upload_placeholder_ui,
      resolve_expr = ptr_resolve_upload_expr,
      resolve_input = ptr_resolve_upload_input,
      prepare_eval_env = ptr_prepare_upload_eval_env_impl,
      copy_defaults = list(label = "Choose a data source for {param}")
    )
  )
}

#' Build the Deferred UI Placeholder for `var`
#'
#' @param id Placeholder id.
#' @param copy Resolved copy for the control.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A placeholder `uiOutput()`.
#' @noRd
ptr_build_var_placeholder_ui <- function(id, copy, meta, context) {
  generate_ui_var_placeholder(id)
}

#' Resolve a `var` Placeholder to a Column Symbol
#'
#' @param value A selected column name.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A symbol naming the selected column, or
#'   `ptr_missing_expr()` when no selection was supplied.
#' @noRd
ptr_resolve_var_expr <- function(value, meta, context) {
  if (is.null(value)) {
    return(ptr_missing_expr())
  }

  selected_column <- ptr_validate_var_input(value, meta, context)
  rlang::sym(selected_column)
}

#' Build the Default Text Placeholder UI
#'
#' @param id Placeholder id.
#' @param copy Resolved copy for the control.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A text input UI control.
#' @noRd
ptr_build_text_placeholder_ui <- function(id, copy, meta, context) {
  ptr_attach_help(
    shiny::textInput(id, copy$label, placeholder = copy$placeholder),
    copy$help
  )
}

#' Resolve a `text` Placeholder to a Scalar Character Expression
#'
#' @param value A raw text input value.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A scalar character expression, or `ptr_missing_expr()`.
#' @noRd
ptr_resolve_text_expr <- function(value, meta, context) {
  if (is.null(value) || identical(value, "")) {
    return(ptr_missing_expr())
  }

  assertthat::assert_that(is.character(value))
  rlang::expr(!!value)
}

#' Build the Default Numeric Placeholder UI
#'
#' @param id Placeholder id.
#' @param copy Resolved copy for the control.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A numeric input UI control.
#' @noRd
ptr_build_num_placeholder_ui <- function(id, copy, meta, context) {
  ptr_attach_help(
    shiny::numericInput(id, copy$label, NA),
    copy$help
  )
}

#' Resolve a `num` Placeholder to a Numeric Expression
#'
#' @param value A raw numeric input value.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A numeric expression, or `ptr_missing_expr()`.
#' @noRd
ptr_resolve_num_expr <- function(value, meta, context) {
  if (is.na(value) || is.null(value)) {
    return(ptr_missing_expr())
  }

  assertthat::assert_that(is.numeric(value))
  rlang::expr(!!value)
}

#' Build the Default Expression Placeholder UI
#'
#' @param id Placeholder id.
#' @param copy Resolved copy for the control.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A text input UI control.
#' @noRd
ptr_build_expr_placeholder_ui <- function(id, copy, meta, context) {
  ptr_attach_help(
    shiny::textInput(id, copy$label, placeholder = copy$placeholder),
    copy$help
  )
}

#' Parse a Raw `expr` Placeholder Input
#'
#' @param value A raw expression string.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A parsed expression, or `ptr_missing_expr()`.
#' @noRd
ptr_resolve_expr_expr <- function(value, meta, context) {
  if (is.null(value) || identical(value, "")) {
    return(ptr_missing_expr())
  }

  rlang::parse_expr(value)
}

#' Build the Default Upload Placeholder UI
#'
#' @param id Placeholder id.
#' @param copy Resolved copy for the control.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A Shiny UI object.
#' @noRd
ptr_build_upload_placeholder_ui <- function(id, copy, meta, context) {
  generate_ui_upload(id, ui_text = context$ui_text)
}

#' Resolve Raw Upload Input to an Object Name
#'
#' @param input A Shiny input-like object.
#' @param id Placeholder id.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A single object-name string or `""` when no upload is available.
#' @noRd
ptr_resolve_upload_input <- function(input, id, meta, context) {
  upload_info <- ptr_resolve_upload_info(input, id, strict = FALSE)
  if (is.null(upload_info)) {
    return("")
  }

  upload_info$object_name
}

#' Resolve an Upload Placeholder to an Expression
#'
#' @param value A raw uploaded-object name.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A parsed symbol expression, or `ptr_missing_expr()`.
#' @noRd
ptr_resolve_upload_expr <- function(value, meta, context) {
  if (is.null(value) || identical(value, "")) {
    return(ptr_missing_expr())
  }

  rlang::parse_expr(value)
}

#' Inject Uploaded Data Objects into an Evaluation Environment
#'
#' @param input A Shiny input-like object.
#' @param metas Placeholder metadata records for `upload`.
#' @param eval_env An evaluation environment.
#' @param context A placeholder context list.
#'
#' @return The updated evaluation environment.
#' @noRd
ptr_prepare_upload_eval_env_impl <- function(input, metas, eval_env, context) {
  for (meta in metas) {
    upload_info <- ptr_resolve_upload_info(input, meta$id, strict = FALSE)
    if (is.null(upload_info)) {
      next
    }

    assign(upload_info$object_name, upload_info$data, envir = eval_env)
  }

  eval_env
}

#' Resolve the Dataset Object Used by One Layer
#'
#' @param ptr_obj A `ptr_obj`.
#' @param layer_name A parsed layer name.
#' @param input A Shiny input-like object.
#' @param context A placeholder context list.
#' @param eval_env An evaluation environment.
#'
#' @return A list with `has_data` and `data`.
#' @noRd
ptr_resolve_layer_data <- function(ptr_obj,
                                      layer_name,
                                      input,
                                      context,
                                      eval_env) {
  params <- ptr_obj$param_list[[layer_name]]
  index_paths <- ptr_obj$index_path_list[[layer_name]]
  data_index <- which(vapply(seq_along(params), function(j) {
    ptr_param_matches_data(params[[j]], index_paths[[j]])
  }, logical(1)))

  if (length(data_index) > 0) {
    data_index <- unname(data_index[[1]])
    data_id <- ptr_obj$id_list[[layer_name]][[data_index]]
    data_keyword <- ptr_obj$keywords_list[[layer_name]][[data_index]]
    data_keyword_string <- rlang::as_string(data_keyword)

    if (data_keyword_string %in% names(ptr_obj$placeholders)) {
      data_meta <- ptr_obj$placeholder_map[[layer_name]][[data_id]]
      spec <- ptr_obj$placeholders[[data_meta$keyword]]
      value <- ptr_resolve_placeholder_input(spec, input, data_meta, context)
      resolved_expr <- ptr_resolve_placeholder_expr(spec, value, data_meta, context)

      if (identical(resolved_expr, ptr_missing_expr_symbol())) {
        return(list(has_data = TRUE, data = NULL))
      }

      data_obj <- tryCatch(
        eval(resolved_expr, envir = eval_env),
        error = function(e) NULL
      )
      return(list(has_data = TRUE, data = data_obj))
    }

    data_obj <- tryCatch(
      eval(data_keyword, envir = eval_env),
      error = function(e) NULL
    )
    return(list(has_data = TRUE, data = data_obj))
  }

  # Fallback: check the raw expression for a positional first argument
  # (e.g., ggplot(mtcars, aes(...)) where mtcars is unnamed and not a placeholder)
  layer_expr <- ptr_obj$expr_list[[layer_name]]
  if (is.call(layer_expr) && length(layer_expr) >= 2) {
    first_arg <- layer_expr[[2]]
    first_arg_name <- names(layer_expr)[2]
    if (is.symbol(first_arg) && (is.null(first_arg_name) || identical(first_arg_name, "") || identical(first_arg_name, "data"))) {
      data_obj <- tryCatch(
        eval(first_arg, envir = eval_env),
        error = function(e) NULL
      )
      if (is.data.frame(data_obj)) {
        return(list(has_data = TRUE, data = data_obj))
      }
    }
  }

  list(has_data = FALSE, data = NULL)
}

#' Detect Whether a Parsed Parameter Refers to `data`
#'
#' @param param A parsed parameter value.
#' @param index_path An optional index path for positional-arg detection.
#'
#' @return A single logical value.
#' @noRd
ptr_param_matches_data <- function(param, index_path = NULL) {
  if (identical(param, "data") || identical(as.character(param)[1], "data")) {
    return(TRUE)
  }

  # Positional first argument (index_path == 2) is conventionally `data` in

  # ggplot() and geom_*/stat_* layers.
  if ((is.null(param) || identical(param, "") || identical(param, NA_character_)) &&
      !is.null(index_path) && length(index_path) == 1 && index_path[1] == 2) {
    return(TRUE)
  }

  FALSE
}

#' Build the Available-Column Map for All `var` Placeholders
#'
#' @param ptr_obj A `ptr_obj`.
#' @param input A Shiny input-like object.
#' @param context A placeholder context list.
#' @param eval_env An evaluation environment.
#'
#' @return A named list keyed by layer name with `has_data` and `columns`.
#' @noRd
ptr_build_var_column_map <- function(ptr_obj, input, context, eval_env) {
  var_metas <- ptr_flatten_placeholder_map(ptr_obj, keyword = "var")
  if (length(var_metas) == 0) {
    return(list())
  }

  layer_names <- unique(vapply(var_metas, `[[`, character(1), "layer_name"))
  global_data_info <- ptr_resolve_layer_data(
    ptr_obj,
    "ggplot",
    input,
    context,
    eval_env
  )

  column_map <- lapply(layer_names, function(layer_name) {
    layer_data_info <- ptr_resolve_layer_data(
      ptr_obj,
      layer_name,
      input,
      context,
      eval_env
    )

    has_data <- isTRUE(layer_data_info$has_data) || isTRUE(global_data_info$has_data)
    layer_data <- if (isTRUE(layer_data_info$has_data)) {
      layer_data_info$data
    } else {
      global_data_info$data
    }

    list(
      has_data = has_data,
      columns = if (is.null(layer_data)) NULL else names(layer_data)
    )
  })

  rlang::set_names(column_map, layer_names)
}

#' Validate One `var` Runtime Input
#'
#' @param value A selected column name.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return The validated column name string.
#' @noRd
ptr_validate_var_input <- function(value, meta, context) {
  if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(value[[1]])) {
    rlang::abort(paste0("Input '", meta$id, "' must select exactly one column name."))
  }

  column_info <- context$var_column_map[[meta$layer_name]]
  if (is.null(column_info)) {
    rlang::abort(paste0("Input '", meta$id, "' cannot be resolved because no dataset information is available for layer '", meta$layer_name, "'."))
  }

  if (!isTRUE(column_info$has_data) || is.null(column_info$columns)) {
    rlang::abort(paste0("Input '", meta$id, "' cannot be resolved because data columns are not available for layer '", meta$layer_name, "'."))
  }

  selected_column <- value[[1]]
  if (!(selected_column %in% column_info$columns)) {
    rlang::abort(paste0("Input '", meta$id, "' must match one available column name for layer '", meta$layer_name, "'."))
  }

  selected_column
}

#' Bind Deferred `var` Controls for the Current Runtime Context
#'
#' @param input A Shiny input-like object.
#' @param output A Shiny output object.
#' @param metas Placeholder metadata records for `var`.
#' @param context A placeholder context list.
#'
#' @return A named list of generated `var` UI controls.
#' @noRd
ptr_bind_var_ui_impl <- function(input, output, metas, context) {
  ptr_obj <- context$ptr_obj
  eval_env <- ptr_prepare_eval_env(
    ptr_obj,
    input,
    envir = context$envir
  )
  context$input <- input
  context$eval_env <- eval_env
  context$var_column_map <- ptr_build_var_column_map(
    ptr_obj,
    input,
    context,
    eval_env
  )
  deferred_ui <- list()

  metas_by_layer <- split(metas, vapply(metas, `[[`, character(1), "layer_name"))

  for (layer_name in names(metas_by_layer)) {
    column_info <- context$var_column_map[[layer_name]]
    if (is.null(column_info)) {
      column_info <- list(has_data = FALSE, columns = NULL)
    }

    if (!isTRUE(column_info$has_data)) {
      rlang::abort(paste0("Variable inputs cannot be rendered because data columns are not available for layer '", layer_name, "'."))
    }

    for (meta in metas_by_layer[[layer_name]]) {
      ui <- generate_ui_var(
        column_info$columns,
        meta$id,
        meta$param,
        layer_name = meta$layer_name,
        ui_text = context$ui_text
      )

      if (!is.null(ui)) {
        deferred_ui[[meta$id]] <- ui
      }

      local({
        captured_ui <- ui
        output[[ptr_var_output_id(meta$id)]] <- shiny::renderUI({
          captured_ui
        })
      })
    }
  }

  deferred_ui
}
