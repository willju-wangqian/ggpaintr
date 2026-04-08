#' Construct a Custom ggpaintr Placeholder
#'
#' Build one placeholder specification for use with
#' `ggpaintr_effective_placeholders()`. Custom placeholders can define their own
#' UI control, runtime expression replacement, deferred UI binding, and
#' evaluation-environment preparation.
#'
#' To make a custom placeholder exportable through `generate_shiny()`, define
#' any hook functions inline inside the `ggpaintr_placeholder()` call so the
#' exported app can serialize the full placeholder definition.
#'
#' @param keyword A single syntactic placeholder name used inside the formula.
#' @param build_ui Function with signature `(id, copy, meta, context)` returning
#'   a Shiny UI control or placeholder.
#' @param resolve_expr Function with signature `(value, meta, context)`
#'   returning an R expression or `ggpaintr_missing_expr()`.
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
#' @return An object of class `ggpaintr_placeholder`.
#' @examples
#' date_placeholder <- ggpaintr_placeholder(
#'   keyword = "date",
#'   build_ui = function(id, copy, meta, context) {
#'     shiny::dateInput(id, copy$label)
#'   },
#'   resolve_expr = function(value, meta, context) {
#'     if (is.null(value) || identical(value, "")) {
#'       return(ggpaintr_missing_expr())
#'     }
#'
#'     rlang::expr(as.Date(!!value))
#'   },
#'   copy_defaults = list(label = "Choose a date for {param}")
#' )
#' date_placeholder$keyword
#' @export
ggpaintr_placeholder <- function(keyword,
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
    class = c("ggpaintr_placeholder", "list")
  )

  ggpaintr_validate_placeholder(placeholder)
  placeholder
}

#' Build the Effective Placeholder Registry for ggpaintr
#'
#' Combine the built-in placeholder definitions with optional custom
#' placeholders. Custom placeholders override built-in placeholders on keyword
#' collision.
#'
#' @param placeholders Either `NULL`, a named list of
#'   `ggpaintr_placeholder` objects, or an existing
#'   `ggpaintr_placeholder_registry`.
#'
#' @return An object of class `ggpaintr_placeholder_registry`.
#' @examples
#' registry <- ggpaintr_effective_placeholders()
#' all(c("var", "text", "num", "expr", "upload") %in% names(registry))
#'
#' date_placeholder <- ggpaintr_placeholder(
#'   keyword = "date",
#'   build_ui = function(id, copy, meta, context) {
#'     shiny::dateInput(id, copy$label)
#'   },
#'   resolve_expr = function(value, meta, context) {
#'     if (is.null(value) || identical(value, "")) {
#'       return(ggpaintr_missing_expr())
#'     }
#'
#'     rlang::expr(as.Date(!!value))
#'   }
#' )
#' custom_registry <- ggpaintr_effective_placeholders(list(date = date_placeholder))
#' "date" %in% names(custom_registry)
#' @export
ggpaintr_effective_placeholders <- function(placeholders = NULL) {
  if (inherits(placeholders, "ggpaintr_placeholder_registry")) {
    return(placeholders)
  }

  custom_placeholders <- ggpaintr_normalize_placeholders(placeholders)
  builtin_placeholders <- paintr_builtin_placeholders()
  registry_entries <- builtin_placeholders

  for (keyword in names(custom_placeholders)) {
    registry_entries[[keyword]] <- custom_placeholders[[keyword]]
  }

  structure(
    registry_entries,
    class = c("ggpaintr_placeholder_registry", "list"),
    custom_placeholders = custom_placeholders
  )
}

#' Return the Sentinel for Removing a Placeholder Argument
#'
#' Placeholder resolvers should return `ggpaintr_missing_expr()` when the target
#' argument should be removed from the completed expression.
#'
#' @return An object of class `ggpaintr_missing_expr`.
#' @examples
#' inherits(ggpaintr_missing_expr(), "ggpaintr_missing_expr")
#' @export
ggpaintr_missing_expr <- function() {
  structure(list(), class = "ggpaintr_missing_expr")
}

#' Extract One Placeholder Registry Entry with `$`
#'
#' @param x A `ggpaintr_placeholder_registry`.
#' @param name An entry name.
#'
#' @return A registry entry or the stored custom placeholder list.
#' @noRd
`$.ggpaintr_placeholder_registry` <- function(x, name) {
  if (identical(name, "custom_placeholders")) {
    return(attr(x, "custom_placeholders"))
  }

  .subset2(unclass(x), name)
}

#' Extract One Placeholder Registry Entry with `[[`
#'
#' @param x A `ggpaintr_placeholder_registry`.
#' @param i An index or name.
#' @param ... Unused.
#'
#' @return A registry entry or the stored custom placeholder list.
#' @noRd
`[[.ggpaintr_placeholder_registry` <- function(x, i, ...) {
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
ggpaintr_validate_placeholder <- function(placeholder) {
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

  if (!inherits(placeholder, "ggpaintr_placeholder")) {
    stop("placeholders must inherit from 'ggpaintr_placeholder'.", call. = FALSE)
  }

  missing_names <- setdiff(required_names, names(placeholder))
  if (length(missing_names) > 0) {
    stop(
      "placeholder is missing required entries: ",
      paste(missing_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.character(placeholder$keyword) || length(placeholder$keyword) != 1) {
    stop("placeholder$keyword must be a single string.", call. = FALSE)
  }

  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", placeholder$keyword)) {
    stop(
      "placeholder$keyword must be a syntactic placeholder name.",
      call. = FALSE
    )
  }

  if (!is.function(placeholder$build_ui)) {
    stop("placeholder$build_ui must be a function.", call. = FALSE)
  }

  if (!is.function(placeholder$resolve_expr)) {
    stop("placeholder$resolve_expr must be a function.", call. = FALSE)
  }

  optional_function_names <- c("resolve_input", "bind_ui", "prepare_eval_env")
  for (name in optional_function_names) {
    value <- placeholder[[name]]
    if (!is.null(value) && !is.function(value)) {
      stop("placeholder$", name, " must be NULL or a function.", call. = FALSE)
    }
  }

  if (is.null(placeholder$copy_defaults)) {
    placeholder$copy_defaults <- list()
  }

  if (!is.list(placeholder$copy_defaults) || is.null(names(placeholder$copy_defaults))) {
    stop("placeholder$copy_defaults must be a named list.", call. = FALSE)
  }

  unknown_copy_fields <- setdiff(names(placeholder$copy_defaults), paintr_copy_leaf_fields())
  if (length(unknown_copy_fields) > 0) {
    stop(
      "placeholder$copy_defaults has unsupported fields: ",
      paste(sort(unknown_copy_fields), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  for (field_name in names(placeholder$copy_defaults)) {
    value <- placeholder$copy_defaults[[field_name]]
    if (!is.character(value) || length(value) != 1) {
      stop(
        "placeholder$copy_defaults$", field_name, " must be a single string.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

#' Normalize User-Supplied Placeholder Definitions
#'
#' @param placeholders Placeholder definitions or `NULL`.
#'
#' @return A named list of `ggpaintr_placeholder` objects.
#' @noRd
ggpaintr_normalize_placeholders <- function(placeholders = NULL) {
  if (is.null(placeholders)) {
    return(list())
  }

  if (!is.list(placeholders)) {
    stop("placeholders must be NULL or a named list.", call. = FALSE)
  }

  normalized <- list()
  seen_keywords <- character()

  for (i in seq_along(placeholders)) {
    placeholder <- placeholders[[i]]
    ggpaintr_validate_placeholder(placeholder)

    keyword <- placeholder$keyword
    supplied_name <- names(placeholders)[i]
    if (!is.null(supplied_name) &&
        !identical(supplied_name, "") &&
        !identical(supplied_name, keyword)) {
      stop(
        "placeholders name '", supplied_name,
        "' does not match placeholder keyword '", keyword, "'.",
        call. = FALSE
      )
    }

    if (keyword %in% seen_keywords) {
      stop(
        "placeholders contains duplicated keywords: ",
        keyword,
        ".",
        call. = FALSE
      )
    }

    normalized[[keyword]] <- placeholder
    seen_keywords <- c(seen_keywords, keyword)
  }

  normalized
}

#' Return the Flattened Placeholder Metadata for a Parsed Formula
#'
#' @param paintr_obj A `paintr_obj`.
#' @param keyword Optional placeholder keyword filter.
#'
#' @return A list of metadata records.
#' @noRd
paintr_flatten_placeholder_map <- function(paintr_obj, keyword = NULL) {
  placeholder_map <- paintr_obj[["placeholder_map"]]
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
#' @param paintr_obj A `paintr_obj`.
#' @param copy_rules Optional copy rules.
#' @param envir Environment used to resolve local objects.
#'
#' @return A named list.
#' @noRd
paintr_placeholder_context <- function(paintr_obj,
                                       copy_rules = NULL,
                                       envir = parent.frame()) {
  list(
    paintr_obj = paintr_obj,
    placeholders = paintr_obj$placeholders,
    copy_rules = paintr_effective_copy_rules(
      copy_rules,
      placeholders = paintr_obj$placeholders
    ),
    envir = envir
  )
}

#' Return the Internal Missing-Expression Symbol
#'
#' @return A symbol.
#' @noRd
paintr_missing_expr_symbol <- function() {
  rlang::sym("_NULL_PLACEHOLDER")
}

#' Detect the Public Missing-Expression Sentinel
#'
#' @param x An object.
#'
#' @return A single logical value.
#' @noRd
ggpaintr_is_missing_expr <- function(x) {
  inherits(x, "ggpaintr_missing_expr")
}

#' Resolve One Placeholder Input Value
#'
#' @param spec A `ggpaintr_placeholder`.
#' @param input A Shiny input-like object.
#' @param meta A metadata record.
#' @param context A placeholder context list.
#'
#' @return A raw input value.
#' @noRd
paintr_resolve_placeholder_input <- function(spec, input, meta, context) {
  if (is.null(spec$resolve_input)) {
    return(input[[meta$id]])
  }

  spec$resolve_input(input, meta$id, meta, context)
}

#' Resolve One Placeholder Expression Replacement
#'
#' @param spec A `ggpaintr_placeholder`.
#' @param value A raw value.
#' @param meta A metadata record.
#' @param context A placeholder context list.
#'
#' @return An R object suitable for `expr_pluck<-`.
#' @noRd
paintr_resolve_placeholder_expr <- function(spec, value, meta, context) {
  resolved_expr <- spec$resolve_expr(value, meta, context)

  if (ggpaintr_is_missing_expr(resolved_expr)) {
    return(paintr_missing_expr_symbol())
  }

  if (is.function(resolved_expr)) {
    stop(
      "Placeholder '", meta$keyword, "' returned a function instead of an expression.",
      call. = FALSE
    )
  }

  resolved_expr
}

#' Register Deferred UI for Placeholder Specs
#'
#' @param input A Shiny input-like object.
#' @param output A Shiny output object.
#' @param paintr_obj A `paintr_obj`.
#' @param envir Environment used to resolve local data objects.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A named list of deferred UI controls.
#' @noRd
paintr_bind_placeholder_ui <- function(input,
                                       output,
                                       paintr_obj,
                                       envir = parent.frame(),
                                       copy_rules = NULL) {
  context <- paintr_placeholder_context(
    paintr_obj,
    copy_rules = copy_rules,
    envir = envir
  )
  deferred_ui <- list()

  for (keyword in names(paintr_obj$placeholders)) {
    spec <- paintr_obj$placeholders[[keyword]]
    metas <- paintr_flatten_placeholder_map(paintr_obj, keyword = keyword)

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
#' @param spec A `ggpaintr_placeholder`.
#'
#' @return A named list of copy defaults.
#' @noRd
paintr_placeholder_copy_defaults <- function(spec) {
  defaults <- spec$copy_defaults
  if (is.null(defaults)) {
    defaults <- list()
  }

  defaults
}

#' Validate That a Placeholder Definition Can Be Exported
#'
#' @param placeholder A `ggpaintr_placeholder`.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
paintr_validate_exportable_placeholder <- function(placeholder) {
  if (is.null(placeholder$definition_call) ||
      !rlang::is_call(placeholder$definition_call, "ggpaintr_placeholder")) {
    stop(
      "Custom placeholders must be created with ggpaintr_placeholder() to be exportable.",
      call. = FALSE
    )
  }

  inline_args <- c("build_ui", "resolve_expr", "resolve_input", "bind_ui", "prepare_eval_env")
  call_args <- as.list(placeholder$definition_call)[-1]

  for (arg_name in inline_args) {
    arg_expr <- call_args[[arg_name]]
    if (is.null(arg_expr) || identical(arg_expr, quote(NULL))) {
      next
    }

    if (!rlang::is_call(arg_expr, "function")) {
      stop(
        "Custom placeholder '", placeholder$keyword,
        "' must define ", arg_name,
        " inline so exported apps stay standalone.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

#' Serialize Custom Placeholder Definitions for Export
#'
#' @param placeholders A placeholder registry or custom placeholder list.
#'
#' @return A named list of exportable placeholder definition calls.
#' @noRd
paintr_exportable_custom_placeholders <- function(placeholders = NULL) {
  registry <- ggpaintr_effective_placeholders(placeholders)
  custom_placeholders <- registry$custom_placeholders

  if (length(custom_placeholders) == 0) {
    return(list())
  }

  for (keyword in names(custom_placeholders)) {
    paintr_validate_exportable_placeholder(custom_placeholders[[keyword]])
  }

  exportable <- lapply(custom_placeholders, `[[`, "definition_call")
  names(exportable) <- names(custom_placeholders)
  exportable
}

#' Build the Built-In Placeholder Registry
#'
#' @return A named list of built-in placeholders.
#' @noRd
paintr_builtin_placeholders <- function() {
  list(
    var = ggpaintr_placeholder(
      keyword = "var",
      build_ui = paintr_build_var_placeholder_ui,
      resolve_expr = paintr_resolve_var_expr,
      bind_ui = paintr_bind_var_ui_impl,
      copy_defaults = list(
        label = "Choose a column for {param}",
        empty_text = "Choose one column"
      )
    ),
    text = ggpaintr_placeholder(
      keyword = "text",
      build_ui = paintr_build_text_placeholder_ui,
      resolve_expr = paintr_resolve_text_expr,
      copy_defaults = list(label = "Enter text for {param}")
    ),
    num = ggpaintr_placeholder(
      keyword = "num",
      build_ui = paintr_build_num_placeholder_ui,
      resolve_expr = paintr_resolve_num_expr,
      copy_defaults = list(label = "Enter a number for {param}")
    ),
    expr = ggpaintr_placeholder(
      keyword = "expr",
      build_ui = paintr_build_expr_placeholder_ui,
      resolve_expr = paintr_resolve_expr_expr,
      copy_defaults = list(label = "Enter an expression for {param}")
    ),
    upload = ggpaintr_placeholder(
      keyword = "upload",
      build_ui = paintr_build_upload_placeholder_ui,
      resolve_expr = paintr_resolve_upload_expr,
      resolve_input = paintr_resolve_upload_input,
      prepare_eval_env = paintr_prepare_upload_eval_env_impl,
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
paintr_build_var_placeholder_ui <- function(id, copy, meta, context) {
  generate_ui_var_placeholder(id)
}

#' Resolve a `var` Placeholder to a Column Symbol
#'
#' @param value A selected column name.
#' @param meta Placeholder metadata.
#' @param context A placeholder context list.
#'
#' @return A symbol naming the selected column, or
#'   `ggpaintr_missing_expr()` when no selection was supplied.
#' @noRd
paintr_resolve_var_expr <- function(value, meta, context) {
  if (is.null(value)) {
    return(ggpaintr_missing_expr())
  }

  selected_column <- paintr_validate_var_input(value, meta, context)
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
paintr_build_text_placeholder_ui <- function(id, copy, meta, context) {
  paintr_attach_help(
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
#' @return A scalar character expression, or `ggpaintr_missing_expr()`.
#' @noRd
paintr_resolve_text_expr <- function(value, meta, context) {
  if (is.null(value) || identical(value, "")) {
    return(ggpaintr_missing_expr())
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
paintr_build_num_placeholder_ui <- function(id, copy, meta, context) {
  paintr_attach_help(
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
#' @return A numeric expression, or `ggpaintr_missing_expr()`.
#' @noRd
paintr_resolve_num_expr <- function(value, meta, context) {
  if (is.na(value) || is.null(value)) {
    return(ggpaintr_missing_expr())
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
paintr_build_expr_placeholder_ui <- function(id, copy, meta, context) {
  paintr_attach_help(
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
#' @return A parsed expression, or `ggpaintr_missing_expr()`.
#' @noRd
paintr_resolve_expr_expr <- function(value, meta, context) {
  if (is.null(value) || identical(value, "")) {
    return(ggpaintr_missing_expr())
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
paintr_build_upload_placeholder_ui <- function(id, copy, meta, context) {
  generate_ui_upload(id, copy_rules = context$copy_rules)
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
paintr_resolve_upload_input <- function(input, id, meta, context) {
  upload_info <- paintr_resolve_upload_info(input, id, strict = FALSE)
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
#' @return A parsed symbol expression, or `ggpaintr_missing_expr()`.
#' @noRd
paintr_resolve_upload_expr <- function(value, meta, context) {
  if (is.null(value) || identical(value, "")) {
    return(ggpaintr_missing_expr())
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
paintr_prepare_upload_eval_env_impl <- function(input, metas, eval_env, context) {
  for (meta in metas) {
    upload_info <- paintr_resolve_upload_info(input, meta$id, strict = FALSE)
    if (is.null(upload_info)) {
      next
    }

    assign(upload_info$object_name, upload_info$data, envir = eval_env)
  }

  eval_env
}

#' Resolve the Dataset Object Used by One Layer
#'
#' @param paintr_obj A `paintr_obj`.
#' @param layer_name A parsed layer name.
#' @param input A Shiny input-like object.
#' @param context A placeholder context list.
#' @param eval_env An evaluation environment.
#'
#' @return A list with `has_data` and `data`.
#' @noRd
paintr_resolve_layer_data <- function(paintr_obj,
                                      layer_name,
                                      input,
                                      context,
                                      eval_env) {
  params <- paintr_obj$param_list[[layer_name]]
  data_index <- which(vapply(params, paintr_param_matches_data, logical(1)))

  if (length(data_index) == 0) {
    return(list(has_data = FALSE, data = NULL))
  }

  data_index <- unname(data_index[[1]])
  data_id <- paintr_obj$id_list[[layer_name]][[data_index]]
  data_keyword <- paintr_obj$keywords_list[[layer_name]][[data_index]]
  data_keyword_string <- rlang::as_string(data_keyword)

  if (data_keyword_string %in% names(paintr_obj$placeholders)) {
    data_meta <- paintr_obj$placeholder_map[[layer_name]][[data_id]]
    spec <- paintr_obj$placeholders[[data_meta$keyword]]
    value <- paintr_resolve_placeholder_input(spec, input, data_meta, context)
    resolved_expr <- paintr_resolve_placeholder_expr(spec, value, data_meta, context)

    if (identical(resolved_expr, paintr_missing_expr_symbol())) {
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

  list(has_data = TRUE, data = data_obj)
}

#' Detect Whether a Parsed Parameter Refers to `data`
#'
#' @param param A parsed parameter value.
#'
#' @return A single logical value.
#' @noRd
paintr_param_matches_data <- function(param) {
  identical(param, "data") || identical(as.character(param)[1], "data")
}

#' Build the Available-Column Map for All `var` Placeholders
#'
#' @param paintr_obj A `paintr_obj`.
#' @param input A Shiny input-like object.
#' @param context A placeholder context list.
#' @param eval_env An evaluation environment.
#'
#' @return A named list keyed by layer name with `has_data` and `columns`.
#' @noRd
paintr_build_var_column_map <- function(paintr_obj, input, context, eval_env) {
  var_metas <- paintr_flatten_placeholder_map(paintr_obj, keyword = "var")
  if (length(var_metas) == 0) {
    return(list())
  }

  layer_names <- unique(vapply(var_metas, `[[`, character(1), "layer_name"))
  global_data_info <- paintr_resolve_layer_data(
    paintr_obj,
    "ggplot",
    input,
    context,
    eval_env
  )

  column_map <- lapply(layer_names, function(layer_name) {
    layer_data_info <- paintr_resolve_layer_data(
      paintr_obj,
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
paintr_validate_var_input <- function(value, meta, context) {
  if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(value[[1]])) {
    stop(
      paste0("Input '", meta$id, "' must select exactly one column name."),
      call. = FALSE
    )
  }

  column_info <- context$var_column_map[[meta$layer_name]]
  if (is.null(column_info)) {
    stop(
      paste0(
        "Input '",
        meta$id,
        "' cannot be resolved because no dataset information is available for layer '",
        meta$layer_name,
        "'."
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(column_info$has_data) || is.null(column_info$columns)) {
    stop(
      paste0(
        "Input '",
        meta$id,
        "' cannot be resolved because data columns are not available for layer '",
        meta$layer_name,
        "'."
      ),
      call. = FALSE
    )
  }

  selected_column <- value[[1]]
  if (!(selected_column %in% column_info$columns)) {
    stop(
      paste0(
        "Input '",
        meta$id,
        "' must match one available column name for layer '",
        meta$layer_name,
        "'."
      ),
      call. = FALSE
    )
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
paintr_bind_var_ui_impl <- function(input, output, metas, context) {
  paintr_obj <- context$paintr_obj
  eval_env <- paintr_prepare_eval_env(
    paintr_obj,
    input,
    envir = context$envir
  )
  context$input <- input
  context$eval_env <- eval_env
  context$var_column_map <- paintr_build_var_column_map(
    paintr_obj,
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
      stop(
        paste0(
          "Variable inputs cannot be rendered because data columns are not available for layer '",
          layer_name,
          "'."
        ),
        call. = FALSE
      )
    }

    for (meta in metas_by_layer[[layer_name]]) {
      ui <- generate_ui_var(
        column_info$columns,
        meta$id,
        meta$param,
        layer_name = meta$layer_name,
        copy_rules = context$copy_rules
      )

      if (!is.null(ui)) {
        deferred_ui[[meta$id]] <- ui
      }

      output[[paste0("var-", meta$id)]] <- shiny::renderUI({
        ui
      })
    }
  }

  deferred_ui
}
