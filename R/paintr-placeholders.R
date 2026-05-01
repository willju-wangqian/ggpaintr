
#' Construct a Custom ggpaintr Placeholder
#'
#' Build one placeholder specification for use with
#' `ptr_merge_placeholders()`. Custom placeholders can define their own
#' UI control, runtime expression replacement, deferred UI binding, and
#' evaluation-environment preparation.
#'
#' # Data-independent placeholders (the common case)
#'
#' If the widget choices do not depend on the active dataset (date pickers,
#' sliders, color pickers, free-text variants), supply `build_ui` and
#' `resolve_expr` and you are done. The `id` passed to `build_ui` is already
#' namespaced by ggpaintr; use it directly.
#'
#' # Data-aware placeholders (`bind_ui`)
#'
#' If the widget needs to read columns or values from the dataset (a
#' multi-column selector inside `dplyr::select()`, a faceting variable picker,
#' etc.), the dataset is not available at registration time -- it depends on
#' the user's formula and on input values. Use `bind_ui` to register a Shiny
#' `renderUI()` for the widget.
#'
#' The recommended pattern is the empty-container shape used by the built-in
#' `var` placeholder: `build_ui` returns a placeholder `shiny::uiOutput()` and
#' `bind_ui` fills it in. See *Examples*.
#'
#' ## `bind_ui(input, output, metas, context)` contract
#'
#' * `metas` is a list of meta objects (one per occurrence of `keyword` in the
#'   formula). Each meta has `$id` (raw, *not* yet namespaced), `$keyword`,
#'   `$param` (the ggplot argument name), and `$layer_name`
#'   (e.g. `"ggplot"`, `"geom_point"`).
#' * `context` carries:
#'   * `$ptr_obj` -- the parsed formula tree
#'     (use [ptr_resolve_layer_data()] to read layer data).
#'   * `$eval_env` -- the evaluation environment used for the plot.
#'   * `$envir` -- the user's calling environment.
#'   * `$ns_fn`, `$ui_ns_fn` -- input/output namespace functions
#'     (use [ptr_ns_id()] to namespace `meta$id`).
#'   * `$var_column_map` -- cached column lists for `var` placeholders.
#'   * `$ui_text` -- merged ui text for labels/help.
#' * Return value: either `NULL` (treated as a no-op; render via
#'   `output[[...]]` side effects) or a named list keyed by `meta$id`. Items
#'   in that list are merged into ggpaintr's deferred-UI map and rendered in
#'   the layer panel. Returning a list is preferred for widgets you want
#'   ggpaintr to lay out next to other controls.
#'
#' ## Namespacing rule
#'
#' `meta$id` is the *raw* placeholder id (e.g. `"ggplot_3_2"`). Under
#' [ptr_app()] the default namespace functions are `shiny::NS(NULL)` so the
#' raw id round-trips unchanged, but when ggpaintr is embedded inside a
#' Shiny module those functions wrap a real namespace. Always namespace
#' `meta$id` via [ptr_ns_id()] before assigning into `output` or referencing
#' `input`.
#'
#' @param keyword A single syntactic placeholder name used inside the formula.
#' @param build_ui Function with signature `(id, copy, meta, context)` returning
#'   a Shiny UI control or placeholder. `id` is already namespaced.
#' @param resolve_expr Function with signature `(value, meta, context)`
#'   returning an R expression or `ptr_missing_expr()`.
#' @param resolve_input Optional function with signature
#'   `(input, id, meta, context)` returning the raw value to hand to
#'   `resolve_expr()`. Defaults to `input[[id]]`.
#' @param bind_ui Optional function with signature `(input, output, metas,
#'   context)` for registering deferred UI. See *Data-aware placeholders*
#'   above for the contract. The built-in `var` placeholder uses this hook.
#' @param prepare_eval_env Optional function with signature
#'   `(input, metas, eval_env, context)` returning an updated evaluation
#'   environment.
#' @param copy_defaults Optional named list with `label`, `help`,
#'   `placeholder`, and `empty_text`. Defaults to
#'   `list(label = "Enter a value for {param}")`.
#'
#' @return An object of class `ptr_define_placeholder`.
#' @seealso [ptr_resolve_layer_data()], [ptr_ns_id()],
#'   [ptr_merge_placeholders()].
#' @examples
#' # ---- Data-independent placeholder: a date picker -------------------------
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
#'
#' # ---- Data-aware placeholder: a numeric-columns-only picker --------------
#' # build_ui returns an empty uiOutput; bind_ui fills it in once the layer
#' # data has resolved. Use ptr_resolve_layer_data() to fetch the data frame
#' # and ptr_ns_id() to namespace the input/output ids.
#' numvar_placeholder <- ptr_define_placeholder(
#'   keyword = "numvar",
#'   build_ui = function(id, copy, meta, context) {
#'     shiny::uiOutput(paste0(id, "_container"))
#'   },
#'   bind_ui = function(input, output, metas, context) {
#'     for (meta in metas) {
#'       local({
#'         m <- meta
#'         layer_data <- ptr_resolve_layer_data(
#'           context$ptr_obj, m$layer_name, input, context, context$eval_env
#'         )
#'         choices <- if (isTRUE(layer_data$has_data) &&
#'                        is.data.frame(layer_data$data)) {
#'           df <- layer_data$data
#'           names(df)[vapply(df, is.numeric, logical(1))]
#'         } else {
#'           character()
#'         }
#'         input_id  <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL), m$id)
#'         output_id <- ptr_ns_id(
#'           context$ui_ns_fn %||% shiny::NS(NULL),
#'           paste0(m$id, "_container")
#'         )
#'         output[[output_id]] <- shiny::renderUI({
#'           shiny::selectInput(
#'             input_id,
#'             paste("Numeric column for", m$param),
#'             choices = choices
#'           )
#'         })
#'       })
#'     }
#'     invisible(NULL)
#'   },
#'   resolve_expr = function(value, meta, context) {
#'     if (is.null(value) || identical(value, "")) {
#'       return(ptr_missing_expr())
#'     }
#'
#'     rlang::sym(value)
#'   }
#' )
#' numvar_placeholder$keyword
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
      copy_defaults = copy_defaults
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
  if (identical(i, "custom_placeholders")) {
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
    "copy_defaults"
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
      formal_names <- names(formals(fn))
      if (length(formal_names) == 1 && identical(formal_names, "...")) {
        cli::cli_warn(paste0(
          "placeholder$", hook_name,
          " uses only `...` -- positional argument mismatches will not be caught at definition time."
        ))
      } else if (actual < expected) {
        rlang::abort(paste0(
          "placeholder$", hook_name, " must accept at least ",
          expected, " arguments, but has ", actual, "."
        ))
      }
    }
  }

  if (!is.null(placeholder$copy_defaults) &&
      (!is.list(placeholder$copy_defaults) || is.null(names(placeholder$copy_defaults)))) {
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
#' @param placeholders A list of `ptr_define_placeholder` objects (optionally
#'   named), or `NULL`. If named, names must match the placeholder keyword.
#'   If unnamed, the keyword is used as the registry key.
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
  seen_env <- new.env(hash = TRUE, parent = emptyenv())

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

    if (exists(keyword, envir = seen_env, inherits = FALSE)) {
      rlang::abort(paste0("placeholders contains duplicated keywords: ", keyword, "."))
    }

    normalized[[keyword]] <- placeholder
    assign(keyword, TRUE, envir = seen_env)
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
#' @return An environment with reference semantics.
#' @noRd
ptr_define_placeholder_context <- function(ptr_obj,
                                       ui_text = NULL,
                                       envir = parent.frame(),
                                       expr_check = TRUE,
                                       eval_env = NULL,
                                       var_column_map = NULL) {
  ctx <- new.env(parent = emptyenv())
  ctx$ptr_obj <- ptr_obj
  ctx$placeholders <- ptr_obj$placeholders
  ctx$ui_text <- ptr_merge_ui_text(
    ui_text,
    placeholders = ptr_obj$placeholders,
    known_param_keys = ptr_known_param_keys_from_obj(ptr_obj)
  )
  ctx$envir <- envir
  ctx$expr_check <- expr_check
  ctx$eval_env <- eval_env
  ctx$var_column_map <- var_column_map
  ctx
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
  shared_name <- meta$shared
  if (!is.null(shared_name)) {
    binding <- context$shared_bindings[[shared_name]]
    if (!is.null(binding)) {
      return(binding())
    }
  }
  input_id <- ptr_ns_id(context$ns_fn %||% shiny::NS(NULL), meta$id)
  if (is.null(spec$resolve_input)) {
    return(input[[input_id]])
  }

  spec$resolve_input(input, input_id, meta, context)
}

#' Resolve One Placeholder Expression Replacement
#'
#' @param spec A `ptr_define_placeholder`.
#' @param value A raw value.
#' @param meta A metadata record.
#' @param context A placeholder context list.
#'
#' @return An R object suitable for `expr_pluck<-`.
#' @importFrom rlang %||%
#' @noRd
ptr_resolve_placeholder_expr <- function(spec, value, meta, context) {
  resolved_expr <- spec$resolve_expr(value, meta, context)

  if (ptr_is_missing_expr(resolved_expr)) {
    return(ptr_missing_expr_symbol())
  }

  if (is.function(resolved_expr)) {
    rlang::abort(paste0("Placeholder '", meta$keyword, "' returned a function instead of an expression."))
  }

  needs_safety_check <- is.call(resolved_expr) || is.symbol(resolved_expr) ||
    is.pairlist(resolved_expr) || is.character(resolved_expr)

  if (needs_safety_check) {
    validate_expr_safety(resolved_expr, context$expr_check %||% TRUE)
  }

  is_scalar_type <- is.numeric(resolved_expr) || is.logical(resolved_expr) ||
    is.integer(resolved_expr) || is.double(resolved_expr) ||
    is.complex(resolved_expr) || is.null(resolved_expr)

  if (!needs_safety_check && !is_scalar_type) {
    rlang::abort(paste0(
      "Placeholder '", meta$keyword, "' resolve_expr returned an unsupported type: ",
      class(resolved_expr)[[1]], ". Allowed types: call, symbol, pairlist, character, ",
      "numeric, logical, integer, double, complex, NULL."
    ))
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
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return A named list of deferred UI controls.
#' @noRd
ptr_bind_placeholder_ui <- function(input,
                                       output,
                                       ptr_obj,
                                       envir = parent.frame(),
                                       ui_text = NULL,
                                       eval_env = NULL,
                                       var_column_map = NULL,
                                       expr_check = TRUE,
                                       ns_fn = shiny::NS(NULL),
                                       ui_ns_fn = ns_fn) {
  context <- ptr_define_placeholder_context(
    ptr_obj,
    ui_text = ui_text,
    envir = envir,
    expr_check = expr_check,
    eval_env = eval_env,
    var_column_map = var_column_map
  )
  context$ns_fn <- ns_fn
  context$ui_ns_fn <- ui_ns_fn
  deferred_ui <- list()

  for (keyword in names(ptr_obj$placeholders)) {
    spec <- ptr_obj$placeholders[[keyword]]
    metas <- ptr_flatten_placeholder_map(ptr_obj, keyword = keyword)
    metas <- Filter(function(m) is.null(m$shared), metas)

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

.ptr_builtin_cache <- new.env(parent = emptyenv())

#' Build the Built-In Placeholder Registry
#'
#' @return A named list of built-in placeholders.
#' @noRd
ptr_builtin_placeholders <- function() {
  if (!is.null(.ptr_builtin_cache$registry)) {
    return(.ptr_builtin_cache$registry)
  }
  result <- list(
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
      copy_defaults = list(
        label = "Enter text for {param}",
        placeholder = "Plain text - quotes are added automatically"
      )
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
  .ptr_builtin_cache$registry <- result
  result
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
  shiny::uiOutput(
    ptr_ns_id(
      context$ui_ns_fn %||% shiny::NS(NULL),
      ptr_var_output_id(id)
    )
  )
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

  if (is.character(value) && length(value) == 1L && nchar(value) >= 2L) {
    first <- substr(value, 1L, 1L)
    last <- substr(value, nchar(value), nchar(value))
    if (identical(first, last) && first %in% c('"', "'")) {
      value <- substr(value, 2L, nchar(value) - 1L)
    }
  }

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
  if (is.null(value) || length(value) == 0L || is.na(value)) {
    return(ptr_missing_expr())
  }

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

  parsed_list <- tryCatch(
    rlang::parse_exprs(value),
    error = function(e) {
      rlang::abort(
        paste0(
          "expr placeholder: could not parse input ",
          "as R expression: ", conditionMessage(e)
        ),
        parent = e
      )
    }
  )
  if (length(parsed_list) != 1L) {
    rlang::abort(
      paste0("expr placeholder: input must contain exactly one expression, but ",
             length(parsed_list), " were found.")
    )
  }
  parsed <- parsed_list[[1]]

  if (!identical(context$expr_check, FALSE)) {
    validate_expr_safety(parsed, expr_check = context$expr_check)
  }

  parsed
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

  if (!grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", value)) {
    rlang::abort(paste0(
      "upload placeholder: invalid object name: ", value
    ))
  }
  rlang::sym(value)
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
    input_id <- ptr_ns_id(context$ns_fn %||% shiny::NS(NULL), meta$id)
    upload_info <- ptr_resolve_upload_info(input, input_id, strict = FALSE)
    if (is.null(upload_info)) {
      next
    }

    assign(upload_info$object_name, upload_info$data, envir = eval_env)
  }

  eval_env
}

#' Resolve the Dataset for a ggpaintr Layer
#'
#' Return the active data frame for a parsed layer (e.g. `"ggplot"`,
#' `"geom_point"`). Designed to be called from a custom placeholder's
#' `bind_ui()` callback when the widget needs to know columns or values
#' from the dataset that the user is currently plotting.
#'
#' Resolution order:
#' 1. Look up the layer's `data` argument by parameter name. If the layer
#'    has a `data = <placeholder>` argument (e.g. `data = upload`), evaluate
#'    that placeholder to get the data frame.
#' 2. If the `data` argument is an unbound symbol, evaluate it in `eval_env`.
#' 3. Otherwise fall back to the first positional argument when it is a bare
#'    symbol (e.g. `ggplot(mtcars, aes(...))`). This heuristic does **not**
#'    handle non-data-first call shapes such as `merge(x, y)`; in those
#'    cases supply `data = ` explicitly in the formula.
#'
#' Returns `list(has_data = FALSE, data = NULL)` if no dataset is resolvable,
#' so callers can early-return without rendering an empty widget.
#'
#' @param ptr_obj A `ptr_obj` (available as `context$ptr_obj` inside a
#'   `bind_ui()` callback).
#' @param layer_name Layer name as a string. Use `meta$layer_name` to scope
#'   to the meta currently being bound, or `"ggplot"` for the base layer.
#' @param input A Shiny `input` reactive values object (the first argument
#'   of `bind_ui()`).
#' @param context The placeholder context (the fourth argument of
#'   `bind_ui()`).
#' @param eval_env An evaluation environment, typically `context$eval_env`.
#'
#' @return A named list with components `has_data` (logical scalar) and
#'   `data` (the resolved data frame, or `NULL`).
#' @seealso [ptr_define_placeholder()], [ptr_ns_id()].
#' @export
#' @noRd
ptr_data_arg_index <- function(layer_expr) {
  if (!is.call(layer_expr) || length(layer_expr) < 2L) {
    return(NA_integer_)
  }
  arg_names <- names(layer_expr)
  if (!is.null(arg_names)) {
    named <- which(arg_names == "data")
    if (length(named) > 0L) {
      return(as.integer(named[[1]]))
    }
  }
  if (is.null(arg_names)) {
    return(2L)
  }
  unnamed <- which(is.na(arg_names) | arg_names == "")
  unnamed <- unnamed[unnamed >= 2L]
  if (length(unnamed) > 0L) {
    return(as.integer(unnamed[[1]]))
  }
  NA_integer_
}

#' @noRd
ptr_compute_data_pipeline_info <- function(expr_list, placeholder_map) {
  result <- list()
  for (layer_name in names(expr_list)) {
    layer_expr <- expr_list[[layer_name]]
    data_idx <- ptr_data_arg_index(layer_expr)
    if (is.na(data_idx)) next
    data_expr <- tryCatch(layer_expr[[data_idx]], error = function(e) NULL)
    if (!is.call(data_expr)) next

    layer_metas <- placeholder_map[[layer_name]]
    if (is.null(layer_metas) || length(layer_metas) == 0L) next

    matching_ids <- character(0)
    for (id in names(layer_metas)) {
      meta <- layer_metas[[id]]
      ip <- meta$index_path
      if (length(ip) >= 2L && identical(as.integer(ip[[1]]), as.integer(data_idx))) {
        matching_ids <- c(matching_ids, id)
      }
    }
    if (length(matching_ids) == 0L) next

    result[[layer_name]] <- list(
      data_arg_index = as.integer(data_idx),
      placeholder_ids = matching_ids
    )
  }
  result
}

#' @noRd
ptr_unset_data_marker <- function() {
  rlang::sym("__PTR_UNSET_DATA_PLACEHOLDER__")
}

#' @noRd
ptr_expr_contains_marker <- function(expr, marker) {
  if (is.symbol(expr)) {
    return(identical(rlang::as_string(expr), rlang::as_string(marker)))
  }
  if (is.call(expr) || is.pairlist(expr)) {
    for (i in seq_along(expr)) {
      if (ptr_expr_contains_marker(expr[[i]], marker)) return(TRUE)
    }
  }
  FALSE
}

#' @noRd
ptr_trim_and_eval <- function(expr, eval_env, marker) {
  if (!is.call(expr)) {
    if (is.symbol(expr) &&
        identical(rlang::as_string(expr), rlang::as_string(marker))) {
      return(list(ok = FALSE, value = NULL))
    }
    val <- tryCatch(eval(expr, envir = eval_env), error = function(e) NULL)
    if (is.null(val)) return(list(ok = FALSE, value = NULL))
    return(list(ok = TRUE, value = val))
  }

  args <- as.list(expr)
  if (length(args) >= 2L) {
    upstream <- ptr_trim_and_eval(args[[2]], eval_env, marker)
    if (!isTRUE(upstream$ok)) {
      return(list(ok = FALSE, value = NULL))
    }
    args[[2]] <- upstream$value
  }

  keep <- rep(TRUE, length(args))
  if (length(args) >= 3L) {
    for (i in 3:length(args)) {
      if (ptr_expr_contains_marker(args[[i]], marker)) keep[i] <- FALSE
    }
  }
  trimmed <- as.call(args[keep])

  result <- tryCatch(eval(trimmed, envir = eval_env), error = function(e) NULL)
  if (!is.null(result)) {
    return(list(ok = TRUE, value = result))
  }

  if (length(args) >= 2L) {
    return(list(ok = TRUE, value = args[[2]]))
  }
  list(ok = FALSE, value = NULL)
}

#' @noRd
ptr_resolve_data_pipeline_expr <- function(layer_expr,
                                           data_arg_index,
                                           placeholder_ids,
                                           ptr_obj,
                                           layer_name,
                                           input,
                                           context,
                                           eval_env) {
  marker <- ptr_unset_data_marker()
  layer_metas <- ptr_obj$placeholder_map[[layer_name]]
  substituted <- layer_expr

  for (id in placeholder_ids) {
    meta <- layer_metas[[id]]
    spec <- ptr_obj$placeholders[[meta$keyword]]

    value <- tryCatch(
      ptr_resolve_placeholder_input(spec, input, meta, context),
      error = function(e) NULL
    )

    replacement <- marker
    if (!is.null(value) &&
        !(is.character(value) && length(value) == 1L && !nzchar(value))) {
      resolved <- tryCatch(
        ptr_resolve_placeholder_expr(spec, value, meta, context),
        error = function(e) NULL
      )
      if (!is.null(resolved) &&
          !identical(resolved, ptr_missing_expr_symbol())) {
        replacement <- resolved
      }
    }
    expr_pluck(substituted, meta$index_path) <- replacement
  }

  data_expr <- substituted[[data_arg_index]]
  ptr_trim_and_eval(data_expr, eval_env, marker)
}

ptr_resolve_layer_data <- function(ptr_obj,
                                      layer_name,
                                      input,
                                      context,
                                      eval_env) {
  pipeline <- ptr_obj$data_pipeline_info[[layer_name]]
  if (!is.null(pipeline)) {
    layer_expr <- ptr_obj$expr_list[[layer_name]]
    res <- ptr_resolve_data_pipeline_expr(
      layer_expr = layer_expr,
      data_arg_index = pipeline$data_arg_index,
      placeholder_ids = pipeline$placeholder_ids,
      ptr_obj = ptr_obj,
      layer_name = layer_name,
      input = input,
      context = context,
      eval_env = eval_env
    )
    if (isTRUE(res$ok)) {
      return(list(has_data = TRUE, data = res$value))
    }
  }

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

  layer_expr <- ptr_obj$expr_list[[layer_name]]

  # Prefer the named `data = ...` argument. Handles both bare symbols and
  # call expressions (e.g., `data = mtcars |> dplyr::filter(mpg > 20)`).
  data_expr <- if (is.call(layer_expr)) layer_expr$data else NULL
  if (!is.null(data_expr)) {
    data_obj <- tryCatch(
      eval(data_expr, envir = eval_env),
      error = function(e) NULL
    )
    return(list(has_data = TRUE, data = data_obj))
  }

  # Fallback: unnamed positional first argument. Allow calls (the |> at the
  # head of a formula parses to `ggplot(filter(mtcars, ...), aes(...))`) in
  # addition to bare symbols.
  if (is.call(layer_expr) && length(layer_expr) >= 2) {
    first_arg <- layer_expr[[2]]
    first_arg_name <- names(layer_expr)[2]
    if (is.null(first_arg_name) || identical(first_arg_name, "") || identical(first_arg_name, "data")) {
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


# Snapshot the current values of the data-pipeline placeholder inputs for a
# layer. The result is used by Phase C to detect whether the cached resolved
# data is stale relative to the current input state.
ptr_snapshot_data_placeholder_inputs <- function(ptr_obj, layer_name, input, context) {
  pipeline <- ptr_obj$data_pipeline_info[[layer_name]]
  if (is.null(pipeline)) {
    return(list())
  }
  layer_metas <- ptr_obj$placeholder_map[[layer_name]]
  result <- list()
  for (id in pipeline$placeholder_ids) {
    meta <- layer_metas[[id]]
    if (is.null(meta)) next
    spec <- ptr_obj$placeholders[[meta$keyword]]
    value <- tryCatch(
      ptr_resolve_placeholder_input(spec, input, meta, context),
      error = function(e) NULL
    )
    result[[id]] <- value
  }
  result
}

#' Detect Whether a Parsed Parameter Refers to `data`
#'
#' @param param A parsed parameter value.
#' @param index_path An optional index path for positional-arg detection.
#'
#' @return A single logical value.
#' @noRd
ptr_param_matches_data <- function(param, index_path = NULL) {
  if (identical(param, "data")) {
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
ptr_build_var_column_map <- function(ptr_obj, input, context, eval_env,
                                     resolved_data = NULL) {
  var_metas <- ptr_flatten_placeholder_map(ptr_obj, keyword = "var")
  if (length(var_metas) == 0) {
    return(list())
  }

  layer_names <- unique(vapply(var_metas, `[[`, character(1), "layer_name"))

  resolve_one <- function(layer_name) {
    if (!is.null(resolved_data) && !is.null(resolved_data[[layer_name]])) {
      cached <- resolved_data[[layer_name]]()
      if (!is.null(cached)) {
        return(list(has_data = TRUE, data = cached))
      }
    }
    ptr_resolve_layer_data(ptr_obj, layer_name, input, context, eval_env)
  }

  global_data_info <- resolve_one("ggplot")

  column_map <- lapply(layer_names, function(layer_name) {
    layer_data_info <- resolve_one(layer_name)

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
  eval_env <- context$eval_env
  if (is.null(eval_env)) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      cli::cli_warn("ptr_bind_var_ui_impl: eval_env not cached; rebuilt")
    }
    eval_env <- ptr_prepare_eval_env(
      ptr_obj,
      input,
      envir = context$envir,
      ns_fn = context$ns_fn %||% shiny::NS(NULL)
    )
  }
  context$input <- input
  context$eval_env <- eval_env
  if (is.null(context$var_column_map)) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      cli::cli_warn("ptr_bind_var_ui_impl: var_column_map not cached; rebuilt")
    }
    context$var_column_map <- ptr_build_var_column_map(
      ptr_obj,
      input,
      context,
      eval_env
    )
  }
  deferred_ui <- list()

  metas_by_layer <- split(metas, vapply(metas, `[[`, character(1), "layer_name"))

  for (layer_name in names(metas_by_layer)) {
    column_info <- context$var_column_map[[layer_name]]
    if (is.null(column_info)) {
      column_info <- list(has_data = FALSE, columns = NULL)
    }

    if (!isTRUE(column_info$has_data)) {
      next
    }

    for (meta in metas_by_layer[[layer_name]]) {
      ui_id <- ptr_ns_id(context$ui_ns_fn %||% shiny::NS(NULL), meta$id)
      current_selection <- tryCatch(
        shiny::isolate(input[[ui_id]]),
        error = function(e) NULL
      )
      if (is.null(current_selection)) current_selection <- character(0)
      ui <- generate_ui_var(
        column_info$columns,
        ui_id,
        meta$param,
        layer_name = meta$layer_name,
        ui_text = context$ui_text,
        selected = current_selection
      )

      if (!is.null(ui)) {
        deferred_ui[[meta$id]] <- ui
      }

      local({
        captured_ui <- ui
        output[[ptr_ns_id(context$ns_fn %||% shiny::NS(NULL), ptr_var_output_id(meta$id))]] <- shiny::renderUI({
          captured_ui
        })
      })
    }
  }

  deferred_ui
}
