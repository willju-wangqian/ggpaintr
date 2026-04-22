#' Build Standard Output Ids for ggpaintr Integration
#'
#' Create a validated set of top-level output and control ids for embedding the
#' `ggpaintr` runtime inside a larger Shiny app.
#'
#' @param control_panel Output id used for the generated control panel.
#' @param draw_button Input id used for the draw button.
#' @param plot_output Output id used for the plot output.
#' @param error_output Output id used for the inline error UI.
#' @param code_output Output id used for the generated code output.
#'
#' @return An object of class `ptr_build_ids`.
#' @examples
#' ids <- ptr_build_ids(
#'   control_panel = "custom_controls",
#'   draw_button = "run_plot"
#' )
#' ids$control_panel
#' @export
ptr_build_ids <- function(control_panel = "controlPanel",
                         draw_button = "draw",
                         plot_output = "outputPlot",
                         error_output = "outputError",
                         code_output = "outputCode") {
  ids <- list(
    control_panel = control_panel,
    draw_button = draw_button,
    plot_output = plot_output,
    error_output = error_output,
    code_output = code_output
  )

  ptr_validate_ids(ids)
  structure(ids, class = c("ptr_build_ids", "list"))
}

#' Validate a ggpaintr Id Registry
#'
#' @param ids A named list of ids.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_ids <- function(ids) {
  required_names <- c(
    "control_panel",
    "draw_button",
    "plot_output",
    "error_output",
    "code_output"
  )

  if (!is.list(ids) || is.null(names(ids))) {
    rlang::abort("ids must be a named list.")
  }

  missing_names <- setdiff(required_names, names(ids))
  if (length(missing_names) > 0) {
    rlang::abort(paste0("ids is missing required entries: ", paste(missing_names, collapse = ", "), "."))
  }

  extra_names <- setdiff(names(ids), required_names)
  if (length(extra_names) > 0) {
    rlang::abort(paste0("ids has unsupported entries: ", paste(extra_names, collapse = ", "), "."))
  }

  for (name in required_names) {
    value <- ids[[name]]
    if (!is.character(value) || length(value) != 1 || identical(trimws(value), "")) {
      rlang::abort(paste0("ids$", name, " must be a single non-empty string."))
    }
  }

  duplicate_ids <- unique(unname(ids)[duplicated(unname(ids))])
  if (length(duplicate_ids) > 0) {
    rlang::abort(paste0("ids must be unique. Duplicated values: ", paste(duplicate_ids, collapse = ", "), "."))
  }

  invisible(TRUE)
}

#' Normalize a ggpaintr Id Registry
#'
#' @param ids A `ptr_build_ids` object or named list.
#'
#' @return A validated `ptr_build_ids` object.
#' @noRd
ptr_normalize_ids <- function(ids = NULL) {
  if (is.null(ids)) {
    return(ptr_build_ids())
  }

  if (inherits(ids, "ptr_build_ids")) {
    return(ids)
  }

  ptr_validate_ids(ids)
  structure(ids, class = c("ptr_build_ids", "list"))
}

#' Validate a ggpaintr State Object
#'
#' @param ptr_state A `ptr_state` object.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_state <- function(ptr_state) {
  required_names <- c(
    "obj",
    "runtime",
    "extras",
    "var_ui_list",
    "shared_env_reactive",
    "raw_ui_text",
    "effective_ui_text",
    "placeholders",
    "custom_placeholders",
    "ids",
    "envir"
  )

  if (!inherits(ptr_state, "ptr_state")) {
    rlang::abort("ptr_state must inherit from 'ptr_state'.")
  }

  missing_names <- setdiff(required_names, names(ptr_state))
  if (length(missing_names) > 0) {
    rlang::abort(paste0("ptr_state is missing required entries: ", paste(missing_names, collapse = ", "), "."))
  }

  if (!is.function(ptr_state$obj) ||
      !is.function(ptr_state$runtime) ||
      !is.function(ptr_state$extras) ||
      !is.function(ptr_state$var_ui_list)) {
    rlang::abort("ptr_state reactive accessors must be functions.")
  }

  ptr_validate_ids(ptr_state$ids)
  if (!inherits(ptr_state$placeholders, "ptr_define_placeholder_registry")) {
    rlang::abort("ptr_state$placeholders must inherit from 'ptr_define_placeholder_registry'.")
  }
  invisible(TRUE)
}

#' Build Reactive Server State for ggpaintr
#'
#' Create the shared reactive state used by the extensible `ptr_*` server
#' helpers. This object can be passed to the bind helpers or inspected directly
#' inside a larger Shiny app.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param expr_check Controls `expr` placeholder validation.
#'   `TRUE` (default) applies the built-in denylist of dangerous
#'   functions.
#'   `FALSE` disables all checking.
#'   A named list with `deny_list` and/or `allow_list` character
#'   vectors supplies a custom check; when both are given,
#'   denied entries are removed from the allowlist.
#'
#' @param ns An optional namespace function (`character -> character`) used to
#'   prefix all Shiny ids produced by this state instance. Pass
#'   `shiny::NS("page1")` or `session$ns` to avoid id collisions when embedding
#'   two or more ggpaintr formulas in the same Shiny session. The same `ns`
#'   value must be passed to `ptr_input_ui()` and `ptr_output_ui()`. Defaults
#'   to `shiny::NS(NULL)` (identity — no prefixing).
#' @return An object of class `ptr_state`.
#' @examples
#' state <- ptr_server_state(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' is.function(state$runtime)
#' @export
ptr_server_state <- function(formula,
                             envir = parent.frame(),
                             ui_text = NULL,
                             ids = ptr_build_ids(),
                             placeholders = NULL,
                             expr_check = TRUE,
                             ns = shiny::NS(NULL)) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  ids <- ptr_normalize_ids(ids)
  placeholder_registry <- ptr_merge_placeholders(placeholders)

  parsed <- ptr_parse_formula(formula, placeholders = placeholder_registry)
  parsed <- ptr_ns_obj(parsed, ns)

  # Prefix top-level ids through ns
  namespaced_ids <- structure(
    lapply(ids, ns),
    class = class(ids)
  )

  structure(
    list(
      obj = shiny::reactiveVal(parsed),
      runtime = shiny::reactiveVal(NULL),
      extras = shiny::reactiveVal(list()),
      var_ui_list = shiny::reactiveVal(list()),
      shared_env_reactive = NULL,
      raw_ui_text = ui_text,
      effective_ui_text = ptr_merge_ui_text(
        ui_text,
        placeholders = placeholder_registry,
        known_param_keys = ptr_known_param_keys_from_obj(parsed)
      ),
      placeholders = placeholder_registry,
      custom_placeholders = placeholder_registry$custom_placeholders,
      ids = namespaced_ids,
      envir = envir,
      expr_check = expr_check,
      ns_fn = ns
    ),
    class = c("ptr_state", "list")
  )
}

#' Bind the Generated Control Panel into a Shiny App
#'
#' Register the dynamic `var` controls and render the standard tabbed control
#' panel into the target `uiOutput()`.
#'
#' @param input A Shiny `input` object.
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_setup_controls(input, output, ps)
#'   ptr_register_draw(input, ps)
#'   ptr_register_plot(output, ps)
#' }
#' }
#' @export
ptr_setup_controls <- function(input,
                               output,
                               ptr_state,
                               ids = ptr_state$ids) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ids)

  shared_env_reactive_raw <- shiny::reactive({
    shiny::req(ptr_state$obj())
    obj <- ptr_state$obj()
    eval_env <- tryCatch(
      ptr_prepare_eval_env(obj, input, envir = ptr_state$envir),
      error = function(e) NULL
    )
    var_column_map <- if (!is.null(eval_env)) {
      context <- ptr_define_placeholder_context(
        obj, ui_text = NULL, envir = ptr_state$envir,
        expr_check = ptr_state$expr_check
      )
      context$input <- input
      context$eval_env <- eval_env
      tryCatch(
        ptr_build_var_column_map(obj, input, context, eval_env),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    list(eval_env = eval_env, var_column_map = var_column_map)
  })
  shared_env_reactive <- shiny::debounce(shared_env_reactive_raw, millis = 300)
  ptr_state$shared_env_reactive <- shared_env_reactive

  shiny::observe({
    shiny::req(ptr_state$obj())
    cached <- shared_env_reactive()
    result <- tryCatch(
      ptr_bind_placeholder_ui(
        input,
        output,
        ptr_state$obj(),
        envir = ptr_state$envir,
        ui_text = ptr_state$effective_ui_text,
        eval_env = cached$eval_env,
        var_column_map = cached$var_column_map,
        expr_check = ptr_state$expr_check,
        ns_fn = ptr_state$ns_fn %||% shiny::NS(NULL)
      ),
      error = function(e) list()
    )
    ptr_state$var_ui_list(result)
  })

  output[[ids$control_panel]] <- shiny::renderUI({
    shiny::req(ptr_state$obj())
    shiny::column(
      12,
      ptr_get_tab_ui(
        ptr_state$obj(),
        ui_text = ptr_state$effective_ui_text,
        ns_fn = ptr_state$ns_fn %||% shiny::NS(NULL)
      )
    )
  })

  invisible(ptr_state)
}

#' Bind Draw Behavior into a Shiny App
#'
#' @param input A Shiny `input` object.
#' @param ptr_state A `ptr_state` object.
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_setup_controls(input, output, ps)
#'   ptr_register_draw(input, ps)
#'   ptr_register_plot(output, ps)
#' }
#' }
#' @export
ptr_register_draw <- function(input,
                               ptr_state,
                               ids = ptr_state$ids) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ids)

  shiny::observeEvent(input[[ids$draw_button]], {
    shiny::req(ptr_state$obj())
    cached <- tryCatch(ptr_state$shared_env_reactive(), error = function(e) NULL)
    runtime_result <- ptr_complete_expr_safe(
      ptr_state$obj(),
      input,
      envir = ptr_state$envir,
      eval_env = cached$eval_env,
      var_column_map = cached$var_column_map,
      expr_check = ptr_state$expr_check,
      ns_fn = ptr_state$ns_fn %||% shiny::NS(NULL)
    )
    runtime_result <- ptr_assemble_plot_safe(runtime_result, envir = ptr_state$envir, expr_check = ptr_state$expr_check)
    runtime_result <- ptr_validate_plot_render_safe(runtime_result)
    ptr_state$runtime(runtime_result)
  })

  invisible(ptr_state)
}

#' Return the Built Plot from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ptr_exec()`.
#'
#' @return A `ggplot` object or `NULL`.
#' @examples
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
#' runtime <- ptr_exec(
#'   obj,
#'   inputs
#' )
#' inherits(ptr_extract_plot(runtime), "ggplot")
#' @export
ptr_extract_plot <- function(runtime_result) {
  if (is.null(runtime_result) || !isTRUE(runtime_result[["ok"]])) {
    return(NULL)
  }

  runtime_result[["plot"]]
}

#' Return Default Error UI from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ptr_exec()`.
#'
#' @return A Shiny tag or `NULL`.
#' @examples
#' \dontrun{
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
#' runtime <- ptr_exec(obj, inputs)
#' ptr_extract_error(runtime)
#' }
#' @export
ptr_extract_error <- function(runtime_result) {
  if (is.null(runtime_result) || isTRUE(runtime_result[["ok"]])) {
    return(NULL)
  }

  ptr_error_ui(runtime_result[["message"]])
}

#' Return Generated Code Text from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ptr_exec()`.
#' @param extras Optional list of quosures captured by [ptr_gg_extra()],
#'   appended to the runtime code text when the runtime succeeded. Extras
#'   are suppressed when `runtime_result$ok` is not `TRUE`, so stale extras
#'   from a prior successful draw never surface during a failed draw.
#'
#' @return A character string or `NULL`.
#' @examples
#' \dontrun{
#' obj <- ptr_parse_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ptr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
#' runtime <- ptr_exec(obj, inputs)
#' ptr_extract_code(runtime)
#' }
#' @export
ptr_extract_code <- function(runtime_result, extras = NULL) {
  if (is.null(runtime_result)) {
    return(NULL)
  }

  base <- runtime_result[["code_text"]]
  if (!isTRUE(runtime_result[["ok"]])) {
    return(base)
  }
  if (is.null(extras) || length(extras) == 0) {
    return(base)
  }

  extra_text <- vapply(extras, rlang::quo_text, character(1))
  if (is.null(base) || !nzchar(base)) {
    return(paste(extra_text, collapse = " +\n  "))
  }
  paste(c(base, extra_text), collapse = " +\n  ")
}

#' Bind Default Plot Rendering into a Shiny App
#'
#' Bind the same default `renderPlot()` behavior used by `ptr_server()`.
#' Advanced users who want to transform the plot should instead write their own
#' `renderPlot()` and call `ptr_extract_plot()`.
#'
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_register_draw(input, ps)
#'   ptr_register_plot(output, ps)
#' }
#' }
#' @export
ptr_register_plot <- function(output,
                               ptr_state,
                               ids = ptr_state$ids) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ids)

  output[[ids$plot_output]] <- shiny::renderPlot({
    plot_obj <- ptr_extract_plot(ptr_state$runtime())
    if (is.null(plot_obj)) {
      graphics::plot.new()
      return(invisible(NULL))
    }

    plot_obj
  })

  invisible(ptr_state)
}

#' Bind Default Error Rendering into a Shiny App
#'
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_register_draw(input, ps)
#'   ptr_register_error(output, ps)
#' }
#' }
#' @export
ptr_register_error <- function(output,
                                ptr_state,
                                ids = ptr_state$ids) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ids)

  output[[ids$error_output]] <- shiny::renderUI({
    ptr_extract_error(ptr_state$runtime())
  })

  invisible(ptr_state)
}

#' Bind Default Code Rendering into a Shiny App
#'
#' @param output A Shiny `output` object.
#' @param ptr_state A `ptr_state` object.
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ptr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ptr_register_draw(input, ps)
#'   ptr_register_code(output, ps)
#' }
#' }
#' @export
ptr_register_code <- function(output,
                               ptr_state,
                               ids = ptr_state$ids) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ids)

  output[[ids$code_output]] <- shiny::renderText({
    ptr_extract_code(ptr_state$runtime(), extras = ptr_state$extras())
  })

  invisible(ptr_state)
}

#' Capture Out-of-Runtime ggplot Additions for the Code Output
#'
#' Advanced helper for custom `renderPlot({...})` blocks inside embedded
#' ggpaintr apps. When an advanced developer adds ggplot components (themes,
#' scales, coords, guides, labs, ...) to the plot returned by
#' [ptr_extract_plot()], those additions render visually but never reach the
#' generated code output. `ptr_gg_extra()` solves that by capturing the
#' expressions it receives and storing them on the `ptr_state` so the default
#' code binder ([ptr_register_code()]) can surface them alongside the
#' formula-driven code.
#'
#' The helper is intentionally **not** exposed through [ptr_app()] or
#' [ptr_app_bslib()]. It is only meaningful when the caller owns their own
#' `renderPlot({...})` block built on [ptr_server_state()] and the
#' `ptr_register_*()` helpers.
#'
#' Semantics:
#'
#' * Replace-per-call. Each call overwrites the previously captured extras.
#'   Pass every component you want in a single call, e.g.
#'   `ptr_gg_extra(ps, theme_minimal(), scale_x_log10())`.
#' * Suppression on failure. When the underlying runtime reports
#'   `ok = FALSE`, the code binder ignores extras so stale values from a
#'   prior successful draw never surface during a failed draw.
#' * No new S3 class. The return value is a plain list, so
#'   `plot_obj + ptr_gg_extra(ps, ...)` dispatches through ggplot2's built-in
#'   `ggplot_add.list` method.
#'
#' @param ptr_state A `ptr_state` object created by [ptr_server_state()].
#' @param ... ggplot components (theme, scale, coord, guides, labs, ...).
#'   Captured with [rlang::enquos()] for code output, evaluated for the
#'   returned list.
#'
#' @return A list of evaluated ggplot components, suitable for
#'   `plot_obj + ptr_gg_extra(ptr_state, ...)`.
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ptr_server_state(
#'     "ggplot(data = iris, aes(x = var, y = var)) + geom_point() + labs(title = text)"
#'   )
#'   ptr_setup_controls(input, output, ps)
#'   ptr_register_draw(input, ps)
#'   ptr_register_error(output, ps)
#'   ptr_register_code(output, ps)
#'
#'   output$outputPlot <- shiny::renderPlot({
#'     plot_obj <- ptr_extract_plot(ps$runtime())
#'     if (is.null(plot_obj)) {
#'       graphics::plot.new()
#'       return(invisible(NULL))
#'     }
#'     plot_obj + ptr_gg_extra(ps, ggplot2::theme_minimal(base_size = 16))
#'   })
#' }
#' }
#' @export
ptr_gg_extra <- function(ptr_state, ...) {
  if (missing(ptr_state)) {
    rlang::abort("ptr_gg_extra() requires `ptr_state` as the first argument.")
  }
  if (!is.list(ptr_state) || !is.function(ptr_state$extras)) {
    rlang::abort(
      "`ptr_state` does not expose an `extras` reactiveVal. Rebuild it with ptr_server_state() from the current ggpaintr version."
    )
  }

  quos <- rlang::enquos(..., .named = FALSE)
  vals <- rlang::list2(...)

  ptr_state$extras(quos)

  vals
}

#' Build Default ggpaintr Control Widgets
#'
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param ns An optional namespace function (`character -> character`). Must
#'   match the `ns` passed to `ptr_server_state()`. Defaults to
#'   `shiny::NS(NULL)` (no prefixing).
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ptr_input_ui(
#'   ptr_build_ids(draw_button = "render_plot"),
#'   ui_text = list(shell = list(draw_button = list(label = "Render plot")))
#' )
#' inherits(ui, "shiny.tag.list")
#' @export
ptr_input_ui <- function(ids = ptr_build_ids(), ui_text = NULL,
                         ns = shiny::NS(NULL)) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  ids <- ptr_normalize_ids(ids)
  shell_copy <- ptr_resolve_shell_ui_text(ui_text)

  shiny::tagList(
    shiny::uiOutput(ns(ids$control_panel)),
    shiny::actionButton(ns(ids$draw_button), shell_copy$draw_copy$label)
  )
}

#' Build Default ggpaintr Output Widgets
#'
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param ns An optional namespace function (`character -> character`). Must
#'   match the `ns` passed to `ptr_server_state()`. Defaults to
#'   `shiny::NS(NULL)` (no prefixing).
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ptr_output_ui(ptr_build_ids(plot_output = "main_plot"))
#' inherits(ui, "shiny.tag.list")
#' @export
ptr_output_ui <- function(ids = ptr_build_ids(), ns = shiny::NS(NULL)) {
  if (!is.function(ns)) {
    rlang::abort("`ns` must be a namespace function (e.g. shiny::NS(\"id\") or session$ns).")
  }
  ids <- ptr_normalize_ids(ids)

  shiny::tagList(
    shiny::plotOutput(ns(ids$plot_output)),
    shiny::uiOutput(ns(ids$error_output)),
    shiny::verbatimTextOutput(ns(ids$code_output))
  )
}

#' Build a ggpaintr Shiny App
#'
#' Create a Shiny app from a single ggplot-like formula string. The app exposes
#' generated controls, a draw button, inline error handling, and code output.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param expr_check Controls `expr` placeholder validation.
#'   `TRUE` (default) applies the built-in denylist of dangerous
#'   functions.
#'   `FALSE` disables all checking.
#'   A named list with `deny_list` and/or `allow_list` character
#'   vectors supplies a custom check; when both are given,
#'   denied entries are removed from the allowlist.
#'
#' @param ns An optional namespace function (`character -> character`). See
#'   [ptr_server_state()] for details. For standalone apps created with
#'   `ptr_app()`, namespacing is rarely needed; it is most useful when
#'   embedding ggpaintr inside a larger Shiny module.
#' @return A `shiny.appobj`.
#' @examples
#' app <- ptr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
#' inherits(app, "shiny.appobj")
#' @export
ptr_app <- function(formula,
                         envir = parent.frame(),
                         ui_text = NULL,
                         placeholders = NULL,
                         expr_check = TRUE,
                         ns = shiny::NS(NULL)) {
  app_parts <- ptr_app_components(
    formula,
    envir = envir,
    ui_text = ui_text,
    placeholders = placeholders,
    expr_check = expr_check,
    ns = ns
  )
  shiny::shinyApp(ui = app_parts$ui, server = app_parts$server)
}

#' Register ggpaintr Server Logic
#'
#' Wire the standard `ggpaintr` server behavior into an existing Shiny app.
#' The returned state object exposes reactive access to the parsed formula,
#' latest runtime result, and current dynamic `var` UI definitions so callers
#' can extend the app with additional observers and outputs.
#'
#' @param input A Shiny `input` object.
#' @param output A Shiny `output` object.
#' @param session A Shiny `session` object.
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param ids A `ptr_build_ids` object controlling the Shiny element IDs used by
#'   the integration helpers. Defaults to `ptr_build_ids()`.
#' @param expr_check Controls `expr` placeholder validation.
#'   `TRUE` (default) applies the built-in denylist of dangerous
#'   functions.
#'   `FALSE` disables all checking.
#'   A named list with `deny_list` and/or `allow_list` character
#'   vectors supplies a custom check; when both are given,
#'   denied entries are removed from the allowlist.
#' @param ns An optional namespace function (`character -> character`). See
#'   [ptr_server_state()] for details.
#'
#' @return A `ptr_state` object containing reactive accessors named `obj`,
#'   `runtime`, and `var_ui_list`, plus shared metadata used by the bind helpers.
#' @note When embedding inside a `shiny::moduleServer()` call, pass
#'   `session$ns` as the `ns` argument so all generated Shiny ids are scoped
#'   to the module namespace.
#' @export
ptr_server <- function(input,
                            output,
                            session,
                            formula,
                            envir = parent.frame(),
                            ui_text = NULL,
                            placeholders = NULL,
                            ids = ptr_build_ids(),
                            expr_check = TRUE,
                            ns = shiny::NS(NULL)) {
  ptr_state <- ptr_server_state(
    formula,
    envir = envir,
    ui_text = ui_text,
    placeholders = placeholders,
    ids = ids,
    expr_check = expr_check,
    ns = ns
  )

  ptr_state <- ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_plot(output, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)

  ptr_state
}

#' Resolve Standard Shell Copy for ggpaintr
#'
#' @param ui_text Optional named list of copy overrides.
#'
#' @return A named list with shell copy entries.
#' @noRd
ptr_resolve_shell_ui_text <- function(ui_text = NULL) {
  list(
    title_copy = ptr_resolve_ui_text("title", ui_text = ui_text),
    draw_copy = ptr_resolve_ui_text("draw_button", ui_text = ui_text)
  )
}

#' Build the Standard ggpaintr App UI
#'
#' @param title_label App title text.
#' @param draw_label Draw button text.
#' @param ns An optional namespace function (`character -> character`).
#'
#' @return A Shiny UI object.
#' @noRd
ptr_build_app_ui <- function(title_label, draw_label,
                             ids = ptr_build_ids(),
                             ns = shiny::NS(NULL)) {
  shiny::fluidPage(
    shiny::titlePanel(title_label),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ns(ids$control_panel)),
        shiny::actionButton(ns(ids$draw_button), draw_label)
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns(ids$plot_output)),
        shiny::uiOutput(ns(ids$error_output)),
        shiny::verbatimTextOutput(ns(ids$code_output))
      )
    )
  )
}

#' Build Reusable App Components for ggpaintr
#'
#' @param formula A single formula string.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Optional named list of copy overrides.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param ns An optional namespace function (`character -> character`).
#'
#' @return A list with `ui` and `server`.
#' @noRd
ptr_app_components <- function(formula,
                                  envir = parent.frame(),
                                  ui_text = NULL,
                                  placeholders = NULL,
                                  expr_check = TRUE,
                                  ns = shiny::NS(NULL)) {
  shell_copy <- ptr_resolve_shell_ui_text(ui_text)

  ui <- ptr_build_app_ui(
    shell_copy$title_copy$label,
    shell_copy$draw_copy$label,
    ns = ns
  )

  server <- function(input, output, session) {
    ptr_server(
      input,
      output,
      session,
      formula,
      envir = envir,
      ui_text = ui_text,
      placeholders = placeholders,
      expr_check = expr_check,
      ns = ns
    )
    invisible(NULL)
  }

  list(ui = ui, server = server)
}
