#' Build Standard Output Ids for ggpaintr Integration
#'
#' Create a validated set of top-level output and control ids for embedding the
#' `ggpaintr` runtime inside a larger Shiny app.
#'
#' @param control_panel Output id used for the generated control panel.
#' @param draw_button Input id used for the draw button.
#' @param export_button Output id used for the export download button.
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
                         export_button = "shinyExport",
                         plot_output = "outputPlot",
                         error_output = "outputError",
                         code_output = "outputCode") {
  ids <- list(
    control_panel = control_panel,
    draw_button = draw_button,
    export_button = export_button,
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
    "export_button",
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
    ptr_validate_ids(ids)
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
    "var_ui_list",
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
#'
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
                                  placeholders = NULL) {
  ids <- ptr_normalize_ids(ids)
  placeholder_registry <- ptr_merge_placeholders(placeholders)

  structure(
    list(
      obj = shiny::reactiveVal(ptr_parse_formula(formula, placeholders = placeholder_registry)),
      runtime = shiny::reactiveVal(NULL),
      var_ui_list = shiny::reactiveVal(list()),
      raw_ui_text = ui_text,
      effective_ui_text = ptr_merge_ui_text(
        ui_text,
        placeholders = placeholder_registry
      ),
      placeholders = placeholder_registry,
      custom_placeholders = placeholder_registry$custom_placeholders,
      ids = ids,
      envir = envir
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
#' @export
ptr_register_controls <- function(input,
                                        output,
                                        ptr_state,
                                        ids = ptr_state$ids) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ids)

  shiny::observe({
    shiny::req(ptr_state$obj())
    result <- tryCatch(
      register_var_ui_outputs(
        input,
        output,
        ptr_state$obj(),
        envir = ptr_state$envir,
        ui_text = ptr_state$effective_ui_text
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
        ui_text = ptr_state$effective_ui_text
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
#'   ptr_register_controls(input, output, ps)
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
    ptr_state$runtime(ptr_exec(
      ptr_state$obj(),
      input,
      envir = ptr_state$envir
    ))
  })

  invisible(ptr_state)
}

#' Bind Export Behavior into a Shiny App
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
#'   ptr_register_export(output, ps)
#' }
#' }
#' @export
ptr_register_export <- function(output,
                                 ptr_state,
                                 ids = ptr_state$ids) {
  ptr_validate_state(ptr_state)
  ids <- ptr_normalize_ids(ids)

  output[[ids$export_button]] <- shiny::downloadHandler(
    filename = "ggpaintr-app.R",
    content = function(file) {
      shiny::req(ptr_state$obj())
      ptr_generate_shiny(
        ptr_state$obj(),
        file,
        ui_text = ptr_state$raw_ui_text,
        placeholders = ptr_state$placeholders
      )
    }
  )

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
#'
#' @return A character string or `NULL`.
#' @export
ptr_extract_code <- function(runtime_result) {
  if (is.null(runtime_result)) {
    return(NULL)
  }

  runtime_result[["code_text"]]
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
    ptr_extract_code(ptr_state$runtime())
  })

  invisible(ptr_state)
}

#' Build Default ggpaintr Control Widgets
#'
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ptr_input_ui(
#'   ptr_build_ids(draw_button = "render_plot"),
#'   ui_text = list(shell = list(draw_button = list(label = "Render plot")))
#' )
#' inherits(ui, "shiny.tag.list")
#' @export
ptr_input_ui <- function(ids = ptr_build_ids(), ui_text = NULL) {
  ids <- ptr_normalize_ids(ids)
  shell_copy <- ptr_resolve_shell_ui_text(ui_text)

  shiny::tagList(
    shiny::uiOutput(ids$control_panel),
    shiny::actionButton(ids$draw_button, shell_copy$draw_copy$label),
    shiny::downloadButton(ids$export_button, shell_copy$export_copy$label)
  )
}

#' Build Default ggpaintr Output Widgets
#'
#' @param ids A `ptr_build_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ptr_output_ui(ptr_build_ids(plot_output = "main_plot"))
#' inherits(ui, "shiny.tag.list")
#' @export
ptr_output_ui <- function(ids = ptr_build_ids()) {
  ids <- ptr_normalize_ids(ids)

  shiny::tagList(
    shiny::plotOutput(ids$plot_output),
    shiny::uiOutput(ids$error_output),
    shiny::verbatimTextOutput(ids$code_output)
  )
}

#' Build a ggpaintr Shiny App
#'
#' Create a Shiny app from a single ggplot-like formula string. The app exposes
#' generated controls, a draw button, inline error handling, code output, and an
#' export button for producing a standalone app script.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#'
#' @return A `shiny.appobj`.
#' @examples
#' app <- ptr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
#' inherits(app, "shiny.appobj")
#' @export
ptr_app <- function(formula,
                         envir = parent.frame(),
                         ui_text = NULL,
                         placeholders = NULL) {
  app_parts <- ptr_app_components(
    formula,
    envir = envir,
    ui_text = ui_text,
    placeholders = placeholders
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
#'
#' @return A `ptr_state` object containing reactive accessors named `obj`,
#'   `runtime`, and `var_ui_list`, plus shared metadata used by the bind helpers.
#' @export
ptr_server <- function(input,
                            output,
                            session,
                            formula,
                            envir = parent.frame(),
                            ui_text = NULL,
                            placeholders = NULL) {
  ptr_state <- ptr_server_state(
    formula,
    envir = envir,
    ui_text = ui_text,
    placeholders = placeholders
  )

  ptr_register_controls(input, output, ptr_state)
  ptr_register_export(output, ptr_state)
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
    draw_copy = ptr_resolve_ui_text("draw_button", ui_text = ui_text),
    export_copy = ptr_resolve_ui_text("export_button", ui_text = ui_text)
  )
}

#' Build the Standard ggpaintr App UI
#'
#' @param title_label App title text.
#' @param draw_label Draw button text.
#' @param export_label Export button text.
#'
#' @return A Shiny UI object.
#' @noRd
ptr_build_app_ui <- function(title_label, draw_label, export_label,
                             ids = ptr_build_ids()) {
  shiny::fluidPage(
    shiny::titlePanel(title_label),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput(ids$control_panel),
        shiny::actionButton(ids$draw_button, draw_label),
        shiny::downloadButton(ids$export_button, export_label)
      ),
      shiny::mainPanel(
        shiny::plotOutput(ids$plot_output),
        shiny::uiOutput(ids$error_output),
        shiny::verbatimTextOutput(ids$code_output)
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
#'
#' @return A list with `ui` and `server`.
#' @noRd
ptr_app_components <- function(formula,
                                  envir = parent.frame(),
                                  ui_text = NULL,
                                  placeholders = NULL) {
  shell_copy <- ptr_resolve_shell_ui_text(ui_text)

  ui <- ptr_build_app_ui(
    shell_copy$title_copy$label,
    shell_copy$draw_copy$label,
    shell_copy$export_copy$label
  )

  server <- function(input, output, session) {
    ptr_server(
      input,
      output,
      session,
      formula,
      envir = envir,
      ui_text = ui_text,
      placeholders = placeholders
    )
    invisible(NULL)
  }

  list(ui = ui, server = server)
}
