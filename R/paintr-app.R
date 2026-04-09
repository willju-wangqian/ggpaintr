# TODO(future): add inst/shiny-demo/ interactive demo app for hands-on
# discovery by new users.

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
#' @return An object of class `ggpaintr_ids`.
#' @examples
#' ids <- ggpaintr_ids(
#'   control_panel = "custom_controls",
#'   draw_button = "run_plot"
#' )
#' ids$control_panel
#' @export
ggpaintr_ids <- function(control_panel = "controlPanel",
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

  ggpaintr_validate_ids(ids)
  structure(ids, class = c("ggpaintr_ids", "list"))
}

#' Validate a ggpaintr Id Registry
#'
#' @param ids A named list of ids.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ggpaintr_validate_ids <- function(ids) {
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
#' @param ids A `ggpaintr_ids` object or named list.
#'
#' @return A validated `ggpaintr_ids` object.
#' @noRd
ggpaintr_normalize_ids <- function(ids = NULL) {
  if (is.null(ids)) {
    return(ggpaintr_ids())
  }

  if (inherits(ids, "ggpaintr_ids")) {
    ggpaintr_validate_ids(ids)
    return(ids)
  }

  ggpaintr_validate_ids(ids)
  structure(ids, class = c("ggpaintr_ids", "list"))
}

#' Validate a ggpaintr State Object
#'
#' @param ggpaintr_state A `ggpaintr_state` object.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ggpaintr_validate_state <- function(ggpaintr_state) {
  required_names <- c(
    "obj",
    "runtime",
    "var_ui_list",
    "raw_copy_rules",
    "effective_copy_rules",
    "placeholders",
    "custom_placeholders",
    "ids",
    "envir"
  )

  if (!inherits(ggpaintr_state, "ggpaintr_state")) {
    rlang::abort("ggpaintr_state must inherit from 'ggpaintr_state'.")
  }

  missing_names <- setdiff(required_names, names(ggpaintr_state))
  if (length(missing_names) > 0) {
    rlang::abort(paste0("ggpaintr_state is missing required entries: ", paste(missing_names, collapse = ", "), "."))
  }

  if (!is.function(ggpaintr_state$obj) ||
      !is.function(ggpaintr_state$runtime) ||
      !is.function(ggpaintr_state$var_ui_list)) {
    rlang::abort("ggpaintr_state reactive accessors must be functions.")
  }

  ggpaintr_validate_ids(ggpaintr_state$ids)
  if (!inherits(ggpaintr_state$placeholders, "ggpaintr_placeholder_registry")) {
    rlang::abort("ggpaintr_state$placeholders must inherit from 'ggpaintr_placeholder_registry'.")
  }
  invisible(TRUE)
}

#' Build Reactive Server State for ggpaintr
#'
#' Create the shared reactive state used by the extensible `ggpaintr_*` server
#' helpers. This object can be passed to the bind helpers or inspected directly
#' inside a larger Shiny app.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#' @param copy_rules Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#'
#' @return An object of class `ggpaintr_state`.
#' @examples
#' state <- ggpaintr_server_state(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' is.function(state$runtime)
#' @export
ggpaintr_server_state <- function(formula,
                                  envir = parent.frame(),
                                  copy_rules = NULL,
                                  ids = ggpaintr_ids(),
                                  placeholders = NULL) {
  ids <- ggpaintr_normalize_ids(ids)
  placeholder_registry <- ggpaintr_effective_placeholders(placeholders)

  structure(
    list(
      obj = shiny::reactiveVal(ggpaintr_formula(formula, placeholders = placeholder_registry)),
      runtime = shiny::reactiveVal(NULL),
      var_ui_list = shiny::reactiveVal(list()),
      raw_copy_rules = copy_rules,
      effective_copy_rules = ggpaintr_effective_copy_rules(
        copy_rules,
        placeholders = placeholder_registry
      ),
      placeholders = placeholder_registry,
      custom_placeholders = placeholder_registry$custom_placeholders,
      ids = ids,
      envir = envir
    ),
    class = c("ggpaintr_state", "list")
  )
}

#' Bind the Generated Control Panel into a Shiny App
#'
#' Register the dynamic `var` controls and render the standard tabbed control
#' panel into the target `uiOutput()`.
#'
#' @param input A Shiny `input` object.
#' @param output A Shiny `output` object.
#' @param ggpaintr_state A `ggpaintr_state` object.
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ggpaintr_state`.
#' @export
ggpaintr_bind_control_panel <- function(input,
                                        output,
                                        ggpaintr_state,
                                        ids = ggpaintr_state$ids) {
  ggpaintr_validate_state(ggpaintr_state)
  ids <- ggpaintr_normalize_ids(ids)

  shiny::observe({
    shiny::req(ggpaintr_state$obj())
    result <- tryCatch(
      register_var_ui_outputs(
        input,
        output,
        ggpaintr_state$obj(),
        envir = ggpaintr_state$envir,
        copy_rules = ggpaintr_state$effective_copy_rules
      ),
      error = function(e) list()
    )
    ggpaintr_state$var_ui_list(result)
  })

  output[[ids$control_panel]] <- shiny::renderUI({
    shiny::req(ggpaintr_state$obj())
    shiny::column(
      12,
      ggpaintr_get_tab_ui(
        ggpaintr_state$obj(),
        copy_rules = ggpaintr_state$effective_copy_rules
      )
    )
  })

  invisible(ggpaintr_state)
}

#' Bind Draw Behavior into a Shiny App
#'
#' @param input A Shiny `input` object.
#' @param ggpaintr_state A `ggpaintr_state` object.
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ggpaintr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ggpaintr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ggpaintr_bind_control_panel(input, output, ps)
#'   ggpaintr_bind_draw(input, ps)
#'   ggpaintr_bind_plot(output, ps)
#' }
#' }
#' @export
ggpaintr_bind_draw <- function(input,
                               ggpaintr_state,
                               ids = ggpaintr_state$ids) {
  ggpaintr_validate_state(ggpaintr_state)
  ids <- ggpaintr_normalize_ids(ids)

  shiny::observeEvent(input[[ids$draw_button]], {
    shiny::req(ggpaintr_state$obj())
    ggpaintr_state$runtime(ggpaintr_build_runtime(
      ggpaintr_state$obj(),
      input,
      envir = ggpaintr_state$envir
    ))
  })

  invisible(ggpaintr_state)
}

#' Bind Export Behavior into a Shiny App
#'
#' @param output A Shiny `output` object.
#' @param ggpaintr_state A `ggpaintr_state` object.
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ggpaintr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ggpaintr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ggpaintr_bind_export(output, ps)
#' }
#' }
#' @export
ggpaintr_bind_export <- function(output,
                                 ggpaintr_state,
                                 ids = ggpaintr_state$ids) {
  ggpaintr_validate_state(ggpaintr_state)
  ids <- ggpaintr_normalize_ids(ids)

  output[[ids$export_button]] <- shiny::downloadHandler(
    filename = "ggpaintr-app.R",
    content = function(file) {
      shiny::req(ggpaintr_state$obj())
      generate_shiny(
        ggpaintr_state$obj(),
        file,
        copy_rules = ggpaintr_state$raw_copy_rules,
        placeholders = ggpaintr_state$placeholders
      )
    }
  )

  invisible(ggpaintr_state)
}

#' Return the Built Plot from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ggpaintr_build_runtime()`.
#'
#' @return A `ggplot` object or `NULL`.
#' @examples
#' obj <- ggpaintr_formula(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' spec <- ggpaintr_runtime_input_spec(obj)
#' inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
#' inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "mpg"
#' inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "disp"
#' runtime <- ggpaintr_build_runtime(
#'   obj,
#'   inputs
#' )
#' inherits(ggpaintr_plot_value(runtime), "ggplot")
#' @export
ggpaintr_plot_value <- function(runtime_result) {
  if (is.null(runtime_result) || !isTRUE(runtime_result[["ok"]])) {
    return(NULL)
  }

  runtime_result[["plot"]]
}

#' Return Default Error UI from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ggpaintr_build_runtime()`.
#'
#' @return A Shiny tag or `NULL`.
#' @export
ggpaintr_error_value <- function(runtime_result) {
  if (is.null(runtime_result) || isTRUE(runtime_result[["ok"]])) {
    return(NULL)
  }

  ggpaintr_error_ui(runtime_result[["message"]])
}

#' Return Generated Code Text from a Runtime Result
#'
#' @param runtime_result A runtime result list returned by `ggpaintr_build_runtime()`.
#'
#' @return A character string or `NULL`.
#' @export
ggpaintr_code_value <- function(runtime_result) {
  if (is.null(runtime_result)) {
    return(NULL)
  }

  runtime_result[["code_text"]]
}

#' Bind Default Plot Rendering into a Shiny App
#'
#' Bind the same default `renderPlot()` behavior used by `ggpaintr_server()`.
#' Advanced users who want to transform the plot should instead write their own
#' `renderPlot()` and call `ggpaintr_plot_value()`.
#'
#' @param output A Shiny `output` object.
#' @param ggpaintr_state A `ggpaintr_state` object.
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ggpaintr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ggpaintr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ggpaintr_bind_draw(input, ps)
#'   ggpaintr_bind_plot(output, ps)
#' }
#' }
#' @export
ggpaintr_bind_plot <- function(output,
                               ggpaintr_state,
                               ids = ggpaintr_state$ids) {
  ggpaintr_validate_state(ggpaintr_state)
  ids <- ggpaintr_normalize_ids(ids)

  output[[ids$plot_output]] <- shiny::renderPlot({
    plot_obj <- ggpaintr_plot_value(ggpaintr_state$runtime())
    if (is.null(plot_obj)) {
      graphics::plot.new()
      return(invisible(NULL))
    }

    plot_obj
  })

  invisible(ggpaintr_state)
}

#' Bind Default Error Rendering into a Shiny App
#'
#' @param output A Shiny `output` object.
#' @param ggpaintr_state A `ggpaintr_state` object.
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ggpaintr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ggpaintr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ggpaintr_bind_draw(input, ps)
#'   ggpaintr_bind_error(output, ps)
#' }
#' }
#' @export
ggpaintr_bind_error <- function(output,
                                ggpaintr_state,
                                ids = ggpaintr_state$ids) {
  ggpaintr_validate_state(ggpaintr_state)
  ids <- ggpaintr_normalize_ids(ids)

  output[[ids$error_output]] <- shiny::renderUI({
    ggpaintr_error_value(ggpaintr_state$runtime())
  })

  invisible(ggpaintr_state)
}

#' Bind Default Code Rendering into a Shiny App
#'
#' @param output A Shiny `output` object.
#' @param ggpaintr_state A `ggpaintr_state` object.
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return Invisibly returns `ggpaintr_state`.
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   ps <- ggpaintr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
#'   ggpaintr_bind_draw(input, ps)
#'   ggpaintr_bind_code(output, ps)
#' }
#' }
#' @export
ggpaintr_bind_code <- function(output,
                               ggpaintr_state,
                               ids = ggpaintr_state$ids) {
  ggpaintr_validate_state(ggpaintr_state)
  ids <- ggpaintr_normalize_ids(ids)

  output[[ids$code_output]] <- shiny::renderText({
    ggpaintr_code_value(ggpaintr_state$runtime())
  })

  invisible(ggpaintr_state)
}

#' Build Default ggpaintr Control Widgets
#'
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#' @param copy_rules Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ggpaintr_controls_ui(
#'   ggpaintr_ids(draw_button = "render_plot"),
#'   copy_rules = list(shell = list(draw_button = list(label = "Render plot")))
#' )
#' inherits(ui, "shiny.tag.list")
#' @export
ggpaintr_controls_ui <- function(ids = ggpaintr_ids(), copy_rules = NULL) {
  ids <- ggpaintr_normalize_ids(ids)
  shell_copy <- ggpaintr_resolve_shell_copy(copy_rules)

  shiny::tagList(
    shiny::uiOutput(ids$control_panel),
    shiny::actionButton(ids$draw_button, shell_copy$draw_copy$label),
    shiny::downloadButton(ids$export_button, shell_copy$export_copy$label)
  )
}

#' Build Default ggpaintr Output Widgets
#'
#' @param ids A `ggpaintr_ids` object describing the top-level Shiny ids used by
#'   the integration helpers.
#'
#' @return A Shiny UI object.
#' @examples
#' ui <- ggpaintr_outputs_ui(ggpaintr_ids(plot_output = "main_plot"))
#' inherits(ui, "shiny.tag.list")
#' @export
ggpaintr_outputs_ui <- function(ids = ggpaintr_ids()) {
  ids <- ggpaintr_normalize_ids(ids)

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
#' @param copy_rules Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#'
#' @return A `shiny.appobj`.
#' @examples
#' app <- ggpaintr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
#' inherits(app, "shiny.appobj")
#' @export
ggpaintr_app <- function(formula,
                         envir = parent.frame(),
                         copy_rules = NULL,
                         placeholders = NULL) {
  app_parts <- ggpaintr_app_components(
    formula,
    envir = envir,
    copy_rules = copy_rules,
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
#' @param copy_rules Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#'
#' @return A `ggpaintr_state` object containing reactive accessors named `obj`,
#'   `runtime`, and `var_ui_list`, plus shared metadata used by the bind helpers.
#' @export
ggpaintr_server <- function(input,
                            output,
                            session,
                            formula,
                            envir = parent.frame(),
                            copy_rules = NULL,
                            placeholders = NULL) {
  ggpaintr_state <- ggpaintr_server_state(
    formula,
    envir = envir,
    copy_rules = copy_rules,
    placeholders = placeholders
  )

  ggpaintr_bind_control_panel(input, output, ggpaintr_state)
  ggpaintr_bind_export(output, ggpaintr_state)
  ggpaintr_bind_draw(input, ggpaintr_state)
  ggpaintr_bind_plot(output, ggpaintr_state)
  ggpaintr_bind_error(output, ggpaintr_state)
  ggpaintr_bind_code(output, ggpaintr_state)

  ggpaintr_state
}

#' Resolve Standard Shell Copy for ggpaintr
#'
#' @param copy_rules Optional named list of copy overrides.
#'
#' @return A named list with shell copy entries.
#' @noRd
ggpaintr_resolve_shell_copy <- function(copy_rules = NULL) {
  list(
    title_copy = ggpaintr_resolve_copy("title", copy_rules = copy_rules),
    draw_copy = ggpaintr_resolve_copy("draw_button", copy_rules = copy_rules),
    export_copy = ggpaintr_resolve_copy("export_button", copy_rules = copy_rules)
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
ggpaintr_build_app_ui <- function(title_label, draw_label, export_label) {
  shiny::fluidPage(
    shiny::titlePanel(title_label),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("controlPanel"),
        shiny::actionButton("draw", draw_label),
        shiny::downloadButton("shinyExport", export_label)
      ),
      shiny::mainPanel(
        shiny::plotOutput("outputPlot"),
        shiny::uiOutput("outputError"),
        shiny::verbatimTextOutput("outputCode")
      )
    )
  )
}

#' Build Reusable App Components for ggpaintr
#'
#' @param formula A single formula string.
#' @param envir Environment used to resolve local data objects.
#' @param copy_rules Optional named list of copy overrides.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#'
#' @return A list with `ui` and `server`.
#' @noRd
ggpaintr_app_components <- function(formula,
                                  envir = parent.frame(),
                                  copy_rules = NULL,
                                  placeholders = NULL) {
  shell_copy <- ggpaintr_resolve_shell_copy(copy_rules)

  ui <- ggpaintr_build_app_ui(
    shell_copy$title_copy$label,
    shell_copy$draw_copy$label,
    shell_copy$export_copy$label
  )

  server <- function(input, output, session) {
    ggpaintr_server(
      input,
      output,
      session,
      formula,
      envir = envir,
      copy_rules = copy_rules,
      placeholders = placeholders
    )
    invisible(NULL)
  }

  list(ui = ui, server = server)
}
