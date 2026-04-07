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
#'
#' @return A `shiny.appobj`.
#' @examples
#' app <- ggpaintr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
#' inherits(app, "shiny.appobj")
#' @export
ggpaintr_app <- function(formula, envir = parent.frame(), copy_rules = NULL) {
  app_parts <- paintr_app_components(formula, envir = envir, copy_rules = copy_rules)
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
#'
#' @return A list containing reactive accessors named `obj`, `runtime`, and
#'   `var_ui_list`.
#' @export
ggpaintr_server <- function(input,
                            output,
                            session,
                            formula,
                            envir = parent.frame(),
                            copy_rules = NULL) {
  raw_copy_rules <- copy_rules
  effective_copy_rules <- paintr_effective_copy_rules(copy_rules)
  obj <- shiny::reactiveVal(paintr_formula(formula))
  runtime <- shiny::reactiveVal(NULL)
  var_ui_list <- shiny::reactiveVal(list())

  shiny::observe({
    shiny::req(obj())
    var_ui_list(register_var_ui_outputs(
      input,
      output,
      obj(),
      envir = envir,
      copy_rules = effective_copy_rules
    ))
  })

  output$controlPanel <- shiny::renderUI({
    shiny::req(obj())
    shiny::column(12, paintr_get_tab_ui(obj(), copy_rules = effective_copy_rules))
  })

  output$shinyExport <- shiny::downloadHandler(
    filename = "ggpaintr-app.R",
    content = function(file) {
      shiny::req(obj())
      generate_shiny(
        obj(),
        var_ui_list(),
        file,
        copy_rules = raw_copy_rules
      )
    }
  )

  shiny::observeEvent(input$draw, {
    shiny::req(obj())
    runtime(paintr_build_runtime(obj(), input, envir = envir))
  })

  output$outputPlot <- shiny::renderPlot({
    runtime_result <- runtime()
    if (is.null(runtime_result) || !isTRUE(runtime_result[["ok"]])) {
      graphics::plot.new()
      return(invisible(NULL))
    }

    runtime_result[["plot"]]
  })

  output$outputError <- shiny::renderUI({
    runtime_result <- runtime()
    if (is.null(runtime_result) || isTRUE(runtime_result[["ok"]])) {
      return(NULL)
    }

    paintr_error_ui(runtime_result[["message"]])
  })

  output$outputCode <- shiny::renderText({
    runtime_result <- runtime()
    if (is.null(runtime_result)) {
      return(NULL)
    }

    runtime_result[["code_text"]]
  })

  list(obj = obj, runtime = runtime, var_ui_list = var_ui_list)
}

#' Resolve Standard Shell Copy for ggpaintr
#'
#' @param copy_rules Optional named list of copy overrides.
#'
#' @return A named list with shell copy entries.
#' @keywords internal
paintr_resolve_shell_copy <- function(copy_rules = NULL) {
  list(
    title_copy = paintr_resolve_copy("title", copy_rules = copy_rules),
    draw_copy = paintr_resolve_copy("draw_button", copy_rules = copy_rules),
    export_copy = paintr_resolve_copy("export_button", copy_rules = copy_rules)
  )
}

#' Build the Standard ggpaintr App UI
#'
#' @param title_label App title text.
#' @param draw_label Draw button text.
#' @param export_label Export button text.
#'
#' @return A Shiny UI object.
#' @keywords internal
paintr_build_app_ui <- function(title_label, draw_label, export_label) {
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
#'
#' @return A list with `ui` and `server`.
#' @keywords internal
paintr_app_components <- function(formula, envir = parent.frame(), copy_rules = NULL) {
  shell_copy <- paintr_resolve_shell_copy(copy_rules)

  ui <- paintr_build_app_ui(
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
      copy_rules = copy_rules
    )
    invisible(NULL)
  }

  list(ui = ui, server = server)
}
