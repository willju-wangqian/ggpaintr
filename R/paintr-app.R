#' Build a ggpaintr Shiny App
#'
#' Create a Shiny app from a single ggplot-like formula string. The app exposes
#' generated controls, a draw button, inline error handling, code output, and an
#' export button for producing a standalone app script.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building the app.
#'
#' @return A `shiny.appobj`.
#' @examples
#' app <- ggpaintr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
#' inherits(app, "shiny.appobj")
#' @export
ggpaintr_app <- function(formula, envir = parent.frame()) {
  app_parts <- paintr_app_components(formula, envir = envir)
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
#'
#' @return A list containing reactive accessors named `obj`, `runtime`, and
#'   `var_ui_list`.
#' @export
ggpaintr_server <- function(input, output, session, formula, envir = parent.frame()) {
  obj <- shiny::reactiveVal(paintr_formula(formula))
  runtime <- shiny::reactiveVal(NULL)
  var_ui_list <- shiny::reactiveVal(list())

  shiny::observe({
    shiny::req(obj())
    var_ui_list(output_embed_var(input, output, obj(), envir = envir))
  })

  output$controlPanel <- shiny::renderUI({
    shiny::req(obj())
    shiny::column(12, paintr_get_tab_ui(obj()))
  })

  output$shinyExport <- shiny::downloadHandler(
    filename = "ggpaintr-app.R",
    content = function(file) {
      shiny::req(obj())
      generate_shiny(obj(), var_ui_list(), file)
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

#' Build Reusable App Components for ggpaintr
#'
#' @param formula A single formula string.
#' @param envir Environment used to resolve local data objects.
#'
#' @return A list with `ui` and `server`.
#' @keywords internal
paintr_app_components <- function(formula, envir = parent.frame()) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("ggpaintr demo"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::uiOutput("controlPanel"),
        shiny::actionButton("draw", "click to draw the plot"),
        shiny::downloadButton("shinyExport", "export the shiny app")
      ),
      shiny::mainPanel(
        shiny::plotOutput("outputPlot"),
        shiny::uiOutput("outputError"),
        shiny::verbatimTextOutput("outputCode")
      )
    )
  )

  server <- function(input, output, session) {
    ggpaintr_server(input, output, session, formula, envir = envir)
    invisible(NULL)
  }

  list(ui = ui, server = server)
}
