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
    session$userData$paintr <- shiny::reactiveValues(obj = list(NULL))
    session$userData$paintr$obj <- paintr_formula(formula)

    shiny::observe({
      shiny::req(session$userData$paintr$obj)
      session$userData$paintr$var_ui_list <-
        output_embed_var(input, output, session$userData$paintr$obj, envir = envir)
    })

    output$controlPanel <- shiny::renderUI({
      shiny::req(session$userData$paintr$obj)
      shiny::column(12, paintr_get_tab_ui(session$userData$paintr$obj))
    })

    output$shinyExport <- shiny::downloadHandler(
      filename = "ggpaintr-app.R",
      content = function(file) {
        shiny::req(session$userData$paintr$obj)
        generate_shiny(session$userData$paintr$obj, session$userData$paintr$var_ui_list, file)
      }
    )

    shiny::observeEvent(input$draw, {
      shiny::req(session$userData$paintr$obj)

      runtime_result <- paintr_build_runtime(
        session$userData$paintr$obj,
        input,
        envir = envir
      )

      output$outputPlot <- shiny::renderPlot({
        if (!isTRUE(runtime_result[["ok"]])) {
          graphics::plot.new()
          return(invisible(NULL))
        }

        runtime_result[["plot"]]
      })

      output$outputError <- shiny::renderUI({
        if (isTRUE(runtime_result[["ok"]])) {
          return(NULL)
        }

        paintr_error_ui(runtime_result[["message"]])
      })

      output$outputCode <- shiny::renderText({
        runtime_result[["code_text"]]
      })
    })
  }

  list(ui = ui, server = server)
}
