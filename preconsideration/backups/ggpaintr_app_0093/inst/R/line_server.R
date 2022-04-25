# server part for the scatter plot
line_control_id <- "lineControl"

ns_line <- NS(line_control_id)

line_main <- reactive({
  req(dataContainer())

  dataBox <- dataContainer()
  lineUI <-
    controlUI(line_control_id, dataBox,
              mapping = c('x', 'y', 'color'),
              plot_settings = c('scaleColor', 'misc', 'theme'),
              geom_args = NULL
              # extra_uiFunc = list(something = mappingUI),
              # extra_uiFuncArgs = list(something = list(NS("boxControl"), dataBox))
    )

  lineUI

}) %>% bindCache(input$drawLine) %>% bindEvent(input$drawLine)

observe({
  output$drawControls <- renderUI({
    req(line_main())

    column(
      12,
      bsCollapse(
        id = NS(line_control_id)("lineControlCollapse"), open = "mapping",
        multiple = FALSE,
        bsCollapsePanel(
          "mapping",
          column(
            12, offset = 0, style='padding:0px;',
            br(),
            # p("tips: this variable should be ..."),
            line_main()[['ui']][['mapping_ui']][['x']],
            line_main()[['ui']][['mapping_ui']][['y']],
            line_main()[['ui']][['mapping_ui']][['color']]

          )
        ),
        bsCollapsePanel(
          "advanced settings",
          h3("misc"),
          line_main()[['ui']][['plot_settings_ui']][['misc']],
          br(),
          h3("theme settings"),
          line_main()[['ui']][['plot_settings_ui']][['theme']]
        )
      ),
      h3("choose colors (if applicable)"),
      line_main()[['ui']][['plot_settings_ui']][['scaleColor']],
      br(),
      actionButton(NS(line_control_id)("buttonDraw"), "Draw the plot"),
      br()
    )
  })

}) %>% bindEvent(input$drawLine)

observe({
  req(dataContainer(), line_main())

  dataBox <- dataContainer()

  lineComponent <- ggGeomGenerator(id = line_control_id,
                                  data = dataBox,
                                  geom_FUN = "geom_line",
                                  id_list = line_main()[['ids']],
                                  params_list = list(
                                    mapping = c('x', 'y', 'color')
                                  )
  )

  flip_output <- flipHandler(line_control_id,
                             line_main()[['ids']][['plot_settings']][['misc']][2])

  facet_output <- facetHandler(line_control_id,
                               line_main()[['ids']][['plot_settings']][['misc']][1])

  theme_output <- themeHandler(line_control_id,
                               line_main()[['ids']][['plot_settings']][['theme']],
                               theme_param = c("legend.position", "legend.direction") )

  scaleColors <- tryCatch(
    {
      if (!is.null(selectedColors_line())) {
        scaleColorHandler(line_control_id,
                          selectedColors_line(),
                          color_fill = 'color')
      } else {
        NULL
      }
    },
    error = function(cond) {
      return(NULL)
    }
  )


  results <- get_plot_code(lineComponent,
                           flip_output,
                           facet_output,
                           theme_output,
                           scaleColors,
                           data = dataBox,
                           data_path = code_container[['data']])


  output$mainPlot <- renderPlot({

    validate(need(results[['plot']], "plot is not rendered"))

    results[['plot']]
  })



  output$mycode = renderText({

    results[['code']]

  })

}) %>% bindEvent(input[[NS(line_control_id)("buttonDraw")]])



# selectedColors_scatter <- reactive({
#   req(box_main(), dataContainer())
#
#   colorGenerator(box_control_id,
#                  dataContainer(),
#                  box_main()[['ids']][['mapping']][['fill']],
#                  box_main()[['ids']][['plot_settings']][['scaleColor']])
#
# }) %>%
#   bindCache(input[[NS(box_control_id)(box_main()[['ids']][['mapping']][['fill']])]]) %>%
#   bindEvent(input[[NS(box_control_id)(box_main()[['ids']][['mapping']][['fill']])]])
#
# #
# # selectedColors_scatter <- scaleColorServer(box_control_id,
# #                                    box_main()[['ids']][['mapping']][['fill']],
# #                                    box_main()[['ids']][['plot_settings']][['scaleColor']],
# #                                    box_main,
# #                                    dataContainer)
#



scaleColorIDs_line <- reactive({
  req(line_main())

  color <-  line_main()[['ids']][['mapping']][['color']]
  scaleColor <- line_main()[['ids']][['plot_settings']][['scaleColor']]

  list(color = color, scaleColor = scaleColor)

})

selectedColors_line <- reactive({
  req(scaleColorIDs_line(), dataContainer())

  colorGenerator(line_control_id,
                 dataContainer(),
                 scaleColorIDs_line()[[1]],
                 scaleColorIDs_line()[[2]])

}) %>% bindCache(input[[ns_line(scaleColorIDs_line()[[1]])]]) %>%
  bindEvent(input[[ns_line(scaleColorIDs_line()[[1]])]])

observe({
  req(selectedColors_line(), scaleColorIDs_line())

  if(selectedColors_line()[['type']] == "TOO_MANY_LEVELS") {
    output[[ns_line(scaleColorIDs_line()[[2]])]] <- renderUI({
      validate(paste( paste0("There are more than 11 levels in ",
                             input[[ns_line(scaleColorIDs_line()[[1]])]], "."),
                      "Too many levels.", sep = "\n"))
    })
  } else {
    output[[ns_line(scaleColorIDs_line()[[2]])]] <- renderUI({
      selectedColors_line()[['ui']]
    })
  }

}) %>% bindEvent(input[[ns_line(scaleColorIDs_line()[[1]])]])

# scaleColorRenderUI(box_control_id,
#                    scaleColorIDs_scatter()[['fill']],
#                    scaleColorIDs_scatter()[['scaleColor']],
#                    box_main,
#                    selectedColors_scatter)





