# server part for the scatter plot
scatter_control_id <- "scatterControl"

ns_scatter <- NS(scatter_control_id)

scatter_main <- reactive({
  req(dataContainer())

  dataBox <- dataContainer()
  scatterUI <-
    controlUI(scatter_control_id, dataBox,
              mapping = c('x', 'y', 'color','size','shape'),
              plot_settings = c('scaleColor', 'misc', 'theme'),
              geom_args = NULL
              # extra_uiFunc = list(something = mappingUI),
              # extra_uiFuncArgs = list(something = list(NS("boxControl"), dataBox))
    )

  scatterUI

}) %>% bindCache(input$drawScatter) %>% bindEvent(input$drawScatter)

observe({
  output$drawControls <- renderUI({
    req(scatter_main())

    column(
      12,
      bsCollapse(
        id = NS(scatter_control_id)("scatterControlCollapse"), open = "mapping",
        multiple = FALSE,
        bsCollapsePanel(
          "mapping",
          column(
            12, offset = 0, style='padding:0px;',
            br(),
            # p("tips: this variable should be ..."),
            scatter_main()[['ui']][['mapping_ui']][['x']],
            scatter_main()[['ui']][['mapping_ui']][['y']],
            scatter_main()[['ui']][['mapping_ui']][['color']],
            scatter_main()[['ui']][['mapping_ui']][['size']],
            scatter_main()[['ui']][['mapping_ui']][['shape']]
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          h3("misc"),
          scatter_main()[['ui']][['plot_settings_ui']][['misc']],
          br(),
          h3("theme settings"),
          scatter_main()[['ui']][['plot_settings_ui']][['theme']]
        )
      ),
      h3("choose colors (if applicable)"),
      scatter_main()[['ui']][['plot_settings_ui']][['scaleColor']],
      br(),
      actionButton(NS(scatter_control_id)("buttonDraw"), "Draw the plot"),
      br()
    )
  })

}) %>% bindEvent(input$drawScatter)

observe({
  req(dataContainer(), scatter_main())

  dataBox <- dataContainer()

  scatterComponent <- ggGeomGenerator(id = scatter_control_id,
                                  data = dataBox,
                                  geom_FUN = "geom_point",
                                  id_list = scatter_main()[['ids']],
                                  params_list = list(
                                    mapping = c('x', 'y', 'color','size','shape')
                                  )
  )

  flip_output <- flipHandler(scatter_control_id,
                             scatter_main()[['ids']][['plot_settings']][['misc']][2])

  facet_output <- facetHandler(scatter_control_id,
                               scatter_main()[['ids']][['plot_settings']][['misc']][1])

  theme_output <- themeHandler(scatter_control_id,
                               scatter_main()[['ids']][['plot_settings']][['theme']],
                               theme_param = c("legend.position", "legend.direction") )

  scaleColors <- tryCatch(
    {
      if (!is.null(selectedColors_scatter())) {
        scaleColorHandler(scatter_control_id,
                          selectedColors_scatter(),
                          color_fill = 'color')
      } else {
        NULL
      }
    },
    error = function(cond) {
      return(NULL)
    }
  )


  results <- get_plot_code(scatterComponent,
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

}) %>% bindEvent(input[[NS(scatter_control_id)("buttonDraw")]])



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



scaleColorIDs_scatter <- reactive({
  req(scatter_main())

  color <-  scatter_main()[['ids']][['mapping']][['color']]
  scaleColor <- scatter_main()[['ids']][['plot_settings']][['scaleColor']]

  list(color = color, scaleColor = scaleColor)

})

selectedColors_scatter <- reactive({
  req(scaleColorIDs_scatter(), dataContainer())

  colorGenerator(scatter_control_id,
                 dataContainer(),
                 scaleColorIDs_scatter()[[1]],
                 scaleColorIDs_scatter()[[2]])

}) %>% bindCache(input[[ns_scatter(scaleColorIDs_scatter()[[1]])]]) %>%
  bindEvent(input[[ns_scatter(scaleColorIDs_scatter()[[1]])]])

observe({
  req(selectedColors_scatter(), scaleColorIDs_scatter())

  if(selectedColors_scatter()[['type']] == "TOO_MANY_LEVELS") {
    output[[ns_scatter(scaleColorIDs_scatter()[[2]])]] <- renderUI({
      validate(paste( paste0("There are more than 11 levels in ",
                             input[[ns_scatter(scaleColorIDs_scatter()[[1]])]], "."),
                      "Too many levels.", sep = "\n"))
    })
  } else {
    output[[ns_scatter(scaleColorIDs_scatter()[[2]])]] <- renderUI({
      selectedColors_scatter()[['ui']]
    })
  }

}) %>% bindEvent(input[[ns_scatter(scaleColorIDs_scatter()[[1]])]])

# scaleColorRenderUI(box_control_id,
#                    scaleColorIDs_scatter()[['fill']],
#                    scaleColorIDs_scatter()[['scaleColor']],
#                    box_main,
#                    selectedColors_scatter)





