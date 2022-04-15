# server part for the box chart
box_control_id <- "boxControl"

ns_box <- NS(box_control_id)

box_main <- reactive({
  req(dataContainer())

  dataBox <- dataContainer()
  boxUI <-
    controlUI(box_control_id, dataBox,
              mapping = c('x', 'y', 'fill'),
              plot_settings = c('scaleColor', 'misc', 'theme'),
              geom_args = NULL
              # extra_uiFunc = list(something = mappingUI),
              # extra_uiFuncArgs = list(something = list(NS("boxControl"), dataBox))
    )

  boxUI

}) %>% bindCache(input$drawBox) %>% bindEvent(input$drawBox)

observe({
  output$drawControls <- renderUI({
    req(box_main())

    column(
      12,
      bsCollapse(
        id = NS(box_control_id)("boxControlCollapse"), open = "mapping",
        multiple = FALSE,
        bsCollapsePanel(
          "mapping",
          column(
            12, offset = 0, style='padding:0px;',
            br(),
            box_main()[['ui']][['mapping_ui']][['x']],
            box_main()[['ui']][['mapping_ui']][['y']],
            box_main()[['ui']][['mapping_ui']][['fill']]
            # box_main()[['ui']][['mapping_ui']][['something']]
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          h3("misc"),
          box_main()[['ui']][['plot_settings_ui']][['misc']],
          br(),
          h3("theme settings"),
          box_main()[['ui']][['plot_settings_ui']][['theme']]
        )
      ),
      h3("choose colors (if applicable)"),
      box_main()[['ui']][['plot_settings_ui']][['scaleColor']],
      br(),
      actionButton(NS(box_control_id)("buttonDraw"), "Draw the plot"),
      br()
    )
  })

}) %>% bindEvent(input$drawBox)

observe({
  req(dataContainer(), box_main())

  dataBox <- dataContainer()

  boxComponent <- ggGeomGenerator(id = box_control_id,
                                  data = dataBox,
                                  geom_FUN = "geom_boxplot",
                                  id_list = box_main()[['ids']],
                                  params_list = list(
                                    mapping = c('x', 'y', 'fill')
                                  )
  )

  flip_output <- flipHandler(box_control_id,
                             box_main()[['ids']][['plot_settings']][['misc']][2])

  facet_output <- facetHandler(box_control_id,
                               box_main()[['ids']][['plot_settings']][['misc']][1])

  theme_output <- themeHandler(box_control_id,
                               box_main()[['ids']][['plot_settings']][['theme']],
                               theme_param = c("legend.position", "legend.direction") )

  scaleColors <- tryCatch(
    {
      if (!is.null(selectedColors())) {
        scaleColorHandler(box_control_id,
                          selectedColors(),
                          color_fill = 'fill')
      } else {
        NULL
      }
    },
    error = function(cond) {
      return(NULL)
    }
  )


  results <- get_plot_code(boxComponent,
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

}) %>% bindEvent(input[[NS(box_control_id)("buttonDraw")]])



# selectedColors <- reactive({
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
# # selectedColors <- scaleColorServer(box_control_id,
# #                                    box_main()[['ids']][['mapping']][['fill']],
# #                                    box_main()[['ids']][['plot_settings']][['scaleColor']],
# #                                    box_main,
# #                                    dataContainer)
#



scaleColorIDs <- reactive({
  req(box_main())

  fill <-  box_main()[['ids']][['mapping']][['fill']]
  scaleColor <- box_main()[['ids']][['plot_settings']][['scaleColor']]

  list(fill = fill, scaleColor = scaleColor)

})

selectedColors <- reactive({
  req(scaleColorIDs(), dataContainer())

  colorGenerator(box_control_id,
                 dataContainer(),
                 scaleColorIDs()[[1]],
                 scaleColorIDs()[[2]])

}) %>% bindCache(input[[ns_box(scaleColorIDs()[[1]])]]) %>%
  bindEvent(input[[ns_box(scaleColorIDs()[[1]])]])

observe({
  req(selectedColors(), scaleColorIDs())

  if(selectedColors()[['type']] == "TOO_MANY_LEVELS") {
    output[[ns_box(scaleColorIDs()[[2]])]] <- renderUI({
      validate(paste( paste0("There are more than 11 levels in ",
                             input[[ns_box(scaleColorIDs()[[1]])]], "."),
                      "Too many levels.", sep = "\n"))
    })
  } else {
    output[[ns_box(scaleColorIDs()[[2]])]] <- renderUI({
      selectedColors()[['ui']]
    })
  }

}) %>% bindEvent(input[[ns_box(scaleColorIDs()[[1]])]])

# scaleColorRenderUI(box_control_id,
#                    scaleColorIDs()[['fill']],
#                    scaleColorIDs()[['scaleColor']],
#                    box_main,
#                    selectedColors)





