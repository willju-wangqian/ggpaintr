# server part for the box chart
box_control_id <- "boxControl"

ns_box <- NS(box_control_id)

box_main <- reactive({
  req(dataContainer())

  dataBox <- dataContainer()
  boxUI <-
    controlUI(box_control_id, dataBox,
              mapping = c('x', 'y', 'fill'),
              plot_settings = c('scaleColor', 'misc', 'theme', 'theme_choose'),
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
            # p("tips: this variable should be ..."),
            box_main()[['ui']][['mapping_ui']][['x']],
            box_main()[['ui']][['mapping_ui']][['y']],
            box_main()[['ui']][['mapping_ui']][['fill']]
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          h3("misc"),
          box_main()[['ui']][['plot_settings_ui']][['misc']],
          br(),
          h3("theme settings"),
          box_main()[['ui']][['plot_settings_ui']][['theme']],
          br(),
          box_main()[['ui']][['plot_settings_ui']][['theme_choose']]
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

  theme_choose <- themeChooseHandler(box_control_id,
                                     box_main()[['ids']][['plot_settings']][['theme_choose']])

  scaleColors <- tryCatch(
    {
      if (!is.null(selectedColors_box())) {
        scaleColorHandler(box_control_id,
                          selectedColors_box(),
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
                           theme_choose,
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



# selectedColors_box <- reactive({
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
# # selectedColors_box <- scaleColorServer(box_control_id,
# #                                    box_main()[['ids']][['mapping']][['fill']],
# #                                    box_main()[['ids']][['plot_settings']][['scaleColor']],
# #                                    box_main,
# #                                    dataContainer)
#


scaleColorIDs_box <- reactive({
  req(box_main())

  fill <-  box_main()[['ids']][['mapping']][['fill']]
  scaleColor <- box_main()[['ids']][['plot_settings']][['scaleColor']]

  list(fill = fill, scaleColor = scaleColor)

})

selectedColors_box <- reactive({
  req(scaleColorIDs_box(), dataContainer())

  colorGenerator(box_control_id,
                 dataContainer(),
                 scaleColorIDs_box()[[1]],
                 scaleColorIDs_box()[[2]])

}) %>% bindCache(input[[ns_box(scaleColorIDs_box()[[1]])]]) %>%
  bindEvent(input[[ns_box(scaleColorIDs_box()[[1]])]])

observe({
  req(selectedColors_box(), scaleColorIDs_box())

  if(selectedColors_box()[['type']] == "TOO_MANY_LEVELS") {
    output[[ns_box(scaleColorIDs_box()[[2]])]] <- renderUI({
      validate(paste( paste0("There are more than 11 levels in ",
                             input[[ns_box(scaleColorIDs_box()[[1]])]], "."),
                      "Too many levels.", sep = "\n"))
    })
  } else {
    output[[ns_box(scaleColorIDs_box()[[2]])]] <- renderUI({
      selectedColors_box()[['ui']]
    })
  }

}) %>% bindEvent(input[[ns_box(scaleColorIDs_box()[[1]])]])








