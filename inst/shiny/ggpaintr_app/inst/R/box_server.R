# server part for the box chart
box_control_id <- "boxControl"

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
      box_main()[['ui']][['geom_args_ui']][['stat']]
    )
  })

}) %>% bindEvent(input$drawBox)

observe({
  req(dataContainer(), box_main())

  dataBox <- dataContainer()

  boxComponent <- ggGeomGenerator(id = box_control_id,
                                  data = dataBox,
                                  geom_FUN = geom_boxplot,
                                  id_list = box_main()[['ids']],
                                  params_list = list(
                                    mapping = c('x', 'y', 'fill')
                                  )
  )

  pp <- ggplot(data = dataBox) +
    boxComponent()

  pp <- pp + flipHandler(box_control_id,
                         box_main()[['ids']][['plot_settings']][['misc']][2])

  pp <- pp + facetHandler(box_control_id,
                          box_main()[['ids']][['plot_settings']][['misc']][1])

  pp <- pp + themeHandler(box_control_id,
                          box_main()[['ids']][['plot_settings']][['theme']],
                          theme_param = c("legend.position", "legend.direction") )

  selected_colors <- tryCatch(
    {
      if (!is.null(selectedColors())) selectedColors() else NULL
    },
    error = function(cond) {
      return(NULL)
    }
  )

  pp <- pp + scaleColorHandler(box_control_id,
                               selected_colors,
                               color_fill = 'fill')

  output$mainPlot <- renderPlot({

    validate(need(pp, "plot is not rendered"))

    pp
  })

}) %>% bindEvent(input[[NS(box_control_id)("buttonDraw")]])

selectedColors <- reactive({
  req(box_main(), dataContainer())

  colorGenerator(box_control_id,
                 dataContainer(),
                 box_main()[['ids']][['mapping']][['fill']],
                 box_main()[['ids']][['plot_settings']][['scaleColor']])

}) %>% bindCache(input[[NS(box_control_id)(box_main()[['ids']][['mapping']][['fill']])]]) %>%
  bindEvent(input[[NS(box_control_id)(box_main()[['ids']][['mapping']][['fill']])]])

observe({
  req(selectedColors(), box_main())

  ns <- NS(box_control_id)
  scaleColorID <- box_main()[['ids']][['plot_settings']][['scaleColor']]
  fillID <- box_main()[['ids']][['mapping']][['fill']]

  if(selectedColors()[['type']] == "TOO_MANY_LEVELS") {
    output[[ns(scaleColorID)]] <- renderUI({
      validate(paste( paste0("There are more than 11 levels in ", input[[ns(fillID)]], "."),
                      "Too many levels.", sep = "\n"))
    })
  } else {
    output[[ns(scaleColorID)]] <- renderUI({
      selectedColors()[['ui']]
    })
  }

}) %>% bindEvent(input[[NS(box_control_id)(box_main()[['ids']][['mapping']][['fill']])]])








