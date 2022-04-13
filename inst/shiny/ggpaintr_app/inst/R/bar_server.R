# server part for the box chart
bar_control_id <- "barControl"

bar_main <- reactive({
  req(dataContainer())

  dataBox <- dataContainer()
  boxUI <-
    controlUI(bar_control_id, dataBox,
              mapping = c('x', 'y', 'color'),
              plot_settings = c( 'misc', 'theme'),
              geom_args = NULL
              # extra_uiFunc = list(something = mappingUI),
              # extra_uiFuncArgs = list(something = list(NS("boxControl"), dataBox))
    )

  boxUI

}) %>% bindCache(input$drawBar) %>% bindEvent(input$drawBar)

observe({
  output$drawControls <- renderUI({
    req(bar_main())

    column(
      12,
      bsCollapse(
        id = NS(bar_control_id)("boxControlCollapse"), open = "mapping",
        multiple = FALSE,
        bsCollapsePanel(
          "mapping",
          column(
            12, offset = 0, style='padding:0px;',
            br(),
            bar_main()[['ui']][['mapping_ui']][['x']],
            bar_main()[['ui']][['mapping_ui']][['y']],
            bar_main()[['ui']][['mapping_ui']][['color']]
            # bar_main()[['ui']][['mapping_ui']][['something']]
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          br(),
          h3("misc"),
          bar_main()[['ui']][['plot_settings_ui']][['misc']],
          br(),
          h3("theme settings"),
          bar_main()[['ui']][['plot_settings_ui']][['theme']]
        )
      ),
      actionButton(NS(bar_control_id)("buttonDraw"), "Draw the plot"),
      bar_main()[['ui']][['geom_args_ui']][['stat']]
    )
  })
}) %>% bindEvent(input$drawBar)


observe({
  req(dataContainer(), bar_main())

  dataBox <- dataContainer()

  barComponent <- ggGeomGenerator(id = bar_control_id,
                                  data = dataBox,
                                  geom_FUN = geom_col,
                                  id_list = bar_main()[['ids']],
                                  params_list = list(
                                    mapping = c('x', 'y', 'color')
                                  ),
                                  color_fill = TRUE
  )

  pp <- ggplot(data = dataBox) +
    barComponent()

  pp <- pp + flipHandler(bar_control_id,
                         bar_main()[['ids']][['plot_settings']][['misc']][2])

  pp <- pp + facetHandler(bar_control_id,
                          bar_main()[['ids']][['plot_settings']][['misc']][1])

  pp <- pp + themeHandler(bar_control_id,
                          bar_main()[['ids']][['plot_settings']][['theme']],
                          theme_param = c("legend.position", "legend.direction") )

  output$mainPlot <- renderPlot({
    pp
  })

}) %>% bindEvent(input[[NS(bar_control_id)("buttonDraw")]])
