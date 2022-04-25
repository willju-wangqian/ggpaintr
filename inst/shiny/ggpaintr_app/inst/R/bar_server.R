# server part for the bar chart
bar_control_id <- "barControl"

ns_bar <- NS(bar_control_id)

bar_main <- reactive({
  req(dataContainer())

  dataBar <- dataContainer()
  barUI <-
    controlUI(bar_control_id, dataBar,
              mapping = c('x', 'y', 'fill'),
              geom_args = c('stat', 'position', 'alpha'),
              plot_settings = c('scaleColor', 'misc', 'theme', 'theme_choose', 'labs')
              # extra_uiFunc = list(something = mappingUI),
              # extra_uiFuncArgs = list(something = list(NS("barControl"), dataBar))
    )

  barUI

}) %>% bindCache(input$drawBar) %>% bindEvent(input$drawBar)

observe({
  output$drawControls <- renderUI({
    req(bar_main())

    column(
      12,
      bsCollapse(
        id = NS(bar_control_id)("barControlCollapse"), open = "mapping",
        multiple = FALSE,
        bsCollapsePanel(
          "mapping",
          column(
            12, offset = 0, style='padding:0px;',
            br(),
            # p("tips: this variable should be ..."),
            bar_main()[['ui']][['mapping_ui']][['x']],
            bar_main()[['ui']][['mapping_ui']][['fill']]
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          h3("Setting for bar chart"),
          p("you can choose a variable for y axis and set", code("stat = identity")),
          bar_main()[['ui']][['mapping_ui']][['y']],
          bar_main()[['ui']][['geom_args_ui']][['stat']],
          bar_main()[['ui']][['geom_args_ui']][['position']],
          bar_main()[['ui']][['geom_args_ui']][['alpha']],
          h3("misc"),
          bar_main()[['ui']][['plot_settings_ui']][['misc']],
          br(),
          h3("theme settings"),
          bar_main()[['ui']][['plot_settings_ui']][['theme']],
          br(),
          bar_main()[['ui']][['plot_settings_ui']][['theme_choose']],
          h3("labels"),
          bar_main()[['ui']][['plot_settings_ui']][['labs']]
        )
      ),
      h3("choose colors (if applicable)"),
      bar_main()[['ui']][['plot_settings_ui']][['scaleColor']],
      br(),
      actionButton(NS(bar_control_id)("buttonDraw"), "Draw the plot"),
      br()
    )
  })

}) %>% bindEvent(input$drawBar)

observe({
  req(dataContainer(), bar_main())

  dataBar <- dataContainer()

  barComponent <- ggGeomGenerator(id = bar_control_id,
                                  data = dataBar,
                                  geom_FUN = "geom_bar",
                                  id_list = bar_main()[['ids']],
                                  params_list = list(
                                    mapping = c('x', 'y', 'fill'),
                                    geom_args = c('stat', 'position', 'alpha')
                                  )
  )

  flip_output <- flipHandler(bar_control_id,
                             bar_main()[['ids']][['plot_settings']][['misc']][2])

  facet_output <- facetHandler(bar_control_id,
                               bar_main()[['ids']][['plot_settings']][['misc']][1])

  theme_output <- stringParamHandler(bar_control_id,
                                     bar_main()[['ids']][['plot_settings']][['theme']],
                                     param = c("legend.position", "legend.direction"),
                                     FUN = "theme")

  theme_choose <- themeChooseHandler(bar_control_id,
                                     bar_main()[['ids']][['plot_settings']][['theme_choose']])

  labs_output <- stringParamHandler(bar_control_id,
                                    bar_main()[['ids']][['plot_settings']][['labs']],
                                    param = c('x', 'y', 'title', 'subtitle'),
                                    FUN = "labs")

  scaleColors <- tryCatch(
    {
      if (!is.null(selectedColors_bar())) {
        scaleColorHandler(bar_control_id,
                          selectedColors_bar(),
                          color_fill = 'fill')
      } else {
        NULL
      }
    },
    error = function(cond) {
      return(NULL)
    }
  )


  results <- get_plot_code(barComponent,
                           flip_output,
                           facet_output,
                           theme_output,
                           theme_choose,
                           labs_output,
                           scaleColors,
                           data = dataBar,
                           data_path = code_container[['data']])


  output$mainPlot <- renderPlot({

    validate(need(results[['plot']], "plot is not rendered"))

    results[['plot']]
  })

  output$mycode = renderText({

    results[['code']]

  })

}) %>% bindEvent(input[[NS(bar_control_id)("buttonDraw")]])


scaleColorIDs_bar <- reactive({
  req(bar_main())

  fill <-  bar_main()[['ids']][['mapping']][['fill']]
  scaleColor <- bar_main()[['ids']][['plot_settings']][['scaleColor']]

  list(fill = fill, scaleColor = scaleColor)

})

selectedColors_bar <- reactive({
  req(scaleColorIDs_bar(), dataContainer())

  colorGenerator(bar_control_id,
                 dataContainer(),
                 scaleColorIDs_bar()[[1]],
                 scaleColorIDs_bar()[[2]])

}) %>% bindCache(input[[ns_bar(scaleColorIDs_bar()[[1]])]]) %>%
  bindEvent(input[[ns_bar(scaleColorIDs_bar()[[1]])]])

observe({
  req(selectedColors_bar(), scaleColorIDs_bar())

  if(selectedColors_bar()[['type']] == "TOO_MANY_LEVELS") {
    output[[ns_bar(scaleColorIDs_bar()[[2]])]] <- renderUI({
      validate(paste( paste0("There are more than 11 levels in ",
                             input[[ns_bar(scaleColorIDs_bar()[[1]])]], "."),
                      "Too many levels.", sep = "\n"))
    })
  } else {
    output[[ns_bar(scaleColorIDs_bar()[[2]])]] <- renderUI({
      selectedColors_bar()[['ui']]
    })
  }

}) %>% bindEvent(input[[ns_bar(scaleColorIDs_bar()[[1]])]])








