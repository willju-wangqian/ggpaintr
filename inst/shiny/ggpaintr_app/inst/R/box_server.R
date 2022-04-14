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
      br(),
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

  # selected_colors <- tryCatch(
  #   {
  #     if (!is.null(selectedColors())) selectedColors() else NULL
  #   },
  #   error = function(cond) {
  #     return(NULL)
  #   }
  # )
  #
  # pp <- pp + scaleColorHandler(box_control_id,
  #                              selected_colors,
  #                              color_fill = 'fill')

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

  pp <- pp + scaleColors

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


observe({
  req(box_main())

  controltype = 'boxControl'

  get_var = function(var_name){
    paste(controltype, '-', var_name, sep = '')
  }

  # ggplot + mappings + geom_boxplot
  mappings = names(box_main()[['ui']][['mapping_ui']])
  variables = paste(get_var('map'), stringr::str_to_title(mappings), sep = '')
  variables = as.vector(sapply(variables, function(x){input[[x]]}))
  variables_not_null = sapply(variables, function(x) {!is.null(x)})
  mappings = mappings[variables_not_null]
  variables = variables[variables_not_null]

  mapping_code = paste(mappings, variables, sep = ' = ')
  mapping_code = paste(mapping_code, collapse = ', ')
  mapping_code = paste('ggplot(data = df, aes(', mapping_code, '))', ' + geom_boxplot()', sep = '')

  # settings
  flip_code = ''
  if(input[[get_var('miscFlip')]]){
    flip_code = paste(' + ', flip_code, 'coord_flip()', sep = '')
  }

  facet_code = ''
  if(!is.null(input[[get_var('miscFacet')]])){
    facet_variables = input[[get_var('miscFacet')]]
    if(length(facet_variables) > 1){
      facet_variables = paste(facet_variables, collapse = ' ~ ')
    }
    facet_code = paste(' + ', facet_code, 'facet_grid(', facet_variables, ')', sep = '')
  }


  # legend settings
  legend_code = ''
  # variables = c('legend.direction', 'legend.position')
  # legend_settings = c(input[[get_var('themeLegendDirection')]],
  #                      input[[get_var('themeLegendPosition')]])
  #
  # if(!is.null(legend_settings)){
  #   legend_settings_not_null = sapply(legend_settings, function(x) {!is.null(x)})
  #   print(legend_settings_not_null)
  #   variables = as.vector(variables[legend_settings_not_null])
  #   print(variables)
  #   legend_settings = as.vector(legend_settings[legend_settings_not_null])
  #   print(legend_settings)
  #
  #   legend_settings = paste(variables, legend_settings, sep = ' = ')
  #
  #   legend_code = paste(legend_code, ' + theme(',
  #                       paste(legend_settings, collapse = ', '),
  #                       ')', sep = '')
  # }

  legend_direction = input[[get_var('themeLegendDirection')]]
  legend_position = input[[get_var('themeLegendPosition')]]


  if(!is.null(legend_direction) | !is.null(legend_position)){
    legend_code = paste(legend_code, ' + theme(', sep = '')

    if(!is.null(legend_direction) & is.null(legend_position)){
      legend_code = paste(legend_code, 'legend.direction = ', "'", legend_direction, "'", sep = '')
    }
    else if(!is.null(legend_position) & is.null(legend_direction)){
      legend_code = paste(legend_code, 'legend.position = ', "'", legend_position, "'", sep = '')
    }
    else{
      legend_code = paste(legend_code,
                          'legend.direction = ', "'", legend_direction, "'",
                          ', ',
                          'legend.position = ', "'", legend_position, "'", sep = '')
    }

    legend_code = paste(legend_code, ')', sep = '')
  }



  output$mycode = renderText({
    paste("# Remember to replace 'df' in the code with the name of your data frame in R:\n\n",
      mapping_code,
      flip_code,
      facet_code,
      legend_code,
      sep = '')
  })
}) %>% bindEvent(input[[NS(box_control_id)("buttonDraw")]])





