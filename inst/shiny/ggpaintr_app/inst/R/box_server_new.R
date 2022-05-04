############# set up expressions ##############
# boxplot

observe({
  req(result_container)

  expr <- rlang::expr(
    geom_boxplot(aes(x, y, fill)) +
      coord_flip +
      facet_grid +
      theme(legend.direction, legend.position) +
      labs(x, y, title) +
      theme_choose +
      scale_fill
  )

  result_container[['expr']] <- expr
  result_container[['module_id']] <- "boxPlot"

}) %>% bindEvent(input$drawBox)

# scatter plot

observe({
  req(result_container)

  expr <- rlang::expr(
    geom_point(aes(x, y, color, size)) +
      labs(x, y, title) +
      coord_flip +
      facet_grid +
      theme(legend.direction, legend.position) +
      theme_choose +
      scale_color
  )

  result_container[['expr']] <- expr
  result_container[['module_id']] <- "scatterPlot"

}) %>% bindEvent(input$drawScatter)

# line chart

observe({
  req(result_container)

  expr <- rlang::expr(
    geom_line(aes(x, y, color)) +
      labs(x, y, title) +
      coord_flip +
      facet_grid +
      theme(legend.direction, legend.position) +
      theme_choose +
      scale_color
  )

  result_container[['expr']] <- expr
  result_container[['module_id']] <- "lineChart"

}) %>% bindEvent(input$drawLine)

# bar chart

observe({
  req(result_container)

  expr <- rlang::expr(
    geom_col(aes(x, y, fill), position) +
      labs(x, y, title) +
      coord_flip +
      facet_grid +
      theme(legend.direction, legend.position) +
      theme_choose +
      scale_fill
  )

  result_container[['expr']] <- expr
  result_container[['module_id']] <- "barChart"

}) %>% bindEvent(input$drawBar)

# violin

observe({
  req(result_container)

  expr <- rlang::expr(
    geom_violin(aes(x, y, fill)) +
      labs(x, y, title) +
      coord_flip +
      facet_grid +
      theme(legend.direction, legend.position) +
      theme_choose +
      scale_fill
  )

  result_container[['expr']] <- expr
  result_container[['module_id']] <- "violinChart"

}) %>% bindEvent(input$drawViolin)




############# paintr #################

paintr_main <- reactive({
  req(dataContainer(), result_container[['expr']])

  paintr(
    result_container[['module_id']],
    dataContainer(), data_path = result_container[['data']],
    !!result_container[['expr']]
  )

}) %>% bindEvent(result_container[['expr']], dataContainer())

observe({

  req(paintr_main())

  if(!is.null(paintr_get_ui(paintr_main(), "scale_fill"))) {
    result_container[['scale_fill']] <- scaleColor_build_reactivity(
      result_container[['module_id']], paintr_main, "fill"
    )
  } else {
    result_container[['scale_fill']] <- NULL
  }

  if(!is.null(paintr_get_ui(paintr_main(), "scale_color"))) {
    result_container[['scale_color']] <- scaleColor_build_reactivity(
      result_container[['module_id']], paintr_main, "color"
    )
  } else {
    result_container[['scale_color']] <- NULL
  }

  output$drawControls <- renderUI({
    req(paintr_main(), dataContainer(), result_container)

    column(
      12,
      br(),
      bsCollapse(
        id = NS(result_container[['module_id']])("boxControlCollapse"), open = "mapping",
        multiple = FALSE,
        bsCollapsePanel(
          "mapping",
          column(
            12, offset = 0, style='padding:0px;',
            br(),
            paintr_get_ui(paintr_main(), "x"),
            paintr_get_ui(paintr_main(), "y"),
            paintr_get_ui(paintr_main(), "z"),
            paintr_get_ui(paintr_main(), "color"),
            paintr_get_ui(paintr_main(), "fill"),
            paintr_get_ui(paintr_main(), "shape"),
            paintr_get_ui(paintr_main(), "size", scope = "mapping")
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          paintr_get_ui(paintr_main(), "position"),
          paintr_get_ui(paintr_main(), "size", scope = "geom_args"),
          paintr_get_ui(paintr_main(), "alpha"),
          paintr_get_ui(paintr_main(), "stat"),
          paintr_get_ui(paintr_main(), "coord_flip"),
          paintr_get_ui(paintr_main(), "facet_grid"),
          paintr_get_ui(paintr_main(), "theme"),
          paintr_get_ui(paintr_main(), "theme_choose"),
          paintr_get_ui(paintr_main(), "labs")
        )
      ),
      h3("choose colors (if applicable)"),
      paintr_get_ui(paintr_main(), "scale_color"),
      h3("choose fills (if applicable)"),
      paintr_get_ui(paintr_main(), "scale_fill"),
    )
  })

}) %>% bindEvent(paintr_main(), dataContainer())

paintr_result <- reactive({
  req(dataContainer(), paintr_main())

  paintr_plot_code(paintr_main(),
                   result_container[['scale_color']],
                   result_container[['scale_fill']])


}) %>% bindEvent(input[["buttonDraw"]])

observe({
  req(paintr_result())

  output$mainPlot <- renderPlot({

    validate(need(paintr_result()[['plot']], "plot is not rendered"))

    paintr_result()[['plot']]
  })

  output$mycode <- renderText({

    paintr_result()[['code']]

  })

}) %>% bindEvent(input[["buttonDraw"]])











