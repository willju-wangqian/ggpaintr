# server part for the box chart
box_control_id <- "boxControl"

ns_box <- NS(box_control_id)

box_main <- reactive({
  req(dataContainer())

  # paintr_expr <- expr(
  #   geom_boxplot(aes(x, y, fill), position, stat) +
  #     coord_flip +
  #     facet_grid +
  #     theme(legend.direction, legend.position) +
  #     labs(x, y, title) +
  #     theme_choose
  # )
  #
  # result <- paintr(
  #   box_control_id,
  #   names(dataContainer()),
  #   !!paintr_expr
  # )

  result <- paintr(
    box_control_id,
    names(dataContainer()),
    geom_boxplot(aes(x, y, color, size), position, size_geom) +
      coord_flip +
      facet_grid +
      theme(legend.direction, legend.position) +
      labs(x, y, title) +
      theme_choose
  )

  result

}) %>% bindCache(input$drawBox) %>% bindEvent(input$drawBox)

selectedColors_box <- scaleColorWrapper(box_control_id, box_main, dataContainer,
                                        "color", "scaleColorUIOutput")

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
            paintr_get_ui(box_main(), "x"),
            paintr_get_ui(box_main(), "y"),
            paintr_get_ui(box_main(), "color"),
            paintr_get_ui(box_main(), "size", scope = "mapping")
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          h3("position"),
          paintr_get_ui(box_main(), "position"),
          paintr_get_ui(box_main(), "size", scope = "geom_args"),
          h3("misc"),
          paintr_get_ui(box_main(), "coord_flip"),
          paintr_get_ui(box_main(), "facet_grid"),
          h3("theme settings"),
          paintr_get_ui(box_main(), "theme"),
          br(),
          paintr_get_ui(box_main(), "theme_choose"),
          br(),
          paintr_get_ui(box_main(), "labs")
        )
      ),
      h3("choose colors (if applicable)"),
      # paintr_get_ui(box_main(), "scaleColor"),
      uiOutput(ns_box("scaleColorUIOutput")),
      br(),
      actionButton(NS(box_control_id)("buttonDraw"), "Draw the plot"),
      br()
    )
  })

}) %>% bindEvent(input$drawBox)


box_result <- reactive({
  req(dataContainer(), box_main())

  paintr_list <- paintr_plot_code(box_main(), box_control_id, dataContainer())

  scaleColors <- tryCatch(
    {
      if (!is.null(selectedColors_box())) {
        scaleColorHandler(box_control_id,
                          selectedColors_box(),
                          color_fill = 'color')
      } else {
        NULL
      }
    },
    error = function(cond) {
      return(NULL)
    }
  )

  # browser()
  #
  # scaleColors <- if (!is.null(selectedColors_box())) {
  #   scaleColorHandler(box_control_id,
  #                     selectedColors_box(),
  #                     color_fill = 'fill')
  # } else {
  #   NULL
  # }



  results <- get_plot_code(
    c(paintr_list,
      scale_color = list(scaleColors)),
    data = dataContainer(),
    data_path = result_container[['data']])

  results

}) %>% bindEvent(input[[NS(box_control_id)("buttonDraw")]])


observe({
  req(box_result())

  output$mainPlot <- renderPlot({

    validate(need(box_result()[['plot']], "plot is not rendered"))

    box_result()[['plot']]
  })

  output$mycode <- renderText({

    box_result()[['code']]

  })

}) %>% bindEvent(input[[NS(box_control_id)("buttonDraw")]])











