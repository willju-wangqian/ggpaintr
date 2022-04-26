# server part for the box chart
box_control_id <- "boxControl"

ns_box <- NS(box_control_id)

box_main <- reactive({
  req(dataContainer())

  result <- paintr_components(
    box_control_id, names(dataContainer()),
    geom_boxplot(aes(x, y, fill)) + coord_flip + facet_grid +
      theme(legend.position, legend.direction) + labs(x, y, title, subtitle) + theme_choose)

  result

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
            paintr_get_ui(box_main(), "x"),
            paintr_get_ui(box_main(), "y"),
            paintr_get_ui(box_main(), "fill")
          )
        ),
        bsCollapsePanel(
          "advanced settings",
          h3("misc"),
          paintr_get_ui(box_main(), "coord_flip"),
          paintr_get_ui(box_main(), "facet_grid"),
          h3("theme settings"),
          paintr_get_ui(box_main(), "theme"),
          br(),
          paintr_get_ui(box_main(), "theme_choose")
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


tt <- reactive({
  req(dataContainer(), box_main())

  # paintr_list <- paintr_plot_code(box_main(), box_control_id, dataContainer(),
  #                                 input, output, session)

  paintr_list <- paintr_plot_code(box_main(), box_control_id, dataContainer())

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

  results <- get_plot_code(
    c(paintr_list,
      scale_color = list(scaleColors)),
    data = dataContainer(),
    data_path = result_container[['data']])

  result_container[['plot']] <- results[['plot']]
  result_container[['code']] <- results[['code']]

  result_container

}) %>% bindEvent(input[[NS(box_control_id)("buttonDraw")]])


observe({
  req(tt())

  output$mainPlot <- renderPlot({

    validate(need(tt()[['plot']], "plot is not rendered"))

    tt()[['plot']]
  })

  output$mycode = renderText({

    tt()[['code']]

  })

  # browser()
}) %>% bindEvent(input[[NS(box_control_id)("buttonDraw")]])

#
# selectedColors_box <-  scaleColorWrapper(box_control_id, box_main, dataContainer,
#                                          "fill", "scaleColor")


# selectedColors_box <- reactive({
#
#   req(box_main(), dataContainer())
#
#   result <- scaleColorWrapper(box_control_id, box_main, dataContainer,
#                               "fill", "scaleColor")
#   browser()
#   result()
# })




scaleColorIDs_box <- reactive({
  req(box_main())

  fill <-  paintr_get_ui(box_main(), "fill", type = "id")
  # scaleColor <- paintr_get_ui(box_main(), "scaleColor", type = "id")
  scaleColor <- "scaleColorUIOutput"

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








