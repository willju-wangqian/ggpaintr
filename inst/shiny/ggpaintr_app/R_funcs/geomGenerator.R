#' Title
#'
#' @param id
#' @param data
#' @param geom_FUN
#' @param id_list
#' @param params_list
#' @param color_fill
#' @param color_group
#' @param userFun a function that returns a named list, where the names of
#' this named list are parameters (except for `mapping`) of `geom_FUN`, and the elements
#' of this list are arguments of the corresponding parameters
#' @param ... arguments that go into `userFUN`
#'
#' @return
#' @export
#'
#' @examples
ggGeomGenerator <- function(id, data, geom_FUN, id_list, params_list,
                            color_fill = FALSE, color_group = FALSE,
                            userFUN = NULL, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      # generate the basic plot
      # ggPlotObject <- reactive({

        geomArgList <- list()

        aesList <- connect_param_id(input,
                                    id_list[['mapping']],
                                    params = params_list[['mapping']],
                                    color_fill = color_fill,
                                    color_group = color_group)

        geomArgList <- if (!is.null(userFUN)) {
          userFUN(...)
        } else {
          connect_param_id(input,
                           id_list[['geom_args']],
                           params = params_list[['geom_args']])
        }

        geomArgList[['mapping']] <- do.call(aes_string, aesList)

        geomArgList <- check_remove_null(geomArgList)

        if(is.null(geomArgList)) {
          warning(paste0("no argument is passed into ", geom_FUN, "()." ))
        }

        geom_fun <- tryCatch(
          {
            match.fun(geom_FUN)
          },
          error = function(cond) {
            return(NULL)
          }
        )

        p <- do.call(geom_fun, geomArgList)

        # get code for aes
        aesList <- check_remove_null(aesList)
        aes_code <- paste_arg_param(aesList)
        aes_code <- paste0( "aes(", aes_code, ")" )

        # get code for geomArgList
        geomArgList <- geomArgList[-which(names(geomArgList) == 'mapping')]
        geomArgList <- check_remove_null(geomArgList)
        geomArg_code <- paste_arg_param(geomArgList)
        if (geomArg_code != "") {
          geomArg_code <- paste0(" ", geomArg_code)
        }

        final_code <- paste0(geom_FUN, "(", aes_code, geomArg_code, ")" )


        return(list(plot = p, code = final_code))

      # })

      # ggPlotObject

    }
  )
}

colorGenerator <- function(id, dataColor, fillID, scaleColorID, colorPalette = "RdYlBu") {
  moduleServer(
    id,
    function(input, output, session) {

      req(input[[fillID]])

      assert_that(
        hasName(dataColor, input[[fillID]])
      )

      if(is.null(scaleColorID)) {
        return(NULL)
      }

      color_var <- dataColor[[input[[fillID]]]]
      ns <- NS(id)

      if (is.character(color_var) || is.factor(color_var) ) {
        num_color <- length(unique( color_var ))

        TOO_MANY_LEVELS <- num_color > 11

        if(TOO_MANY_LEVELS) {
          return(list(type = "TOO_MANY_LEVELS"))
        }

        init_colors <- RColorBrewer::brewer.pal(num_color, "RdYlBu")
        labels <- unique( color_var )

        colorPickers <- multipleColorPickerUI(ns, init_colors, labels)

        return(c(colorPickers, type = "categorical"))

      } else if ( is.numeric(color_var) ) {

        init_colors <- RColorBrewer::brewer.pal(11, "RdBu")[c(9,3)]
        labels <- c('low', 'high')
        colorPickers <- multipleColorPickerUI(ns, init_colors, labels)

        return(c(colorPickers, type = "numerical"))
      } else {
        return(NULL)
      }
    }
  )
}


scaleColorHandler <- function(id, selected_colors, color_fill) {
  moduleServer(
    id,
    function(input, output, session) {

      color_fill_options <- c("color", "fill")

      if (is.null(selected_colors) || (!( color_fill %in% color_fill_options )) ) {
        return(NULL)
      }

      if (selected_colors[['type']] == "numerical") {
        assert_that(
          length(selected_colors[['id']]) == 2
        )

        colors <- lapply(selected_colors[['id']], function(ii) {
          input[[ii]]
        })
        names(colors) <- c("low", "high")

        code <- paste_arg_param(colors)


        if (color_fill == "color") {
          return(list(plot = do.call(scale_color_gradient, colors),
                      code = paste0("scale_color_gradient(", code, ")")))
        } else {
          return(list(plot = do.call(scale_fill_gradient, colors),
                      code = paste0("scale_fill_gradient(", code, ")")))
        }

      } else if (selected_colors[['type']] == "categorical") {

        colors <- sapply(selected_colors[['id']], function(ii) {
          input[[ii]]
        })

        names(colors) <- NULL

        if (color_fill == "color") {
          return(list(plot = scale_color_manual(values = colors),
                      code =  paste0("scale_color_manual(values = c(",
                                     paste(shQuote(colors, type="csh"), collapse=", "),
                                     "))") ))
        } else {
          return(list(plot = scale_fill_manual(values = colors),
                      code =  paste0("scale_fill_manual(values = c(",
                                     paste(shQuote(colors, type="csh"), collapse=", "),
                                     "))") ))
        }

      } else {
        return(NULL)
      }

    }
  )
}



# scaleColorServer <- function(id, color_id, scaleColor_id, box_main, dataContainer) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       selectedColors <- reactive({
#         req(box_main(), dataContainer())
#
#         colorGenerator(id,
#                        dataContainer(),
#                        color_id,
#                        scaleColor_id)
#
#       }) %>%
#         bindCache(input[[color_id]]) %>%
#         bindEvent(input[[color_id]], ignoreNULL =  FALSE)
#
#       browser()
#
#       return(selectedColors)
#     }
#   )
# }


# scaleColorRenderUI <- function(id, color_id, scaleColor_id, box_main, selectedColors) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#       observe({
#         req(selectedColors(), box_main())
#
#         if(selectedColors()[['type']] == "TOO_MANY_LEVELS") {
#           output[[scaleColor_id]] <- renderUI({
#             validate(paste( paste0("There are more than 11 levels in ", input[[color_id]], "."),
#                             "Too many levels.", sep = "\n"))
#           })
#         } else {
#           output[[scaleColor_id]] <- renderUI({
#             selectedColors()[['ui']]
#           })
#         }
#
#       }) %>% bindEvent(input[[color_id]])
#
#     }
#   )
#
# }






#' Title
#'
#' @param id
#' @param data
#'
#' @return
#' @export
#'
#' @examples
geomBarGenerator <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      # generate the basic plot
      ggPlotObject <- reactive({

        barArgList <- list()

        aesList <- list()

        # browser()

        if(!is.null(input$mapX)) aesList$x <- input$mapX
        if(!is.null(input$mapY)) aesList$y <- input$mapY
        if(!is.null(input$mapColor)) {
          aesList$color <- input$mapColor
          aesList$fill <- input$mapColor
          aesList$group <- input$mapColor
        }
        if(!is.null(input$settingBarStat)) {
          barArgList$stat = input$settingBarStat
          # if(input$mapStat == "bin" & has_name(aesList, y)) {
          #   cat("check1\n")
          #   aesList <- aesList[- which(names(aesList) == "y")]
          #   cat("good\n")
          # }
        }

        if(!is.null(input$settingBarPosition)) {
          if(is.na(input$settingBarPositionWidth))
            widthParam <- NULL
          else
            widthParam <- input$settingBarPositionWidth

          if(input$settingBarPosition == "position_dodge") {
            barArgList$position <- getFromNamespace(input$settingBarPosition, "ggplot2")(
              width = widthParam
            )
          } else {
            barArgList$position <- getFromNamespace(input$settingBarPosition, "ggplot2")()
          }

          # barArgList$position <- getFromNamespace(input$settingBarPosition, "ggplot2")(
          #   width = widthParam
          # )
        }

        if(is.na(input$settingBarWidth))
          barArgList$width <- NULL
        else
          barArgList$width <- input$settingBarWidth

        barArgList$mapping = do.call(aes_string, aesList)

        do.call(geom_col, barArgList)

      })

      ggPlotObject

    }
  )
}

#' Title
#'
#' @param id
#' @param data
#'
#' @return
#' @export
#'
#' @examples
geomTextGenerator <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      # generate the basic plot
      ggPlotObject <- reactive({

        textArgList <- list()

        aesList <- list()

        if(!is.null(input$mapX)) aesList$x <- input$mapX
        if(!is.null(input$mapY)) {
          aesList$y <- input$mapY
          aesList$label <- paste0("round(", input$mapY, ",", input$settingTextRounding, ")")
        }
        if(!is.null(input$mapColor)) {
          aesList$group <- input$mapColor
        }


        if(!is.null(input$settingTextPosition)) {
          if(is.na(input$settingTextPositionWidth))
            widthParam <- NULL
          else
            widthParam <- input$settingTextPositionWidth

          textArgList$position <- getFromNamespace(input$settingTextPosition, "ggplot2")(
            width = widthParam
          )
        }

        if(!is.na(input$settingTextSize))
          textArgList$size <- rel(input$settingTextSize)

        if(!is.na(input$settingTextHjust))
          textArgList$hjust <- input$settingTextHjust

        if(!is.na(input$settingTextVjust))
          textArgList$vjust <- input$settingTextVjust

        textArgList$mapping = do.call(aes_string, aesList)

        do.call(geom_text, textArgList)

      })

      ggPlotObject

    }
  )
}


#' Title
#'
#' @param id
#' @param data
#'
#' @return
#' @export
#'
#' @examples
geomLinerangeGenerator <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      # generate the basic plot
      ggPlotObject <- reactive({

        linerangeArgList <- list()

        aesList <- list()

        # mapping
        if(!is.null(input$mapX)) aesList$x <- input$mapX
        if(!is.null(input$mapY)) {
          aesList$ymax <- input$mapY
          aesList$ymin <- 0
        }
        if(!is.null(input$mapColor)) {
          aesList$color <- input$mapColor
          # aesList$fill <- input$mapColor
          aesList$group <- input$mapColor
        }

        # position and position width
        if(!is.null(input$settingLinerangePosition)) {
          if(is.na(input$settingLinerangePositionWidth))
            widthParam <- NULL
          else
            widthParam <- input$settingLinerangePositionWidth

          linerangeArgList$position <- getFromNamespace(input$settingLinerangePosition, "ggplot2")(
            width = widthParam
          )
        }

        # alpha
        if(is.na(input$settingLinerangeAlpha))
          linerangeArgList$alpha <- NULL
        else
          linerangeArgList$alpha <- input$settingLinerangeAlpha

        # size
        if(is.na(input$settingLinerangeSize))
          linerangeArgList$size <- NULL
        else
          linerangeArgList$size <- input$settingLinerangeSize

        linerangeArgList$mapping = do.call(aes_string, aesList)

        # hardcoding for show.legend
        linerangeArgList$show.legend <- FALSE

        do.call(geom_linerange, linerangeArgList)

      })

      ggPlotObject

    }
  )
}


#' Title
#'
#' @param id
#' @param data
#'
#' @return
#' @export
#'
#' @examples
geomPointGenerator <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      # generate the basic plot
      ggPlotObject <- reactive({

        pointArgList <- list()

        aesList <- list()

        # mapping
        if(!is.null(input$mapX)) aesList$x <- input$mapX
        if(!is.null(input$mapY)) {
          aesList$y <- input$mapY
        }
        if(!is.null(input$mapColor)) {
          aesList$color <- input$mapColor
          aesList$fill <- input$mapColor
          aesList$group <- input$mapColor
        }
        if(!is.null(input$mapShape)) {
          aesList$shape <- input$mapShape
        }

        # position and position width
        if(!is.null(input$settingPointPosition)) {
          if(is.na(input$settingPointPositionWidth))
            widthParam <- NULL
          else
            widthParam <- input$settingPointPositionWidth

          pointArgList$position <- getFromNamespace(input$settingPointPosition, "ggplot2")(
            width = widthParam
          )
        }

        # alpha
        if(is.na(input$settingPointAlpha))
          pointArgList$alpha <- NULL
        else
          pointArgList$alpha <- input$settingPointAlpha

        # size
        if(is.na(input$settingPointSize))
          pointArgList$size <- NULL
        else
          pointArgList$size <- input$settingPointSize

        pointArgList$mapping = do.call(aes_string, aesList)

        do.call(geom_point, pointArgList)

      })

      ggPlotObject

    }
  )
}

#' Title
#'
#' @param id
#' @param data
#'
#' @return
#' @export
#'
#' @examples
geomLineGenerator <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {

      # generate the basic plot
      ggPlotObject <- reactive({

        lineArgList <- list()

        aesList <- list()

        # mapping
        if(!is.null(input$mapX)) aesList$x <- input$mapX
        if(!is.null(input$mapY)) {
          aesList$y <- input$mapY
        }
        if(!is.null(input$mapColor)) {
          aesList$color <- input$mapColor
          # aesList$fill <- input$mapColor
          aesList$group <- input$mapColor
        }

        # position and position width
        # if(!is.null(input$settingPointPosition)) {
        #   if(is.na(input$settingPointPositionWidth))
        #     widthParam <- NULL
        #   else
        #     widthParam <- input$settingPointPositionWidth
        #
        #   lineArgList$position <- getFromNamespace(input$settingPointPosition, "ggplot2")(
        #     width = widthParam
        #   )
        # }

        # alpha
        if(is.na(input$settingLineAlpha))
          lineArgList$alpha <- NULL
        else
          lineArgList$alpha <- input$settingLineAlpha

        # size
        if(is.na(input$settingLineSize))
          lineArgList$size <- NULL
        else
          lineArgList$size <- input$settingLineSize

        lineArgList$mapping = do.call(aes_string, aesList)

        do.call(geom_line, lineArgList)

      })

      ggPlotObject

    }
  )
}





