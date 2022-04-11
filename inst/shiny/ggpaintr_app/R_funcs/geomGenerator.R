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
      ggPlotObject <- reactive({

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
          warning(paste("no argument is passed into", as.character(quote(geom_FUN)) ))
        }

        do.call(geom_FUN, geomArgList)

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





