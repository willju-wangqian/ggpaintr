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
ggGeomHandler <- function(id, data, geom_FUN, id_list, params_list,
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
      geomArg_code <- paste_arg_param(geomArgList, add_quo = TRUE)
      if (geomArg_code != "") {
        geomArg_code <- paste0(", ", geomArg_code)
      }

      final_code <- paste0(geom_FUN, "(", aes_code, geomArg_code, ")" )


      return(list(plot = p, code = final_code))

      # })

      # ggPlotObject

    }
  )
}


#' Module server for `coord_flip()`
#'
#' @param id a string which becomes a name space for the module server
#' @param module_id the id of the ui element (not including its prefix created by
#' the name space) which determines whether or not to call `coord_flip()`
#'
#' @return `NULL` or `coord_flip()`
#' @export
#'
#' @examples
#' \dontrun{
#' flipHandler("my_boxplot", "settingFlip")
#' }
flipHandler <- function(id, module_id) {
  moduleServer(
    id,
    function(input, output, session) {
      if(is.null(module_id) || is.null(input[[module_id]])) {
        return(NULL)
      }


      if(input[[module_id]]) {
        return(list(plot = coord_flip(),
                    code = "coord_flip()"))
      } else {
        return(NULL)
      }

    }
  )
}


#' Title
#'
#' @param id
#' @param pp
#' @param module_id
#'
#' @return
#' @export
#'
#' @examples
facetHandler <- function(id, module_id) {
  moduleServer(
    id,
    function(input, output, session) {

      if(is.null(module_id)) return(NULL)

      if(!is.null(input[[module_id]])) {
        selectedVars <- input[[module_id]]

        ff <- NULL
        code <- NULL
        if(length(selectedVars) == 2) {
          code <- paste(selectedVars[1], "~", selectedVars[2])
          ff <- as.formula(code)
        } else {
          code <- paste(selectedVars[1], "~ .")
          ff <- as.formula(code)
        }
        return(list(plot = facet_grid(ff),
                    code = paste0("facet_grid(", code, ")")))
      } else {
        return(NULL)
      }

    }
  )
}

#' theme()
#'
#' @param id
#' @param module_id
#' @param theme_param
#'
#' @return
#' @export
#'
#' @examples
themeHandler <- function(id, module_id, param) {
  stringParamHandler(id, module_id, param, "theme")
}


labsHandler <- function(id, module_id, param) {
  stringParamHandler(id, module_id, param, "labs")
}


stringParamHandler <- function(id, module_id, param, FUN) {
  moduleServer(
    id,
    function(input, output, session) {

      if( any(is.na(module_id)) || any(is.na(param)) ) return(NULL)

      assert_that(
        length(module_id) == length(param)
      )

      settingArgsList <- lapply(module_id, function(mm_id) {
        if(is.null(input[[mm_id]])) return(NULL)
        if (input[[mm_id]] == '') return(NULL)
        input[[mm_id]]
      })

      names(settingArgsList) <- param

      settingArgsList <- check_remove_null(settingArgsList)

      # browser()

      if( is.null(settingArgsList) ) {
        return(NULL)
      } else {

        code <- paste_arg_param(settingArgsList, add_quo = TRUE)
        code <- paste0(FUN, "(", code, ")")

        return(list(plot = do.call(eval(rlang::parse_expr(FUN)), settingArgsList),
                    code = code))
      }
    }
  )
}


#' Title
#'
#' @param id
#' @param module_id
#'
#' @return
#' @export
#'
#' @examples
themeChooseHandler <- function(id, module_id) {
  moduleServer(
    id,
    function(input, output, session) {

      if (is.null(module_id) || is.null(input[[module_id]])) {
        return(NULL)
      }

      return( list(plot = match.fun(input[[module_id]])(),
                   code = paste0(input[[module_id]], "()")) )

    }
  )
}

#' Title
#'
#' @param id
#' @param selected_colors
#' @param color_fill
#'
#' @return
#' @export
#'
#' @examples
scaleColorHandler_inner <- function(id, selected_colors, color_fill) {
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


scaleColorFillHandler <- function(id, selected_color_fill_rctv, color_fill) {
  if (is.null(selected_color_fill_rctv)) return(NULL)

  tryCatch(
    {
      if (!is.null(selected_color_fill_rctv())) {
        scaleColorHandler_inner(id,
                                selected_color_fill_rctv(),
                                color_fill = color_fill)
      } else {
        NULL
      }
    },
    error = function(cond) {
      return(NULL)
    }
  )
}






