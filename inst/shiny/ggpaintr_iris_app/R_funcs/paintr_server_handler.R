#' Generate the geom component of a `ggplot2` plot
#'
#' @param id id for the module server
#' @param data data
#' @param geom_FUN `geom_<chart>`
#' @param id_list list of id of all ui elements
#' @param params_list list of parameters that the ui elements should correspond to
#' @param color_fill bool; optional. Whether or not to use the same variable for both color and fill
#' @param color_group bool; optional. Whether or not to use the same variable for both color and group
#' @param userFUN a function that returns a named list, where the names of
#' this named list are parameters (except for `mapping`) of `geom_<chart>`, and the elements
#' of this list are arguments of the corresponding parameters
#' @param ... arguments that go into `userFUN`
#'
#' @return list of plot and code of a geom component of a `ggplot2` plot
#'
#' @import ggplot2
#'
#' @export
ggGeomHandler <- function(id, data, geom_FUN, id_list, params_list,
                            color_fill = FALSE, color_group = FALSE,
                            userFUN = NULL, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      # generate the basic plot

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


    }
  )
}


#' Module server for `coord_flip()`
#'
#' @param id id for the module server
#' @param module_id the id of the ui element (not including its prefix created by
#' the name space) which determines whether or not to call `coord_flip()`
#'
#' @import ggplot2
#'
#' @return `coord_flip()` and its code or `NULL`
#' @export
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


#' Module server for `facet_grid()`
#'
#' @param id id for the module server
#' @param module_id the id of the ui element (not including its prefix created by
#' the name space) which gives input to `facet_grid()`
#'
#' @import ggplot2 stats
#'
#' @return `facet_grid()` and its code or `NULL`
#' @export
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

#' Module server for `theme()`
#'
#' @param id id for the module server
#' @param module_id the id of the ui elements (not including its prefix created by
#' the name space) which gives input as arguments of `theme()`
#' @param param parameters of `theme()` that correspond to the ui elements
#'
#' @note the order of `param` should match the order of `module_id`. Currently the
#' following parameters of `theme()` are implemented
#'  - `legend.position`
#'  - `legend.direction`
#'  - `legend.box`
#'
#' @return `theme()` and its code or `NULL`
#'
#' @export
themeHandler <- function(id, module_id, param) {
  stringParamHandler(id, module_id, param, "theme")
}


#' Module server for `labs()`
#'
#' @param id id for the module server
#' @param module_id the id of the ui elements (not including its prefix created by
#' the name space) which gives input as arguments of `labs()`
#' @param param parameters of `labs()` that correspond to the ui elements
#'
#' @note the order of `param` should match the order of `module_id`. Currently the
#' following parameters of `labs()` are implemented
#'  - `x`
#'  - `y`
#'  - `title`
#'  - `subtitle`
#'
#' @return `labs()` and its code or `NULL`
#' @export
labsHandler <- function(id, module_id, param) {
  stringParamHandler(id, module_id, param, "labs")
}


#' Module server for a `ggplot2` function which takes strings as arguments
#'
#' @param id id for the module server
#' @param module_id the id of the ui elements (not including its prefix created by
#' the name space) which gives input as arguments of `FUN()`
#' @param param parameters of `theme()` that correspond to the ui elements
#' @param FUN a `ggplot2` function which takes strings as arguments
#'
#' @return the return of `FUN` and its code or `NULL`
#' @export
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


#' Module server that provides several `ggplot2` themes
#'
#' @param id id for the module server
#' @param module_id the id of the ui elements (not including its prefix created by
#' the name space)
#'
#' @note currently the following themes of `ggplot2` are implemented:
#'  - `theme_gray()`
#'  - `theme_classic()`
#'  - `theme_bw()`
#'  - `theme_minimal()`
#'
#' @return an implemented `ggplot2` theme and its code or `NULL`
#' @export
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

#' The inner module server that sets colors through `scale_fill_gradient`, `scale_fill_manual`,
#' `scale_color_gradient`, `scale_color_manual`
#'
#' @param id id for the module server
#' @param selected_colors the selected colors from input
#' @param color_fill string. either `color` or `fill`. Specifies whether it's `scale_color`
#' or `scale_fill`
#'
#' @return the return of `scale_<fill/color>_<gradient/manual>` and its code or `NULL`
#'
#' @import ggplot2
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


#' Module server that sets colors through `scale_fill_gradient`, `scale_fill_manual`,
#' `scale_color_gradient`, `scale_color_manual`
#'
#' @param id id for the module server
#' @param selected_color_fill_rctv the reactive values returned by `scaleColor_build_reactivity()`
#' @param color_fill string. either `color` or `fill`. Specifies whether it's `scale_color`
#' or `scale_fill`
#'
#' @note this function handles the `error` when `selected_color_fill_rctv()` returns error
#'
#' @return the return of `scale_<fill/color>_<gradient/manual>` and its code or `NULL`
#' @export
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






