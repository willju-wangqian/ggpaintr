#################

#' Title
#'
#' @param user_defined
#'
#' @return
#' @export
#'
#' @examples
getControlList <- function(scope = "mapping", type = "ui") {

  type <- match.arg(type, c("ui", "handler"))
  scope <- match.arg(scope, c("mapping", "geom_args", "plot_settings"))

  uiControlList <- list(
    mapping = list(x = "mappingXUI",
                   y = "mappingYUI",
                   z = "mappingZUI",
                   color = "mappingColorUI",
                   shape = "mappingShapeUI",
                   size = "mappingSizeUI",
                   fill = "mappingFillUI",
                   group = "mappingGroupUI"),
    geom_args = list(stat = "argsStatUI",
                     position = "argsPositionUI",
                     alpha = "argsAlphaUI",
                     size = "argsSizeUI"),
    plot_settings = list(theme = "themeUI",
                         theme_choose = "themeChooseUI",
                         coord_flip = "settingFlipUI",
                         facet_grid = "settingFacetUI",
                         labs = "labsUI",
                         scaleColor = "scaleColorUI",
                         scaleFill = "scaleFillUI")
  )

  handlerControlList <- list(
    labs = "labsHandler",
    theme = "themeHandler",
    theme_choose = "themeChooseHandler",
    facet_grid = "facetHandler",
    coord_flip = "flipHandler",
    scaleColor = "scaleColorFillHandler",
    scaleFill = "scaleColorFillHandler"
  )

  if (type == "ui") {
    return(uiControlList[[scope]])
  }

  if (type == "handler") {
    return(handlerControlList)
  }

}

#' Title
#'
#' @param ui_part
#'
#' @return
#' @export
#'
#' @examples
matchControls <- function(selected, scope = "mapping", type = "ui") {

  controlList <- getControlList(scope, type)

  if ( is.null(controlList[[selected]]) ) {
    warning( paste("The", type, "part for", selected, "has not been implemented yet.")  )
    return(NULL)
  } else {
    # return( getFromNamespace(controlList[[selected]], asNamespace("ggpaintr")) )
    return( match.fun(controlList[[selected]]) )
  }

}

#' Title
#'
#' @param name
#' @param mp
#' @param defaultArgs
#' @param extraFunc optional. A named list of extra functions provided by the user.
#' For example `list(param1 = my_func1, param2 = my_func2)`
#' @param extraFuncArgs optional. A list of function arguments provided by the user.
#' Function arguments of one function should be formed in a list as one element of `extraFuncArgs`
#' For example `list(param1 = list(my_func1_arg1, my_func1_arg2), param2 = list(my_func2_arg1, my_func2_arg2))`
#'
#'
#' @note `extraFunc` and `extraFuncArgs` allow users to override
#'
#'
#' @return
#' @export
#'
#' @examples
callFuncUI <- function(name, defaultArgs, scope, extraFunc = NULL, extraFuncArgs = NULL) {

  if ( is.null(name) ) {
    return(NULL)
  }

  UI_FUN <- if (!is.null(extraFunc[[name]])) {
    extraFunc[[name]]
  } else {
    matchControls(name, scope, type = "ui")
  }

  if( !is.null(UI_FUN) ) {

    # UI_FUN_args_names <- names(formals(UI_FUN))[sapply(formals(UI_FUN), is.symbol)]
    UI_FUN_args_names <- names(formals(UI_FUN))

    UI_FUN_args <- if( !is.null(extraFuncArgs[[name]]) ) {
      extraFuncArgs[[name]]
    } else {
      defaultArgs[UI_FUN_args_names]
    }

    return( do.call(UI_FUN, check_remove_null(UI_FUN_args)) )
  } else {
    return(NULL)
  }

}

#' Title
#'
#' @param id
#' @param data
#' @param mapping
#' @param geom_args
#' @param extra_uiFunc
#' @param extra_uiFuncArgs
#'
#' @return
#' @export
#'
#' @examples
controlUI <- function(id, data_vars, mapping, defaultArgs, geom_args = NULL, plot_settings = NULL,
                      extra_uiFunc = NULL, extra_uiFuncArgs = NULL) {
  ns <- NS(id)

  mapping <- check_char_set_names(mapping)
  geom_args <- check_char_set_names(geom_args)
  plot_settings <- check_char_set_names(plot_settings)

  mapping_ui <- mapply(callFuncUI, names(mapping),
                       MoreArgs = list(
                         defaultArgs = defaultArgs, # list(ns = ns, data_vars = data_vars),
                         scope = "mapping",
                         extraFunc = extra_uiFunc,
                         extraFuncArgs = extra_uiFuncArgs
                       ),
                       SIMPLIFY = FALSE)

  geom_args_ui <- mapply(callFuncUI, names(geom_args),
                         MoreArgs = list(
                           defaultArgs = defaultArgs,
                           scope = "geom_args",
                           extraFunc = extra_uiFunc,
                           extraFuncArgs = extra_uiFuncArgs
                         ),
                         SIMPLIFY = FALSE)

  plot_settings_ui <- mapply(callFuncUI, names(plot_settings),
                             MoreArgs = list(
                               defaultArgs = defaultArgs,
                               scope = "plot_settings",
                               extraFunc = extra_uiFunc,
                               extraFuncArgs = extra_uiFuncArgs
                             ),
                             SIMPLIFY = FALSE)


  mapping_ui <- check_remove_null(mapping_ui)
  geom_args_ui <- check_remove_null(geom_args_ui)
  plot_settings_ui <- check_remove_null(plot_settings_ui)

  result <- list(
    ui = list(mapping = empty_list_null(purrr::map(mapping_ui, 1)),
              geom_args = empty_list_null(purrr::map(geom_args_ui, 1)),
              plot_settings = empty_list_null(purrr::map(plot_settings_ui, 1))),
    id = list(mapping = empty_list_null(purrr::map(mapping_ui, 2)),
               geom_args = empty_list_null(purrr::map(geom_args_ui, 2)),
               plot_settings = empty_list_null(purrr::map(plot_settings_ui, 2)))
  )

  return(result)

}
