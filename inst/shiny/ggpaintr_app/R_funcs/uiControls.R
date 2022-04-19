#' Title
#'
#' @param id
#' @param data
#'
#' @return
#' @export
#'
#' @examples
boxControlUI <- function(id, data) {
  ns <- NS(id)

  mapping_part <- list(x="mapX", y="mapY", color="mapColor")

  ui_part <- bsCollapse(
    id = ns("boxControlCollapse"), open = "mapping", multiple = FALSE,
    bsCollapsePanel(
      "mapping",
      column(12, offset = 0, style='padding:0px;',
             br(),
             mappingXUI(ns, data, mapping_part$x),
             mappingYUI(ns, data, mapping_part$y),
             mappingColorUI(ns, data, mapping_part$color)
      )
    ),
    bsCollapsePanel(
      "advanced settings",
      # h3("bar settings"),
      # barSettingUI(ns),
      # br(),
      # h3("label settings"),
      # checkboxInput(ns("addTextButton"), "Add labels", value = FALSE, width = NULL),
      # textSettingUI(ns),
      br(),
      h3("misc"),
      miscUI(ns, data),
      br(),
      h3("theme settings"),
      themeUI(ns)
    )
  )

  return(list(ui = ui_part, mapping = mapping_part))

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
barControlUI <- function(id, data) {
  ns <- NS(id)

  bsCollapse(
    id = ns("barControlCollapse"), open = "mapping", multiple = FALSE,
    bsCollapsePanel(
      "mapping",
      mappingUI(ns, data)
    ),
    bsCollapsePanel(
      "advanced settings",
      h3("bar settings"),
      barSettingUI(ns),
      br(),
      h3("label settings"),
      checkboxInput(ns("addTextButton"), "Add labels", value = FALSE, width = NULL),
      textSettingUI(ns),
      br(),
      h3("misc"),
      miscUI(ns, data),
      br(),
      h3("theme settings"),
      themeUI(ns)
    )
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
lolliControlUI <- function(id, data) {
  ns <- NS(id)

  bsCollapse(
    id = ns("lolliControlCollapse"), open = "mapping", multiple = FALSE,
    bsCollapsePanel(
      "mapping",
      mappingUI(ns, data)
    ),
    bsCollapsePanel(
      "advanced settings",
      h3("lollipop settings"),
      lolliSettingUI(ns),
      br(),
      h3("label settings"),
      checkboxInput(ns("addTextButton"), "Add labels", value = FALSE, width = NULL),
      textSettingUI(ns),
      br(),
      h3("misc"),
      miscUI(ns, data),
      br(),
      h3("theme settings"),
      themeUI(ns)
    )
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
lineControlUI <- function(id, data) {
  ns <- NS(id)

  bsCollapse(
    id = ns("lineControlCollapse"), open = "mapping", multiple = FALSE,
    bsCollapsePanel(
      "mapping",
      mappingUI(ns, data)
    ),
    bsCollapsePanel(
      "advanced settings",
      h3("line settings"),
      lineSettingUI(ns),
      br(),
      h3("label settings"),
      checkboxInput(ns("addTextButton"), "Add labels", value = FALSE, width = NULL),
      textSettingUI(ns),
      br(),
      h3("misc"),
      miscUI(ns, data),
      br(),
      h3("theme settings"),
      themeUI(ns)
    )
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
pointControlUI <- function(id, data) {
  ns <- NS(id)

  bsCollapse(
    id = ns("pointControlCollapse"), open = "mapping", multiple = FALSE,
    bsCollapsePanel(
      "mapping",
      # mappingUI(ns, data)
      column(12, offset = 0, style='padding:0px;',
        mappingXUI(ns, data),
        mappingYUI(ns, data),
        mappingColorUI(ns, data),
        mappingShapeUI(ns, data)
      )
    ),
    bsCollapsePanel(
      "advanced settings",
      h3("point settings"),
      pointSettingUI(ns),
      br(),
      h3("label settings"),
      checkboxInput(ns("addTextButton"), "Add labels", value = FALSE, width = NULL),
      textSettingUI(ns),
      br(),
      h3("misc"),
      miscUI(ns, data),
      br(),
      h3("theme settings"),
      themeUI(ns)
    )
  )
}

#################

#' Title
#'
#' @param user_defined
#'
#' @return
#' @export
#'
#' @examples
getUIControlList <- function(user_defined) {
  uiControlList <- list(
    x = "mappingXUI",
    y = "mappingYUI",
    color = "mappingColorUI",
    shape = "mappingShapeUI",
    size = "mappingSizeUI",
    fill = "mappingFillUI",
    group = "mappingGroupUI",
    theme = "themeUI",
    misc = "miscUI",
    scaleColor = "scaleColorUI"
  )
  uiControlList
}

#' Title
#'
#' @param ui_part
#'
#' @return
#' @export
#'
#' @examples
matchUIControls <- function(ui_part) {

  uiControlList <- getUIControlList()

  if ( is.null(uiControlList[[ui_part]]) ) {
    warning( paste("The ui part for", ui_part, "has not been implemented yet.")  )
    return(NULL)
  } else {
    # return( getFromNamespace(uiControlList[[ui_part]], asNamespace("ggpaintr")) )
    return( match.fun(uiControlList[[ui_part]]) )
  }

}

#' Title
#'
#' @param name
#' @param mp
#' @param defaultArgs
#' @param extraFunc
#' @param extraFuncArgs
#'
#' @return
#' @export
#'
#' @examples
callFuncUI <- function(name, mp, defaultArgs, extraFunc, extraFuncArgs) {

  if ( is.null(name) || is.null(mp) ) {
    return(NULL)
  }

  UI_FUN <- if (!is.null(extraFunc[[name]])) {
    extraFunc[[name]]
  } else {
    matchUIControls(name)
  }

  if( !is.null(UI_FUN) ) {

    UI_FUN_args_names <- names(formals(UI_FUN))[sapply(formals(UI_FUN), is.symbol)]

    UI_FUN_args <- if( !is.null(extraFuncArgs[[name]]) ) {
      extraFuncArgs[[name]]
    } else {
      defaultArgs[UI_FUN_args_names]
    }

    return( do.call(UI_FUN, UI_FUN_args) )
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
controlUI <- function(id, data, mapping, geom_args = NULL, plot_settings = NULL,
                      extra_uiFunc = NULL, extra_uiFuncArgs = NULL) {
  ns <- NS(id)

  mapping <- check_char_set_names(mapping)
  geom_args <- check_char_set_names(geom_args)
  plot_settings <- check_char_set_names(plot_settings)

  mapping_ui <- mapply(callFuncUI, names(mapping), mapping,
                       MoreArgs = list(
                         defaultArgs = list(ns = ns, data = data),
                         extraFunc = extra_uiFunc,
                         extraFuncArgs = extra_uiFuncArgs
                       ),
                       SIMPLIFY = FALSE)

  geom_args_ui <- mapply(callFuncUI, names(geom_args), geom_args,
                         MoreArgs = list(
                           defaultArgs = list(ns = ns, data = data),
                           extraFunc = extra_uiFunc,
                           extraFuncArgs = extra_uiFuncArgs
                         ),
                         SIMPLIFY = FALSE)

  plot_settings_ui <- mapply(callFuncUI, names(plot_settings), plot_settings,
                             MoreArgs = list(
                               defaultArgs = list(ns = ns, data = data),
                               extraFunc = extra_uiFunc,
                               extraFuncArgs = extra_uiFuncArgs
                             ),
                             SIMPLIFY = FALSE)


  mapping_ui <- check_remove_null(mapping_ui)
  geom_args_ui <- check_remove_null(geom_args_ui)
  plot_settings_ui <- check_remove_null(plot_settings_ui)

  result <- list(
    ui = list(mapping_ui = empty_list_null(purrr::map(mapping_ui, 1)),
              geom_args_ui = empty_list_null(purrr::map(geom_args_ui, 1)),
              plot_settings_ui = empty_list_null(purrr::map(plot_settings_ui, 1))),
    ids = list(mapping = empty_list_null(purrr::map(mapping_ui, 2)),
               geom_args = empty_list_null(purrr::map(geom_args_ui, 2)),
               plot_settings = empty_list_null(purrr::map(plot_settings_ui, 2)))
  )

  return(result)

}
