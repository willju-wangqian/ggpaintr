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
    shape = "mappingShapeUI"
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
    return( getFromNamespace(uiControlList[[ui_part]], asNamespace("ggpaintr")) )
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

    UI_FUN_args <- if( !is.null(extraFuncArgs[[name]]) ) {
      extraFuncArgs[[name]]
    } else {
      c(defaultArgs, list(mp))
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
#' @param other_arguments
#' @param extra_uiFunc
#' @param extra_uiFuncArgs
#'
#' @return
#' @export
#'
#' @examples
controlUI <- function(id, data, mapping, other_arguments = NULL,
                      extra_uiFunc = NULL, extra_uiFuncArgs = NULL) {
  ns <- NS(id)

  assert_that(
    !is.null(names(mapping))
  )

  if(!is.null(other_arguments)) {
    assert_that( !is.null(names(other_arguments))  )
  }

  mapping_ui <- mapply(callFuncUI, names(mapping), mapping,
                       MoreArgs = list(
                         defaultArgs = list(ns, data),
                         extraFunc = extra_uiFunc,
                         extraFuncArgs = extra_uiFuncArgs
                       ),
                       SIMPLIFY = FALSE)

  other_ui <- mapply(callFuncUI, names(other_arguments), other_arguments,
                     MoreArgs = list(
                       defaultArgs = list(ns, data),
                       extraFunc = extra_uiFunc,
                       extraFuncArgs = extra_uiFuncArgs
                     ),
                     SIMPLIFY = FALSE)

  if (is.null(unlist(mapping_ui))) {
    mapping_ui <- NULL
  }

  if (is.null(unlist(other_ui))) {
    other_ui <- NULL
  }

  valid_piece_mapping <- !sapply(mapping_ui, is.null)
  valid_piece_param <- !sapply(other_ui, is.null)

  return(list(
    ui = list(mapping_ui = mapping_ui[valid_piece_mapping],
              param_ui = other_ui[valid_piece_param]),
    param = list(mapping = mapping[valid_piece_mapping],
                 other_arguments = other_arguments[valid_piece_param])
  ))

}
