
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
