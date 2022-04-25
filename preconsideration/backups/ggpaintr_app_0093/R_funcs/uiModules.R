#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
mappingXUI <- function(ns, data, id = "mapX") {
  ui <- pickerInput(ns(id), "x:",
                    choices = names(data),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
mappingYUI <- function(ns, data, id = "mapY") {
  ui <- pickerInput(ns(id), "y:",
                    choices = names(data),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
mappingColorUI <- function(ns, data, id = "mapColor") {
  ui <- pickerInput(ns(id), "color:",
                    choices = names(data),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' Title
#'
#' @param ns
#' @param data
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mappingFillUI <- function(ns, data, id = "mapFill") {
  ui <- pickerInput(ns(id), "fill:",
                    choices = names(data),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' Title
#'
#' @param ns
#' @param data
#' @param id
#'
#' @return
#' @export
#'
#' @examples
mappingGroupUI <- function(ns, data, id = "mapGroup") {
  ui <- pickerInput(ns(id), "group:",
                    choices = names(data),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
mappingShapeUI <- function(ns, data, id="mapShape") {
  ui <- pickerInput(ns(id), "shape:",
                    choices = names(data),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
mappingSizeUI <- function(ns, data, id="mapSize") {
  ui <- pickerInput(ns(id), "size:",
                    choices = names(data),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}



#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
themeUI <- function(ns) {
  ui <- column(12, offset = 0, style='padding:0px;',
               br(),
               pickerInput(ns("themeLegendPosition"), "Legend Position:",
                           choices = c("top", "bottom", "left", "right"),
                           selected = "",
                           multiple = TRUE,
                           options = pickerOptions(maxOptions = 1)),
               # textInput(ns("themeLegendTitle"), "Legend Title")
               pickerInput(ns("themeLegendDirection"), "Legend Direction:",
                           choices = c("vertical", "horizontal"),
                           selected = "",
                           multiple = TRUE,
                           options = pickerOptions(maxOptions = 1)),
  )

  return(list(ui=ui, id=c('themeLegendPosition', 'themeLegendDirection')))
}

#' Title
#'
#' @param ns
#'
#' @return
#' @export
#'
#' @examples
themeChooseUI <- function(ns) {
  ui <- pickerInput(ns("themeChoose"), "Choose a theme:",
              choices = c("default" = "theme_gray", "classic" = "theme_classic",
                          "dark-on-light" = "theme_bw", "minimal" = "theme_minimal"),
              selected = "",
              multiple = TRUE,
              options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id="themeChoose"))

}


#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
miscUI <- function(ns, data) {
  ui <- column(12, offset = 0, style='padding:0px;',
               br(),
               pickerInput(ns("miscFacet"), "choose variables for facet (max 2):",
                           choices = names(data),
                           selected = "",
                           multiple = TRUE,
                           options = pickerOptions(maxOptions = 2)),
               checkboxInput(ns("miscFlip"), "Flip the coordinate", value = FALSE, width = NULL)
  )

  return(list(ui=ui, id = c('miscFacet', 'miscFlip')))
}


scaleColorUI <- function(ns, id="scaleColorUIOutput") {
  ui <- uiOutput(ns(id))

  return(list(ui=ui, id=id))
}

multipleColorPickerUI <- function(ns, init_colors, labels, id = "colorPicker") {
  assert_that(
    length(init_colors) == length(labels)
  )

  ids <- sapply(seq_along(init_colors), function(i) { paste(id, i, sep="-") })

  pickerUI_list <- mapply(function(color, label, id) {
    colourpicker::colourInput(inputId = ns(id), # DO NOT change
                              label = paste0("Colour for ", label , ':'), # Text shown on template
                              value = color)
  }, init_colors, labels, ids, SIMPLIFY = FALSE)


  ui <- do.call(tagList, pickerUI_list)

  return(list(ui = ui, id = ids))
}

argsStatUI <- function(ns, data, id="argsStat") {
  ui <- pickerInput(ns(id), "stat:",
                    choices = c("bin", "identity", "count"),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

argsPositionUI <- function(ns, data, id="argsPosition") {
  ui <- pickerInput(ns(id), "position:",
                    choices = c("dodge", "dodge2", "stack", "fill"),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}


#' Title
#'
#' @param ns
#'
#' @return
#' @export
#'
#' @examples
barSettingUI <- function(ns) {
  column(12, offset = 0, style='padding:0px;',
         br(),
         pickerInput(ns("settingBarStat"), "stat:",
                     choices = c("bin", "identity", "count"),
                     selected = "",
                     multiple = TRUE,
                     options = pickerOptions(maxOptions = 1)),
         numericInput(ns("settingBarWidth"), "bar width (between 0 and 1):",
                      0.5,
                      min = 0, max = 1),
         pickerInput(ns("settingBarPosition"), "position:",
                     choices = c("dodge" = "position_dodge", "fill" = "position_fill"),
                     selected = c("dodge" = "position_dodge"),
                     multiple = TRUE,
                     options = pickerOptions(maxOptions = 1)),
         numericInput(ns("settingBarPositionWidth"), "position width:",
                      DODGE_WID_DEFAULT,
                      min = 0, max = 1)
  )
}

#' Title
#'
#' @param ns
#'
#' @return
#' @export
#'
#' @examples
textSettingUI <- function(ns) {
  column(
    12,  offset = 0, style='padding:0px;',
    numericInput(ns("settingTextRounding"), "round the labels if they are numeric:",
                 2, min = 0),
    numericInput(ns("settingTextSize"), "label size:",
                 NULL),
    numericInput(ns("settingTextHjust"), "hjust:",
                 NULL),
    numericInput(ns("settingTextVjust"), "vjust:",
                 NULL),
    pickerInput(ns("settingTextPosition"), "position:",
                choices = c("dodge" = "position_dodge", "identity" = "position_identity"),
                selected = c("dodge" = "position_dodge"),
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1)),
    numericInput(ns("settingTextPositionWidth"), "label position width:",
                 DODGE_WID_DEFAULT),

  )



}

#' Title
#'
#' @param ns
#' @param data
#'
#' @return
#' @export
#'
#' @examples
mappingUI <- function(ns, data) {
  ui <- column(12, offset = 0, style='padding:0px;',
         br(),
         pickerInput(ns("mapX"), "x:",
                     choices = names(data),
                     selected = "",
                     multiple = TRUE,
                     options = pickerOptions(maxOptions = 1)),
         pickerInput(ns("mapY"), "y:",
                     choices = names(data),
                     selected = "",
                     multiple = TRUE,
                     options = pickerOptions(maxOptions = 1)),
         pickerInput(ns("mapColor"), "color:",
                     choices = names(data),
                     selected = "",
                     multiple = TRUE,
                     options = pickerOptions(maxOptions = 1))
  )

  return(list(ui = ui, id= c('mapX', 'mapY', 'mapColor') ))
}




#' Title
#'
#' @param ns
#'
#' @return
#' @export
#'
#' @examples
lolliSettingUI <- function(ns) {
  column(12, offset = 0, style='padding:0px;',
         br(),
         h4("line segment setting:"),
         pickerInput(ns("settingLinerangePosition"), "position:",
                     choices = c("dodge" = "position_dodge", "fill" = "position_fill"),
                     selected = c("dodge" = "position_dodge"),
                     multiple = TRUE,
                     options = pickerOptions(maxOptions = 1)),
         numericInput(ns("settingLinerangePositionWidth"), "position width:",
                      DODGE_WID_DEFAULT,
                      min = 0, max = 1),
         numericInput(ns("settingLinerangeAlpha"), "alpha:",
                      0.7,
                      min = 0, max = 1),
         numericInput(ns("settingLinerangeSize"), "size:",
                      1.2, min = 0),
         br(),
         h4("dot setting"),
         pickerInput(ns("settingPointPosition"), "position:",
                     choices = c("dodge" = "position_dodge", "fill" = "position_fill"),
                     selected = c("dodge" = "position_dodge"),
                     multiple = TRUE,
                     options = pickerOptions(maxOptions = 1)),
         numericInput(ns("settingPointPositionWidth"), "position width:",
                      DODGE_WID_DEFAULT,
                      min = 0, max = 1),
         numericInput(ns("settingPointAlpha"), "alpha:",
                      0.9,
                      min = 0, max = 1),
         numericInput(ns("settingPointSize"), "size:",
                      3, min = 0)
  )
}


#' Title
#'
#' @param ns
#'
#' @return
#' @export
#'
#' @examples
lineSettingUI <- function(ns) {
  column(12, offset = 0, style='padding:0px;',
         br(),
         h4("line settings:"),
         # pickerInput(ns("settingLinerangePosition"), "position:",
         #             choices = c("dodge" = "position_dodge", "identity" = "position_identity"),
         #             selected = c("identity" = "position_identity"),
         #             multiple = TRUE,
         #             options = pickerOptions(maxOptions = 1)),
         # numericInput(ns("settingLinerangePositionWidth"), "position width:",
         #              NULL,
         #              min = 0, max = 1),
         numericInput(ns("settingLineAlpha"), "alpha:",
                      1,
                      min = 0, max = 1),
         numericInput(ns("settingLineSize"), "size:",
                      1, min = 0),
         # br(),
         # h4("dot setting"),
         # pickerInput(ns("settingPointPosition"), "position:",
         #             choices = c("dodge" = "position_dodge", "fill" = "position_fill"),
         #             selected = c("dodge" = "position_dodge"),
         #             multiple = TRUE,
         #             options = pickerOptions(maxOptions = 1)),
         # numericInput(ns("settingPointPositionWidth"), "position width:",
         #              DODGE_WID_DEFAULT,
         #              min = 0, max = 1),
         # numericInput(ns("settingPointAlpha"), "alpha:",
         #              0.9,
         #              min = 0, max = 1),
         # numericInput(ns("settingPointSize"), "size:",
         #              3, min = 0)
  )
}


#' Title
#'
#' @param ns
#'
#' @return
#' @export
#'
#' @examples
pointSettingUI <-  function(ns) {
  column(12, offset = 0, style='padding:0px;',
         br(),
         h4("point settings:"),
         numericInput(ns("settingPointAlpha"), "alpha:",
                      1,
                      min = 0, max = 1),
         numericInput(ns("settingPointSize"), "size:",
                      1, min = 0),
  )
}



