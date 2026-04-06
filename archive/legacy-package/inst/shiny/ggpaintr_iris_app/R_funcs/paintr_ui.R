#' UI component and its id for aesthetic mapping of x
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingXUI <- function(ns, data_vars, id = "mapX") {
  ui <- pickerInput(ns(id), "x:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}


#' UI component and its id for aesthetic mapping of y
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingYUI <- function(ns, data_vars, id = "mapY") {
  ui <- pickerInput(ns(id), "y:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' UI component and its id for aesthetic mapping of z
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingZUI <- function(ns, data_vars, id = "mapZ") {
  ui <- pickerInput(ns(id), "z:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' UI component and its id for aesthetic mapping of color
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingColorUI <- function(ns, data_vars, id = "mapColor") {
  ui <- pickerInput(ns(id), "color:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' UI component and its id for aesthetic mapping of fill
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingFillUI <- function(ns, data_vars, id = "mapFill") {
  ui <- pickerInput(ns(id), "fill:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' UI component and its id for aesthetic mapping of group
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingGroupUI <- function(ns, data_vars, id = "mapGroup") {
  ui <- pickerInput(ns(id), "group:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' UI component and its id for aesthetic mapping of shape
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingShapeUI <- function(ns, data_vars, id="mapShape") {
  ui <- pickerInput(ns(id), "shape:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' UI component and its id for aesthetic mapping of size
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
mappingSizeUI <- function(ns, data_vars, id="mapSize") {
  ui <- pickerInput(ns(id), "size:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}



#' ui component for `theme()`
#'
#' @param ns the name space of this ui
#' @param theme_selected vector of selected parameters of `theme()`
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
themeUI <- function(ns, theme_selected = TRUE) {

  ui_list <- list(
    legend.position = list(pickerInput(ns("themeLegendPosition"), "Legend Position:",
                                       choices = c("top", "bottom", "left", "right"),
                                       selected = "",
                                       multiple = TRUE,
                                       options = pickerOptions(maxOptions = 1))),
    legend.direction = list(pickerInput(ns("themeLegendDirection"), "Legend Direction:",
                                        choices = c("vertical", "horizontal"),
                                        selected = "",
                                        multiple = TRUE,
                                        options = pickerOptions(maxOptions = 1))),
    legend.box = list(pickerInput(ns("themeLegendBox"), "Legend Box:",
                                  choices = c("vertical", "horizontal"),
                                  selected = "",
                                  multiple = TRUE,
                                  options = pickerOptions(maxOptions = 1)))
  )

  id <- c(legend.position = 'themeLegendPosition', legend.direction = 'themeLegendDirection',
          legend.box = "themeLegendBox")
  id <- id[theme_selected]

  ui <- column(12, offset = 0, style='padding:0px;',
               br(),
               tagList(
                 ui_list[theme_selected]
               )
  )

  return(list(ui=ui, id=id))
}


#' ui component for `labs()`
#'
#' @param ns the name space of this ui
#' @param labs_selected ector of selected parameters of `labs()`
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#' @examples
labsUI <- function(ns, labs_selected = TRUE) {

  lab_type <- c('x', 'y', 'title', 'subtitle')
  lab_id <- c('labX', 'labY', 'labTitle', 'labSubtitle')
  ui_list <- mapply(function(type, id) {
    list(textInput(ns(id), paste0("label ", type ,":")))
  }, lab_type, lab_id)

  ui <- column(12, offset = 0, style='padding:0px;',
               br(),
               tagList(
                 ui_list[labs_selected]
               )
  )
  names(lab_id) <- lab_type
  id <- lab_id[labs_selected]

  return(list(ui=ui, id=id))
}



#' ui component for choosing themes
#'
#' @param ns the name space of this ui
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
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


#' ui component for selecting variables for `facet_grid()`
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
settingFacetUI <- function(ns, data_vars, id = "miscFacet") {
  ui <- pickerInput(ns(id), "choose variables for facet (max 2):",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 2))
  return(list(ui=ui, id = id))
}

#' ui component for deciding whether or not to toggle `coord_flip()`
#'
#' @param ns the name space of this ui
#' @param data_vars variable names of the dataset
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#' @examples
settingFlipUI <- function(ns, data_vars, id = "miscFlip") {
  ui <- checkboxInput(ns(id), "Flip the coordinate", value = FALSE, width = NULL)

  return(list(ui=ui, id = id))
}

#' ui component for scale color
#'
#' @param ns the name space of this ui
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#' @examples
scaleColorUI <- function(ns, id="scaleColorUIOutput") {
  ui <- uiOutput(ns(id))

  return(list(ui=ui, id=id))
}

#' ui component for scale fill
#'
#' @param ns the name space of this ui
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#' @examples
scaleFillUI <- function(ns, id="scaleFillUIOutput") {
  ui <- uiOutput(ns(id))

  return(list(ui=ui, id=id))
}


#' ui component for setting `colourInput`
#'
#' @param ns the name space of this ui
#' @param init_colors initial colors for `colourInput`
#' @param labels labels for `colourInput`
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import colourpicker
#'
#' @examples
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


#' ui component for setting `stat` in `geom_<chart>`
#'
#' @param ns the name space of this ui
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
argsStatUI <- function(ns, id="argsStat") {
  ui <- pickerInput(ns(id), "stat:",
                    choices = c("bin", "identity", "count"),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' ui component for setting `position` in `geom_<chart>`
#'
#' @param ns the name space of this ui
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#' @examples
argsPositionUI <- function(ns, id="argsPosition") {
  ui <- pickerInput(ns(id), "position:",
                    choices = c("dodge", "dodge2", "stack", "fill"),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=id))
}

#' ui component for setting `alpha` in `geom_<chart>`
#'
#' @param ns the name space of this ui
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#' @examples
argsAlphaUI <- function(ns, id="argsAlpha") {
  ui <- numericInput(ns(id), "alpha:",
                     1,
                     min = 0, max = 1)
  return(list(ui=ui, id=id))
}

#' ui component for setting `size` in `geom_<chart>`
#'
#' @param ns the name space of this ui
#' @param id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#' @examples
argsSizeUI <- function(ns, id="argsSize") {
  ui <- numericInput(ns(id), "geom size:",
                     0.5, min = 0)
  return(list(ui=ui, id=id))
}

