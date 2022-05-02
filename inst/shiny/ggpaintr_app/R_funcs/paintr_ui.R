#' UI component and its id for aesthetic mapping of x
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
mappingXUI <- function(id, data_vars, ui_id = "mapX") {

  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "x:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}


#' UI component and its id for aesthetic mapping of y
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
mappingYUI <- function(id, data_vars, ui_id = "mapY") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "y:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' UI component and its id for aesthetic mapping of z
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
mappingZUI <- function(id, data_vars, ui_id = "mapZ") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "z:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' UI component and its id for aesthetic mapping of color
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
mappingColorUI <- function(id, data_vars, ui_id = "mapColor") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "color:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' UI component and its id for aesthetic mapping of fill
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
mappingFillUI <- function(id, data_vars, ui_id = "mapFill") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "fill:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' UI component and its id for aesthetic mapping of group
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
mappingGroupUI <- function(id, data_vars, ui_id = "mapGroup") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "group:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' UI component and its id for aesthetic mapping of shape
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
mappingShapeUI <- function(id, data_vars, ui_id="mapShape") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "shape:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' UI component and its id for aesthetic mapping of size
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
mappingSizeUI <- function(id, data_vars, ui_id="mapSize") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "size:",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}



#' ui component for `theme()`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param theme_selected vector of selected parameters of `theme()`
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
themeUI <- function(id, theme_selected = TRUE) {
  ns <- NS(id)

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
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param labs_selected ector of selected parameters of `labs()`
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#'
labsUI <- function(id, labs_selected = TRUE) {
  ns <- NS(id)

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
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
themeChooseUI <- function(id, ui_id = "themeChoose") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "Choose a theme:",
                    choices = c("default" = "theme_gray", "classic" = "theme_classic",
                                "dark-on-light" = "theme_bw", "minimal" = "theme_minimal"),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))

}


#' ui component for selecting variables for `facet_grid()`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
settingFacetUI <- function(id, data_vars, ui_id = "miscFacet") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "choose variables for facet (max 2):",
                    choices = data_vars,
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 2))
  return(list(ui=ui, id=ui_id))
}

#' ui component for deciding whether or not to toggle `coord_flip()`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data_vars variable names of the dataset
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#'
settingFlipUI <- function(id, data_vars, ui_id = "miscFlip") {
  ns <- NS(id)

  ui <- checkboxInput(ns(ui_id), "Flip the coordinate", value = FALSE, width = NULL)

  return(list(ui=ui, id=ui_id))
}

#' ui component for scale color
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#'
scaleColorUI <- function(id, ui_id="scaleColorUIOutput") {
  ns <- NS(id)

  ui <- uiOutput(ns(ui_id))

  return(list(ui=ui, id=ui_id))
}

#' ui component for scale fill
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#'
scaleFillUI <- function(id, ui_id="scaleFillUIOutput") {
  ns <- NS(id)

  ui <- uiOutput(ns(ui_id))

  return(list(ui=ui, id=ui_id))
}


#' ui component for setting `colourInput`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param init_colors initial colors for `colourInput`
#' @param labels labels for `colourInput`
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @importFrom colourpicker colourInput
#'
#'
multipleColorPickerUI <- function(id, init_colors, labels, ui_id = "colorPicker") {
  ns <- NS(id)

  assert_that(
    length(init_colors) == length(labels)
  )

  ids <- sapply(seq_along(init_colors), function(i) { paste(ui_id, i, sep="-") })

  pickerUI_list <- mapply(function(color, label, id) {
    colourInput(inputId = ns(id), # DO NOT change
                label = paste0("Colour for ", label , ':'), # Text shown on template
                value = color)
  }, init_colors, labels, ids, SIMPLIFY = FALSE)


  ui <- do.call(tagList, pickerUI_list)

  return(list(ui = ui, id=ids))
}


#' ui component for setting `stat` in `geom_<chart>`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
argsStatUI <- function(id, ui_id="argsStat") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "stat:",
                    choices = c("bin", "identity", "count"),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' ui component for setting `position` in `geom_<chart>`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny shinyWidgets
#'
#'
argsPositionUI <- function(id, ui_id="argsPosition") {
  ns <- NS(id)

  ui <- pickerInput(ns(ui_id), "position:",
                    choices = c("dodge", "dodge2", "stack", "fill"),
                    selected = "",
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
  return(list(ui=ui, id=ui_id))
}

#' ui component for setting `alpha` in `geom_<chart>`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#'
argsAlphaUI <- function(id, ui_id="argsAlpha") {
  ns <- NS(id)

  ui <- numericInput(ns(ui_id), "alpha:",
                     1,
                     min = 0, max = 1)
  return(list(ui=ui, id=ui_id))
}

#' ui component for setting `size` in `geom_<chart>`
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param ui_id id of the ui element
#'
#' @return a list with two elements
#' - ui: the ui element of this piece
#' - id: the id of this ui element
#' @export
#'
#' @import shiny
#'
#'
argsSizeUI <- function(id, ui_id="argsSize") {
  ns <- NS(id)

  ui <- numericInput(ns(ui_id), "geom size:",
                     0.5, min = 0)
  return(list(ui=ui, id=ui_id))
}

