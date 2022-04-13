library(assertthat)
library(tidyverse)
library(palmerpenguins)
library(shinyWidgets)
library(shiny)
library(DT)
library(shinydashboard)
library(shinyFiles)
library(shinybusy)
library(shinyAce)
library(formatR)
library(shinyhelper)
library(shinyBS)
library(here)

setwd("~/Research/ggpaintr/inst/shiny/ggpaintr_app")

sapply(list.files("R"), function(fileName) {
  source(paste0("R/", fileName))
})

dataBox <- readRDS(here("inst", "extdata", "kindergartners_data_firsttofifth.rds"))

pp <- dataBox %>% ggplot() +
  geom_boxplot(aes(x=school_year, y=score_mean, color=subject, fill = subject, group = subject)) +
  theme(legend.direction = "horizontal")
pp
mapping_part <- list(x="mapX", y="mapY", color="mapColor")

mapping_part %>% pmap(~{
  (..1)
})

named_list <- list(one = c(1, 1),
                   two = c(2, 2),
                   three = c(3, 3))
pmap(named_list, function(x,y,z) x+y+z)

pmap(unname(named_list), function(x,y,z) x+y+z)

pmap(named_list, ~ with(list(...), names(one)))

pmap(named_list, ~{
  dots <- list(...)

  dots[['one']]

})

ns <- NS("my_test")
my_test_output <- list()
tt <- list()
tt <- mapping_part %>% iwalk(~{
  cat(.y, ns(.x), '\n')
  .x
})
# my_test_output[['abc']] <- 'abc'
# my_test_output
tt

getUI <- function(strr, id) {
  paste(strr, " good ", id)
}

mapping_part %>% imap(~{
  getUI(.x, ns(.y))
})

mapply(function(name, choice) {
  paste(name, "good ", choice)
}, names(mapping_part), mapping_part, SIMPLIFY = FALSE)


mapping <- list(
  x = "mapX",
  y = "mapY",
  color = "mapColor"
)

other_arguments <- list(
  stat = "boxStat",
  position = "boxPosition"
)

getUIControlList <- function(user_defined) {
  uiControlList <- list(
    x = "mappingXUI",
    y = "mappingYUI",
    color = "mappingColorUI",
    shape = "mappingShapeUI"
  )
  uiControlList
}

matchUIControls <- function(ui_part) {

  uiControlList <- getUIControlList()

  if ( is.null(uiControlList[[ui_part]]) ) {
    warning( paste("The ui part for", ui_part, "has not been implemented yet.")  )
    return(NULL)
  } else {
    return( getFromNamespace(uiControlList[[ui_part]], asNamespace("ggpaintr")) )
  }

}

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

id <- "boxPlot"
data <- mtcars

labels <- unique(as.character(mtcars$cyl))
init_colors <- RColorBrewer::brewer.pal(length(labels), "Set2")


pickerUI_list <- mapply(function(color, label, i) {
  colourpicker::colourInput(inputId = paste(id, i, sep="-"), # DO NOT change
                            label = paste0("Colour for level ", label , ':'), # Text shown on template
                            value = color)
}, init_colors, labels, seq_along(init_colors), SIMPLIFY = FALSE)



boxMain <- controlUI(
  id = "boxPlot",
  data = data,
  mapping = c('x', 'y', 'color'),
  plot_settings = c('misc', 'theme')
)

mapping_ui <- c(mapping_ui, shape= list(NULL))


tt_ui$param
tt_ui$ui$mapping_ui
tt_ui$ui$param_ui

test_statUI <- function(x,y,z) {
  h3(paste(x,y,z,'\n'))
}


test_positionUI <- function(x,y,z) {
  h3(paste(x,y,z,'\n'))
}
test_statUI(1,2,3)
test_positionUI(4,5,6)

myMappingXUI <- function(ns, x, y) {
  h3(paste(ns(x + y)))
}

ggplot(mtcars) +
  geom_point(mapping = aes_string("x" = "mpg", 'y' = 'cyl'))


params_list <- list(mapping = c('x', 'y', 'color'))

ggplot(iris) +
  geom_point(aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  scale_color_manual(values = c("red", "green", "blue"))

p1 <- ggplot(iris) +
  geom_boxplot(aes(x=Species, y=Sepal.Width, fill=Species))

test_scale <- scale_fill_manual(values = c("red", "green", "blue"))
p1 + test_scale






