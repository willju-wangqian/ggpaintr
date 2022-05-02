library(palmerpenguins)
library(shiny)
library(DT)
library(shinydashboard)
library(shinyBS)
library(rlang)
# library(ggpaintr)

# library(tidyverse)
# library(shinyWidgets)
# library(colourpicker)
# library(RColorBrewer)
# library(assertthat)

sapply(list.files("R_funcs/"), function(fileName) {
  source(paste0("R_funcs/", fileName))
})


DODGE_WID_DEFAULT <- 0.5
myStatUI <- function(x, y) { h3(paste(x + y)) }

# shinyApp(ui, server)



