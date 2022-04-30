library(tidyverse)
library(palmerpenguins)
library(shinyWidgets)
library(shiny)
library(DT)
library(shinydashboard)
library(colourpicker)
library(RColorBrewer)
library(assertthat)

library(ggpaintr)

# library(shinyFiles)
# library(shinybusy)
# library(shinyAce)
# library(formatR)
# library(shinyhelper)
library(shinyBS)

# setwd('C:/Downloads/project/ggpaintr/inst/shiny/ggpaintr_app/')

# sapply(list.files("R_funcs/"), function(fileName) {
#   source(paste0("R_funcs/", fileName))
# })

# data("penguins")

DODGE_WID_DEFAULT <- 0.5
myStatUI <- function(x, y) { h3(paste(x + y)) }

# shinyApp(ui, server)



