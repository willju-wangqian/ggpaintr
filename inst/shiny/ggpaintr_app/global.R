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

library(colourpicker)

library(assertthat)

# library(ggpaintr)

setwd('C:/Downloads/project/ggpaintr/inst/shiny/ggpaintr_app/')

source('ui.R')
source('server.R')

sapply(list.files("R_funcs/"), function(fileName) {
  source(paste0("R_funcs/", fileName))
})
# source("R/modules.R")
# source("R/helper.R")

# data("penguins")

DODGE_WID_DEFAULT <- 0.5
myStatUI <- function(x, y) { h3(paste(x + y)) }

shinyApp(ui, server)



