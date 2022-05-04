if(!require(palmerpenguins)) {
  install.packages("palmerpenguins")
  library(palmerpenguins)
}
if(!require(shiny)) {
  install.packages("shiny")
  library(shiny)
}
if(!require(DT)) {
  install.packages("DT")
  library(DT)
}
if(!require(shinydashboard)) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if(!require(shinyBS)) {
  install.packages("shinyBS")
  library(shinyBS)
}
if(!require(rlang)) {
  install.packages("rlang")
  library(rlang)
}
if(!require(shinyWidgets)) {
  install.packages("shinyWidgets")
  library(shinyWidgets)
}
# require(palmerpenguins)
# require(shiny)
# require(DT)
# require(shinydashboard)
# require(shinyBS)
# require(rlang)
# require(shinyWidgets)
# library(ggpaintr)

library(tidyverse)
library(colourpicker)
library(RColorBrewer)
library(assertthat)
sapply(list.files("R_funcs/"), function(fileName) {
  source(paste0("R_funcs/", fileName))
})


# DODGE_WID_DEFAULT <- 0.5
# myStatUI <- function(x, y) { h3(paste(x + y)) }

# shinyApp(ui, server)



