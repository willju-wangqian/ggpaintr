# Common runner header. Loads ggpaintr and runs the supplied app file on PORT.
suppressPackageStartupMessages({
  devtools::load_all(".", quiet=TRUE)
  library(shiny)
})
options(shiny.host="127.0.0.1", shiny.port=as.integer(Sys.getenv("PORT","4321")))
args <- commandArgs(trailingOnly=TRUE)
script <- args[1]
res <- source(script, local=globalenv(), echo=FALSE)$value
if (inherits(res, "shiny.appobj")) {
  print(res)
} else {
  stop("Last value of ", script, " is not a shiny.appobj (got ", paste(class(res), collapse=","), ")")
}
