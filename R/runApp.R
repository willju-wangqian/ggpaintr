#' Call ggpaintr_app
#'
#' @param name name of the installed shiny app. Default is `NULL`. Other choices
#' include: `ggpaintr_demo`
#'
#' @return `shiny::runApp`
#' @export
#'
run_ggpaintr_app <- function(name = NULL) {

  if (is.null(name)) {
    shiny_name <- "ggpaintr_app"
  } else {
    shiny_name <- name
  }

  appDir <- system.file("shiny", shiny_name, package = "ggpaintr")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `ggpaintr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
