#' Call ggpaintr_app
#'
#' @return `shiny::runApp`
#' @export
#'
run_ggpaintr_app <- function() {
  appDir <- system.file("shiny", "ggpaintr_app", package = "ggpaintr")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `ggpaintr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
