#' collecting all arguments for theme()
#'
#' @param input
#'
#' @return
#' @export
#'
#' @examples
themeCollector <- function(input) {
  themeSettings <- list()

  if(!is.null(input$themeLegendPosition))
    themeSettings$legend.position = input$themeLegendPosition

  # if(input$themeLegendTitle != "")
  #   themeSettings$legend.title = input$themeLegendTitle

  themeSettings
}

#' Title
#'
#' @param id
#' @param reactiveList
#'
#' @return
#' @export
#'
#' @examples
plotSettingServer <- function(id, reactiveList) {
  moduleServer(
    id,
    function(input, output, session) {
      reactiveList$flip = reactive(input$miscFlip)
      reactiveList$facet = reactive(input$miscFacet)
      reactiveList$themeSettings = reactive(themeCollector(input))

      reactiveList
      # relist <- reactiveValues(
      #   pp = ggPlotObject,
      #   flip = reactive(input$mapFlip),
      #   facet = reactive(input$mapFacet),
      #   themeSettings = reactive(themeCollector(input))
      # )
    }
  )
}

#' Title
#'
#' @param reactiveList
#'
#' @return
#' @export
#'
#' @examples
plotGenerator <- function(reactiveList) {
  pp <- reactiveList$pp()
  if(reactiveList$flip()) {
    pp <- pp + coord_flip()
  }

  if(!is.null(reactiveList$facet())) {
    selectedVars <- reactiveList$facet()

    ff <- NULL
    if(length(selectedVars) == 2) {
      ff <- as.formula(paste(selectedVars[1], "~", selectedVars[2]))
    } else {
      ff <- as.formula(paste(selectedVars[1], "~."))
    }
    pp <- pp + facet_grid(ff)
  }

  if(length(reactiveList$themeSettings()) != 0) {
    pp <- pp + do.call(theme, reactiveList$themeSettings())
  }

  # after adding all the components, return the plot
  pp
}








