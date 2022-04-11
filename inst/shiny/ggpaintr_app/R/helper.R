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

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
check_char_set_names <- function(x) {
  if(!is.null(x)) {
    assert_that( is.character(x) )
    x <- purrr::set_names(x)
  }

  x
}

#' Title
#'
#' @param x list
#'
#' @return
#' @export
#'
#' @examples
check_remove_null <- function(x) {
  if(is.null(x)) return(NULL)

  x <- x[!sapply(x, is.null)]
  if(length(x) == 0) {
    x <- NULL
  }
  x
}

#' Title
#'
#' @param session_input
#' @param mapping_id
#' @param params
#'
#' @return
#' @export
#'
#' @examples
connect_param_id <- function(session_input, id_list, params,
                             color_fill = FALSE, color_group = FALSE) {
  if(is.null(params) || is.null(id_list)) {
    return(NULL)
  }

  id_list <- unlist(id_list)

  assert_that(
    length(id_list) == length(params)
  )

  aes_list <- lapply(id_list, function(id, input) {
    input[[id]]
  }, input = session_input)

  names(aes_list) <- params

  if(color_fill) {
    assert_that(
      hasName(aes_list, "color") || hasName(aes_list, "fill")
    )

    if (is.null(aes_list[['fill']])) {
      aes_list[['fill']] <- aes_list[['color']]
    }

    if(is.null(aes_list[['color']])) {
      aes_list[['color']] <- aes_list[['fill']]
    }
  }

  if(color_group) {
    assert_that(
      hasName(aes_list, "color") || hasName(aes_list, "group")
    )

    if (is.null(aes_list[['group']])) {
      aes_list[['group']] <- aes_list[['color']]
    }

    if(is.null(aes_list[['color']])) {
      aes_list[['color']] <- aes_list[['group']]
    }
  }

  aes_list
}

remove_null_list <- function(x) {
  x[!sapply(x, is.null)]
}

empty_list_null <- function(x) {
  if(length(x) == 0) {
    return(NULL)
  } else {
    return(x)
  }
}



