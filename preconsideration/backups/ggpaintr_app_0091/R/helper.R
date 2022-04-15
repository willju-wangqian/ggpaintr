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
#' @param id
#' @param pp
#' @param module_id
#'
#' @return
#' @export
#'
#' @examples
flipHandler <- function(id, module_id) {
  moduleServer(
    id,
    function(input, output, session) {
      if(input[[module_id]]) {
        return(list(plot = coord_flip(),
                    code = "coord_flip()"))
      } else {
        return(NULL)
      }

    }
  )
}


#' Title
#'
#' @param id
#' @param pp
#' @param module_id
#'
#' @return
#' @export
#'
#' @examples
facetHandler <- function(id, module_id) {
  moduleServer(
    id,
    function(input, output, session) {
      if(!is.null(input[[module_id]])) {
        selectedVars <- input[[module_id]]

        ff <- NULL
        code <- NULL
        if(length(selectedVars) == 2) {
          code <- paste(selectedVars[1], "~", selectedVars[2])
          ff <- as.formula(code)
        } else {
          code <- paste(selectedVars[1], "~.")
          ff <- as.formula(code)
        }
        return(list(plot = facet_grid(ff),
                    code = code))
      } else {
        return(NULL)
      }

    }
  )
}

themeHandler <- function(id, module_id, theme_param) {
  moduleServer(
    id,
    function(input, output, session) {

      assert_that(
        length(module_id) == length(theme_param)
      )

      themeSettings <- lapply(module_id, function(mm_id) {
        input[[mm_id]]
      })

      names(themeSettings) <- theme_param

      themeSettings <- check_remove_null(themeSettings)

      # browser()

      if( is.null(themeSettings) ) {
        return(NULL)
      } else {

        code <- paste_arg_param(themeSettings)
        code <- paste0("theme(", code, ")")

        return(list(plot = do.call(theme, themeSettings),
                    code = code))
      }


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
  params <- unlist(params)

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



