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
get_plot <- function(data, gg_list) {

  p <- ggplot(data = data)
  for (i in seq_along(gg_list)) {
    p <- p + gg_list[[i]]
  }

  return(p)

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
          code <- paste(selectedVars[1], "~ .")
          ff <- as.formula(code)
        }
        return(list(plot = facet_grid(ff),
                    code = paste0("facet_grid(", code, ")")))
      } else {
        return(NULL)
      }

    }
  )
}

#' Title
#'
#' @param id
#' @param module_id
#' @param theme_param
#'
#' @return
#' @export
#'
#' @examples
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

        code <- paste_arg_param(themeSettings, add_quo = TRUE)
        code <- paste0("theme(", code, ")")

        return(list(plot = do.call(theme, themeSettings),
                    code = code))
      }


    }
  )
}


#' Title
#'
#' @param id
#' @param module_id
#'
#' @return
#' @export
#'
#' @examples
themeChooseHandler <- function(id, module_id) {
  moduleServer(
    id,
    function(input, output, session) {

      if (is.null(input[[module_id]])) {
        return(NULL)
      }

      return( list(plot = match.fun(input[[module_id]])(),
                   code = paste0(input[[module_id]], "()")) )

    }
  )
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

  check_remove_null(aes_list)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
remove_null_list <- function(x) {
  x[!sapply(x, is.null)]
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
empty_list_null <- function(x) {
  if(length(x) == 0) {
    return(NULL)
  } else {
    return(x)
  }
}

#' Title
#'
#' @param x
#' @param add_quo
#'
#' @return
#' @export
#'
#' @examples
paste_arg_param <- function(x, add_quo = FALSE) {
  if(is.null(x)) {
    return("")
  }

  assert_that(
    !is.null( names(x) )
  )

  code_args <- mapply(function(aa, var){
    if(add_quo) {
      paste0(aa, " = '", var, "'")
    } else {
      paste0(aa, " = ", var)
    }
  }, names(x), x, SIMPLIFY = FALSE )
  code_args[['sep']] <- ', '
  code <- do.call(paste, code_args)

  return(code)
}


#' Title
#'
#' @param code_list
#'
#' @return
#' @export
#'
#' @examples
get_code <- function(code_list) {

  assert_that(
    has_name(code_list, "data"),
    has_name(code_list, "geom")
  )

  basic_code <- paste0(
    "data <- ", code_list[['data']], "\n\n",

    "ggplot(data = data) + ", "\n",
    "  ", code_list[['geom']]
  )

  code_list[['data']] <- NULL
  code_list[['geom']] <- NULL
  code_list <- check_remove_null(code_list)

  if(is.null(code_list)) {
    final_code <- basic_code
  } else {
    code_list[['sep']] <- " +\n  "
    other_code <- do.call(paste, code_list)
    final_code <- paste0(basic_code, " +\n  ", other_code)
  }

  return(final_code)
}


#' Title
#'
#' @param geom_component
#' @param ...
#' @param data
#' @param data_path
#'
#' @return
#' @export
#'
#' @examples
get_plot_code <- function(geom_component, ..., data, data_path) {

  total_list <- c(
    list(geom_component),
    list(...)
  )

  plot_list <- map(total_list, 1)
  code_list <- map(total_list, 2)

  names(code_list)[1] <- "geom"
  code_list[['data']] <- data_path

  pp <- get_plot(data, plot_list)
  final_code <- get_code(code_list)

  return(list(plot = pp, code = final_code))

}


