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
        return(coord_flip())
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
        if(length(selectedVars) == 2) {
          ff <- as.formula(paste(selectedVars[1], "~", selectedVars[2]))
        } else {
          ff <- as.formula(paste(selectedVars[1], "~."))
        }
        return(facet_grid(ff))
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

      if( is.null(themeSettings) ) {
        return(NULL)
      } else {
        return(do.call(theme, themeSettings))
      }


    }
  )
}

scaleColorHandler <- function(id, selected_colors, color_fill) {
  moduleServer(
    id,
    function(input, output, session) {

      color_fill_options <- c("color", "fill")

      if (is.null(selected_colors) || (!( color_fill %in% color_fill_options )) ) {
        return(NULL)
      }

      if (selected_colors[['type']] == "numerical") {
        assert_that(
          length(selected_colors[['id']]) == 2
        )

        colors <- lapply(selected_colors[['id']], function(ii) {
          input[[ii]]
        })
        names(colors) <- c("low", "high")

        if (color_fill == "color") {
          return(do.call(scale_color_gradient, colors))
        } else {
          return(do.call(scale_fill_gradient, colors))
        }

      } else if (selected_colors[['type']] == "categorical") {

        colors <- sapply(selected_colors[['id']], function(ii) {
          input[[ii]]
        })

        names(colors) <- NULL

         if (color_fill == "color") {
          return(scale_color_manual(values = colors))
        } else {
          return(scale_fill_manual(values = colors))
        }

      } else {
        return(NULL)
      }

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


#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
empty_list_null <- function(x) {
  if(length(x) == 0) {
    return(NULL)
  } else {
    return(x)
  }
}



