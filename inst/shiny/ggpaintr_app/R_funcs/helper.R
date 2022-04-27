#' Module server for `coord_flip()`
#'
#' @param id a string which becomes a name space for the module server
#' @param module_id the id of the ui element (not including its prefix created by
#' the name space) which determines whether or not to call `coord_flip()`
#'
#' @return `NULL` or `coord_flip()`
#' @export
#'
#' @examples
#' \dontrun{
#' flipHandler("my_boxplot", "settingFlip")
#' }
flipHandler <- function(id, module_id) {
  moduleServer(
    id,
    function(input, output, session) {
      if(is.null(module_id) || is.null(input[[module_id]])) {
        return(NULL)
      }


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

      if(is.null(module_id)) return(NULL)

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

#' theme()
#'
#' @param id
#' @param module_id
#' @param theme_param
#'
#' @return
#' @export
#'
#' @examples
themeHandler <- function(id, module_id, param) {
  stringParamHandler(id, module_id, param, "theme")
}


labsHandler <- function(id, module_id, param) {
  stringParamHandler(id, module_id, param, "labs")
}



# themeHandler <- function(id, module_id, theme_param) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#
#       assert_that(
#         length(module_id) == length(theme_param)
#       )
#
#       themeSettings <- lapply(module_id, function(mm_id) {
#         input[[mm_id]]
#       })
#
#       names(themeSettings) <- theme_param
#
#       themeSettings <- check_remove_null(themeSettings)
#
#       # browser()
#
#       if( is.null(themeSettings) ) {
#         return(NULL)
#       } else {
#
#         code <- paste_arg_param(themeSettings, add_quo = TRUE)
#         code <- paste0("theme(", code, ")")
#
#         return(list(plot = do.call(theme, themeSettings),
#                     code = code))
#       }
#     }
#   )
# }


stringParamHandler <- function(id, module_id, param, FUN) {
  moduleServer(
    id,
    function(input, output, session) {

      if( any(is.na(module_id)) || any(is.na(param)) ) return(NULL)

      assert_that(
        length(module_id) == length(param)
      )

      settingArgsList <- lapply(module_id, function(mm_id) {
        if(is.null(input[[mm_id]])) return(NULL)
        if (input[[mm_id]] == '') return(NULL)
        input[[mm_id]]
      })

      names(settingArgsList) <- param

      settingArgsList <- check_remove_null(settingArgsList)

      # browser()

      if( is.null(settingArgsList) ) {
        return(NULL)
      } else {

        code <- paste_arg_param(settingArgsList, add_quo = TRUE)
        code <- paste0(FUN, "(", code, ")")

        return(list(plot = do.call(eval(rlang::parse_expr(FUN)), settingArgsList),
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

      if (is.null(module_id) || is.null(input[[module_id]])) {
        return(NULL)
      }

      return( list(plot = match.fun(input[[module_id]])(),
                   code = paste0(input[[module_id]], "()")) )

    }
  )
}

#' Title
#'
#' @param id
#' @param selected_colors
#' @param color_fill
#'
#' @return
#' @export
#'
#' @examples
scaleColorHandler_inner <- function(id, selected_colors, color_fill) {
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

        code <- paste_arg_param(colors)


        if (color_fill == "color") {
          return(list(plot = do.call(scale_color_gradient, colors),
                      code = paste0("scale_color_gradient(", code, ")")))
        } else {
          return(list(plot = do.call(scale_fill_gradient, colors),
                      code = paste0("scale_fill_gradient(", code, ")")))
        }

      } else if (selected_colors[['type']] == "categorical") {

        colors <- sapply(selected_colors[['id']], function(ii) {
          input[[ii]]
        })

        names(colors) <- NULL

        if (color_fill == "color") {
          return(list(plot = scale_color_manual(values = colors),
                      code =  paste0("scale_color_manual(values = c(",
                                     paste(shQuote(colors, type="csh"), collapse=", "),
                                     "))") ))
        } else {
          return(list(plot = scale_fill_manual(values = colors),
                      code =  paste0("scale_fill_manual(values = c(",
                                     paste(shQuote(colors, type="csh"), collapse=", "),
                                     "))") ))
        }

      } else {
        return(NULL)
      }

    }
  )
}


scaleColorFillHandler <- function(id, selected_color_fill_rctv, color_fill) {
  if (is.null(selected_color_fill_rctv)) return(NULL)

  tryCatch(
    {
      if (!is.null(selected_color_fill_rctv())) {
        scaleColorHandler_inner(id,
                                selected_color_fill_rctv(),
                                color_fill = color_fill)
      } else {
        NULL
      }
    },
    error = function(cond) {
      return(NULL)
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
#' @param geom_component
#' @param ...
#' @param data
#' @param data_path
#'
#' @return
#' @export
#'
#' @examples
get_plot_code <- function(componentList, data, data_path) {

  componentList <- check_remove_null(componentList)

  # browser()

  check_component <- sapply(componentList, function(cc) {
    hasName(cc, "code") && hasName(cc, "plot")
  })

  if(!all(check_component)) {
    need_fix <- names(componentList)[which(!check_component)]
    if(is.null(need_fix)) {
      warning("One or more handlers do not provide both code and plot at the same time")
    } else {
      warning(paste0("the handler(s) of: ", paste(need_fix, collapse = " "), " do not provide both code and plot at the same time"))
    }
  }


  plot_list <- map(componentList, 1)
  code_list <- map(componentList, 2)

  names(code_list)[1] <- "geom"
  code_list[['data']] <- data_path

  pp <- get_plot(data, plot_list)
  final_code <- get_code(code_list)

  return(list(plot = pp, code = final_code))

}


