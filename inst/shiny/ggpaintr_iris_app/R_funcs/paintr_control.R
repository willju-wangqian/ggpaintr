#' Title
#'
#' @param id
#' @param paintr_obj
#' @param data
#' @param color_or_fill
#'
#' @return
#' @export
#'
#' @import dplyr shiny RColorBrewer assertthat rlang
#'
#' @examples
scaleColor_build_reactivity <- function(id, paintr_obj, data, color_or_fill) {
  moduleServer(
    id,
    function(input, output, session) {

      selectedColors_box <- reactive({
        req(paintr_obj(), data())

        color_or_fill <- match.arg(color_or_fill, c("color", "fill"))
        fill_id <- paintr_get_ui(paintr_obj(), color_or_fill, type = "id")

        if(color_or_fill == "color") {
          scaleColorid <- paintr_get_ui(paintr_obj(), "scaleColor", type = "id")
        } else {
          scaleColorid <- paintr_get_ui(paintr_obj(), "scaleFill", type = "id")
        }

        req(input[[fill_id]])

        assert_that(
          has_name(data(), input[[fill_id]])
        )

        if(is.null(scaleColorid)) {
          result <- NULL
        }

        color_var <- data()[[input[[fill_id]]]]
        ns <- NS(id)

        if (is.character(color_var) || is.factor(color_var) ) {
          num_color <- length(unique( color_var ))

          TOO_MANY_LEVELS <- num_color > 11

          if(TOO_MANY_LEVELS) {
            result <- list(type = "TOO_MANY_LEVELS")
          }

          init_colors <- brewer.pal(num_color, "RdYlBu")
          labels <- unique( color_var )

          colorPickers <- multipleColorPickerUI(ns, init_colors,
                                                labels, id = paste0(color_or_fill, "Picker"))

          result <- c(colorPickers, type = "categorical")

        } else if ( is.numeric(color_var) ) {

          init_colors <- brewer.pal(11, "RdBu")[c(9,3)]
          labels <- c('low', 'high')
          colorPickers <- multipleColorPickerUI(ns, init_colors,
                                                labels, id = paste0(color_or_fill, "Picker"))

          result <- c(colorPickers, type = "numerical")
        } else {
          result <- NULL
        }

        result

      })  %>% bindEvent(paintr_obj(), input[[paintr_get_ui(paintr_obj(), color_or_fill, type = "id")]],
                        ignoreInit = TRUE)

      observe({

        req(selectedColors_box(), paintr_obj(), data())

        color_or_fill <- match.arg(color_or_fill, c("color", "fill"))
        fill_id <- paintr_get_ui(paintr_obj(), color_or_fill, type = "id")

        if(color_or_fill == "color") {
          scaleColorid <- paintr_get_ui(paintr_obj(), "scaleColor", type = "id")
        } else {
          scaleColorid <- paintr_get_ui(paintr_obj(), "scaleFill", type = "id")
        }

        if(selectedColors_box()[['type']] == "TOO_MANY_LEVELS") {
          output[[scaleColorid]] <- renderUI({
            validate(paste( paste0("There are more than 11 levels in ",
                                   input[[fill_id]], "."),
                            "Too many levels.", sep = "\n"))
          })
        } else {
          output[[scaleColorid]] <- renderUI({
            selectedColors_box()[['ui']]
          })
        }

      })  %>% bindEvent(paintr_obj(), input[[paintr_get_ui(paintr_obj(), color_or_fill, type = "id")]])


      selectedColors_box

    }

  )
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

  param_arg_list <- lapply(id_list, function(id, input) {
    input[[id]]
  }, input = session_input)

  names(param_arg_list) <- params

  if(color_fill) {
    assert_that(
      hasName(param_arg_list, "color") || hasName(param_arg_list, "fill")
    )

    if (is.null(param_arg_list[['fill']])) {
      param_arg_list[['fill']] <- param_arg_list[['color']]
    }

    if(is.null(param_arg_list[['color']])) {
      param_arg_list[['color']] <- param_arg_list[['fill']]
    }
  }

  if(color_group) {
    assert_that(
      hasName(param_arg_list, "color") || hasName(param_arg_list, "group")
    )

    if (is.null(param_arg_list[['group']])) {
      param_arg_list[['group']] <- param_arg_list[['color']]
    }

    if(is.null(param_arg_list[['color']])) {
      param_arg_list[['color']] <- param_arg_list[['group']]
    }
  }

  check_remove_null(param_arg_list)
}


#' Title
#'
#' @param user_defined
#'
#' @return
#' @export
#'
#' @examples
getControlList <- function(scope = "mapping", type = "ui", show_all = FALSE) {

  type <- match.arg(type, c("ui", "handler"))
  scope <- match.arg(scope, c("mapping", "geom_args", "plot_settings"))

  uiControlList <- list(
    mapping = list(x = "mappingXUI",
                   y = "mappingYUI",
                   z = "mappingZUI",
                   color = "mappingColorUI",
                   shape = "mappingShapeUI",
                   size = "mappingSizeUI",
                   fill = "mappingFillUI",
                   group = "mappingGroupUI"),
    geom_args = list(stat = "argsStatUI",
                     position = "argsPositionUI",
                     alpha = "argsAlphaUI",
                     size = "argsSizeUI"),
    plot_settings = list(theme = "themeUI",
                         theme_choose = "themeChooseUI",
                         coord_flip = "settingFlipUI",
                         facet_grid = "settingFacetUI",
                         labs = "labsUI",
                         scaleColor = "scaleColorUI",
                         scaleFill = "scaleFillUI")
  )

  handlerControlList <- list(
    labs = "labsHandler",
    theme = "themeHandler",
    theme_choose = "themeChooseHandler",
    facet_grid = "facetHandler",
    coord_flip = "flipHandler",
    scaleColor = "scaleColorFillHandler",
    scaleFill = "scaleColorFillHandler"
  )

  if(show_all) {
    return(print(list(ui = uiControlList,
                      handler = handlerControlList)))
  }

  if (type == "ui") {
    return(uiControlList[[scope]])
  }

  if (type == "handler") {
    return(handlerControlList)
  }

}

#' Title
#'
#' @param ui_part
#'
#' @return
#' @export
#'
#' @examples
matchControls <- function(selected, scope = "mapping", type = "ui") {

  controlList <- getControlList(scope, type)

  if ( is.null(controlList[[selected]]) ) {
    warning( paste("The", type, "part for", selected, "has not been implemented yet.")  )
    return(NULL)
  } else {
    # return( getFromNamespace(controlList[[selected]], asNamespace("ggpaintr")) )
    return( match.fun(controlList[[selected]]) )
  }

}

#' Title
#'
#' @param name
#' @param mp
#' @param defaultArgs
#' @param extraFunc optional. A named list of extra functions provided by the user.
#' For example `list(param1 = my_func1, param2 = my_func2)`
#' @param extraFuncArgs optional. A list of function arguments provided by the user.
#' Function arguments of one function should be formed in a list as one element of `extraFuncArgs`
#' For example `list(param1 = list(my_func1_arg1, my_func1_arg2), param2 = list(my_func2_arg1, my_func2_arg2))`
#'
#'
#' @note `extraFunc` and `extraFuncArgs` allow users to override
#'
#'
#' @return
#' @export
#'
#' @examples
callFuncUI <- function(name, defaultArgs, scope, extraFunc = NULL, extraFuncArgs = NULL) {

  if ( is.null(name) ) {
    return(NULL)
  }

  UI_FUN <- if (!is.null(extraFunc[[name]])) {
    extraFunc[[name]]
  } else {
    matchControls(name, scope, type = "ui")
  }

  if( !is.null(UI_FUN) ) {

    # UI_FUN_args_names <- names(formals(UI_FUN))[sapply(formals(UI_FUN), is.symbol)]
    UI_FUN_args_names <- names(formals(UI_FUN))

    UI_FUN_args <- if( !is.null(extraFuncArgs[[name]]) ) {
      extraFuncArgs[[name]]
    } else {
      defaultArgs[UI_FUN_args_names]
    }

    return( do.call(UI_FUN, check_remove_null(UI_FUN_args)) )
  } else {
    return(NULL)
  }

}

#' Title
#'
#' @param id
#' @param data
#' @param mapping
#' @param geom_args
#' @param extra_uiFunc
#' @param extra_uiFuncArgs
#'
#' @return
#' @export
#'
#' @examples
controlUI <- function(id, data_vars, mapping, defaultArgs, geom_args = NULL, plot_settings = NULL,
                      extra_uiFunc = NULL, extra_uiFuncArgs = NULL) {
  ns <- NS(id)

  mapping <- check_char_set_names(mapping)
  geom_args <- check_char_set_names(geom_args)
  plot_settings <- check_char_set_names(plot_settings)

  mapping_ui <- mapply(callFuncUI, names(mapping),
                       MoreArgs = list(
                         defaultArgs = defaultArgs,
                         scope = "mapping",
                         extraFunc = extra_uiFunc,
                         extraFuncArgs = extra_uiFuncArgs
                       ),
                       SIMPLIFY = FALSE)

  geom_args_ui <- mapply(callFuncUI, names(geom_args),
                         MoreArgs = list(
                           defaultArgs = defaultArgs,
                           scope = "geom_args",
                           extraFunc = extra_uiFunc,
                           extraFuncArgs = extra_uiFuncArgs
                         ),
                         SIMPLIFY = FALSE)

  plot_settings_ui <- mapply(callFuncUI, names(plot_settings),
                             MoreArgs = list(
                               defaultArgs = defaultArgs,
                               scope = "plot_settings",
                               extraFunc = extra_uiFunc,
                               extraFuncArgs = extra_uiFuncArgs
                             ),
                             SIMPLIFY = FALSE)


  mapping_ui <- check_remove_null(mapping_ui)
  geom_args_ui <- check_remove_null(geom_args_ui)
  plot_settings_ui <- check_remove_null(plot_settings_ui)

  result <- list(
    ui = list(mapping = check_remove_null(purrr::map(mapping_ui, 1)),
              geom_args = check_remove_null(purrr::map(geom_args_ui, 1)),
              plot_settings = check_remove_null(purrr::map(plot_settings_ui, 1))),
    id = list(mapping = check_remove_null(purrr::map(mapping_ui, 2)),
              geom_args = check_remove_null(purrr::map(geom_args_ui, 2)),
              plot_settings = check_remove_null(purrr::map(plot_settings_ui, 2)))
  )

  return(result)

}

