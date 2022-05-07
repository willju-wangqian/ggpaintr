#' Build reactivity for `scale_color` or `scale_fill`
#'
#' @param id An ID string that corresponds with the ID used for all component of this `paintr_obj`
#' @param paintr_rctv a reactive value of a `paintr_obj` object created by `paintr()`
#' @param color_or_fill string. either `color` or `fill`. Specifies whether it's `scale_color`
#' or `scale_fill`
#'
#' @return a reactive object that is passed into `paintr_plot_code()`
#' @export
#'
#' @import shiny RColorBrewer
#' @importFrom assertthat assert_that has_name
#' @importFrom magrittr %>%
#'
scaleColor_build_reactivity <- function(id, paintr_rctv, color_or_fill) {

  moduleServer(
    id,
    function(input, output, session) {

      selectedColors_box <- reactive({
        req(paintr_rctv())

        data <- paintr_rctv()[['data']]

        color_or_fill <- match.arg(color_or_fill, c("color", "fill"))
        fill_id <- paintr_get_ui(paintr_rctv(), color_or_fill, type = "id")

        if(color_or_fill == "color") {
          scaleColorid <- paintr_get_ui(paintr_rctv(), "scale_color", type = "id")
        } else {
          scaleColorid <- paintr_get_ui(paintr_rctv(), "scale_fill", type = "id")
        }

        req(input[[fill_id]], has_name(data, input[[fill_id]]))

        if(is.null(scaleColorid)) {
          result <- NULL
        }

        color_var <- data[[input[[fill_id]]]]
        ns <- NS(id)

        if (is.character(color_var) || is.factor(color_var) ) {
          num_color <- length(unique( color_var ))

          TOO_MANY_LEVELS <- num_color > 11

          if(TOO_MANY_LEVELS) {
            result <- list(type = "TOO_MANY_LEVELS")
          }

          init_colors <- brewer.pal(num_color, "RdYlBu")
          if(length(init_colors) > num_color) {
            init_colors <- init_colors[1:num_color]
          }

          labels <- unique( color_var )

          colorPickers <- multipleColorPickerUI(id, init_colors,
                                                labels, ui_id = paste0(color_or_fill, "Picker"))

          result <- c(colorPickers, type = "categorical")

        } else if ( is.numeric(color_var) ) {

          init_colors <- brewer.pal(11, "RdBu")[c(9,3)]
          labels <- c('low', 'high')
          colorPickers <- multipleColorPickerUI(id, init_colors,
                                                labels, ui_id = paste0(color_or_fill, "Picker"))

          result <- c(colorPickers, type = "numerical")
        } else {
          result <- NULL
        }

        result

      }) %>%
        bindEvent({
          req(paintr_rctv(), paintr_get_ui(paintr_rctv(), color_or_fill, type = "id"))
          input[[paintr_get_ui(paintr_rctv(), color_or_fill, type = "id")]]
        })

      observe({

        req(selectedColors_box(), paintr_rctv())

        color_or_fill <- match.arg(color_or_fill, c("color", "fill"))
        fill_id <- paintr_get_ui(paintr_rctv(), color_or_fill, type = "id")

        if(color_or_fill == "color") {
          scaleColorid <- paintr_get_ui(paintr_rctv(), "scale_color", type = "id")
        } else {
          scaleColorid <- paintr_get_ui(paintr_rctv(), "scale_fill", type = "id")
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

      })  %>%
        # bindEvent(input[[paintr_get_ui(paintr_obj(), color_or_fill, type = "id")]])
        bindEvent({
          req(paintr_rctv(), paintr_get_ui(paintr_rctv(), color_or_fill, type = "id"))
          input[[paintr_get_ui(paintr_rctv(), color_or_fill, type = "id")]]
        })


      selectedColors_box

    }

  )
}


#' Connect parameters to their input obtained from the ui elements by their ids
#'
#' @param session_input input of a shiny session
#' @param id_list list of ids
#' @param params list of parameters
#' @param color_fill bool; optional. Whether or not to use the same variable for both color and fill
#' @param color_group bool; optional. Whether or not to use the same variable for both color and group
#'
#' @return a named list which has parameters as names and ids as list elements
#'
#' @importFrom assertthat assert_that has_name
#'
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
      has_name(param_arg_list, "color") || has_name(param_arg_list, "fill")
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
      has_name(param_arg_list, "color") || has_name(param_arg_list, "group")
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


#' Get all the implemented keys and their corresponding ui functions or handler functions
#'
#' @param scope optional. `scope` can be one of `mapping`, `geom_args`, or `plot_settings`
#' @param type optional. By default it's `ui`. Can be `ui` or `handler`
#' @param show_all optional. bool. `show_all = TRUE` prints all keys
#'
#' @return a named list where names are ggpaintr keys and list elements are the corresponding
#' ui functions or handler functions
#' @export
#'
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
                         scale_color = "scaleColorUI",
                         scale_fill = "scaleFillUI")
  )

  handlerControlList <- list(
    labs = "labsHandler",
    theme = "themeHandler",
    theme_choose = "themeChooseHandler",
    facet_grid = "facetHandler",
    coord_flip = "flipHandler",
    scale_color = "scaleColorFillHandler",
    scale_fill = "scaleColorFillHandler"
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

#' match key to its ui or handler function
#'
#' @param selected the key to be matched
#' @param scope optional. `scope` can be one of `mapping`, `geom_args`, or `plot_settings`
#' @param type optional. By default it's `ui`. Can be `ui` or `handler`
#'
#' @return the ui or handler function of `selected` key
#'
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

#' Calls the selected ui functions by the name of the key
#'
#' @param name name of the key
#' @param defaultArgs some default arguments passed to the ui functions
#' @param scope `scope` can be one of `mapping`, `geom_args`, or `plot_settings`
#' @param extraFunc optional. A named list of extra functions provided by the user.
#' For example `list(param1 = my_func1, param2 = my_func2)`
#' @param extraFuncArgs optional. A list of function arguments provided by the user.
#' Function arguments of one function should be formed in a list as one element of `extraFuncArgs`
#' For example `list(param1 = list(my_func1_arg1, my_func1_arg2), param2 = list(my_func2_arg1, my_func2_arg2))`
#'
#' @return function call of the selected function
#'
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

#' get all ui elements
#'
#' @param id An ID string that corresponds with the ID used for all component of this `paintr_obj`
#' @param defaultArgs some default arguments passed to the ui functions
#' @param mapping keys of mapping
#' @param geom_args keys of geom_args
#' @param plot_settings keys of plot_settings
#' @param extra_uiFunc optional. A named list of extra functions provided by the user.
#' For example `list(param1 = my_func1, param2 = my_func2)`
#' @param extra_uiFuncArgs optional. A list of function arguments provided by the user.
#' Function arguments of one function should be formed in a list as one element of `extraFuncArgs`
#' For example `list(param1 = list(my_func1_arg1, my_func1_arg2), param2 = list(my_func2_arg1, my_func2_arg2))`
#'
#' @return a list of ui and id
#'
controlUI <- function(id, defaultArgs, mapping, geom_args = NULL, plot_settings = NULL,
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

