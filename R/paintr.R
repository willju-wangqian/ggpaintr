#' Title
#'
#' @param expr
#'
#' @return
#' @export
#'
#' @examples
paintr_geom_construct <- function(expr){

  code <- enexpr(expr)
  rr <- unwrap_expr(!!code)

  plot_setting_piece <- list()
  geom_func <- NULL
  args_piece <- NULL
  mapping_piece <- NULL

  repeat(
    if(rr[[1]] == '+') {

      if( length(rr[[3]]) != 1) {
        plot_setting_piece <- append_list_name(plot_setting_piece,
                                               name = rr[[3]][[1]],
                                               value = unlist(rr[[3]][-1]))
      } else {
        plot_setting_piece <- append_list_name(plot_setting_piece,
                                               name = rr[[3]][[1]],
                                               value = NA)
      }

      rr <- rr[[2]]
    } else if (str_detect(rr[[1]], "geom")) {
      geom_func <- rr[[1]]

      args_piece <- unlist(rr[-c(1:2)])
      rr <- rr[[2]]
    } else if (str_detect(rr[[1]], "aes")) {
      mapping_piece <- unlist(rr[-1])
      break

    } else {
      break
    }
  )

  if(is.null(geom_func)) {
    stop("geom not found in the expression")
    return(NULL)
  } else if (is.null(mapping_piece)) {
    stop("mapping (aes) not found in the expression")
    return(NULL)
  }

  # args_piece <- fix_repeated_param(args_piece, "size")

  result <- list(geom_FUN = geom_func,
                 mapping = mapping_piece,
                 geom_args = args_piece,
                 plot_settings = plot_setting_piece)

  return(check_remove_null(result))
}

#' Title
#'
#' @param id
#' @param data_vars
#' @param expr
#' @param extra_uiFunc
#' @param extra_uiFuncArgs
#'
#' @return
#' @export
#'
#' @examples
paintr <- function(id, data_vars, expr, extra_ui = NULL, extra_ui_args = NULL) {
  code <- enexpr(expr)

  gg_components <- paintr_geom_construct(!!code)

  mapping <- if (has_name(gg_components, "mapping")) gg_components[['mapping']] else  NULL
  geom_args <- if (has_name(gg_components, "geom_args")) gg_components[['geom_args']] else NULL
  plot_settings <- if (has_name(gg_components, "plot_settings")) gg_components[['plot_settings']] else NULL

  defaultArgs <- list(
    ns = NS(id),
    data_vars = data_vars
  )

  defaultArgs <- addDefaultArgs(defaultArgs, "labs", "labs_selected", plot_settings)
  defaultArgs <- addDefaultArgs(defaultArgs, "theme", "theme_selected", plot_settings)

  shiny_components <- controlUI(id, data_vars,
                                defaultArgs = defaultArgs,
                                mapping, geom_args, names(plot_settings),
                                extra_uiFunc = extra_ui, extra_uiFuncArgs = extra_ui_args)

  result <- list(gg_components = gg_components,
                 shiny_components = shiny_components)
  attr(result, "class") <- "paintr_obj"
  return(result)

}



#' Title
#'
#' @param paintr_obj
#' @param selected_ui_name
#' @param type
#' @param scope
#'
#' @return
#' @export
#'
#' @examples
paintr_get_ui <- function(paintr_obj, selected_ui_name, type = "ui", scope = NULL) {

  stopifnot(class(paintr_obj) == "paintr_obj")

  type <- match.arg(type, c("ui", "id"))

  ui_names <- list(mapping = paintr_obj$gg_components$mapping,
                   geom_args = paintr_obj$gg_components$geom_args,
                   plot_settings = names(paintr_obj$gg_components$plot_settings))

  ui_selected <- ui_names[sapply(ui_names, function(nn) {  any(str_detect(nn, selected_ui_name)) })]

  if (length(ui_selected) == 0) {
    warning(paste0("The selected ui not found. return NULL\n",
                   "It's either not in getControlList() or not included in the expr."))
    return(NULL)
  } else if (length(ui_selected) > 1) {

    if (is.null(scope)) {
      warning("The selected ui component is ambiguous. Return NULL.\n",
              "Please use parameter scope. See ?paintr_get_ui")
      return(NULL)
    } else {

      scope <- match.arg(scope, c("mapping", "geom_args", "plot_settings"))
      ui_selected <- ui_selected[scope]
      selected_ui_name <- ui_selected[[scope]][str_detect(ui_selected[[scope]], selected_ui_name)]

      return(paintr_obj[['shiny_components']][[type]][[names(ui_selected)]][[selected_ui_name]])

    }

  } else {
    return(paintr_obj[['shiny_components']][[type]][[names(ui_selected)]][[selected_ui_name]])
  }

}


#' Title
#'
#'
#'
#' @param paintr_obj
#' @param id
#' @param data
#'
#' @note this function should be called inside `observeEvent()` since the `isolate()` scope provided
#' by `observeEvent()` is essential.
#'
#' @return
#' @export
#'
#' @examples
paintr_plot_code <- function(paintr_obj, id, data,
                             selected_color_rctv = NULL, selected_fill_rctv = NULL){
  stopifnot(class(paintr_obj) == "paintr_obj")

  geomComponent <- ggGeomHandler(id = id,
                                 data = data,
                                 geom_FUN = paintr_obj[['gg_components']][['geom_FUN']],
                                 id_list = paintr_obj[['shiny_components']][['id']],
                                 params_list = list( mapping = paintr_obj[['gg_components']][['mapping']],
                                                     geom_args = paintr_obj[['gg_components']][['geom_args']] )
  )

  plotSettingComponents <- mapply(function(nn) {
    if (is.null(nn)) return(NULL)

    FUNC <- matchControls(nn, type = "handler")
    if(is.null(FUNC)) return(NULL)

    funcArgsNames <- names(formals(FUNC))

    argList <- list(id = id,
                    module_id = paintr_obj[['shiny_components']][['id']][['plot_settings']][[nn]],
                    param = paintr_obj[['gg_components']][['plot_settings']][[nn]])

    if( nn == "scaleColor") {
      argList[['selected_color_fill_rctv']] <- selected_color_rctv
      argList[['color_fill']] <- 'color'
    }

    if( nn == "scaleFill") {
      argList[['selected_color_fill_rctv']] <- selected_fill_rctv
      argList[['color_fill']] <- 'fill'
    }

    argList <- check_remove_null(argList[funcArgsNames])
    if (is.null(argList)) {
      argList <- list()
    }

    do.call(FUNC, argList)

  }, names(paintr_obj[['gg_components']][['plot_settings']]))

  names(plotSettingComponents) <- names(paintr_obj[['gg_components']][['plot_settings']])

  plotSettingComponents <- check_remove_null(plotSettingComponents)

  return(c(geom = list(geomComponent), plotSettingComponents))

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


