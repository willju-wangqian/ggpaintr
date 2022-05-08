#' Construct a `paintr_obj` based on an expression in `ggplot2` alike layout
#'
#' @param id An ID string that corresponds with the ID used for all component of this `paintr_obj`
#' @param data the dataset for plotting
#' @param expr a `ggplot2` alike expression which is referred to as "paintr expression" in `ggpaintr`
#' @param extra_ui a named list: of extra functions that generate paintr_ui. The
#' names of this list should be key words of a paintr expression
#' For example `extra_ui = list(param1 = my_ui_func1, param2 = my_ui_func2)`
#' @param extra_ui_args a named list: of lists of arguments. The names of this
#' list should be key words of a paintr expression. And the element of a key word
#' should be a list of arguments that goes into the ui function of this key word
#' For example `list(param1 = list(my_ui_func1_arg1, my_ui_func1_arg2), param2 = list(my_ui_func2_arg1, my_ui_func2_arg2))`
#' @param data_path string. path to the dataset; used for code of obtaining the data
#'
#' @importFrom rlang enexpr
#'
#' @details the `expr` should have a `ggplot2` alike layout and be wrapped by `rlang::expr`.
#' And the functionality of `paintr` is based on this `expr` that follows certain
#' rules. In general, a ggpaintr expression should be:
#' ```{r, eval=FALSE}
#' rlang::expr(
#'   geom_<chart>(aes(<mapping_1>, <mapping_2>), <geom_args_1>, <geom_args2>) +
#'     <plot_settings_1> +
#'     <plot_settings_2> +
#'     <plot_settings_3>
#' )
#' ```
#' And note that:
#' - `<mapping_*>` is a `mapping` keyword that represents a aesthetic mapping in `ggplot2`, like `x`, `y`, or `color`
#' - `<geom_args_*>` is a `geom_args` keyword that represents an argument passed into `geom_<chart>`, like `position`
#' - `<plot_settings_*>` is a `plot_settings` keyword that represents a `ggplot2` function like `coord_flip`, which can modify the plot
#' - `geom_<chart>` specifies the geom function used for the plot. `'geom_'`is used to identify `geom_<chart>`
#' - `aes` is used to distinguish `mapping` keywords and `geom_args` keywords in `geom_<chart>`
#'
#' @note `extraFunc` and `extraFuncArgs` allow users to override the ui functions provided by `ggpaintr` package
#'
#' @return a `paintr_obj` that contains all pieces of the paintr expression and
#' and their corresponding ui elements used to build a shiny app. Additionally, it includes:
#'  - `id`
#'  - `data`
#'  - `data_path`
#' @export
#'
#' @examples
#' paintr("boxplot_id", mtcars, geom_boxplot(aes(x, y)))
#'
#' # alternatively, one can define the expression first
#' library(rlang)
#' my_expr <- rlang::expr(geom_boxplot(aes(x, y)))
#' paintr("boxplot_id", mtcars, !!my_expr)
paintr <- function(id, data, expr, extra_ui = NULL, extra_ui_args = NULL, data_path = "data") {
  code <- enexpr(expr)

  gg_components <- paintr_geom_construct(!!code)

  mapping <- if (has_name(gg_components, "mapping")) gg_components[['mapping']] else  NULL
  geom_args <- if (has_name(gg_components, "geom_args")) gg_components[['geom_args']] else NULL
  plot_settings <- if (has_name(gg_components, "plot_settings")) gg_components[['plot_settings']] else NULL

  data_vars <- names(data)

  defaultArgs <- list(
    id = id,
    data_vars = data_vars
  )

  defaultArgs <- addDefaultArgs(defaultArgs, "labs", "labs_selected", plot_settings)
  defaultArgs <- addDefaultArgs(defaultArgs, "theme", "theme_selected", plot_settings)

  shiny_components <- controlUI(id,
                                defaultArgs = defaultArgs,
                                mapping, geom_args, names(plot_settings),
                                extra_uiFunc = extra_ui, extra_uiFuncArgs = extra_ui_args)

  result <- list(gg_components = gg_components,
                 shiny_components = shiny_components,
                 id = id, data = data, data_path = data_path)
  attr(result, "class") <- "paintr_obj"
  return(result)

}

#' Deconstruct the paintr expression
#'
#' @param expr a `ggplot2` alike expression
#'
#' @return a list of pieces of the keywords
#'
#' @importFrom rlang enexpr
#' @importFrom stringr str_detect
#'
paintr_geom_construct <- function(expr){

  code <- enexpr(expr)
  rr <- unwrap_expr(!!code)

  plot_setting_piece <- list()
  geom_func <- NULL
  args_piece <- NULL
  mapping_piece <- NULL


  # here NA is an important placeholder
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

  result <- list(geom_FUN = geom_func,
                 mapping = mapping_piece,
                 geom_args = args_piece,
                 plot_settings = plot_setting_piece)

  return(check_remove_null(result))
}


#' Get a ui element or its id of a paintr keyword from a `paintr_obj`
#'
#' @param paintr_obj a `paintr_obj`
#' @param selected_ui_name the keyword of the desired ui element
#' @param type optional. `type` can be `ui` or `id`
#' @param scope one value of `mapping`, `geom_args`, or `plot_settings`. Used to
#' distinguish keywords with the same name but in different scope. For example `size`
#' can be either a `mapping` keyword or a `geom_args` keyword.
#' @param verbose Whether or not to send warning messages when ui element is not found
#'
#' @return the ui or id of `selected_ui_name`
#' @export
#'
#' @importFrom stringr str_detect
#'
#' @examples
#' ptr_obj <- paintr("boxplot_id", mtcars, geom_boxplot(aes(x, y)))
#' paintr_get_ui(ptr_obj, "x")
paintr_get_ui <- function(paintr_obj, selected_ui_name, type = "ui", scope = NULL, verbose = FALSE) {

  stopifnot(class(paintr_obj) == "paintr_obj")

  type <- match.arg(type, c("ui", "id"))

  ui_names <- list(mapping = paintr_obj$gg_components$mapping,
                   geom_args = paintr_obj$gg_components$geom_args,
                   plot_settings = names(paintr_obj$gg_components$plot_settings))

  ui_selected <- ui_names[sapply(ui_names, function(nn) {  any( nn == selected_ui_name ) })]

  if (length(ui_selected) == 0) {
    if (verbose) {
      warning(paste0("The selected ui not found. return NULL\n",
                     "It's either not in getControlList() or not included in the expr."))
    }
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


#' Generate plot and the corresponding code of `ggplot2` from a `paintr_obj`
#'
#' @param paintr_obj a `paintr_obj`
#' @param data_filter data filtering code
#' @param selected_color_rctv reactive value returned by `scaleColor_build_reactivity` for color
#' @param selected_fill_rctv reactive value returned by `scaleColor_build_reactivity` for fill
#' @param color_fill bool; optional. Whether or not to use the same variable for both color and fill
#' @param color_group bool; optional. Whether or not to use the same variable for both color and group
#' @param userFUN a function that returns a named list, where the names of
#' this named list are parameters (except for `mapping`) of `geom_<chart>`, and the elements
#' of this list are arguments of the corresponding parameters
#' @param ... arguments that go into `userFUN`
#'
#' @note this function should be called inside `observeEvent()` since the `isolate()` scope provided
#' by `observeEvent()` is essential.
#'
#' @return a named list of two elements; plot and code
#' @export
paintr_plot_code <- function(paintr_obj, data_filter = "",
                             selected_color_rctv = NULL, selected_fill_rctv = NULL,
                             color_fill = FALSE, color_group = FALSE, userFUN = NULL, ...){

  stopifnot(class(paintr_obj) == "paintr_obj")

  id <- paintr_obj[['id']]
  data <- paintr_obj[['data']]
  data_path <- paintr_obj[['data_path']]

  geomComponent <- ggGeomHandler(id = id,
                                 data = data,
                                 geom_FUN = paintr_obj[['gg_components']][['geom_FUN']],
                                 id_list = paintr_obj[['shiny_components']][['id']],
                                 params_list = list( mapping = paintr_obj[['gg_components']][['mapping']],
                                                     geom_args = paintr_obj[['gg_components']][['geom_args']] ),
                                 userFUN = userFUN, ...
  )

  plotSettingComponents <- mapply(function(nn) {
    if (is.null(nn)) return(NULL)

    FUNC <- matchControls(nn, type = "handler")
    if(is.null(FUNC)) return(NULL)

    funcArgsNames <- names(formals(FUNC))

    argList <- list(id = id,
                    module_id = paintr_obj[['shiny_components']][['id']][['plot_settings']][[nn]],
                    param = paintr_obj[['gg_components']][['plot_settings']][[nn]])

    if( nn == "scale_color") {
      argList[['selected_color_fill_rctv']] <- selected_color_rctv
      argList[['color_fill']] <- 'color'
    }

    if( nn == "scale_fill") {
      argList[['selected_color_fill_rctv']] <- selected_fill_rctv
      argList[['color_fill']] <- 'fill'
    }

    argList <- check_remove_null(argList[funcArgsNames])
    if (is.null(argList)) {
      argList <- list()
    }

    do.call(FUNC, argList)

  }, names(paintr_obj[['gg_components']][['plot_settings']]), SIMPLIFY = FALSE)

  names(plotSettingComponents) <- names(paintr_obj[['gg_components']][['plot_settings']])

  plotSettingComponents <- check_remove_null(plotSettingComponents)

  componentList <- c(geom = list(geomComponent), plotSettingComponents)

  result <- get_plot_code(componentList, data, data_path = data_path, data_filter = data_filter)

  return(result)

}

#' Extract filtering code
#'
#' @param data dataset
#' @param search filter conditions
#'
#' @importFrom jsonlite fromJSON
#'
#' @return filtering code
#' @export
get_filter <- function(data, search) {
  parse_search_expression <- function(x, name_var, s) {
    if (!nzchar(s)) return(FALSE)
    if (is.numeric(x)) {
      r <- strsplit(s, "...", fixed = TRUE)
      r <- sapply(r, as.numeric)
      out = deparse(bquote((x >= .(r[1])) & (x <= .(r[2]))))
      out = gsub('x', name_var, out)
    } else if (is.factor(x) || is.logical(x)) {
      v <- jsonlite::fromJSON(s)
      out = deparse(bquote(x %in% .(v)))
      out = paste0(name_var, substring(out, 2))
    } else {
      out = deparse(bquote(grepl(.(s), x, fixed = TRUE)))
    }
    out
  }

  result = Map(parse_search_expression, data, names(data), search)
  result = result[sapply(result, is.character)]

  if(length(result) > 0){
    # browser()
    paste0('data', ' <- ', 'data',  ' %>% filter(\n  ',
           paste(unlist(result), collapse = ',\n  '), '\n)')
  }
  else{""}
}



#' Extract code
#'
#' @param code_list list of code
#'
#' @importFrom assertthat assert_that has_name
#'
#' @return code
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

#' Extract plot
#'
#' @param data data used for the plot
#' @param gg_list list of `gg` objects
#'
#' @return `ggplot` object
get_plot <- function(data, gg_list) {

  p <- ggplot(data = data)
  for (i in seq_along(gg_list)) {
    p <- p + gg_list[[i]]
  }

  return(p)

}


#' Extract code and plot
#'
#' @param componentList list of plot components and code components
#' @param data data
#' @param data_path path to the data
#' @param data_filter data filtering code
#'
#' @importFrom assertthat has_name
#' @importFrom purrr map
#'
#' @return a named list of two elements; plot and code
get_plot_code <- function(componentList, data, data_path, data_filter = "") {

  componentList <- check_remove_null(componentList)

  check_component <- sapply(componentList, function(cc) {
    has_name(cc, "code") && has_name(cc, "plot")
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
  code_list[['data']] <- paste0(data_path, '\n', data_filter)

  pp <- get_plot(data, plot_list)
  final_code <- get_code(code_list)

  return(list(plot = pp, code = final_code))

}


