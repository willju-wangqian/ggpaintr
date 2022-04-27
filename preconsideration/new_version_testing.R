# library(sloop)
#
# new_factor <- function(x = integer(), levels = character()) {
#   stopifnot(is.integer(x))
#   stopifnot(is.character(levels))
#
#   structure(
#     x,
#     levels = levels,
#     class = "my_factor"
#   )
# }
#
# new_factor(1:3, c('a', 'b', 'c'))
#
#
# s3_methods_class("lm")
# s3_methods_generic("t.test")
#
#
# new_my_class <- function(x = integer()) {
#   structure(
#     x,
#     class = "my_class"
#   )
# }
#
#
# confint.my_class <- function(object, parm, level = 0.95, ...) {
#   print("good")
# }
# s3_methods_generic("confint")
#
# x <- new_my_class(1L)
# confint(x)
#
#
#
#
#
#
# 128 * 256 * 3 * 3 *32 * 32+ 256 * 512 * 3 * 3 *32 *32
#
# tt <- matrix(c(12, 0, -5, 14, 10, 5, 14, 10, 5, 12, 0, -5), ncol = 4)
# tt
#
# apply(tt, 1, mean)
# apply(tt, 1, sd)
# apply(tt, 1, var)
#
# tt / c(1, 10, 100)
#
# (tt - apply(tt, 1, mean)) / apply(tt, 1, sd)
# apply(tt, 1, function(z) {
#   (z- mean(z)) / sd(z)
# })
#
# tt_new <- (tt - apply(tt, 1, mean)) / apply(tt, 1, sd)
#
# gamma_ <- c(1,1,1)
# beta_ <- c(0, -10, 10)
#
# tt_new * gamma_ + beta_
#
#
# data_name = unlist(str_split(as.character(input$fileData$name), '[.]'))
# data_name = paste(data_name[1:length(data_name)-1], collapse = '.')
#
# output$downloadplot = downloadHandler(
#   filename = function() { paste(data_name, '_boxplot', '.png', sep='') },
#   content = function(file) {
#     ggsave(file, results[['plot']])
#   }
# )
#
#
# tt <- expr(geom_boxplot(aes(x,y), position) + coord_flip + facet_grid)
# tt <- expr(geom_boxplot(mapping = aes(x,y), position = ) + coord_flip() + facet_grid())
# ast(geom_boxplot(mapping = aes(x,y), position) + coord_flip() + facet_grid())
#
#
# ast(geom_boxplot(aes(x,y), position) + coord_flip + facet_grid)
#
# my_test_func <- function(x) {
#   enexpr(x)
# }
#
#
# my_test_func(
#   geom_boxplot(aes(x,y), position) + coord_flip() + facet_grid()
# )
#
#
# rlang::call_standardise(tt)

library(rlang)
library(lobstr)


unwrap_expr <- function(x) {
  code <- enexpr(x)

  if(all(sapply(code, is_symbol))) {
    return(lapply(as.list(code), as_string))
  } else {
    lapply(as.list(code), function(code_piece) {
      if(is_call(code_piece)) {
        return(unwrap_expr(!!code_piece))
      } else {
        return(as_string(code_piece))
      }

    })

  }
}

append_list_name <- function(x, name, value) {
  stopifnot(is_character(name))

  x[[name]] <- value
  x
}

paintr_construct <- function(expr){

  code <- enexpr(expr)
  rr <- unwrap_expr(!!code)

  plot_setting_piece <- list()
  geom_func <- NULL
  args_piece <- NULL
  mapping_piece <- NULL

  repeat(
    if(rr[[1]] == '+') {
      # browser()

      if( length(rr[[3]]) != 1) {
        plot_setting_piece <- append_list_name(plot_setting_piece,
                                               name = rr[[3]][[1]],
                                               value = unlist(rr[[3]][-1]))
      } else {
        # plot_setting_piece <- append(plot_setting_piece, rr[[3]])
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

  # browser()
  args_piece <- fix_repeated_param(args_piece, "size")

  # browser()
  result <- list(geom_FUN = geom_func,
                 mapping = mapping_piece,
                 geom_args = args_piece,
                 plot_settings = plot_setting_piece)

  return(check_remove_null(result))
}

fix_repeated_param <- function(x, param, suffix = "_geom") {

  if(is.null(x)) {
    return(NULL)
  }


  stopifnot(is.character(x) && is.character(param))

  idx <- which(x == param)
  if( length(idx) == 0 ) {
    return(x)
  } else if (length(idx) == 1) {
    x[idx] <- paste0(param, suffix)
    return(x)
  } else {
    stop("Too many repeated parameters.")
    return(NULL)
  }

}

paintr_components <- function(id, data_vars, expr, extra_uiFunc = NULL, extra_uiFuncArgs = NULL) {
  code <- enexpr(expr)

  gg_components <- paintr_construct(!!code)

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
                                extra_uiFunc = extra_uiFunc, extra_uiFuncArgs = extra_uiFuncArgs)

  result <- list(gg_components = gg_components,
                 shiny_components = shiny_components)
  attr(result, "class") <- "paintr_obj"
  return(result)

}

addDefaultArgs <- function(defaultArgs, ui_element, ui_param, plot_settings) {
  if (has_name(plot_settings, ui_element)) {
    defaultArgs[[ui_param]] <- plot_settings[[ui_element]]
  }
  defaultArgs
}


paintr_get_ui <- function(paintr_obj, selected_ui_name, type = "ui", scope = NULL) {

  stopifnot(class(paintr_obj) == "paintr_obj")

  type <- match.arg(type, c("ui", "id"))

  ui_names <- list(mapping = paintr_obj$gg_components$mapping,
                   geom_args = paintr_obj$gg_components$geom_args,
                   plot_settings = names(paintr_obj$gg_components$plot_settings))

  ui_selected <- ui_names[sapply(ui_names, function(nn) {  any(str_detect(nn, selected_ui_name)) })]

  if (length(ui_selected) == 0) {
    warning("The selected ui not found. return NULL")
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


paintr_plot_code <- function(paintr_obj, id, data){
  # input, output, session) {
  # running with session is required
  stopifnot(class(paintr_obj) == "paintr_obj")

  geomComponent <- ggGeomGenerator(id = id,
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

    funcArgsNames <- names(formals(FUNC))[sapply(formals(FUNC), is.symbol)]

    argList <- list(id = id,
                    module_id = paintr_obj[['shiny_components']][['id']][['plot_settings']][[nn]],
                    param = paintr_obj[['gg_components']][['plot_settings']][[nn]])

    do.call(FUNC, argList[funcArgsNames])

  }, names(paintr_obj[['gg_components']][['plot_settings']]))

  names(plotSettingComponents) <- names(paintr_obj[['gg_components']][['plot_settings']])

  plotSettingComponents <- check_remove_null(plotSettingComponents)

  # browser()
  return(c(geom = list(geomComponent), plotSettingComponents))

}
#
# my_expr <- expr(geom_boxplot(aes(x, y, size), position, size) + coord_flip + facet_grid +
#                   theme(legend.title, legend.direction) + labs(x, y, title))
# rr <- paintr_components("boxPlot", mtcars,
#                         !!my_expr)
#
# # rr <- paintr_construct(!!my_expr)
#
# paintr_get_ui(rr, "size", "mapping")
# paintr_get_ui(rr, "labs")
#
#
# paintr_obj <- paintr_components("boxPlot", iris,
#                                 geom_boxplot(aes(x, y)) + coord_flip + facet_grid +
#                                   theme(legend.title, legend.direction) + labs(x, y, title) + theme_choose)
#
# paintr_get_ui(paintr_obj, "labs")

scaleColorWrapper <- function(id, paintr_obj, data, color_name, scaleColor_name) {
  moduleServer(
    id,
    function(input, output, session) {
      selectedColors_box <- reactive({
        req(paintr_obj(), data())

        fill_id <- paintr_get_ui(paintr_obj(), color_name, type = "id")
        scaleColorid <- scaleColor_name

        req(input[[fill_id]])

        assert_that(
          hasName(data(), input[[fill_id]])
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

          init_colors <- RColorBrewer::brewer.pal(num_color, "RdYlBu")
          labels <- unique( color_var )

          colorPickers <- multipleColorPickerUI(ns, init_colors, labels)

          result <- c(colorPickers, type = "categorical")

        } else if ( is.numeric(color_var) ) {

          init_colors <- RColorBrewer::brewer.pal(11, "RdBu")[c(9,3)]
          labels <- c('low', 'high')
          colorPickers <- multipleColorPickerUI(ns, init_colors, labels)

          result <- c(colorPickers, type = "numerical")
        } else {
          result <- NULL
        }

        result

      })  %>% bindEvent(paintr_obj(), input[[paintr_get_ui(paintr_obj(), color_name, type = "id")]])

      observe({

        req(selectedColors_box(), paintr_obj())

        fill_id <- paintr_get_ui(paintr_obj(), color_name, type = "id")
        scaleColorid <- scaleColor_name

        if(selectedColors_box()[['type']] == "TOO_MANY_LEVELS") {
          output[[scaleColorid]] <- renderUI({
            validate(paste( paste0("There are more than 11 levels in ",
                                   input[[ns_box(scaleColorIDs_box()[[1]])]], "."),
                            "Too many levels.", sep = "\n"))
          })
        } else {
          output[[scaleColorid]] <- renderUI({
            selectedColors_box()[['ui']]
          })
        }

      })  %>% bindEvent(paintr_obj(), input[[paintr_get_ui(paintr_obj(), color_name, type = "id")]])

      selectedColors_box
    }

  )
}



