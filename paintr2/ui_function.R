generate_ui_upload <- function(id) {
  .expr <- expr(
    pickerInput(!!id, "select a default dataset:",
                choices = c("iris", "mtcars","penguins", "txhousing", "faithfuld",
                            "diamonds", "economics", "economics_long", "CO2"),
                selected = "",
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  )
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  return(ui)
}

generate_ui_var_placeholder <- function(id) {
  .expr <- expr(uiOutput(paste0("var-", !!id)))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- "placeholder"
  return(ui)
}

generate_ui_text <- function(id, param) {

  # if(is.list(param) && is.null(param[[1]])) {
  #   param <- paste0(str_split(id, "\\+")[[1]][1], " argument")
  # }

  .expr <- expr(textInput(!!id, paste0("text for ", !!param, ":")))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  return(ui)
}

generate_ui_num <- function(id, param) {

  # if(is.list(param) && is.null(param[[1]])) {
  #   param <- paste0(str_split(id, "\\+")[[1]][1], " argument")
  # }

  .expr <- expr(numericInput(!!id, paste0("num for ", !!param, ":"), NA))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  return(ui)
}

generate_ui_expr <- function(id, param) {

  # if(is.list(param) && is.null(param[[1]])) {
  #   param <- paste0(str_split(id, "\\+")[[1]][1], " argument")
  # }

  .expr <- expr(textInput(!!id, paste0("expr for ", !!param, ":")))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  return(ui)
}

generate_ui_var <- function(data_var, id, param) {

  if (is.null(data_var)) return(NULL)

  if((is.list(param) && is.null(param[[1]])) ||
     (param == "")) {
    id_breakdown <- str_split(id, "\\+")[[1]]
    param <- paste0(id_breakdown[1], " argument ", as.numeric(id_breakdown[3])-1 )
  }

  .expr <- expr(
    pickerInput(!!id, paste0(!!param, ": "),
                choices = !!data_var,
                selected = "",
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  )
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  return(ui)

}

generate_ui_individual <- function(key, id, param) {
  key <- as_string(key)

  if((is.list(param) && is.null(param[[1]])) ||
     (param == "")) {
    id_breakdown <- str_split(id, "\\+")[[1]]
    param <- paste0(id_breakdown[1], " argument ", as.numeric(id_breakdown[length(id_breakdown)])-1 )
  }

  if (key == "upload") {
    generate_ui_upload(id)
  } else if (key == "var") {
    generate_ui_var_placeholder(id)
  } else if (key == "text") {
    generate_ui_text(id, param)
  } else if (key == "num") {
    generate_ui_num(id, param)
  } else if (key == "expr") {
    generate_ui_expr(id, param)
  } else {
    NULL
  }
}


output_embed_var <- function(input, output, paintr_obj) {

  # browser()

  param_list <- paintr_obj[['param_list']]
  keywords_list <- paintr_obj[['keywords_list']]
  id_list <- paintr_obj[['id_list']]

  expr_list <- list()
  var_ui_list <- list()


  for (i in seq_along(keywords_list)) {# for each expr

    str_keywords <- sapply(keywords_list[[i]], function(.e) as_string(.e))

    # take care of the global data
    if (names(keywords_list)[i] == 'ggplot') {

      data_index <- which(param_list[[i]] == 'data')
      if (length(data_index) != 0) { # global data is provided

        if (str_keywords[data_index] == 'upload') {

          global_data_flag <- "upload"
          global_data_id <- id_list[[i]][[data_index]]

        } else {

          global_data_flag <- "local"
          global_data <- eval(keywords_list[[i]][[data_index]])

        }

      } else {

        global_data_flag <- NULL
        global_data_id <- NULL
        global_data_var <- NULL

      }

    }


    if ("var" %in% str_keywords) { # if this expr contains var

      data_var <- NULL
      data_index <- which(param_list[[i]] == 'data')

      for (var_index in which(str_keywords == 'var')) {

        if (length(data_index) != 0) { # if this expr has data param

          if (str_keywords[data_index] == 'upload') { # if data = upload
            tmp_data <- tryCatch({
              get(input[[id_list[[i]][[data_index]]]])
            }, error = function(e) NULL)
          } else { # if data = mpg
            tmp_data <- eval(keywords_list[[i]][[data_index]])
          }

          if (!is.null(tmp_data)) {
            data_var <- names(tmp_data)
          }

        } else { # this expr doesn't contain data param, global data should be used

          if (!is.null(global_data_flag)) {

            if (global_data_flag == 'upload') {
              tmp_data <- tryCatch({
                get(input[[global_data_id]])
              }, error = function(e) NULL)
            } else if (global_data_flag == 'local') {
              tmp_data <- global_data
            }

            # data_var <- if (is.null(tmp_data)) NULL else names(tmp_data)
            if (!is.null(tmp_data)) {
              data_var <- names(tmp_data)
            }

          } else {
            stop("data is not provided!")
          }
        }

        var_ui_list[[id_list[[i]][[var_index]]]] <- generate_ui_var(
          data_var,
          id_list[[i]][[var_index]],
          param_list[[i]][[var_index]]
        )

        # browser()

        if (length(data_index) != 0) {

          if (str_keywords[data_index] == 'upload') {

            expr_list[[length(expr_list)+1]] <- expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- renderUI({
                req(input[[!!id_list[[i]][[data_index]]]])

                var_ui_list[[!!id_list[[i]][[var_index]]]]
              })
            )

          } else {

            expr_list[[length(expr_list)+1]] <- expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- renderUI({
                var_ui_list[[!!id_list[[i]][[var_index]]]]
              })
            )

          }

        } else {

          if (global_data_flag == 'upload') {

            expr_list[[length(expr_list)+1]] <- expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- renderUI({
                req(input[[!!global_data_id]])

                var_ui_list[[!!id_list[[i]][[var_index]]]]
              })
            )

          } else {

            expr_list[[length(expr_list)+1]] <- expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- renderUI({
                var_ui_list[[!!id_list[[i]][[var_index]]]]
              })
            )

          }
        }
      }
    }

  }

  for (i in seq_along(expr_list)) eval(expr_list[[i]])
  return(var_ui_list)
}


var_ui_replacement <- function(ui_list, var_ui_list) {
  tmp_ui_list <- ui_list

  var_id <- names(var_ui_list)

  id_domain <- sapply(var_id, function(.id) strsplit(.id, "\\+")[[1]][1])

  for (i in seq_along(var_id)) {
    id <- names(id_domain)[i]
    tmp_ui_list[[id_domain[i]]][[id]] <- var_ui_list[[i]]
  }

  return(tmp_ui_list)

}


##########

foo2 <- function(input) {
  renderUI({
    # req(input[["ggplot+2"]])

    # data <- get(input[["ggplot+2"]])

    pickerInput("tt_id", "x:",
                choices = names(mpg),
                selected = "",
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
}

foo3 <- function(input) {
  renderUI({
    # req(input[["ggplot+2"]])

    # data <- get(input[["ggplot+2"]])

    pickerInput("tt_id", "y:",
                choices = names(mpg),
                selected = "",
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
}



change_output <- function(input, output, tt) {

  id_list <- c("ggplot+3+2+2", "ggplot+3+3")
  param_list <- c("x", "yabcdef")

  expr_list <- list()

  for (i in 1:2) {
    expr_list[[i]] <- expr(
      output[[paste0("var-", !!id_list[i])]] <- generate_ui_var_data_local(names(mpg), !!id_list[i], !!param_list[i])
    )
  }

  # eval(expr_list[[1]])
  # eval(expr_list[[2]])

  for (i in seq_along(expr_list)) eval(expr_list[[i]])
  browser()

}

change_output2 <- function(input, output, tt) {

  output[['var-ggplot+3+2+2']] <- generate_ui_var_data_local(names(mpg), "tt_id1", "x")
  output[['var-ggplot+3+3']] <-  generate_ui_var_data_local(names(mpg), "tt_id2", "y")

}

# generate ui for upload, text, num, expr, etc
# generate placeholder ui for var
# keep track of var placeholders and data


