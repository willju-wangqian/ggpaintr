generate_ui_upload <- function(id) {
  pickerInput(id, "select a default dataset:",
              choices = c("iris", "mtcars","penguins", "txhousing", "faithfuld",
                          "diamonds", "economics", "economics_long", "CO2"),
              selected = "",
              multiple = TRUE,
              options = pickerOptions(maxOptions = 1))
}

generate_ui_var_placeholder <- function(id) {
  uiOutput(paste0("var-", id))
}

generate_ui_text <- function(id, param) {
  textInput(id, paste0("text for ", param, ":"))
}

generate_ui_num <- function(id, param) {
  numericInput(id, paste0("num for ", param, ":"), NA)
}

generate_ui_expr <- function(id, param) {
  textInput(id, paste0("expr for ", param, ":"))
}

generate_ui_individual <- function(key, id, param) {
  key <- as_string(key)

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

generate_ui_var_data_input <- function(input, data_id, id, param) {
  renderUI({
    req(input[[data_id]])

    data <- get(input[[data_id]])

    pickerInput(id, paste0(param, ": "),
                choices = names(data),
                selected = "",
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
}

generate_ui_var_data_local <- function(data_var, id, param) {
  renderUI({
    pickerInput(id, paste0(param, ": "),
                choices = data_var,
                selected = "",
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
}

output_embed_var <- function(input, output, paintr_obj) {

  # browser()

  param_list <- paintr_obj[['param_list']]
  keywords_list <- paintr_obj[['keywords_list']]
  id_list <- paintr_obj[['id_list']]

  expr_list <- list()

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
          global_data_var <- names(eval(keywords_list[[i]][[data_index]]))

        }

      } else {

        global_data_flag <- NULL
        global_data_id <- NULL
        global_data_var <- NULL

      }

    }


    if ("var" %in% str_keywords) { # if this expr contains var

      data_index <- which(param_list[[i]] == 'data')
      if (length(data_index) != 0) { # if this expr has data param

        if (str_keywords[data_index] == 'upload') { # data = upload

          for (var_index in which(str_keywords == 'var')) {

            expr_list[[length(expr_list)+1]] <- expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <-
                generate_ui_var_data_input(input,
                                           !!id_list[[i]][[data_index]],
                                           !!id_list[[i]][[var_index]],
                                           !!param_list[[i]][[var_index]])
            )


          }

        } else { # data = something else, we have a local data

          tmp_data <- eval(keywords_list[[i]][[data_index]])
          data_var <- names(tmp_data)

          for (var_index in which(str_keywords == 'var')) {

            expr_list[[length(expr_list)+1]] <- expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <-
                generate_ui_var_data_local(data_var,
                                           !!id_list[[i]][[var_index]],
                                           !!param_list[[i]][[var_index]])
            )

          }

        }
      } else { # this expr doesn't contain data param, global data should be used

        if (!is.null(global_data_flag)) {

          if (global_data_flag == 'upload') {

            for (var_index in which(str_keywords == 'var')) {

              expr_list[[length(expr_list)+1]] <- expr(
                output[[paste0("var-", !!id_list[[i]][[var_index]])]] <-
                  generate_ui_var_data_input(input,
                                             global_data_id,
                                             !!id_list[[i]][[var_index]],
                                             !!param_list[[i]][[var_index]])
              )

            }

          } else if (global_data_flag == 'local') {

            for (var_index in which(str_keywords == 'var')) {

              expr_list[[length(expr_list)+1]] <- expr(
                output[[paste0("var-", !!id_list[[i]][[var_index]])]] <-
                  generate_ui_var_data_local(global_data_var,
                                             !!id_list[[i]][[var_index]],
                                             !!param_list[[i]][[var_index]])
              )


            }

          }

        } else {

          stop("data is not provided!")

        }

      }
    }

  }

  # browser()
  for (i in seq_along(expr_list)) eval(expr_list[[i]])

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


