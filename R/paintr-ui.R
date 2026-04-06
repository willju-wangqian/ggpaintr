#' Build Upload UI for a Placeholder
#'
#' @param id Placeholder id.
#'
#' @return A Shiny UI object with an attached `ui_expr` attribute.
#' @keywords internal
generate_ui_upload <- function(id) {
  .expr <- rlang::expr(
    shiny::tagList(
      shiny::fileInput(
        !!id,
        "upload a dataset (.csv or .rds):",
        accept = c(".csv", ".rds")
      ),
      shiny::textInput(
        paintr_upload_name_id(!!id),
        "dataset object name:",
        placeholder = "Defaults to the uploaded file name"
      ),
      shiny::helpText("Leave the dataset name blank to use the uploaded file name.")
    )
  )

  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  ui
}

#' Build a Deferred `var` UI Placeholder
#'
#' @param id Placeholder id.
#'
#' @return A placeholder `uiOutput()`.
#' @keywords internal
generate_ui_var_placeholder <- function(id) {
  .expr <- rlang::expr(shiny::uiOutput(paste0("var-", !!id)))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- "placeholder"
  ui
}

#' Build Text Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#'
#' @return A `textInput()` UI object.
#' @keywords internal
generate_ui_text <- function(id, param) {
  .expr <- rlang::expr(shiny::textInput(!!id, paste0("text for ", !!param, ":")))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  ui
}

#' Build Numeric Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#'
#' @return A `numericInput()` UI object.
#' @keywords internal
generate_ui_num <- function(id, param) {
  .expr <- rlang::expr(shiny::numericInput(!!id, paste0("num for ", !!param, ":"), NA))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  ui
}

#' Build Expression Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#'
#' @return A `textInput()` UI object.
#' @keywords internal
generate_ui_expr <- function(id, param) {
  .expr <- rlang::expr(shiny::textInput(!!id, paste0("expr for ", !!param, ":")))
  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  ui
}

#' Build a Variable Selector UI
#'
#' @param data_var Character vector of candidate variables.
#' @param id Placeholder id.
#' @param param Parameter label.
#'
#' @return A `pickerInput()` UI object or `NULL`.
#' @keywords internal
generate_ui_var <- function(data_var, id, param) {
  if (is.null(data_var)) {
    return(NULL)
  }

  if ((is.list(param) && is.null(param[[1]])) || identical(param, "")) {
    id_breakdown <- stringr::str_split(id, "\\+")[[1]]
    param <- paste0(id_breakdown[1], " argument ", as.numeric(id_breakdown[3]) - 1)
  }

  .expr <- rlang::expr(
    shinyWidgets::pickerInput(
      !!id,
      paste0(!!param, ": "),
      choices = !!data_var,
      selected = "",
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(maxOptions = 1)
    )
  )

  ui <- eval(.expr)
  attr(ui, "ui_expr") <- .expr
  ui
}

#' Build UI for One Placeholder
#'
#' @param key Placeholder keyword.
#' @param id Placeholder id.
#' @param param Parameter label.
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
generate_ui_individual <- function(key, id, param) {
  key <- rlang::as_string(key)

  if ((is.list(param) && is.null(param[[1]])) || identical(param, "")) {
    id_breakdown <- stringr::str_split(id, "\\+")[[1]]
    param <- paste0(
      id_breakdown[1], " argument ",
      as.numeric(id_breakdown[length(id_breakdown)]) - 1
    )
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

#' Build Dynamic `var` Controls for a Parsed Formula
#'
#' @param input A Shiny input object.
#' @param output A Shiny output object.
#' @param paintr_obj A `paintr_obj`.
#' @param envir Environment used to resolve local data objects.
#'
#' @return A named list of generated `var` UI controls.
#' @keywords internal
output_embed_var <- function(input, output, paintr_obj, envir = parent.frame()) {
  param_list <- paintr_obj[["param_list"]]
  keywords_list <- paintr_obj[["keywords_list"]]
  id_list <- paintr_obj[["id_list"]]

  expr_list <- list()
  var_ui_list <- list()

  for (i in seq_along(keywords_list)) {
    str_keywords <- vapply(keywords_list[[i]], rlang::as_string, character(1))

    if (names(keywords_list)[i] == "ggplot") {
      data_index <- which(param_list[[i]] == "data")
      if (length(data_index) != 0) {
        if (str_keywords[data_index] == "upload") {
          global_data_flag <- "upload"
          global_data_id <- id_list[[i]][[data_index]]
        } else {
          global_data_flag <- "local"
          global_data <- tryCatch(
            eval(keywords_list[[i]][[data_index]], envir = envir),
            error = function(e) NULL
          )
        }
      } else {
        global_data_flag <- NULL
        global_data_id <- NULL
        global_data_var <- NULL
      }
    }

    if ("var" %in% str_keywords) {
      data_var <- NULL
      data_index <- which(param_list[[i]] == "data")

      for (var_index in which(str_keywords == "var")) {
        if (length(data_index) != 0) {
          if (str_keywords[data_index] == "upload") {
            tmp_data <- tryCatch(
              paintr_get_uploaded_data(input, id_list[[i]][[data_index]]),
              error = function(e) NULL
            )
          } else {
            tmp_data <- tryCatch(
              eval(keywords_list[[i]][[data_index]], envir = envir),
              error = function(e) NULL
            )
          }

          if (!is.null(tmp_data)) {
            data_var <- names(tmp_data)
          }
        } else if (!is.null(global_data_flag)) {
          if (global_data_flag == "upload") {
            tmp_data <- tryCatch(
              paintr_get_uploaded_data(input, global_data_id),
              error = function(e) NULL
            )
          } else if (global_data_flag == "local") {
            tmp_data <- global_data
          }

          if (!is.null(tmp_data)) {
            data_var <- names(tmp_data)
          }
        } else {
          stop("data is not provided!")
        }

        var_ui_list[[id_list[[i]][[var_index]]]] <- generate_ui_var(
          data_var,
          id_list[[i]][[var_index]],
          param_list[[i]][[var_index]]
        )

        if (length(data_index) != 0) {
          if (str_keywords[data_index] == "upload") {
            expr_list[[length(expr_list) + 1]] <- rlang::expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- shiny::renderUI({
                shiny::req(input[[!!id_list[[i]][[data_index]]]])
                var_ui_list[[!!id_list[[i]][[var_index]]]]
              })
            )
          } else {
            expr_list[[length(expr_list) + 1]] <- rlang::expr(
              output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- shiny::renderUI({
                var_ui_list[[!!id_list[[i]][[var_index]]]]
              })
            )
          }
        } else if (global_data_flag == "upload") {
          expr_list[[length(expr_list) + 1]] <- rlang::expr(
            output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- shiny::renderUI({
              shiny::req(input[[!!global_data_id]])
              var_ui_list[[!!id_list[[i]][[var_index]]]]
            })
          )
        } else {
          expr_list[[length(expr_list) + 1]] <- rlang::expr(
            output[[paste0("var-", !!id_list[[i]][[var_index]])]] <- shiny::renderUI({
              var_ui_list[[!!id_list[[i]][[var_index]]]]
            })
          )
        }
      }
    }
  }

  for (i in seq_along(expr_list)) {
    eval(expr_list[[i]])
  }

  var_ui_list
}

#' Add Layer Checkboxes to a UI List
#'
#' @param ui A list of UI elements for one layer.
#' @param nn The layer name.
#'
#' @return A list of UI elements, possibly prefixed with a checkbox.
#' @keywords internal
ui_insert_checkbox <- function(ui, nn) {
  if (nn == "ggplot") {
    return(ui)
  }

  id <- paste0(nn, "+checkbox")
  .expr <- rlang::expr(
    shiny::checkboxInput(
      !!id,
      label = paste("Keep the layer of", !!nn),
      value = TRUE
    )
  )

  checkbox <- eval(.expr)
  attr(checkbox, "ui_expr") <- .expr
  ui <- c(list(checkbox), ui)
  names(ui)[1] <- paste0(nn, "+checkbox")
  ui
}

#' Wrap Layer Controls in Tabs
#'
#' @param ui_list A named list of UI lists.
#'
#' @return A `tabsetPanel()`.
#' @keywords internal
tab_wrap_ui <- function(ui_list) {
  assertthat::assert_that(!is.null(names(ui_list)))

  tab_list <- unname(
    purrr::map2(
      ui_list,
      names(ui_list),
      function(ui, .nn) do.call(shiny::tabPanel, c(.nn, unname(ui)))
    )
  )

  do.call(shiny::tabsetPanel, tab_list)
}

#' Build the Tabbed Control UI for a Parsed Formula
#'
#' @param paintr_obj A `paintr_obj`.
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
paintr_get_tab_ui <- function(paintr_obj) {
  if (!inherits(paintr_obj, "paintr_obj")) {
    return(NULL)
  }

  tab_wrap_ui(paintr_obj[["ui_list"]])
}

