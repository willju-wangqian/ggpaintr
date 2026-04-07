#' Attach Optional Help Text to a UI Control
#'
#' @param ui A Shiny UI object.
#' @param help Optional help text.
#'
#' @return A Shiny UI object.
#' @keywords internal
paintr_attach_help <- function(ui, help = NULL) {
  if (is.null(help) || identical(trimws(help), "")) {
    return(ui)
  }

  shiny::tagList(ui, shiny::helpText(help))
}

#' Build Upload UI for a Placeholder
#'
#' @param id Placeholder id.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A Shiny UI object.
#' @keywords internal
generate_ui_upload <- function(id, copy_rules = NULL) {
  effective_copy_rules <- paintr_effective_copy_rules(copy_rules)
  file_copy <- paintr_resolve_copy("upload_file", copy_rules = effective_copy_rules)
  name_copy <- paintr_resolve_copy("upload_name", copy_rules = effective_copy_rules)

  shiny::tagList(
    shiny::fileInput(
      id,
      file_copy$label,
      accept = c(".csv", ".rds")
    ),
    paintr_attach_help(
      shiny::textInput(
        paintr_upload_name_id(id),
        name_copy$label,
        placeholder = name_copy$placeholder
      ),
      name_copy$help
    )
  )
}

#' Build a Deferred `var` UI Placeholder
#'
#' @param id Placeholder id.
#'
#' @return A placeholder `uiOutput()`.
#' @keywords internal
generate_ui_var_placeholder <- function(id) {
  shiny::uiOutput(paste0("var-", id))
}

#' Build Text Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A `textInput()` UI object.
#' @keywords internal
generate_ui_text <- function(id, param, layer_name = NULL, copy_rules = NULL) {
  copy <- paintr_resolve_copy(
    "control",
    keyword = "text",
    layer_name = layer_name,
    param = param,
    copy_rules = copy_rules
  )

  paintr_attach_help(
    shiny::textInput(id, copy$label, placeholder = copy$placeholder),
    copy$help
  )
}

#' Build Numeric Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A `numericInput()` UI object.
#' @keywords internal
generate_ui_num <- function(id, param, layer_name = NULL, copy_rules = NULL) {
  copy <- paintr_resolve_copy(
    "control",
    keyword = "num",
    layer_name = layer_name,
    param = param,
    copy_rules = copy_rules
  )

  paintr_attach_help(
    shiny::numericInput(id, copy$label, NA),
    copy$help
  )
}

#' Build Expression Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A `textInput()` UI object.
#' @keywords internal
generate_ui_expr <- function(id, param, layer_name = NULL, copy_rules = NULL) {
  copy <- paintr_resolve_copy(
    "control",
    keyword = "expr",
    layer_name = layer_name,
    param = param,
    copy_rules = copy_rules
  )

  paintr_attach_help(
    shiny::textInput(id, copy$label, placeholder = copy$placeholder),
    copy$help
  )
}

#' Build a Variable Selector UI
#'
#' @param data_var Character vector of candidate variables.
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A `pickerInput()` UI object or `NULL`.
#' @keywords internal
generate_ui_var <- function(data_var,
                            id,
                            param,
                            layer_name = NULL,
                            copy_rules = NULL) {
  if (is.null(data_var)) {
    return(NULL)
  }

  copy <- paintr_resolve_copy(
    "control",
    keyword = "var",
    layer_name = layer_name,
    param = param,
    copy_rules = copy_rules
  )

  paintr_attach_help(
    shinyWidgets::pickerInput(
      id,
      copy$label,
      choices = data_var,
      selected = character(0),
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        maxOptions = 1,
        noneSelectedText = copy$empty_text
      )
    ),
    copy$help
  )
}

#' Build UI for One Placeholder
#'
#' @param key Placeholder keyword.
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
generate_ui_individual <- function(key,
                                   id,
                                   param,
                                   layer_name = NULL,
                                   copy_rules = NULL) {
  key <- rlang::as_string(key)
  effective_copy_rules <- paintr_effective_copy_rules(copy_rules)

  if (key == "upload") {
    generate_ui_upload(id, copy_rules = effective_copy_rules)
  } else if (key == "var") {
    generate_ui_var_placeholder(id)
  } else if (key == "text") {
    generate_ui_text(id, param, layer_name = layer_name, copy_rules = effective_copy_rules)
  } else if (key == "num") {
    generate_ui_num(id, param, layer_name = layer_name, copy_rules = effective_copy_rules)
  } else if (key == "expr") {
    generate_ui_expr(id, param, layer_name = layer_name, copy_rules = effective_copy_rules)
  } else {
    NULL
  }
}

#' Build a UI List for a Parsed Formula
#'
#' @param paintr_obj A `paintr_obj`.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A named list of UI controls by layer.
#' @keywords internal
paintr_build_ui_list <- function(paintr_obj, copy_rules = NULL) {
  effective_copy_rules <- paintr_effective_copy_rules(copy_rules)
  keywords_list <- paintr_obj[["keywords_list"]]
  id_list <- paintr_obj[["id_list"]]
  param_list <- paintr_obj[["param_list"]]
  layer_names <- names(keywords_list)

  ui_list <- lapply(seq_along(layer_names), function(i) {
    layer_name <- layer_names[[i]]
    keywords <- keywords_list[[i]]
    ids <- id_list[[i]]
    params <- param_list[[i]]

    ui <- lapply(seq_along(ids), function(j) {
      generate_ui_individual(
        keywords[[j]],
        ids[[j]],
        params[[j]],
        layer_name = layer_name,
        copy_rules = effective_copy_rules
      )
    })

    names(ui) <- names(keywords)
    ui_insert_checkbox(ui, layer_name, copy_rules = effective_copy_rules)
  })

  rlang::set_names(ui_list, layer_names)
}

#' Register Dynamic `var` UI Outputs for a Parsed Formula
#'
#' @param input A Shiny input object.
#' @param output A Shiny output object.
#' @param paintr_obj A `paintr_obj`.
#' @param envir Environment used to resolve local data objects.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A named list of generated `var` UI controls registered on `output`.
#' @keywords internal
register_var_ui_outputs <- function(input,
                                    output,
                                    paintr_obj,
                                    envir = parent.frame(),
                                    copy_rules = NULL) {
  effective_copy_rules <- paintr_effective_copy_rules(copy_rules)
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
          param_list[[i]][[var_index]],
          layer_name = names(keywords_list)[i],
          copy_rules = effective_copy_rules
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
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A list of UI elements, possibly prefixed with a checkbox.
#' @keywords internal
ui_insert_checkbox <- function(ui, nn, copy_rules = NULL) {
  if (nn == "ggplot") {
    return(ui)
  }

  copy <- paintr_resolve_copy("layer_checkbox", copy_rules = copy_rules)
  id <- paste0(nn, "+checkbox")
  checkbox <- shiny::checkboxInput(id, label = copy$label, value = TRUE)

  ui <- c(list(checkbox), ui)
  names(ui)[1] <- id
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
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
paintr_get_tab_ui <- function(paintr_obj, copy_rules = NULL) {
  if (!inherits(paintr_obj, "paintr_obj")) {
    return(NULL)
  }

  tab_wrap_ui(paintr_build_ui_list(paintr_obj, copy_rules = copy_rules))
}
