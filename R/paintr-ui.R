#' Attach Optional Help Text to a UI Control
#'
#' @param ui A Shiny UI object.
#' @param help Optional help text.
#'
#' @return A Shiny UI object.
#' @noRd
ptr_attach_help <- function(ui, help = NULL) {
  if (is.null(help) || identical(trimws(help), "")) {
    return(ui)
  }

  shiny::tagList(ui, shiny::helpText(help))
}

#' Build Upload UI for a Placeholder
#'
#' @param id Placeholder id.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A Shiny UI object.
#' @noRd
generate_ui_upload <- function(id, ui_text = NULL, placeholders = NULL) {
  effective_ui_text <- ptr_merge_ui_text(ui_text, placeholders = placeholders)
  file_copy <- ptr_resolve_ui_text("upload_file", ui_text = effective_ui_text)
  name_copy <- ptr_resolve_ui_text("upload_name", ui_text = effective_ui_text)

  shiny::tagList(
    shiny::fileInput(
      id,
      file_copy$label,
      accept = c(".csv", ".rds")
    ),
    ptr_attach_help(
      shiny::textInput(
        ptr_upload_name_id(id),
        name_copy$label,
        placeholder = name_copy$placeholder
      ),
      name_copy$help
    )
  )
}

#' Build the Output ID for a Deferred `var` UI Element
#'
#' @param id Placeholder id.
#'
#' @return A single output id string.
#' @noRd
ptr_var_output_id <- function(id) {
  paste0("var-", id)
}

#' Build a Deferred `var` UI Placeholder
#'
#' @param id Placeholder id.
#'
#' @return A placeholder `uiOutput()`.
#' @noRd
generate_ui_var_placeholder <- function(id) {
  shiny::uiOutput(ptr_var_output_id(id))
}

#' Build Text Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A `textInput()` UI object.
#' @noRd
generate_ui_text <- function(id, param, layer_name = NULL, ui_text = NULL) {
  copy <- ptr_resolve_ui_text(
    "control",
    keyword = "text",
    layer_name = layer_name,
    param = param,
    ui_text = ui_text
  )

  ptr_attach_help(
    shiny::textInput(id, copy$label, placeholder = copy$placeholder),
    copy$help
  )
}

#' Build Numeric Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A `numericInput()` UI object.
#' @noRd
generate_ui_num <- function(id, param, layer_name = NULL, ui_text = NULL) {
  copy <- ptr_resolve_ui_text(
    "control",
    keyword = "num",
    layer_name = layer_name,
    param = param,
    ui_text = ui_text
  )

  ptr_attach_help(
    shiny::numericInput(id, copy$label, NA),
    copy$help
  )
}

#' Build Expression Placeholder UI
#'
#' @param id Placeholder id.
#' @param param Parameter label.
#' @param layer_name Layer name.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A `textInput()` UI object.
#' @noRd
generate_ui_expr <- function(id, param, layer_name = NULL, ui_text = NULL) {
  copy <- ptr_resolve_ui_text(
    "control",
    keyword = "expr",
    layer_name = layer_name,
    param = param,
    ui_text = ui_text
  )

  ptr_attach_help(
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
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A `pickerInput()` UI object or `NULL`.
#' @noRd
generate_ui_var <- function(data_var,
                            id,
                            param,
                            layer_name = NULL,
                            ui_text = NULL) {
  if (is.null(data_var)) {
    return(NULL)
  }

  copy <- ptr_resolve_ui_text(
    "control",
    keyword = "var",
    layer_name = layer_name,
    param = param,
    ui_text = ui_text
  )

  ptr_attach_help(
    shinyWidgets::pickerInput(
      id,
      copy$label,
      choices = data_var,
      selected = character(0),
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        noneSelectedText = copy$empty_text,
        maxOptions = 1L
      )
    ),
    copy$help
  )
}

#' Build a UI List for a Parsed Formula
#'
#' @param ptr_obj A `ptr_obj`.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A named list of UI controls by layer.
#' @noRd
ptr_build_ui_list <- function(ptr_obj, ui_text = NULL) {
  effective_ui_text <- ptr_merge_ui_text(
    ui_text,
    placeholders = ptr_obj$placeholders
  )
  context <- ptr_define_placeholder_context(
    ptr_obj,
    ui_text = effective_ui_text
  )
  keywords_list <- ptr_obj[["keywords_list"]]
  id_list <- ptr_obj[["id_list"]]
  placeholder_map <- ptr_obj[["placeholder_map"]]
  layer_names <- names(keywords_list)

  ui_list <- lapply(seq_along(layer_names), function(i) {
    layer_name <- layer_names[[i]]
    ids <- id_list[[i]]

    ui <- lapply(seq_along(ids), function(j) {
      id <- ids[[j]]
      meta <- placeholder_map[[layer_name]][[id]]
      if (is.null(meta)) {
        return(NULL)
      }

      spec <- ptr_obj$placeholders[[meta$keyword]]
      copy <- ptr_resolve_ui_text(
        "control",
        keyword = meta$keyword,
        layer_name = meta$layer_name,
        param = meta$param,
        ui_text = effective_ui_text,
        placeholders = ptr_obj$placeholders
      )

      spec$build_ui(id, copy, meta, context)
    })

    names(ui) <- names(keywords_list[[i]])
    ui_insert_checkbox(ui, layer_name, ui_text = effective_ui_text)
  })

  rlang::set_names(ui_list, layer_names)
}

#' Register Dynamic `var` UI Outputs for a Parsed Formula
#'
#' @param input A Shiny input object.
#' @param output A Shiny output object.
#' @param ptr_obj A `ptr_obj`.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A named list of generated `var` UI controls registered on `output`.
#' @noRd
register_var_ui_outputs <- function(input,
                                    output,
                                    ptr_obj,
                                    envir = parent.frame(),
                                    ui_text = NULL,
                                    eval_env = NULL,
                                    var_column_map = NULL) {
  ptr_bind_placeholder_ui(
    input,
    output,
    ptr_obj,
    envir = envir,
    ui_text = ui_text,
    eval_env = eval_env,
    var_column_map = var_column_map
  )
}

#' Add Layer Checkboxes to a UI List
#'
#' @param ui A list of UI elements for one layer.
#' @param nn The layer name.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A list of UI elements, possibly prefixed with a checkbox.
#' @noRd
ui_insert_checkbox <- function(ui, nn, ui_text = NULL) {
  if (nn == "ggplot") {
    return(ui)
  }

  copy <- ptr_resolve_ui_text("layer_checkbox", ui_text = ui_text)
  id <- ptr_checkbox_input_id(nn)
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
#' @noRd
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
#' @param ptr_obj A `ptr_obj`.
#' @param ui_text Effective or user-supplied copy rules.
#'
#' @return A Shiny UI object or `NULL`.
#' @noRd
ptr_get_tab_ui <- function(ptr_obj, ui_text = NULL) {
  if (!inherits(ptr_obj, "ptr_obj")) {
    return(NULL)
  }

  tab_wrap_ui(
    ptr_build_ui_list(
      ptr_obj,
      ui_text = ptr_merge_ui_text(
        ui_text,
        placeholders = ptr_obj$placeholders
      )
    )
  )
}
