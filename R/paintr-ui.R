#' Attach Optional Help Text to a UI Control
#'
#' @param ui A Shiny UI object.
#' @param help Optional help text.
#'
#' @return A Shiny UI object.
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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

#' Build a UI List for a Parsed Formula
#'
#' @param paintr_obj A `paintr_obj`.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A named list of UI controls by layer.
#' @noRd
paintr_build_ui_list <- function(paintr_obj, copy_rules = NULL) {
  effective_copy_rules <- paintr_effective_copy_rules(
    copy_rules,
    placeholders = paintr_obj$placeholders
  )
  context <- paintr_placeholder_context(
    paintr_obj,
    copy_rules = effective_copy_rules
  )
  keywords_list <- paintr_obj[["keywords_list"]]
  id_list <- paintr_obj[["id_list"]]
  placeholder_map <- paintr_obj[["placeholder_map"]]
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

      spec <- paintr_obj$placeholders[[meta$keyword]]
      copy <- paintr_resolve_copy(
        "control",
        keyword = meta$keyword,
        layer_name = meta$layer_name,
        param = meta$param,
        copy_rules = effective_copy_rules,
        placeholders = paintr_obj$placeholders
      )

      spec$build_ui(id, copy, meta, context)
    })

    names(ui) <- names(keywords_list[[i]])
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
#' @noRd
register_var_ui_outputs <- function(input,
                                    output,
                                    paintr_obj,
                                    envir = parent.frame(),
                                    copy_rules = NULL) {
  paintr_bind_placeholder_ui(
    input,
    output,
    paintr_obj,
    envir = envir,
    copy_rules = copy_rules
  )
}

#' Add Layer Checkboxes to a UI List
#'
#' @param ui A list of UI elements for one layer.
#' @param nn The layer name.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A list of UI elements, possibly prefixed with a checkbox.
#' @noRd
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
#' @param paintr_obj A `paintr_obj`.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A Shiny UI object or `NULL`.
#' @noRd
paintr_get_tab_ui <- function(paintr_obj, copy_rules = NULL) {
  if (!inherits(paintr_obj, "paintr_obj")) {
    return(NULL)
  }

  tab_wrap_ui(
    paintr_build_ui_list(
      paintr_obj,
      copy_rules = paintr_effective_copy_rules(
        copy_rules,
        placeholders = paintr_obj$placeholders
      )
    )
  )
}
