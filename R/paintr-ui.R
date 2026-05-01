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
      accept = c(".csv", ".tsv", ".rds", ".xlsx", ".xls", ".json")
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
                            ui_text = NULL,
                            selected = character(0)) {
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

  retained <- intersect(selected, data_var)

  ptr_attach_help(
    shinyWidgets::pickerInput(
      id,
      copy$label,
      choices = data_var,
      selected = retained,
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
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return A named list of UI controls by layer.
#' @noRd
ptr_build_ui_list <- function(ptr_obj, ui_text = NULL, ns_fn = shiny::NS(NULL),
                              checkbox_defaults = NULL) {
  effective_ui_text <- ptr_merge_ui_text(
    ui_text,
    placeholders = ptr_obj$placeholders
  )
  context <- ptr_define_placeholder_context(
    ptr_obj,
    ui_text = effective_ui_text
  )
  context$ui_ns_fn <- ns_fn
  keywords_list <- ptr_obj[["keywords_list"]]
  id_list <- ptr_obj[["id_list"]]
  placeholder_map <- ptr_obj[["placeholder_map"]]
  pipeline_info <- ptr_obj[["data_pipeline_info"]] %||% list()
  layer_names <- names(keywords_list)

  ui_list <- lapply(seq_along(layer_names), function(i) {
    layer_name <- layer_names[[i]]
    ids <- id_list[[i]]
    pipeline_ids <- pipeline_info[[layer_name]]$placeholder_ids %||% character()

    ui <- lapply(seq_along(ids), function(j) {
      id <- ids[[j]]
      meta <- placeholder_map[[layer_name]][[id]]
      if (is.null(meta)) {
        return(NULL)
      }
      if (!is.null(meta$shared)) {
        return(NULL)
      }
      if (id %in% pipeline_ids) {
        return(NULL)
      }

      ui_id <- if (identical(meta$keyword, "var")) {
        id
      } else {
        ptr_ns_id(ns_fn, id)
      }
      ui_meta <- meta
      ui_meta$id <- ui_id
      spec <- ptr_obj$placeholders[[meta$keyword]]
      copy <- ptr_resolve_ui_text(
        "control",
        keyword = meta$keyword,
        layer_name = meta$layer_name,
        param = meta$param,
        ui_text = effective_ui_text,
        placeholders = ptr_obj$placeholders
      )

      spec$build_ui(ui_id, copy, ui_meta, context)
    })

    names(ui) <- names(keywords_list[[i]])
    ui_insert_checkbox(
      ui, layer_name,
      ui_text = effective_ui_text,
      ns_fn = ns_fn,
      checkbox_defaults = checkbox_defaults
    )
  })

  rlang::set_names(ui_list, layer_names)
}


#' Add Layer Checkboxes to a UI List
#'
#' @param ui A list of UI elements for one layer.
#' @param nn The layer name.
#' @param ui_text Effective or user-supplied copy rules.
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return A list of UI elements, possibly prefixed with a checkbox.
#' @noRd
ui_insert_checkbox <- function(ui, nn, ui_text = NULL, ns_fn = shiny::NS(NULL),
                               checkbox_defaults = NULL) {
  if (nn == "ggplot") {
    return(ui)
  }

  copy <- ptr_resolve_ui_text("layer_checkbox", ui_text = ui_text)
  id <- ptr_ns_id(ns_fn, ptr_checkbox_input_id(nn))
  default_value <- if (!is.null(checkbox_defaults) && nn %in% names(checkbox_defaults)) {
    checkbox_defaults[[nn]]
  } else {
    TRUE
  }
  checkbox <- shiny::checkboxInput(id, label = copy$label, value = default_value)

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
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return A Shiny UI object or `NULL`.
#' @noRd
ptr_get_tab_ui <- function(ptr_obj, ui_text = NULL, ns_fn = shiny::NS(NULL),
                           checkbox_defaults = NULL) {
  if (!inherits(ptr_obj, "ptr_obj")) {
    return(NULL)
  }

  tab_wrap_ui(
    ptr_build_ui_list(
      ptr_obj,
      ui_text = ptr_merge_ui_text(
        ui_text,
        placeholders = ptr_obj$placeholders
      ),
      ns_fn = ns_fn,
      checkbox_defaults = checkbox_defaults
    )
  )
}


# Data-pipeline tabset (Phase B)
#
# When a parsed formula contains placeholders inside a data-argument call
# expression (e.g. `mtcars |> head(num) |> ggplot(aes(x = var))`), those
# placeholders are tracked in `ptr_obj$data_pipeline_info`, keyed by layer
# name. `ptr_get_data_tab_ui()` renders one sub-tab per such layer with the
# relevant placeholder controls plus an "Update data" actionButton. The
# button is wired up in Phase C; in Phase B it is purely decorative.

ptr_update_data_input_id <- function(layer_name) {
  paste0("ptr_update_data_", layer_name)
}

ptr_get_data_tab_ui <- function(ptr_obj, ui_text = NULL,
                                ns_fn = shiny::NS(NULL)) {
  if (!inherits(ptr_obj, "ptr_obj")) {
    return(NULL)
  }
  pipeline_info <- ptr_obj$data_pipeline_info
  if (is.null(pipeline_info) || length(pipeline_info) == 0L) {
    return(NULL)
  }

  effective_ui_text <- ptr_merge_ui_text(
    ui_text,
    placeholders = ptr_obj$placeholders
  )
  context <- ptr_define_placeholder_context(
    ptr_obj,
    ui_text = effective_ui_text
  )
  context$ui_ns_fn <- ns_fn

  update_data_copy <- ptr_resolve_ui_text(
    "update_data_button",
    ui_text = effective_ui_text,
    placeholders = ptr_obj$placeholders
  )

  panels <- lapply(names(pipeline_info), function(layer_name) {
    info <- pipeline_info[[layer_name]]
    layer_metas <- ptr_obj$placeholder_map[[layer_name]]

    controls <- lapply(info$placeholder_ids, function(id) {
      meta <- layer_metas[[id]]
      if (is.null(meta)) {
        return(NULL)
      }
      ui_id <- if (identical(meta$keyword, "var")) {
        id
      } else {
        ptr_ns_id(ns_fn, id)
      }
      ui_meta <- meta
      ui_meta$id <- ui_id
      spec <- ptr_obj$placeholders[[meta$keyword]]
      copy <- ptr_resolve_ui_text(
        "control",
        keyword = meta$keyword,
        layer_name = meta$layer_name,
        param = meta$param,
        ui_text = effective_ui_text,
        placeholders = ptr_obj$placeholders
      )
      spec$build_ui(ui_id, copy, ui_meta, context)
    })
    controls <- controls[!vapply(controls, is.null, logical(1))]

    button <- shiny::actionButton(
      ptr_ns_id(ns_fn, ptr_update_data_input_id(layer_name)),
      update_data_copy$label
    )

    do.call(
      shiny::tabPanel,
      c(layer_name, unname(controls), list(button))
    )
  })

  shiny::tagList(
    ptr_data_pipeline_class_handler_script(),
    do.call(shiny::tabsetPanel, panels)
  )
}

# Inline JS that registers a Shiny custom message handler used by the server
# to toggle a CSS class on update-data buttons. The handler is registered
# once per browser window; subsequent UI rebuilds are no-ops.
ptr_data_pipeline_class_handler_script <- function() {
  shiny::tags$script(shiny::HTML(
    paste(
      "if (typeof Shiny !== 'undefined' && !window.__ptr_set_class_registered) {",
      "  window.__ptr_set_class_registered = true;",
      "  Shiny.addCustomMessageHandler('ptr_set_class', function(msg) {",
      "    var el = document.getElementById(msg.id);",
      "    if (!el || !msg.class) return;",
      "    if (msg.add) { el.classList.add(msg.class); }",
      "    else { el.classList.remove(msg.class); }",
      "  });",
      "}",
      sep = "\n"
    )
  ))
}
