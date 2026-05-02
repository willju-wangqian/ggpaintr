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
ptr_build_ui_list <- function(ptr_obj, ui_text = NULL, ns_fn = shiny::NS(NULL)) {
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
    ui
  })

  rlang::set_names(ui_list, layer_names)
}


#' Build a Single Layer-Toggle Checkbox Tag
#'
#' Returns a `shiny::checkboxInput()` that controls whether a layer is
#' included in the rendered plot. Returns `NULL` for the synthetic `ggplot`
#' layer, which has no toggle.
#'
#' @noRd
ptr_layer_checkbox_tag <- function(layer_name, ui_text = NULL,
                                   ns_fn = shiny::NS(NULL),
                                   checkbox_defaults = NULL) {
  if (identical(layer_name, "ggplot")) {
    return(NULL)
  }
  copy <- ptr_resolve_ui_text("layer_checkbox", ui_text = ui_text)
  id <- ptr_ns_id(ns_fn, ptr_checkbox_input_id(layer_name))
  default_value <- if (!is.null(checkbox_defaults) &&
    layer_name %in% names(checkbox_defaults)) {
    checkbox_defaults[[layer_name]]
  } else {
    TRUE
  }
  shiny::checkboxInput(id, label = copy$label, value = default_value)
}

# Local id for the per-layer content wrapper. The outer switcher targets
# this id when toggling the `ptr-layer-disabled` class.
ptr_layer_panel_content_id <- function(layer_name) {
  paste0("ptr_layer_content_", layer_name)
}

ptr_update_data_input_id <- function(layer_name) {
  paste0("ptr_update_data_", layer_name)
}

# Build the data-pipeline controls for a single layer. Returns a list with
# `controls` (UI tags) and `button` (the Update Data actionButton tag).
ptr_build_pipeline_layer_controls <- function(ptr_obj, layer_name,
                                              effective_ui_text,
                                              ns_fn) {
  pipeline_info <- ptr_obj$data_pipeline_info %||% list()
  info <- pipeline_info[[layer_name]]
  if (is.null(info)) {
    return(list(controls = list(), button = NULL))
  }
  layer_metas <- ptr_obj$placeholder_map[[layer_name]]
  context <- ptr_define_placeholder_context(
    ptr_obj,
    ui_text = effective_ui_text
  )
  context$ui_ns_fn <- ns_fn

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
    effective_param <- meta$param
    verb <- ptr_enclosing_verb_name(
      ptr_obj$expr_list[[layer_name]],
      meta$index_path
    )
    param_was_unnamed <- ptr_param_is_unnamed(effective_param)
    if (param_was_unnamed && !is.null(verb)) {
      effective_param <- paste0(verb, "()")
    }
    copy <- ptr_resolve_ui_text(
      "control",
      keyword = meta$keyword,
      layer_name = meta$layer_name,
      param = effective_param,
      ui_text = effective_ui_text,
      placeholders = ptr_obj$placeholders
    )
    if (!param_was_unnamed && !is.null(verb) && !is.null(copy$label)) {
      copy$label <- paste0(copy$label, " in ", verb, "()")
    }
    spec$build_ui(ui_id, copy, ui_meta, context)
  })
  controls <- controls[!vapply(controls, is.null, logical(1))]

  update_data_copy <- ptr_resolve_ui_text(
    "update_data_button",
    ui_text = effective_ui_text,
    placeholders = ptr_obj$placeholders
  )
  button <- shiny::actionButton(
    ptr_ns_id(ns_fn, ptr_update_data_input_id(layer_name)),
    update_data_copy$label
  )

  list(controls = controls, button = button)
}

# Build the inner content of one layer's panel: a tabsetPanel with two
# sub-tabs ("Data" + "Controls") when both kinds of controls exist, a
# single Data tabset when only pipeline placeholders exist, or a flat
# tagList of controls when there is no data pipeline.
ptr_build_layer_panel_inner <- function(controls_ui,
                                        pipeline_ui,
                                        update_data_button_tag,
                                        data_subtab_label,
                                        controls_subtab_label) {
  has_pipeline <- length(pipeline_ui) > 0L
  has_controls <- length(controls_ui) > 0L

  data_panel_body <- if (has_pipeline) {
    c(unname(pipeline_ui), list(update_data_button_tag))
  } else {
    NULL
  }

  if (has_pipeline && has_controls) {
    do.call(
      shiny::tabsetPanel,
      list(
        do.call(shiny::tabPanel, c(data_subtab_label, data_panel_body)),
        do.call(
          shiny::tabPanel,
          c(controls_subtab_label, unname(controls_ui))
        )
      )
    )
  } else if (has_pipeline) {
    do.call(
      shiny::tabsetPanel,
      list(do.call(shiny::tabPanel, c(data_subtab_label, data_panel_body)))
    )
  } else if (has_controls) {
    do.call(shiny::tagList, unname(controls_ui))
  } else {
    NULL
  }
}

# Build a single per-layer panel: layer-toggle checkbox at top, then the
# inner content wrapped in a div whose id is targeted by the disabled-
# layer class handler.
ptr_build_layer_panel <- function(layer_name,
                                  ptr_obj,
                                  controls_ui,
                                  pipeline_ui,
                                  update_data_button_tag,
                                  data_subtab_label,
                                  controls_subtab_label,
                                  ui_text,
                                  ns_fn,
                                  checkbox_defaults) {
  checkbox <- ptr_layer_checkbox_tag(
    layer_name,
    ui_text = ui_text,
    ns_fn = ns_fn,
    checkbox_defaults = checkbox_defaults
  )
  inner <- ptr_build_layer_panel_inner(
    controls_ui = controls_ui,
    pipeline_ui = pipeline_ui,
    update_data_button_tag = update_data_button_tag,
    data_subtab_label = data_subtab_label,
    controls_subtab_label = controls_subtab_label
  )
  default_on <- if (identical(layer_name, "ggplot")) {
    TRUE
  } else if (!is.null(checkbox_defaults) &&
    layer_name %in% names(checkbox_defaults)) {
    isTRUE(checkbox_defaults[[layer_name]])
  } else {
    TRUE
  }
  content_div <- shiny::div(
    id = ptr_ns_id(ns_fn, ptr_layer_panel_content_id(layer_name)),
    class = if (default_on) {
      "ptr-layer-content"
    } else {
      "ptr-layer-content ptr-layer-disabled"
    },
    inner
  )
  body <- if (is.null(checkbox)) {
    list(content_div)
  } else {
    list(checkbox, content_div)
  }
  do.call(shiny::tabPanel, c(list(layer_name), body))
}

#' Build the Layer-Switcher Control UI
#'
#' Renders a `pickerInput()` paired with a hidden `tabsetPanel()` whose
#' panels are keyed by ggplot layer. Each layer panel has a top-level
#' include-this-layer checkbox; when the layer has data-pipeline
#' placeholders, the panel body splits into "Data" + "Controls" sub-tabs
#' (or just "Data" when the layer has no other placeholders), otherwise
#' the controls render flat.
#'
#' @param ptr_obj A `ptr_obj`.
#' @param ui_text Effective or user-supplied copy rules.
#' @param ns_fn A namespace function `character -> character`.
#' @param checkbox_defaults Optional named-logical vector of layer toggle
#'   defaults.
#' @param layer_select_id Local id (un-namespaced) for the layer
#'   `pickerInput()`.
#' @param layer_tabset_id Local id (un-namespaced) for the hidden
#'   `tabsetPanel()`.
#'
#' @return A `shiny::tagList()` or `NULL` when `ptr_obj` is not a
#'   `ptr_obj`.
#' @noRd
ptr_get_layer_switcher_ui <- function(ptr_obj,
                                      ui_text = NULL,
                                      ns_fn = shiny::NS(NULL),
                                      checkbox_defaults = NULL,
                                      layer_select_id = "ptr_layer_select",
                                      layer_tabset_id = "ptr_layer_tabset") {
  if (!inherits(ptr_obj, "ptr_obj")) {
    return(NULL)
  }

  effective_ui_text <- ptr_merge_ui_text(
    ui_text,
    placeholders = ptr_obj$placeholders
  )
  shell_copy <- ptr_resolve_shell_ui_text(effective_ui_text)

  controls_map <- ptr_build_ui_list(
    ptr_obj,
    ui_text = effective_ui_text,
    ns_fn = ns_fn
  )

  layer_names <- names(ptr_obj$keywords_list)
  pipeline_info <- ptr_obj$data_pipeline_info %||% list()

  panels <- lapply(layer_names, function(layer_name) {
    controls_ui <- controls_map[[layer_name]] %||% list()
    controls_ui <- controls_ui[!vapply(controls_ui, is.null, logical(1))]

    pipeline_ui <- list()
    update_data_button_tag <- NULL
    if (layer_name %in% names(pipeline_info)) {
      built <- ptr_build_pipeline_layer_controls(
        ptr_obj,
        layer_name,
        effective_ui_text,
        ns_fn
      )
      pipeline_ui <- built$controls
      update_data_button_tag <- built$button
    }

    ptr_build_layer_panel(
      layer_name = layer_name,
      ptr_obj = ptr_obj,
      controls_ui = controls_ui,
      pipeline_ui = pipeline_ui,
      update_data_button_tag = update_data_button_tag,
      data_subtab_label = shell_copy$data_subtab_copy$label,
      controls_subtab_label = shell_copy$controls_subtab_copy$label,
      ui_text = effective_ui_text,
      ns_fn = ns_fn,
      checkbox_defaults = checkbox_defaults
    )
  })

  picker_id <- ptr_ns_id(ns_fn, layer_select_id)
  tabset_full_id <- ptr_ns_id(ns_fn, layer_tabset_id)

  hidden_tabset <- do.call(
    shiny::tabsetPanel,
    c(list(id = tabset_full_id, type = "hidden"), unname(panels))
  )

  picker <- shinyWidgets::pickerInput(
    inputId = picker_id,
    label = shell_copy$layer_picker_copy$label,
    choices = layer_names,
    selected = if (length(layer_names) > 0L) layer_names[[1L]] else NULL
  )

  shiny::tagList(
    ptr_data_pipeline_class_handler_script(),
    ptr_layer_switcher_style_tag(),
    picker,
    hidden_tabset
  )
}

# Inline CSS for the "layer disabled" visual state. Opacity gives the
# grayed-out look; controls remain interactive so the user can still
# stage edits and re-enable the layer.
ptr_layer_switcher_style_tag <- function() {
  shiny::tags$style(shiny::HTML(
    ".ptr-layer-disabled { opacity: 0.5; }"
  ))
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
