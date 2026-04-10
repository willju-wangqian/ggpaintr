#' Map Public Component Names to Storage Paths
#'
#' Single source of truth for the mapping between public `ptr_resolve_ui_text()`
#' component names and their nested storage paths in the rules structure.
#'
#' @return A named list of character vectors representing storage paths.
#' @noRd
ptr_ui_text_component_paths <- function() {
  list(
    title          = c("shell", "title"),
    draw_button    = c("shell", "draw_button"),
    export_button  = c("shell", "export_button"),
    upload_file    = c("upload", "file"),
    upload_name    = c("upload", "name"),
    layer_checkbox = c("layer_checkbox")
  )
}

#' Default Copy Rules for ggpaintr
#'
#' Internal registry for all user-facing control copy. Maintainers should update
#' this object when they want to refine defaults or add support for new common
#' parameters in one place.
#'
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return A named list of default copy rules.
#' @noRd
ptr_default_ui_text <- function(placeholders = NULL) {
  placeholder_registry <- ptr_merge_placeholders(placeholders)
  default_placeholder_copy <- lapply(
    placeholder_registry,
    ptr_define_placeholder_copy_defaults
  )

  structure(
    list(
      shell = list(
        title = list(label = "ggpaintr Plot Builder"),
        draw_button = list(label = "Update plot"),
        export_button = list(label = "Export Shiny app")
      ),
      upload = list(
        file = list(label = "Choose a data file"),
        name = list(
          label = "Optional dataset name",
          placeholder = "For example: sales_data",
          help = paste(
            "Accepted formats: .csv and .rds.",
            "Leave the name blank to use the file name in generated code."
          )
        )
      ),
      layer_checkbox = list(label = "Include this layer in the plot"),
      defaults = default_placeholder_copy,
      # Common parameter-specific defaults go here.
      params = list(
        x = list(
          var = list(label = "Choose the x-axis column"),
          text = list(label = "X-axis label")
        ),
        y = list(
          var = list(label = "Choose the y-axis column"),
          text = list(label = "Y-axis label")
        ),
        color = list(var = list(label = "Choose the color column")),
        fill = list(var = list(label = "Choose the fill column")),
        group = list(var = list(label = "Choose the grouping column")),
        shape = list(var = list(label = "Choose the shape column")),
        size = list(
          var = list(label = "Choose the size column"),
          num = list(
            label = "Point size",
            help = "Enter a number such as 2 or 3."
          )
        ),
        alpha = list(
          var = list(label = "Choose the transparency column"),
          num = list(
            label = "Transparency",
            help = "Enter a value between 0 and 1."
          )
        ),
        label = list(var = list(label = "Choose the label column")),
        title = list(text = list(label = "Plot title")),
        subtitle = list(text = list(label = "Plot subtitle")),
        caption = list(text = list(label = "Plot caption")),
        legend.position = list(
          text = list(
            label = "Legend position",
            help = "Examples: right, left, top, bottom, none."
          )
        ),
        labeller = list(
          expr = list(
            label = "Facet label function",
            help = "Enter an R helper such as label_both."
          )
        ),
        ncol = list(num = list(label = "Number of facet columns")),
        nrow = list(num = list(label = "Number of facet rows")),
        linewidth = list(num = list(label = "Line width")),
        stroke = list(num = list(label = "Stroke width")),
        bins = list(num = list(label = "Number of bins")),
        binwidth = list(num = list(label = "Bin width"))
      ),
      # Layer-specific unnamed-expression rules go here.
      layers = list(
        facet_wrap = list(
          expr = list(
            `__unnamed__` = list(
              label = "Facet by",
              placeholder = "~ Species",
              help = "Enter a faceting formula such as ~ Species."
            )
          )
        ),
        facet_grid = list(
          expr = list(
            `__unnamed__` = list(
              label = "Facet layout",
              placeholder = "Species ~ .",
              help = paste(
                "Enter a faceting formula such as Species ~ . or . ~ Species."
              )
            )
          )
        )
      )
    ),
    class = "ptr_ui_text"
  )
}

#' Normalize Parameter Aliases for Copy Rules
#'
#' Add new aliases here when different parameter spellings should resolve to the
#' same copy entry.
#'
#' @return A named character vector mapping aliases to canonical keys.
#' @noRd
ptr_ui_text_param_aliases <- function() {
  c(colour = "color")
}

#' Return Allowed Copy Leaf Fields
#'
#' @return A character vector.
#' @noRd
ptr_ui_text_leaf_fields <- function() {
  c("label", "help", "placeholder", "empty_text")
}

#' Return Allowed Copy Keywords
#'
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return A character vector.
#' @noRd
ptr_ui_text_keywords <- function(placeholders = NULL) {
  names(ptr_merge_placeholders(placeholders))
}

#' Detect Whether a Parameter Is Unnamed
#'
#' @param param A parameter value from `ptr_obj$param_list`.
#'
#' @return A single logical value.
#' @noRd
ptr_param_is_unnamed <- function(param) {
  if (is.null(param) || length(param) == 0) {
    return(TRUE)
  }

  if (is.list(param) && length(param) >= 1 && is.null(param[[1]])) {
    return(TRUE)
  }

  param_chr <- as.character(param)[1]
  is.na(param_chr) || identical(param_chr, "")
}

#' Normalize a Copy Rule Parameter Key
#'
#' @param param A parameter name or `NULL`.
#'
#' @return A normalized parameter key.
#' @noRd
ptr_normalize_param_key <- function(param) {
  if (ptr_param_is_unnamed(param)) {
    return("__unnamed__")
  }

  param <- as.character(param)[1]
  alias_map <- ptr_ui_text_param_aliases()
  if (param %in% names(alias_map)) {
    alias_map[[param]]
  } else {
    param
  }
}

#' Build a Human-Readable Parameter Label
#'
#' @param param A parameter name or `NULL`.
#'
#' @return A single readable label string.
#' @noRd
ptr_humanize_param <- function(param) {
  if (ptr_param_is_unnamed(param)) {
    return("this setting")
  }

  param <- ptr_normalize_param_key(param)
  param <- gsub("[._]+", " ", param)
  trimws(param)
}

#' Replace Copy Template Tokens
#'
#' @param text A text template.
#' @param param A parameter label.
#' @param layer_name A layer name.
#'
#' @return A single string or `NULL`.
#' @noRd
ptr_interpolate_ui_text <- function(text, param = NULL, layer_name = NULL) {
  if (is.null(text)) {
    return(NULL)
  }

  text <- gsub("\\{param\\}", ptr_humanize_param(param), text)
  if (!is.null(layer_name)) {
    text <- gsub("\\{layer\\}", layer_name, text)
  }

  text
}

#' Validate One Copy Leaf
#'
#' @param x A leaf list.
#' @param path A human-readable location string.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_ui_text_leaf <- function(x, path) {
  if (!is.list(x) || is.null(names(x)) || length(x) == 0) {
    rlang::abort(paste0(path, " must be a named list."))
  }

  unknown_fields <- setdiff(names(x), ptr_ui_text_leaf_fields())
  if (length(unknown_fields) > 0) {
    rlang::abort(paste0(
      path,
      " has unsupported fields: ",
      paste(sort(unknown_fields), collapse = ", "),
      "."
    ))
  }

  for (field_name in names(x)) {
    value <- x[[field_name]]
    if (!is.character(value) || length(value) != 1) {
      rlang::abort(paste0(path, "$", field_name, " must be a single string."))
    }
  }

  invisible(TRUE)
}

#' Validate Copy Rules
#'
#' @param ui_text User-supplied copy rules.
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_ui_text <- function(ui_text, placeholders = NULL) {
  if (is.null(ui_text)) {
    return(invisible(TRUE))
  }

  if (!is.list(ui_text)) {
    rlang::abort("ui_text must be a named list.")
  }

  allowed_top <- c(
    "shell",
    "upload",
    "layer_checkbox",
    "defaults",
    "params",
    "layers"
  )
  unknown_top <- setdiff(names(ui_text), allowed_top)
  if (length(unknown_top) > 0) {
    rlang::abort(paste0(
      "ui_text has unsupported top-level sections: ",
      paste(sort(unknown_top), collapse = ", "),
      "."
    ))
  }

  if (!is.null(ui_text$shell)) {
    allowed_shell <- c("title", "draw_button", "export_button")
    unknown_shell <- setdiff(names(ui_text$shell), allowed_shell)
    if (length(unknown_shell) > 0) {
      rlang::abort(paste0(
        "ui_text$shell has unsupported entries: ",
        paste(sort(unknown_shell), collapse = ", "),
        "."
      ))
    }

    for (name in names(ui_text$shell)) {
      ptr_validate_ui_text_leaf(ui_text$shell[[name]], paste0("ui_text$shell$", name))
    }
  }

  if (!is.null(ui_text$upload)) {
    allowed_upload <- c("file", "name")
    unknown_upload <- setdiff(names(ui_text$upload), allowed_upload)
    if (length(unknown_upload) > 0) {
      rlang::abort(paste0(
        "ui_text$upload has unsupported entries: ",
        paste(sort(unknown_upload), collapse = ", "),
        "."
      ))
    }

    for (name in names(ui_text$upload)) {
      ptr_validate_ui_text_leaf(ui_text$upload[[name]], paste0("ui_text$upload$", name))
    }
  }

  if (!is.null(ui_text$layer_checkbox)) {
    ptr_validate_ui_text_leaf(ui_text$layer_checkbox, "ui_text$layer_checkbox")
  }

  if (!is.null(ui_text$defaults)) {
    unknown_defaults <- setdiff(
      names(ui_text$defaults),
      ptr_ui_text_keywords(placeholders)
    )
    if (length(unknown_defaults) > 0) {
      rlang::abort(paste0(
        "ui_text$defaults has unsupported entries: ",
        paste(sort(unknown_defaults), collapse = ", "),
        "."
      ))
    }

    for (keyword in names(ui_text$defaults)) {
      ptr_validate_ui_text_leaf(
        ui_text$defaults[[keyword]],
        paste0("ui_text$defaults$", keyword)
      )
    }
  }

  if (!is.null(ui_text$params)) {
    for (param_name in names(ui_text$params)) {
      param_rules <- ui_text$params[[param_name]]
      if (!is.list(param_rules)) {
        rlang::abort(paste0(
          "ui_text$params$", param_name, " must be a named list."
        ))
      }

      unknown_keywords <- setdiff(
        names(param_rules),
        ptr_ui_text_keywords(placeholders)
      )
      if (length(unknown_keywords) > 0) {
        rlang::abort(paste0(
          "ui_text$params$", param_name, " has unsupported keywords: ",
          paste(sort(unknown_keywords), collapse = ", "),
          "."
        ))
      }

      for (keyword in names(param_rules)) {
        ptr_validate_ui_text_leaf(
          param_rules[[keyword]],
          paste0("ui_text$params$", param_name, "$", keyword)
        )
      }
    }
  }

  if (!is.null(ui_text$layers)) {
    for (layer_name in names(ui_text$layers)) {
      layer_rules <- ui_text$layers[[layer_name]]
      if (!is.list(layer_rules)) {
        rlang::abort(paste0(
          "ui_text$layers$", layer_name, " must be a named list."
        ))
      }

      unknown_keywords <- setdiff(
        names(layer_rules),
        ptr_ui_text_keywords(placeholders)
      )
      if (length(unknown_keywords) > 0) {
        rlang::abort(paste0(
          "ui_text$layers$", layer_name, " has unsupported keywords: ",
          paste(sort(unknown_keywords), collapse = ", "),
          "."
        ))
      }

      for (keyword in names(layer_rules)) {
        keyword_rules <- layer_rules[[keyword]]
        if (!is.list(keyword_rules)) {
          rlang::abort(paste0(
            "ui_text$layers$", layer_name, "$", keyword,
            " must be a named list."
          ))
        }

        for (param_name in names(keyword_rules)) {
          ptr_validate_ui_text_leaf(
            keyword_rules[[param_name]],
            paste0("ui_text$layers$", layer_name, "$", keyword, "$", param_name)
          )
        }
      }
    }
  }

  invisible(TRUE)
}

#' Normalize User Copy Rule Keys
#'
#' @param ui_text User-supplied copy rules.
#'
#' @return A normalized copy-rule list.
#' @noRd
ptr_normalize_ui_text <- function(ui_text) {
  if (is.null(ui_text)) {
    return(NULL)
  }

  if (!is.null(ui_text$params)) {
    normalized_params <- list()
    for (param_name in names(ui_text$params)) {
      normalized_key <- ptr_normalize_param_key(param_name)
      existing <- normalized_params[[normalized_key]]
      normalized_params[[normalized_key]] <- if (is.null(existing)) {
        ui_text$params[[param_name]]
      } else {
        ptr_deep_merge_ui_text(existing, ui_text$params[[param_name]])
      }
    }
    ui_text$params <- normalized_params
  }

  if (!is.null(ui_text$layers)) {
    normalized_layers <- list()
    for (layer_name in names(ui_text$layers)) {
      layer_rules <- ui_text$layers[[layer_name]]
      normalized_layer <- list()

      for (keyword in names(layer_rules)) {
        keyword_rules <- layer_rules[[keyword]]
        normalized_keyword <- list()
        for (param_name in names(keyword_rules)) {
          normalized_key <- if (identical(param_name, "__unnamed__")) {
            "__unnamed__"
          } else {
            ptr_normalize_param_key(param_name)
          }

          existing <- normalized_keyword[[normalized_key]]
          normalized_keyword[[normalized_key]] <- if (is.null(existing)) {
            keyword_rules[[param_name]]
          } else {
            ptr_deep_merge_ui_text(existing, keyword_rules[[param_name]])
          }
        }

        normalized_layer[[keyword]] <- normalized_keyword
      }

      normalized_layers[[layer_name]] <- normalized_layer
    }

    ui_text$layers <- normalized_layers
  }

  ui_text
}

#' Recursively Merge Copy Rules
#'
#' @param base A base list.
#' @param overrides Override values.
#'
#' @return A merged list.
#' @noRd
ptr_deep_merge_ui_text <- function(base, overrides) {
  if (is.null(base)) {
    return(overrides)
  }

  if (is.null(overrides)) {
    return(base)
  }

  if (!is.list(base) || !is.list(overrides)) {
    return(overrides)
  }

  result <- base
  for (name in names(overrides)) {
    result[[name]] <- if (name %in% names(result)) {
      ptr_deep_merge_ui_text(result[[name]], overrides[[name]])
    } else {
      overrides[[name]]
    }
  }

  result
}

#' Build Effective Copy Rules
#'
#' @param ui_text Optional user-supplied rules.
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return A merged copy-rule list.
#'
#' @examples
#' # Default rules
#' rules <- ptr_merge_ui_text()
#' rules$shell$title$label
#'
#' # Override the draw button label
#' rules <- ptr_merge_ui_text(
#'   ui_text = list(shell = list(draw_button = list(label = "Render")))
#' )
#' rules$shell$draw_button$label
#' @export
ptr_merge_ui_text <- function(ui_text = NULL, placeholders = NULL) {
  if (inherits(ui_text, "ptr_ui_text")) {
    return(ui_text)
  }

  defaults <- ptr_default_ui_text(placeholders = placeholders)
  if (is.null(ui_text)) {
    return(defaults)
  }

  ptr_validate_ui_text(ui_text, placeholders = placeholders)
  ui_text <- ptr_normalize_ui_text(ui_text)

  merged <- ptr_deep_merge_ui_text(unclass(defaults), ui_text)
  class(merged) <- "ptr_ui_text"
  merged
}

#' Recursively Compact Copy Rules Against Defaults
#'
#' @param current A current copy-rule branch.
#' @param defaults A default copy-rule branch.
#'
#' @return A compacted branch or `NULL`.
#' @noRd
ptr_compact_ui_text_branch <- function(current, defaults = NULL) {
  if (is.null(current)) {
    return(NULL)
  }

  if (!is.list(current) || is.null(names(current))) {
    if (identical(current, defaults)) {
      return(NULL)
    }

    return(current)
  }

  result <- list()
  for (name in names(current)) {
    default_value <- NULL
    if (is.list(defaults) && !is.null(names(defaults)) && name %in% names(defaults)) {
      default_value <- defaults[[name]]
    }

    compact_value <- ptr_compact_ui_text_branch(current[[name]], default_value)
    if (!is.null(compact_value)) {
      result[[name]] <- compact_value
    }
  }

  if (length(result) == 0) {
    return(NULL)
  }

  result
}

#' Compact Effective Copy Rules to Custom Overrides
#'
#' @param ui_text Optional user-supplied or effective copy rules.
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return A named list of custom overrides or `NULL`.
#' @noRd
ptr_compact_ui_text <- function(ui_text = NULL, placeholders = NULL) {
  effective_ui_text <- ptr_merge_ui_text(
    ui_text,
    placeholders = placeholders
  )
  default_ui_text <- ptr_default_ui_text(placeholders = placeholders)

  ptr_compact_ui_text_branch(
    unclass(effective_ui_text),
    unclass(default_ui_text)
  )
}

#' Resolve Copy for One Control or App Element
#'
#' @param component One of `title`, `draw_button`, `export_button`,
#'   `upload_file`, `upload_name`, `layer_checkbox`, or `control`.
#' @param keyword Optional placeholder keyword.
#' @param layer_name Optional layer name.
#' @param param Optional parameter name.
#' @param ui_text Effective or user-supplied copy rules.
#' @param placeholders Optional custom placeholder definitions or an effective
#'   placeholder registry.
#'
#' @return A named list with `label`, `help`, `placeholder`, and `empty_text`.
#'
#' @examples
#' # Resolve copy for the title element
#' ptr_resolve_ui_text("title")
#'
#' # Resolve copy for a var control on the x-axis
#' ptr_resolve_ui_text("control", keyword = "var", param = "x")
#' @export
ptr_resolve_ui_text <- function(component,
                                keyword = NULL,
                                layer_name = NULL,
                                param = NULL,
                                ui_text = NULL,
                                placeholders = NULL) {
  rules <- ptr_merge_ui_text(ui_text, placeholders = placeholders)

  component_map <- ptr_ui_text_component_paths()

  if (component %in% names(component_map)) {
    path <- component_map[[component]]
    resolved <- rules
    for (key in path) {
      if (!key %in% names(resolved) || is.null(resolved[[key]])) {
        rlang::abort(paste0(
          "ptr_resolve_ui_text: rules object is missing expected path '",
          paste(path, collapse = "$"), "' at key '", key, "'."
        ))
      }
      resolved <- resolved[[key]]
    }
  } else if (identical(component, "control")) {
    param_key <- ptr_normalize_param_key(param)
    default_rule <- rules$defaults[[keyword]]
    param_rule <- if (!identical(param_key, "__unnamed__") &&
      !is.null(rules$params[[param_key]])) {
      rules$params[[param_key]][[keyword]]
    } else {
      NULL
    }
    layer_rule <- if (!is.null(layer_name) &&
      !is.null(rules$layers[[layer_name]]) &&
      !is.null(rules$layers[[layer_name]][[keyword]])) {
      rules$layers[[layer_name]][[keyword]][[param_key]]
    } else {
      NULL
    }

    resolved <- ptr_deep_merge_ui_text(
      ptr_deep_merge_ui_text(default_rule, param_rule),
      layer_rule
    )
  } else {
    rlang::abort(paste0("Unknown copy component: ", component, "."))
  }

  for (field_name in ptr_ui_text_leaf_fields()) {
    resolved[[field_name]] <- ptr_interpolate_ui_text(
      resolved[[field_name]],
      param = param,
      layer_name = layer_name
    )
  }

  resolved
}
