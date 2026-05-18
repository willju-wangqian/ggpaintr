#' Map Public Component Names to Storage Paths
#'
#' Single source of truth for the mapping between public `ptr_resolve_ui_text()`
#' component names and their nested storage paths in the rules structure.
#'
#' @return A named list of character vectors representing storage paths.
#' @noRd
ptr_ui_text_component_paths <- function() {
  list(
    title              = c("shell", "title"),
    draw_button        = c("shell", "draw_button"),
    draw_all_button    = c("shell", "draw_all_button"),
    layer_picker       = c("shell", "layer_picker"),
    data_subtab        = c("shell", "data_subtab"),
    controls_subtab    = c("shell", "controls_subtab"),
    upload_file        = c("upload", "file"),
    upload_name        = c("upload", "name"),
    layer_checkbox     = c("layer_checkbox")
  )
}

#' Default Copy Rules for ggpaintr
#'
#' Internal registry for all user-facing control copy. Maintainers should update
#' this object when they want to refine defaults or add support for new common
#' parameters in one place.
#'
#' @return A named list of default copy rules.
#' @noRd
ptr_default_ui_text <- function() {
  kws <- ptr_registry_keywords()
  default_placeholder_copy <- stats::setNames(
    lapply(kws, function(kw) ptr_registry_lookup(kw)$copy_defaults),
    kws
  )

  structure(
    list(
      shell = list(
        title = list(label = "ggpaintr Plot Builder"),
        draw_button = list(label = "Update plot"),
        draw_all_button = list(label = "Draw all"),
        layer_picker = list(label = "Layer"),
        data_subtab = list(label = "Data"),
        controls_subtab = list(label = "Controls"),
        shared_section_title = "Shared controls",
        shared_section_hint  = "Drives every place this variable appears in this plot.",
        shared_panel_title   = "Shared controls",
        shared_panel_hint    = "Drives every plot that uses it."
      ),
      upload = list(
        file = list(label = "Choose a data file"),
        name = list(
          label = "Optional dataset name",
          placeholder = "For example: sales_data",
          help = paste(
            "Accepted formats: .csv, .tsv, .rds, .xlsx, .xls, .json.",
            "Leave the name blank to use the file name in generated code."
          )
        )
      ),
      layer_checkbox = list(label = "Include this layer in the plot"),
      defaults = default_placeholder_copy,
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
        linewidth = list(
          var = list(label = "Choose the size column"),
          num = list(
            label = "Size",
            help = "Enter a number such as 2 or 3."
          )
        ),
        stroke = list(num = list(label = "Stroke width")),
        bins = list(num = list(label = "Number of bins")),
        binwidth = list(num = list(label = "Bin width"))
      ),
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
  c(
    colour = "color",
    size = "linewidth"
  )
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
#' @return A character vector.
#' @noRd
ptr_ui_text_keywords <- function() {
  ptr_registry_keywords()
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
  if (grepl("\\(\\)$", param)) {
    return(param)
  }
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
#'
#' @return Invisibly returns `TRUE`.
#' @noRd
ptr_validate_ui_text <- function(ui_text) {
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
    shell_leaf_keys <- c(
      "title", "draw_button", "draw_all_button",
      "layer_picker", "data_subtab", "controls_subtab"
    )
    shell_string_keys <- c(
      "shared_section_title", "shared_section_hint",
      "shared_panel_title", "shared_panel_hint"
    )
    allowed_shell <- c(shell_leaf_keys, shell_string_keys)
    unknown_shell <- setdiff(names(ui_text$shell), allowed_shell)
    if (length(unknown_shell) > 0) {
      rlang::abort(paste0(
        "ui_text$shell has unsupported entries: ",
        paste(sort(unknown_shell), collapse = ", "),
        "."
      ))
    }

    for (name in names(ui_text$shell)) {
      if (name %in% shell_string_keys) {
        val <- ui_text$shell[[name]]
        if (!is.character(val) || length(val) != 1L) {
          rlang::abort(paste0("ui_text$shell$", name, " must be a single string."))
        }
      } else {
        ptr_validate_ui_text_leaf(ui_text$shell[[name]], paste0("ui_text$shell$", name))
      }
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
      ptr_ui_text_keywords()
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
        ptr_ui_text_keywords()
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
        ptr_ui_text_keywords()
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


#' Inspect, validate, and pre-merge ggpaintr UI copy
#'
#' Returns the effective copy tree ggpaintr uses to label every generated
#' control. Call it with no arguments to see the current defaults, or pass
#' `ui_text =` a list of overrides to get back a validated, merged
#' `ptr_ui_text` object. That object can be reused across `ptr_app()` /
#' `ptr_server()` calls — those entry points short-circuit when handed an
#' already-merged `ptr_ui_text`, so the overrides are validated once.
#'
#' Use it to (1) discover the override schema (`names(ptr_ui_text())` and
#' the section below), and (2) fail fast on a malformed override list before
#' launching an app — `ptr_ui_text()` raises on unknown sections, keywords,
#' or leaf fields.
#'
#' @section UI text schema:
#' Override lists mirror the structure of `ptr_ui_text()`. Recognised paths
#' (every leaf is a named list of the fields below):
#'
#' - `shell$title$<leaf>`
#' - `shell$draw_button$<leaf>`
#' - `shell$draw_all_button$<leaf>`
#' - `shell$layer_picker$<leaf>`
#' - `shell$data_subtab$<leaf>`
#' - `shell$controls_subtab$<leaf>`
#' - `upload$file$<leaf>`
#' - `upload$name$<leaf>`
#' - `layer_checkbox$<leaf>`
#' - `defaults$<keyword>$<leaf>` — per placeholder keyword (`var`, `text`,
#'   `num`, `expr`, `upload`, ...)
#' - `params$<param>$<keyword>$<leaf>` — per aesthetic/argument name
#'   (`x`, `y`, `color`, ...); aliases (`colour`, `size`) are normalized
#' - `layers$<layer_name>$<keyword>$<param>$<leaf>` — per layer override
#'   (use `__unnamed__` as `<param>` for positional arguments)
#'
#' Leaf fields are `label`, `help`, `placeholder`, and `empty_text`. Leaf
#' strings may use the `{param}` and `{layer}` tokens, which are interpolated
#' at resolve time.
#'
#' @param ui_text `NULL` (return defaults), a named list of overrides, or an
#'   already-merged `ptr_ui_text` object (returned unchanged).
#'
#' @return A `ptr_ui_text` object containing the merged copy rules.
#'
#' @examples
#' # Default rules
#' rules <- ptr_ui_text()
#' rules$shell$title$label
#'
#' # Override the draw button label
#' rules <- ptr_ui_text(
#'   ui_text = list(shell = list(draw_button = list(label = "Render")))
#' )
#' rules$shell$draw_button$label
#' @export
ptr_ui_text <- function(ui_text = NULL) {
  if (inherits(ui_text, "ptr_ui_text")) {
    return(ui_text)
  }

  defaults <- ptr_default_ui_text()
  if (is.null(ui_text)) {
    return(defaults)
  }

  ptr_validate_ui_text(ui_text)
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
#'
#' @return A named list of custom overrides or `NULL`.
#' @noRd
ptr_compact_ui_text <- function(ui_text = NULL) {
  effective_ui_text <- ptr_ui_text(ui_text)
  default_ui_text <- ptr_default_ui_text()

  ptr_compact_ui_text_branch(
    unclass(effective_ui_text),
    unclass(default_ui_text)
  )
}

#' Resolve copy for one ggpaintr control or app element
#'
#' Looks up the effective label/help/placeholder for a single UI element,
#' applying the `defaults -> params -> layers` specificity chain and
#' interpolating the `{param}` / `{layer}` tokens. Placeholder authors can
#' call this inside a custom `build_ui` hook so their control is labelled
#' through the same override chain as the built-in controls.
#'
#' @param component One of `title`, `draw_button`, `draw_all_button`,
#'   `layer_picker`, `data_subtab`, `controls_subtab`, `upload_file`,
#'   `upload_name`, `layer_checkbox`, or `control` (for a placeholder
#'   control, in which case `keyword` is required).
#' @param keyword Placeholder keyword (e.g. `"var"`, `"num"`); required when
#'   `component = "control"`.
#' @param param Optional parameter / aesthetic name (e.g. `"x"`); only used
#'   when `component = "control"`.
#' @param layer_name Optional layer name (e.g. `"facet_wrap"`); only used
#'   when `component = "control"`, to pick up a `layers$<layer>$...` override.
#' @param ui_text `NULL`, a list of overrides, or an already-merged
#'   `ptr_ui_text` object (see [ptr_ui_text()]).
#'
#' @return A named list with `label`, `help`, `placeholder`, and `empty_text`.
#'
#' @examples
#' # Resolve copy for the title element
#' ptr_resolve_ui_text("title")
#'
#' # Resolve copy for a var control on the x-axis
#' ptr_resolve_ui_text("control", keyword = "var", param = "x")
#'
#' # Inside a custom `build_ui` hook, label the control through the same
#' # override chain ggpaintr uses for built-in controls:
#' my_build_ui <- function(node, label, ...) {
#'   copy <- ptr_resolve_ui_text(
#'     "control",
#'     keyword = node$keyword,
#'     param = node$param,
#'     ui_text = list(...)$ui_text
#'   )
#'   shiny::textInput(node$id, label = copy$label %||% label)
#' }
#' @export
ptr_resolve_ui_text <- function(component,
                                keyword = NULL,
                                param = NULL,
                                layer_name = NULL,
                                ui_text = NULL) {
  rules <- ptr_ui_text(ui_text)

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
    # `__unnamed__` is intentionally allowed here (unlike param_rule above).
    # Some layers (e.g. facet_wrap, facet_grid) register copy under the
    # `__unnamed__` key for positional arguments.  The lookup returns NULL
    # when no such sub-key exists, so there is no mis-fire for named params.
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
