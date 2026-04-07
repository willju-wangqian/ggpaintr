#' Default Copy Rules for ggpaintr
#'
#' Internal registry for all user-facing control copy. Maintainers should update
#' this object when they want to refine defaults or add support for new common
#' parameters in one place.
#'
#' @return A named list of default copy rules.
#' @keywords internal
paintr_default_copy_rules <- function() {
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
      defaults = list(
        var = list(
          label = "Choose a column for {param}",
          empty_text = "Choose one column"
        ),
        text = list(label = "Enter text for {param}"),
        num = list(label = "Enter a number for {param}"),
        expr = list(label = "Enter an expression for {param}")
      ),
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
    class = "paintr_copy_rules"
  )
}

#' Normalize Parameter Aliases for Copy Rules
#'
#' Add new aliases here when different parameter spellings should resolve to the
#' same copy entry.
#'
#' @return A named character vector mapping aliases to canonical keys.
#' @keywords internal
paintr_copy_param_aliases <- function() {
  c(colour = "color")
}

#' Return Allowed Copy Leaf Fields
#'
#' @return A character vector.
#' @keywords internal
paintr_copy_leaf_fields <- function() {
  c("label", "help", "placeholder", "empty_text")
}

#' Return Allowed Copy Keywords
#'
#' @return A character vector.
#' @keywords internal
paintr_copy_keywords <- function() {
  c("var", "text", "num", "expr")
}

#' Detect Whether a Parameter Is Unnamed
#'
#' @param param A parameter value from `paintr_obj$param_list`.
#'
#' @return A single logical value.
#' @keywords internal
paintr_param_is_unnamed <- function(param) {
  is.null(param) ||
    identical(param, "") ||
    (is.list(param) && length(param) >= 1 && is.null(param[[1]]))
}

#' Normalize a Copy Rule Parameter Key
#'
#' @param param A parameter name or `NULL`.
#'
#' @return A normalized parameter key.
#' @keywords internal
paintr_normalize_param_key <- function(param) {
  if (paintr_param_is_unnamed(param)) {
    return("__unnamed__")
  }

  param <- as.character(param)[1]
  alias_map <- paintr_copy_param_aliases()
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
#' @keywords internal
paintr_humanize_param <- function(param) {
  if (paintr_param_is_unnamed(param)) {
    return("this setting")
  }

  param <- paintr_normalize_param_key(param)
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
#' @keywords internal
paintr_interpolate_copy_text <- function(text, param = NULL, layer_name = NULL) {
  if (is.null(text)) {
    return(NULL)
  }

  text <- gsub("\\{param\\}", paintr_humanize_param(param), text)
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
#' @keywords internal
paintr_validate_copy_leaf <- function(x, path) {
  if (!is.list(x) || is.null(names(x)) || length(x) == 0) {
    stop(path, " must be a named list.", call. = FALSE)
  }

  unknown_fields <- setdiff(names(x), paintr_copy_leaf_fields())
  if (length(unknown_fields) > 0) {
    stop(
      path,
      " has unsupported fields: ",
      paste(sort(unknown_fields), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  for (field_name in names(x)) {
    value <- x[[field_name]]
    if (!is.character(value) || length(value) != 1) {
      stop(path, "$", field_name, " must be a single string.", call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Validate Copy Rules
#'
#' @param copy_rules User-supplied copy rules.
#'
#' @return Invisibly returns `TRUE`.
#' @keywords internal
paintr_validate_copy_rules <- function(copy_rules) {
  if (is.null(copy_rules)) {
    return(invisible(TRUE))
  }

  if (!is.list(copy_rules)) {
    stop("copy_rules must be a named list.", call. = FALSE)
  }

  allowed_top <- c(
    "shell",
    "upload",
    "layer_checkbox",
    "defaults",
    "params",
    "layers"
  )
  unknown_top <- setdiff(names(copy_rules), allowed_top)
  if (length(unknown_top) > 0) {
    stop(
      "copy_rules has unsupported top-level sections: ",
      paste(sort(unknown_top), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (!is.null(copy_rules$shell)) {
    allowed_shell <- c("title", "draw_button", "export_button")
    unknown_shell <- setdiff(names(copy_rules$shell), allowed_shell)
    if (length(unknown_shell) > 0) {
      stop(
        "copy_rules$shell has unsupported entries: ",
        paste(sort(unknown_shell), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    for (name in names(copy_rules$shell)) {
      paintr_validate_copy_leaf(copy_rules$shell[[name]], paste0("copy_rules$shell$", name))
    }
  }

  if (!is.null(copy_rules$upload)) {
    allowed_upload <- c("file", "name")
    unknown_upload <- setdiff(names(copy_rules$upload), allowed_upload)
    if (length(unknown_upload) > 0) {
      stop(
        "copy_rules$upload has unsupported entries: ",
        paste(sort(unknown_upload), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    for (name in names(copy_rules$upload)) {
      paintr_validate_copy_leaf(copy_rules$upload[[name]], paste0("copy_rules$upload$", name))
    }
  }

  if (!is.null(copy_rules$layer_checkbox)) {
    paintr_validate_copy_leaf(copy_rules$layer_checkbox, "copy_rules$layer_checkbox")
  }

  if (!is.null(copy_rules$defaults)) {
    unknown_defaults <- setdiff(names(copy_rules$defaults), paintr_copy_keywords())
    if (length(unknown_defaults) > 0) {
      stop(
        "copy_rules$defaults has unsupported entries: ",
        paste(sort(unknown_defaults), collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    for (keyword in names(copy_rules$defaults)) {
      paintr_validate_copy_leaf(
        copy_rules$defaults[[keyword]],
        paste0("copy_rules$defaults$", keyword)
      )
    }
  }

  if (!is.null(copy_rules$params)) {
    for (param_name in names(copy_rules$params)) {
      param_rules <- copy_rules$params[[param_name]]
      if (!is.list(param_rules)) {
        stop(
          "copy_rules$params$", param_name, " must be a named list.",
          call. = FALSE
        )
      }

      unknown_keywords <- setdiff(names(param_rules), paintr_copy_keywords())
      if (length(unknown_keywords) > 0) {
        stop(
          "copy_rules$params$", param_name, " has unsupported keywords: ",
          paste(sort(unknown_keywords), collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      for (keyword in names(param_rules)) {
        paintr_validate_copy_leaf(
          param_rules[[keyword]],
          paste0("copy_rules$params$", param_name, "$", keyword)
        )
      }
    }
  }

  if (!is.null(copy_rules$layers)) {
    for (layer_name in names(copy_rules$layers)) {
      layer_rules <- copy_rules$layers[[layer_name]]
      if (!is.list(layer_rules)) {
        stop(
          "copy_rules$layers$", layer_name, " must be a named list.",
          call. = FALSE
        )
      }

      unknown_keywords <- setdiff(names(layer_rules), paintr_copy_keywords())
      if (length(unknown_keywords) > 0) {
        stop(
          "copy_rules$layers$", layer_name, " has unsupported keywords: ",
          paste(sort(unknown_keywords), collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      for (keyword in names(layer_rules)) {
        keyword_rules <- layer_rules[[keyword]]
        if (!is.list(keyword_rules)) {
          stop(
            "copy_rules$layers$", layer_name, "$", keyword,
            " must be a named list.",
            call. = FALSE
          )
        }

        for (param_name in names(keyword_rules)) {
          paintr_validate_copy_leaf(
            keyword_rules[[param_name]],
            paste0("copy_rules$layers$", layer_name, "$", keyword, "$", param_name)
          )
        }
      }
    }
  }

  invisible(TRUE)
}

#' Normalize User Copy Rule Keys
#'
#' @param copy_rules User-supplied copy rules.
#'
#' @return A normalized copy-rule list.
#' @keywords internal
paintr_normalize_copy_rules <- function(copy_rules) {
  if (is.null(copy_rules)) {
    return(NULL)
  }

  if (!is.null(copy_rules$params)) {
    normalized_params <- list()
    for (param_name in names(copy_rules$params)) {
      normalized_key <- paintr_normalize_param_key(param_name)
      existing <- normalized_params[[normalized_key]]
      normalized_params[[normalized_key]] <- if (is.null(existing)) {
        copy_rules$params[[param_name]]
      } else {
        paintr_merge_copy_rules(existing, copy_rules$params[[param_name]])
      }
    }
    copy_rules$params <- normalized_params
  }

  if (!is.null(copy_rules$layers)) {
    normalized_layers <- list()
    for (layer_name in names(copy_rules$layers)) {
      layer_rules <- copy_rules$layers[[layer_name]]
      normalized_layer <- list()

      for (keyword in names(layer_rules)) {
        keyword_rules <- layer_rules[[keyword]]
        normalized_keyword <- list()
        for (param_name in names(keyword_rules)) {
          normalized_key <- if (identical(param_name, "__unnamed__")) {
            "__unnamed__"
          } else {
            paintr_normalize_param_key(param_name)
          }

          existing <- normalized_keyword[[normalized_key]]
          normalized_keyword[[normalized_key]] <- if (is.null(existing)) {
            keyword_rules[[param_name]]
          } else {
            paintr_merge_copy_rules(existing, keyword_rules[[param_name]])
          }
        }

        normalized_layer[[keyword]] <- normalized_keyword
      }

      normalized_layers[[layer_name]] <- normalized_layer
    }

    copy_rules$layers <- normalized_layers
  }

  copy_rules
}

#' Recursively Merge Copy Rules
#'
#' @param base A base list.
#' @param overrides Override values.
#'
#' @return A merged list.
#' @keywords internal
paintr_merge_copy_rules <- function(base, overrides) {
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
      paintr_merge_copy_rules(result[[name]], overrides[[name]])
    } else {
      overrides[[name]]
    }
  }

  result
}

#' Build Effective Copy Rules
#'
#' @param copy_rules Optional user-supplied rules.
#'
#' @return A merged copy-rule list.
#' @keywords internal
paintr_effective_copy_rules <- function(copy_rules = NULL) {
  if (inherits(copy_rules, "paintr_copy_rules")) {
    return(copy_rules)
  }

  defaults <- paintr_default_copy_rules()
  if (is.null(copy_rules)) {
    return(defaults)
  }

  paintr_validate_copy_rules(copy_rules)
  copy_rules <- paintr_normalize_copy_rules(copy_rules)

  merged <- paintr_merge_copy_rules(unclass(defaults), copy_rules)
  class(merged) <- "paintr_copy_rules"
  merged
}

#' Resolve Copy for One Control or App Element
#'
#' @param component One of `title`, `draw_button`, `export_button`,
#'   `upload_file`, `upload_name`, `layer_checkbox`, or `control`.
#' @param keyword Optional placeholder keyword.
#' @param layer_name Optional layer name.
#' @param param Optional parameter name.
#' @param copy_rules Effective or user-supplied copy rules.
#'
#' @return A named list with `label`, `help`, `placeholder`, and `empty_text`.
#' @keywords internal
paintr_resolve_copy <- function(component,
                                keyword = NULL,
                                layer_name = NULL,
                                param = NULL,
                                copy_rules = NULL) {
  rules <- paintr_effective_copy_rules(copy_rules)

  resolved <- switch(
    component,
    title = rules$shell$title,
    draw_button = rules$shell$draw_button,
    export_button = rules$shell$export_button,
    upload_file = rules$upload$file,
    upload_name = rules$upload$name,
    layer_checkbox = rules$layer_checkbox,
    control = {
      param_key <- paintr_normalize_param_key(param)
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

      paintr_merge_copy_rules(
        paintr_merge_copy_rules(default_rule, param_rule),
        layer_rule
      )
    },
    stop("Unknown copy component: ", component, ".", call. = FALSE)
  )

  for (field_name in paintr_copy_leaf_fields()) {
    resolved[[field_name]] <- paintr_interpolate_copy_text(
      resolved[[field_name]],
      param = param,
      layer_name = layer_name
    )
  }

  resolved
}
