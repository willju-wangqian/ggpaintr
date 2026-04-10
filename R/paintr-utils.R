#' Determine an Expression's Structural Type
#'
#' Internal helper used while traversing ggplot-like calls.
#'
#' @param x An R expression object.
#'
#' @return A single string describing the expression type.
#' @noRd
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

#' Flatten `+`-Separated Plot Layers
#'
#' @param x An expression or call.
#'
#' @return A nested structure with plot layers separated.
#' @noRd
handle_call_break_sum <- function(x) {
  if (rlang::as_string(x[[1]]) == "+") {
    lapply(x[-1], break_sum)
  } else {
    x
  }
}

#' Recursively Split Plot Expressions
#'
#' @param x An expression or call.
#'
#' @return A split representation of the expression.
#' @noRd
break_sum <- function(x) {
  switch(
    expr_type(x),
    symbol = x,
    constant = x,
    call = handle_call_break_sum(x),
    pairlist = as.pairlist(lapply(x, break_sum))
  )
}

#' Get a Call or Symbol Name
#'
#' @param x An expression object.
#'
#' @return A function or symbol name when available.
#' @noRd
get_fun_names <- function(x) {
  if (rlang::is_call(x)) {
    return(rlang::as_string(x[[1]]))
  }

  if (rlang::is_symbol(x)) {
    return(rlang::as_string(x))
  }

  NULL
}

#' Pluck an Expression by Index Path
#'
#' @param .x An expression-like object.
#' @param index_path An index vector.
#'
#' @return The plucked expression or `NULL`.
#' @noRd
expr_pluck <- function(.x, index_path) {
  tryCatch(.x[[index_path]], error = function(e) NULL)
}

#' Replace an Expression by Index Path
#'
#' @param .x An expression-like object.
#' @param index_path An index vector.
#' @param value A replacement value.
#'
#' @return The updated expression object.
#' @noRd
`expr_pluck<-` <- function(.x, index_path, value) {
  tryCatch(
    .x[[index_path]] <- value,
    error = function(e) {
      rlang::abort(
        paste0(
          "Failed to substitute expression at index path [",
          paste(index_path, collapse = ", "),
          "]: ", e$message
        ),
        parent = e
      )
    }
  )

  .x
}

#' Locate Placeholder Paths Inside an Expression
#'
#' @param x An expression to inspect.
#' @param target Placeholder symbols to detect.
#' @param current_path Internal recursion path.
#' @param result Internal accumulator.
#'
#' @return A list of index paths.
#' @noRd
get_index_path <- function(x,
                           target = c("var", "text", "num", "expr", "upload"),
                           current_path = numeric(),
                           result = list()) {
  for (i in seq_along(x)) {
    new_path <- c(current_path, i)
    if (is.call(x[[i]])) {
      result <- get_index_path(x[[i]], target, new_path, result)
    } else if (is.symbol(x[[i]])) {
      if (rlang::as_string(x[[i]]) %in% target) {
        result <- c(result, list(new_path))
      } else if (!is.null(names(x)) && names(x)[i] == "data") {
        result <- c(result, list(new_path))
      }
    }
  }

  result
}

#' Suffix Duplicate Layer Names
#'
#' @param x A character vector of names.
#'
#' @return A character vector with duplicates made unique.
#' @noRd
handle_duplicate_names <- function(x) {
  if (length(unique(x)) != length(x)) {
    duplicated_items <- unique(x[duplicated(x)])
    counting_list <- rep(list(0), length(duplicated_items))
    counting_list <- rlang::set_names(counting_list, duplicated_items)
    seen <- logical(0)

    for (i in seq_along(x)) {
      if (x[i] %in% names(counting_list)) {
        counting_list[[x[i]]] <- counting_list[[x[i]]] + 1
        if (isTRUE(seen[x[i]])) {
          x[i] <- paste0(x[i], "-", counting_list[[x[i]]])
        } else {
          seen[[x[i]]] <- TRUE
        }
      }
    }
  }

  x
}

#' Build a Stable Placeholder Id
#'
#' @param index_path An index path.
#' @param func_name A call name.
#'
#' @return A single encoded id string.
#' @noRd
encode_id <- function(index_path, func_name) {
  paste(c(func_name, index_path), collapse = "+")
}

#' Read a Parameter Name from an Expression Path
#'
#' @param .expr A call expression.
#' @param .path An index path.
#'
#' @return The parameter name or `NULL`.
#' @noRd
get_expr_param <- function(.expr, .path) {
  if (length(.path) > 1) {
    current_index <- .path[1]
    current_expr <- .expr[[current_index]]
    current_names <- names(current_expr)

    if (is.call(current_expr) && is.null(current_names)) {
      return(get_expr_param(current_expr, .path[-1]))
    }

    if (!is.null(current_names)) {
      return(current_names[.path[2]])
    }
  }

  expr_names <- names(.expr)
  if (!is.null(expr_names)) {
    return(expr_names[.path])
  }

  NULL
}

#' Remove Placeholder Marker Symbols
#'
#' @param .expr An expression object.
#' @param target The placeholder marker symbol.
#' @param current_path Internal recursion path.
#'
#' @return The cleaned expression.
#' @noRd
expr_remove_null <- function(.expr,
                             target = rlang::sym("_NULL_PLACEHOLDER"),
                             current_path = numeric()) {
  for (i in length(.expr):1) {
    new_path <- c(current_path, i)
    if (is.call(.expr[[i]])) {
      .expr[[i]] <- expr_remove_null(.expr[[i]], target, new_path)
    } else if (is.symbol(.expr[[i]]) && identical(.expr[[i]], target)) {
      .expr[[i]] <- NULL
    }
  }

  .expr
}

#' Check Whether a Function Name Looks Like a ggplot2 Layer
#'
#' @param fn_name A single function name string.
#'
#' @return `TRUE` if the name matches known ggplot2 patterns.
#' @noRd
ptr_is_gg_layer_name <- function(fn_name) {
  gg_prefixes <- c(
    "geom_", "stat_", "scale_", "coord_", "facet_", "theme_", "theme",
    "labs", "xlab", "ylab", "ggtitle", "guides", "guide_",
    "annotation_", "borders", "expand_limits", "lims", "xlim", "ylim",
    "after_stat", "after_scale", "stage"
  )
  any(vapply(gg_prefixes, function(p) startsWith(fn_name, p), logical(1)))
}

#' Check Whether a Layer Can Stand Alone Without Arguments
#'
#' Layers like `geom_*()` and `stat_*()` can inherit aesthetics from
#' the base `ggplot()` call, so they are valid even with no arguments.
#' Other gg helpers (`labs()`, `facet_wrap()`, `theme()`, etc.) are
#' either no-ops or will error when called with no arguments.
#'
#' @param fn_name A single function name string.
#'
#' @return `TRUE` if the layer can be called with zero arguments.
#' @noRd
ptr_can_stand_alone <- function(fn_name) {
  standalone_prefixes <- c("geom_", "stat_")
  any(vapply(standalone_prefixes, function(p) startsWith(fn_name, p), logical(1)))
}

#' Remove Empty Non-ggplot Calls
#'
#' @param .expr An expression object.
#'
#' @return The cleaned expression or `NULL`.
#' @noRd
expr_remove_emptycall2 <- function(.expr) {
  for (i in length(.expr):1) {
    if (is.call(.expr[[i]])) {
      if (length(.expr[[i]]) == 1) {
        fn_name <- rlang::as_string(.expr[[i]][[1]])
        if (!ptr_is_gg_layer_name(fn_name)) {
          cli::cli_inform(paste0(
            "The function ", fn_name,
            "() in ", rlang::as_string(.expr[[1]]), "() is removed."
          ))
          .expr[[i]] <- NULL
        }
      } else {
        .expr[[i]] <- expr_remove_emptycall2(.expr[[i]])
      }
    }
  }

  if (is.call(.expr) && length(.expr) == 1) {
    fn_name <- rlang::as_string(.expr[[1]])
    if (!ptr_is_gg_layer_name(fn_name)) {
      cli::cli_inform(paste0("The function ", fn_name, "() is removed."))
      .expr <- NULL
    }
  }

  .expr
}

#' Remove Empty Non-Standalone Layers from an Expression List
#'
#' After placeholder resolution and empty-call pruning, some top-level
#' layers may have lost all arguments (e.g. `labs()`, `facet_wrap()`,
#' `theme()`).  These are either no-ops or will error at eval time.
#' Layers that can inherit aesthetics (`geom_*`, `stat_*`) and the
#' base `ggplot` layer are kept.
#'
#' @param expr_list A named list of layer expressions.
#'
#' @return The filtered expression list with empty non-standalone layers
#'   set to `NULL`.
#' @noRd
ptr_remove_empty_nonstandalone_layers <- function(expr_list) {
  for (nn in names(expr_list)) {
    expr <- expr_list[[nn]]
    if (is.null(expr) || nn == "ggplot") next
    if (is.call(expr) && length(expr) == 1) {
      fn_name <- rlang::as_string(expr[[1]])
      if (!ptr_can_stand_alone(fn_name)) {
        cli::cli_inform(paste0("Layer ", fn_name, "() removed (no arguments provided)."))
        expr_list[[nn]] <- NULL
      }
    }
  }
  expr_list
}

#' Drop `NULL` Elements from a List
#'
#' @param x A list or `NULL`.
#'
#' @return A list without `NULL` values, or `NULL`.
#' @noRd
check_remove_null <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) {
    return(NULL)
  }

  x
}
