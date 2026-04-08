#' Determine an Expression's Structural Type
#'
#' Internal helper used while traversing ggplot-like calls.
#'
#' @param x An R expression object.
#'
#' @return A single string describing the expression type.
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
`expr_pluck<-` <- function(.x, index_path, value) {
  tryCatch({
    .x[[index_path]] <- value
  }, error = function(e) {
    cat(
      paste0(
        "Error in ", deparse(e$call), ": ", e$message,
        "\nModification failed.\n"
      )
    )
  })

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
#' @keywords internal
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
#' @keywords internal
handle_duplicate_names <- function(x) {
  if (length(unique(x)) != length(x)) {
    duplicated_items <- unique(x[duplicated(x)])
    counting_list <- rep(list(0), length(duplicated_items))
    counting_list <- rlang::set_names(counting_list, duplicated_items)

    for (i in seq_along(x)) {
      if (x[i] %in% names(counting_list)) {
        counting_list[[x[i]]] <- counting_list[[x[i]]] + 1
        x[i] <- paste0(x[i], "-", counting_list[[x[i]]])
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
#' @keywords internal
encode_id <- function(index_path, func_name) {
  paste(c(func_name, index_path), collapse = "+")
}

#' Read a Parameter Name from an Expression Path
#'
#' @param .expr A call expression.
#' @param .path An index path.
#'
#' @return The parameter name or `NULL`.
#' @keywords internal
get_expr_param <- function(.expr, .path) {
  if (length(.path) > 1) {
    current_index <- .path[1]
    if (is.null(names(.expr[[current_index]])) &&
        expr_type(.expr[[current_index]]) == "call") {
      return(get_expr_param(.expr[[current_index]], .path[-1]))
    }

    if (!is.null(names(.expr[[current_index]]))) {
      return(names(.expr[[current_index]])[.path[2]])
    }
  } else if (!is.null(names(.expr))) {
    return(names(.expr)[.path])
  }

  list(NULL)
}

#' Remove Placeholder Marker Symbols
#'
#' @param .expr An expression object.
#' @param target The placeholder marker symbol.
#' @param current_path Internal recursion path.
#'
#' @return The cleaned expression.
#' @keywords internal
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

#' Remove Empty Non-ggplot Calls
#'
#' @param .expr An expression object.
#'
#' @return The cleaned expression or `NULL`.
#' @keywords internal
expr_remove_emptycall2 <- function(.expr) {
  for (i in length(.expr):1) {
    if (is.call(.expr[[i]])) {
      if (length(.expr[[i]]) == 1) {
        func_meaning <- tryCatch(eval(.expr[[i]]), error = function(e) NULL)
        if (is.null(func_meaning) || !("gg" %in% attr(func_meaning, "class"))) {
          message(
            paste0(
              "The function ", rlang::as_string(.expr[[i]][[1]]),
              "() in ", rlang::as_string(.expr[[1]]), "() is removed."
            )
          )
          .expr[[i]] <- NULL
        }
      } else {
        .expr[[i]] <- expr_remove_emptycall2(.expr[[i]])
      }
    }
  }

  if (is.call(.expr) && length(.expr) == 1) {
    func_meaning <- tryCatch(eval(.expr), error = function(e) NULL)
    if (is.null(func_meaning) || !("gg" %in% attr(func_meaning, "class"))) {
      message(paste0("The function ", rlang::as_string(.expr[[1]]), "() is removed."))
      .expr <- NULL
    }
  }

  .expr
}

#' Drop `NULL` Elements from a List
#'
#' @param x A list or `NULL`.
#'
#' @return A list without `NULL` values, or `NULL`.
#' @keywords internal
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
