#' Check if a character vector has name
#'
#' If this character vector does not have names, then its elements will be assigned to `names(x)`
#'
#' @param x a character vector
#'
#' @return a character vector with names
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr set_names
#'
#' @examples
#' check_char_set_names(c("a", "b"))
check_char_set_names <- function(x) {
  if(!is.null(x)) {
    assert_that( is.character(x) )
    x <- purrr::set_names(x)
  }

  x
}

#' check if a list has `NULL`; if so, remove it (them)
#'
#' @param x list
#'
#' @return `NULL` or a list
#' @export
#'
#' @examples
#' x <- list(a = 1, b = NULL)
#' check_remove_null(x)
check_remove_null <- function(x) {
  if(is.null(x)) return(NULL)

  # x <- x[!sapply(x, function(xx) { is.null(xx) || is.na(xx) } )]

  x <- x[!sapply(x, is.null )]
  if(length(x) == 0) {
    x <- NULL
  }
  x
}

#' Paste parameter and its argument
#'
#' @param x a named character vector
#' @param add_quo bool. Whether to add quotation marks on the arguments
#'
#' @return a string
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' x <- c(param1 = "arg1", param2 = "arg2")
#' paste_arg_param(x, add_quo = TRUE)
paste_arg_param <- function(x, add_quo = FALSE) {
  if(is.null(x)) {
    return("")
  }

  assert_that(
    !is.null( names(x) )
  )

  code_args <- mapply(function(aa, var){
    if(add_quo) {
      paste0(aa, " = '", var, "'")
    } else {
      paste0(aa, " = ", var)
    }
  }, names(x), x, SIMPLIFY = FALSE )
  code_args[['sep']] <- ', '
  code <- do.call(paste, code_args)

  return(code)
}



#' Unwrap an expression
#'
#' @param x an expression
#'
#' @return list of strings converted from the expression
#' @export
#'
#' @import rlang
#'
#' @examples
#' unwrap_expr(x + y + z)
unwrap_expr <- function(x) {
  code <- enexpr(x)

  if(all(sapply(code, is_symbol))) {
    return(lapply(as.list(code), as_string))
  } else {
    lapply(as.list(code), function(code_piece) {
      if(is_call(code_piece)) {
        return(unwrap_expr(!!code_piece))
      } else {
        return(as_string(code_piece))
      }

    })

  }
}

#' Append a named list
#'
#' @param x a list
#' @param name new name
#' @param value value of the new name
#'
#' @return a list
#' @export
#'
#' @examples
#' x <- list(a = 1, b = 2)
#' append_list_name(x, "c", 3)
append_list_name <- function(x, name, value) {
  stopifnot(is_character(name))

  x[[name]] <- value
  x
}

#' Add one element into a list of arguments
#'
#' @param defaultArgs the default argument list
#' @param ui_element the keyword of the ui element
#' @param ui_param the parameter of the ui function
#' @param plot_settings a list contains all ui elements
#'
#' @return
addDefaultArgs <- function(defaultArgs, ui_element, ui_param, plot_settings) {
  if (has_name(plot_settings, ui_element)) {
    defaultArgs[[ui_param]] <- plot_settings[[ui_element]]
  }
  defaultArgs
}






