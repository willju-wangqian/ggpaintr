#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
check_char_set_names <- function(x) {
  if(!is.null(x)) {
    assert_that( is.character(x) )
    x <- purrr::set_names(x)
  }

  x
}


#' Title
#'
#' @param x list
#'
#' @return
#' @export
#'
#' @examples
check_remove_null <- function(x) {
  if(is.null(x)) return(NULL)

  x <- x[!sapply(x, is.null)]
  if(length(x) == 0) {
    x <- NULL
  }
  x
}

#' Title
#'
#' @param x
#' @param add_quo
#'
#' @return
#' @export
#'
#' @examples
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



#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
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

#' Title
#'
#' @param x
#' @param name
#' @param value
#'
#' @return
#' @export
#'
#' @examples
append_list_name <- function(x, name, value) {
  stopifnot(is_character(name))

  x[[name]] <- value
  x
}


#' Title
#'
#' @param x
#' @param param
#' @param suffix
#'
#' @return
#' @export
#'
#' @examples
fix_repeated_param <- function(x, param, suffix = "_geom") {

  if(is.null(x)) {
    return(NULL)
  }


  stopifnot(is.character(x) && is.character(param))

  idx <- which(x == param)
  if( length(idx) == 0 ) {
    return(x)
  } else if (length(idx) == 1) {
    x[idx] <- paste0(param, suffix)
    return(x)
  } else {
    stop("Too many repeated parameters.")
    return(NULL)
  }

}

#' Title
#'
#' @param defaultArgs
#' @param ui_element
#' @param ui_param
#' @param plot_settings
#'
#' @return
#' @export
#'
#' @examples
addDefaultArgs <- function(defaultArgs, ui_element, ui_param, plot_settings) {
  if (has_name(plot_settings, ui_element)) {
    defaultArgs[[ui_param]] <- plot_settings[[ui_element]]
  }
  defaultArgs
}






