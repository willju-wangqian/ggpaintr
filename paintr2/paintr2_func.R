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

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

my_list <- function(...) {
  enexprs(...)
}

handle_call_break_sum <- function(x) {
  if (as_string(x[[1]]) == '+') {
    lapply(x[-1], break_sum)
  } else {
    x
  }
}

break_sum <- function(x) {
  switch(expr_type(x),
         symbol = x,
         constant = x,
         call = handle_call_break_sum(x),
         pairlist = as.pairlist(lapply(x, break_sum))
  )
}

get_fun_names <- function(x) {
  as_string(x[[1]])
}

get_item <- function(.x, indices) {
  for (index in indices) {
    .x <- .x[[index]]
  }
  return(.x)
}

modify_with_index_path <- function(.x, index_path, new_value) {
  access_expr <- ".x"
  for (index in unlist(index_path)) {
    access_expr <- paste(access_expr, "[[", index, "]]", sep="")
  }
  tmp_expr <- parse_expr(paste(access_expr, "<- enexpr(new_value)"))
  eval(tmp_expr)
  return(.x)
}

expr_pluck <- function(.x, ...) {
  get_item(.x, list2(...))
}

`expr_pluck<-` <- function(.x, ..., value) {
  modify_with_index_path(.x, list2(...), (!!value))
}

get_index_path <- function(x,
                           target = c("var", "text", "num", "expr", "upload"),
                           current_path = numeric(),
                           result = list()) {
  for (i in seq_along(x)) {
    new_path <- c(current_path, i)
    if (is.call(x[[i]])) {
      result <- get_index_path(x[[i]], target, new_path, result)
    } else if (is.symbol(x[[i]])) {
      # browser()
      if (as_string(x[[i]]) %in% target) {
        result <- c(result, list(new_path))
      } else if (!is.null(names(x)) && names(x)[i] == 'data') {
        result <- c(result, list(new_path))
      }
    }
  }
  return(result)
}

handle_duplicate_names <- function(x) {
  if(length(unique(x)) != length(x)) {
    duplicated_items <- unique(x[duplicated(x)])
    counting_list <- rep(list(0), length(duplicated_items))
    counting_list <- set_names(counting_list, duplicated_items)

    for (i in seq_along(x)) {
      if (x[i] %in% names(counting_list)) {
        counting_list[[x[i]]] <- counting_list[[x[i]]] + 1
        x[i] <- paste0(x[i], counting_list[[x[i]]])

      }
    }
  }

  return(x)
}

detect_keywords <- function(.x) {
  if(is.symbol(.x)) {
    if (.x == sym("var")) {
      "var"
    } else if (.x == sym("text")) {
      "text"
    } else if (.x == sym("num")) {
      "num"
    } else if (.x == sym("expr")) {
      "expr"
    } else if (.x == sym("upload")) {
      "upload"
    } else {
      as_string(.x)
    }
  } else {
    NULL
  }
}

switch_keywords <- function(.x, ...) {
  # switch(detect_keywords(.x),
  #        ...,
  #        warning("Don't know how to handle keyword: ", as_string(.x), call. = FALSE)
  # )
  switch(detect_keywords(.x),
         ...
  )

}

handle_var <- function(.expr, index_path, input_item) {
  if (is.null(input_item)) {
    expr_pluck(.expr, index_path) <- sym("NULL_placeholder")
  } else {
    assert_that(is.character(input_item))
    # expr_pluck(.expr, index_path) <- expr(.data[[!!input_item]]) # this won't work with calculations in the expr

    # check white space
    expr_pluck(.expr, index_path) <- parse_expr(input_item)
  }
  .expr
}

handle_num <- function(.expr, index_path, input_item) {
  # browser()
  if (is.na(input_item) | is.null(input_item)) {
    expr_pluck(.expr, index_path) <- sym("NULL_placeholder")
  } else {
    assert_that(is.numeric(input_item))
    expr_pluck(.expr, index_path) <- expr(!!input_item)
  }
  .expr
}

handle_text <- function(.expr, index_path, input_item) {
  if (is.null(input_item) | input_item == "") {
    expr_pluck(.expr, index_path) <- sym("NULL_placeholder")
  } else {
    assert_that(is.character(input_item))
    expr_pluck(.expr, index_path) <- expr(!!input_item)
  }
  .expr
}

handle_expr <- function(.expr, index_path, input_item) {
  if ((!is.null(input_item)) & (input_item != "")) {
    expr_pluck(.expr, index_path) <- parse_expr(input_item)
  } else {
    expr_pluck(.expr, index_path) <- sym("NULL_placeholder")
  }
  .expr
}

handle_upload <- function(.expr, index_path, input_item) { # this needs to be fixed for real data upload
  if ((!is.null(input_item)) & (input_item != "")) {
    expr_pluck(.expr, index_path) <- parse_expr(input_item)
  } else {
    expr_pluck(.expr, index_path) <- sym("NULL_placeholder")
  }
  .expr
}

handle_unknown <- function(.expr, keyword) {
  message(paste0("Don't know how to handle keyword: ", keyword, ". Let it pass"))

  .expr
}

encode_id <- function(index_path, func_name) paste(c(func_name, index_path), collapse = "+")

expr_replace_keywords <- function(.expr, keyword, index_path, input_item) {
  switch_keywords(
    keyword,
    var = handle_var(.expr, index_path, input_item),
    num = handle_num(.expr, index_path, input_item),
    text = handle_text(.expr, index_path, input_item),
    expr = handle_expr(.expr, index_path, input_item),
    upload = handle_upload(.expr, index_path, input_item),
    handle_unknown(.expr, keyword)
  )
}

expr_remove_null <- function(.expr, target = sym("NULL_placeholder"),
                             current_path = numeric()) {
  for (i in (length(.expr):1)) {
    new_path <- c(current_path, i)
    if (is.call(.expr[[i]])) {
      .expr[[i]] <- expr_remove_null(.expr[[i]], target, new_path)
    } else if (is.symbol(.expr[[i]])) {
      if (.expr[[i]] == target) {
        .expr[[i]] <- NULL
      }
    }
  }
  return(.expr)
}

expr_remove_emptycall <- function(.expr, current_path = numeric()) {
  for (i in (length(.expr):1)) {
    if (is.call(.expr[[i]])) {
      if (length(.expr[[i]]) == 1) {
        message(paste0("The function ", as_string(.expr[[i]][[1]]),
                       "() in ", as_string(.expr[[1]]),
                       "() is removed."))
        .expr[[i]] <- NULL
      }
    }
  }
  return(.expr)
}


name_is_null <- function(.n) is.null(names(.n))

get_expr_param <- function(.expr, .path) {
  if (length(.path) > 1) {
    current_index <- .path[1]
    if (name_is_null(.expr[[current_index]]) &
        expr_type(.expr[[current_index]]) == 'call') {
      get_expr_param(.expr[[current_index]], .path[-1])
    } else if (!name_is_null(.expr[[current_index]])) {
      names(.expr[[current_index]])[.path[2]]
    }
  } else {
    if (!name_is_null(.expr)) {
      names(.expr)[.path]
    } else {
      return(list(NULL))
    }
  }
}
