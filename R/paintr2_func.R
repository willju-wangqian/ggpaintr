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
  if (is_call(x)) return(as_string(x[[1]]))
  if (is_symbol(x)) return(as_string(x))
}

# modify_with_index_path <- function(.x, index_path, new_value) {
#   access_expr <- ".x"
#   for (index in unlist(index_path)) {
#     access_expr <- paste(access_expr, "[[", index, "]]", sep="")
#   }
#   tmp_expr <- parse_expr(paste(access_expr, "<- enexpr(new_value)"))
#
#   tryCatch({
#     eval(tmp_expr)
#   }, error = function(e) {
#     cat("Error message: ", e$message, "\n", "Modification failed.\n")
#   })
#
#   return(.x)
# }

expr_pluck <- function(.x, index_path) {
  .x <- tryCatch({
    .x[[index_path]]
  }, error = function(e) {
    NULL
  })
  return(.x)
}

`expr_pluck<-` <- function(.x, index_path, value) {
  tryCatch({
    .x[[index_path]] <- value
  }, error = function(e) {
    cat(
      paste0("Error in ", deparse(e$call), ": ",
             e$message, "\n", "Modification failed.\n")
    )
  })

  return(.x)
}

get_index_path <- function(x,
                           target = c("var", "text", "num", "expr", "upload"),
                           current_path = numeric(),
                           result = list()) {

  # if (is.symbol(x) && as_string(x) %in% target)

  for (i in seq_along(x)) {
    new_path <- c(current_path, i)
    if (is.call(x[[i]])) {
      result <- get_index_path(x[[i]], target, new_path, result)
    } else if (is.symbol(x[[i]])) {
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
        x[i] <- paste0(x[i], "-", counting_list[[x[i]]])

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
  switch(detect_keywords(.x),
         ...
  )
}

paintr_upload_name_id <- function(id) {
  paste0(id, "+name")
}

paintr_upload_default_name <- function(file_name) {
  file_stem <- tools::file_path_sans_ext(basename(file_name))
  file_stem <- gsub("[^[:alnum:]_]+", "_", file_stem)
  file_stem <- gsub("^_+|_+$", "", file_stem)
  if (identical(file_stem, "")) {
    file_stem <- "uploaded_data"
  }

  make.names(file_stem)
}

paintr_read_uploaded_data <- function(file_info) {
  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  if (ext == "csv") {
    return(read.csv(file_info$datapath))
  }
  if (ext == "rds") {
    return(readRDS(file_info$datapath))
  }

  stop("Please upload a .csv or .rds file.", call. = FALSE)
}

paintr_resolve_upload_info <- function(input, upload_id, strict = FALSE) {
  file_info <- input[[upload_id]]

  if (is.null(file_info) || is.null(file_info$datapath) || is.null(file_info$name)) {
    if (strict) {
      stop(
        paste0("Upload required for input '", upload_id, "'."),
        call. = FALSE
      )
    }
    return(NULL)
  }

  data_obj <- paintr_read_uploaded_data(file_info)
  object_name <- input[[paintr_upload_name_id(upload_id)]]
  object_name <- trimws(if (is.null(object_name)) "" else object_name)
  if (identical(object_name, "")) {
    object_name <- paintr_upload_default_name(file_info$name)
  } else {
    object_name <- gsub("[[:space:]]+", "_", object_name)
    object_name <- make.names(object_name)
  }

  ext <- tolower(tools::file_ext(file_info$name))
  read_fun <- switch(
    ext,
    csv = "read.csv",
    rds = "readRDS",
    stop("Please upload a .csv or .rds file.", call. = FALSE)
  )

  list(
    data = data_obj,
    object_name = object_name,
    file_name = file_info$name,
    code_text = paste0(object_name, " <- ", read_fun, "(\"", file_info$name, "\")")
  )
}

paintr_get_uploaded_data <- function(input, upload_id) {
  upload_info <- paintr_resolve_upload_info(input, upload_id, strict = FALSE)

  if (is.null(upload_info)) {
    return(NULL)
  }

  upload_info$data
}

paintr_prepare_eval_env <- function(paintr_obj, input, envir = parent.frame()) {
  eval_env <- rlang::env_clone(envir)
  upload_ids <- character()

  for (expr_name in names(paintr_obj[['keywords_list']])) {
    keyword_list <- paintr_obj[['keywords_list']][[expr_name]]
    upload_matches <- sapply(keyword_list, detect_keywords) == "upload"
    upload_ids <- c(upload_ids, names(keyword_list)[upload_matches])
  }

  for (upload_id in upload_ids) {
    upload_info <- paintr_resolve_upload_info(input, upload_id, strict = FALSE)
    if (is.null(upload_info)) {
      next
    }

    assign(upload_info$object_name, upload_info$data, envir = eval_env)
  }

  eval_env
}


handle_var <- function(.expr, index_path, input_item) {
  if (is.null(input_item)) {
    expr_pluck(.expr, index_path) <- sym("_NULL_PLACEHOLDER")
  } else {
    assert_that(is.character(input_item))
    # expr_pluck(.expr, index_path) <- expr(.data[[!!input_item]]) # this won't work with calculations in the expr

    # check white space
    expr_pluck(.expr, index_path) <- parse_expr(input_item)
  }
  .expr
}

handle_num <- function(.expr, index_path, input_item) {
  if (is.na(input_item) | is.null(input_item)) {
    expr_pluck(.expr, index_path) <- sym("_NULL_PLACEHOLDER")
  } else {
    assert_that(is.numeric(input_item))
    expr_pluck(.expr, index_path) <- expr(!!input_item)
  }
  .expr
}

handle_text <- function(.expr, index_path, input_item) {
  if (is.null(input_item) | input_item == "") {
    expr_pluck(.expr, index_path) <- sym("_NULL_PLACEHOLDER")
  } else {
    assert_that(is.character(input_item))
    expr_pluck(.expr, index_path) <- expr(!!input_item)
    # browser()
  }
  .expr
}

handle_expr <- function(.expr, index_path, input_item) {
  if ((!is.null(input_item)) & (input_item != "")) {
    expr_pluck(.expr, index_path) <- parse_expr(input_item)
  } else {
    expr_pluck(.expr, index_path) <- sym("_NULL_PLACEHOLDER")
  }
  .expr
}

handle_upload <- function(.expr, index_path, input_item) {
  if ((!is.null(input_item)) & (input_item != "")) {
    expr_pluck(.expr, index_path) <- parse_expr(input_item)
  } else {
    expr_pluck(.expr, index_path) <- sym("_NULL_PLACEHOLDER")
  }
  .expr
}

get_parameter_name_by_index_path <- function(.expr, index_path) {
  if (length(index_path) < 1) return(NULL)

  if (length(index_path) == 1) {
    names_vec <- names(.expr)
  } else {
    name_path <- index_path[1:(length(index_path) - 1)]
    names_vec <- names(.expr[[name_path]])
  }

  name_index <- index_path[length(index_path)]
  return( names_vec[name_index] )
}

handle_unknown <- function(.expr, keyword) {

  keyword <- as_string(keyword)

  keyword_path <- get_path(.expr, target = keyword)

  if (length(keyword_path) > 1) {
    message(paste0("Found multiple items for keyword: ", keyword,
                   ". Let it pass."))
    return(.expr)
  }

  keyword_path <- keyword_path[[1]]
  keyword_name <- get_parameter_name_by_index_path(.expr, keyword_path)

  if (keyword_name != "data") {
    message(paste0("Don't know how to handle keyword: ", keyword, ". Let it pass."))
  } else {
    message(paste0("The dataset ", keyword, " has been provided."))
  }

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

enexpr_replace_keywords <- function(.expr, keyword, index_path, input_item) {
  switch_keywords(
    keyword,
    var = handle_server_var(.expr, index_path, enexpr(input_item)),
    num = handle_server_default(.expr, index_path, enexpr(input_item)),
    text = handle_server_default(.expr, index_path, enexpr(input_item)),
    expr = handle_server_expr(.expr, index_path, enexpr(input_item)),
    upload = handle_server_default(.expr, index_path, enexpr(input_item)),
    handle_unknown(.expr, keyword)
  )
}

handle_server_default <- function(.expr, index_path, input_item) {
  expr_pluck(.expr, index_path) <- input_item

  .expr
}

handle_server_var <- function(.expr, index_path, input_item) {
  expr_pluck(.expr, index_path) <- expr(.data[[!!input_item]])

  .expr
}

handle_server_expr <- function(.expr, index_path, input_item) {
  expr_pluck(.expr, index_path) <- parse_expr(
    paste0("!!",
           expr_text(
             expr(parse_expr(!!input_item))
           ))
  )

  .expr
}


expr_remove_null <- function(.expr, target = sym("_NULL_PLACEHOLDER"),
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
  browser()
  return(.expr)
}

expr_remove_emptycall2 <- function(.expr) {
  for (i in (length(.expr):1)) {
    if (is.call(.expr[[i]])) {
      if (length(.expr[[i]]) == 1) {

        func_meaning <- tryCatch({
          eval(.expr[[i]])
        }, error = function(e) {
          NULL
        })

        if (is.null(func_meaning) || (!("gg" %in% attr(func_meaning, "class")))) {
          message(paste0("The function ", as_string(.expr[[i]][[1]]),
                         "() in ", as_string(.expr[[1]]),
                         "() is removed."))
          .expr[[i]] <- NULL
        }

      } else {
        .expr[[i]] <- expr_remove_emptycall2(.expr[[i]])
      }
    }
  }

  if (is.call(.expr) && (length(.expr) == 1)) {

    func_meaning <- tryCatch({
      eval(.expr)
    }, error = function(e) {
      NULL
    })

    if (is.null(func_meaning) || (!("gg" %in% attr(func_meaning, "class")))) {
      message(paste0("The function ", as_string(.expr[[1]]),
                     "() is removed."))
      .expr <- NULL
    }

  }
  return(.expr)
}

key_layer_functions <- function() {
  "geom_"
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

ui_insert_checkbox <- function(ui, nn) {

  if (nn == 'ggplot') return(ui)

  id <- paste0(nn, "+checkbox")
  .expr <- expr(checkboxInput(!!id,
                              label = paste("Keep the layer of", !!nn),
                              value = TRUE))
  checkbox <- eval(.expr)
  attr(checkbox, "ui_expr") <- .expr

  # checkbox <- checkboxInput(paste0(nn, "+checkbox"),
  #                           label = paste("Keep the layer of", nn),
  #                           value = TRUE)
  ui <- c(list(checkbox), ui)
  names(ui)[1] <- paste0(nn, "+checkbox")
  return(ui)
}

paintr_formula <- function(formula) {
  # browser()

  paintr_expr <- parse_expr(formula)
  paintr_expr_list <- unlist(break_sum(paintr_expr))
  paintr_expr_names <- sapply(paintr_expr_list, get_fun_names)
  paintr_expr_names <- handle_duplicate_names(paintr_expr_names)
  paintr_expr_list <- set_names(paintr_expr_list, paintr_expr_names)

  index_path_list <- lapply(paintr_expr_list, get_index_path)
  id_list <- lapply(names(index_path_list), function(.nn) {
    lapply(index_path_list[[.nn]], encode_id, .nn)
  })
  index_path_list <- purrr::map2(index_path_list, id_list, set_names)

  keywords_list <- purrr::map2(
    index_path_list, paintr_expr_list, function(.path, .expr) {
      lapply(.path, function(.x, .exprr) expr_pluck(.exprr, .x),
             .exprr = .expr)
    })

  paintr_expr_param_list <- purrr::map2(
    paintr_expr_list, index_path_list, function(.expr, .path_list) {
      lapply(.path_list, function(.path) {
        get_expr_param(.expr, .path)
      })
    }
  )

  paintr_ui_list <- purrr::pmap(
    list(keywords_list, id_list, paintr_expr_param_list),
    function(k_l, id_l, p_l) purrr::pmap(list(k_l, id_l, p_l), generate_ui_individual)
  )

  paintr_ui_list <- purrr::map2(
    paintr_ui_list, names(paintr_ui_list), ui_insert_checkbox
  )

  result <- list(
    formula_text = formula,
    param_list = paintr_expr_param_list,
    keywords_list = keywords_list,
    index_path_list = index_path_list,
    id_list = id_list,
    expr_list = paintr_expr_list,
    ui_list = paintr_ui_list
  )

  attr(result, "class") <- "paintr_obj"

  return(result)
}

paintr_get_tab_ui <- function(paintr_obj) {
  # check if it's paintr_obj
  # assert_that(class(paintr_obj) == 'paintr_obj')
  if (class(paintr_obj) != 'paintr_obj') return(NULL)

  ui_list <- paintr_obj[['ui_list']]

  return(tab_wrap_ui(ui_list))
}

tab_wrap_ui <- function(ui_list) {
  assert_that(!is.null(names(ui_list)))

  tab_list <- unname(purrr::map2(
    ui_list, names(ui_list),
    function(ui, .nn) do.call(tabPanel, c(.nn, unname(ui)))
  ))
  tab_ui <- do.call(tabsetPanel, tab_list)

  return(tab_ui)
}

expr_text_tab_ui <- function(ui_list) {
  assert_that(!is.null(names(ui_list)))

  tab_expr_list <- unname(purrr::map2(
    ui_list, names(ui_list),
    function(.l, .n) {
      expr_list <- unname(lapply(.l, attr, "ui_expr"))
      expr_list <- check_remove_null(expr_list)
      call2("tabPanel", .n, !!!expr_list)
    }))
  tab_expr <- call2("tabsetPanel", !!!tab_expr_list)

  return(expr_text(tab_expr))

}

expr_apply_checkbox_result <- function(expr, nn, input) {
  if (nn == 'ggplot') return(expr)

  checkbox_id <- paste0(nn, "+checkbox")
  if (input[[checkbox_id]]) {
    return(expr)
  } else {
    return(NULL)
  }
}

paintr_complete_expr <- function(paintr_obj, input, envir = parent.frame()) {
  assert_that(class(paintr_obj) == 'paintr_obj')

  paintr_processed_expr_list <- paintr_obj[['expr_list']]
  unfolded_id_list <- unlist(paintr_obj[['id_list']])
  keywords_list <- paintr_obj[['keywords_list']]
  index_path_list <- paintr_obj[['index_path_list']]
  eval_env <- paintr_prepare_eval_env(paintr_obj, input, envir = envir)

  for (id in unfolded_id_list) {
    input_item <- input[[id]]
    if (detect_keywords(keywords_list[[unlist(strsplit(id, "\\+"))[1]]][[id]]) == "upload") {
      upload_info <- paintr_resolve_upload_info(input, id, strict = FALSE)
      input_item <- if (is.null(upload_info)) "" else upload_info$object_name
    }
    id_domain <- unlist(strsplit(id, "\\+"))[1]
    paintr_processed_expr_list[[id_domain]] <-
      expr_replace_keywords(paintr_processed_expr_list[[id_domain]],
                            keywords_list[[id_domain]][[id]],
                            index_path_list[[id_domain]][[id]],
                            input_item)
  }

  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_null)
  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_emptycall2)
  paintr_processed_expr_list <- purrr::map2(paintr_processed_expr_list,
                                            names(paintr_processed_expr_list),
                                            expr_apply_checkbox_result,
                                            input)
  paintr_processed_expr_list <- check_remove_null(paintr_processed_expr_list)

  code_text_list <- lapply(paintr_processed_expr_list, expr_text)
  code_text <- do.call(paste, c(unname(code_text_list), sep = ' +\n  '))

  return(list(
    complete_expr_list = paintr_processed_expr_list,
    code_text = code_text,
    eval_env = eval_env
  ))

}

paintr_get_plot <- function(plot_expr_list, envir = parent.frame()) {

  plot_list <- lapply(plot_expr_list, eval, envir = envir)

  p <- plot_list[[1]]

  for (i in 2:length(plot_list)) p <- p + plot_list[[i]]

  return(p)

}

paintr_format_runtime_message <- function(stage, condition = NULL, message = NULL) {
  stage_label <- switch(
    stage,
    complete = "Input error",
    plot = "Plot error",
    "Runtime error"
  )

  detail <- message
  if (is.null(detail) || identical(trimws(detail), "")) {
    detail <- if (is.null(condition)) NULL else conditionMessage(condition)
  }

  if (is.null(detail) || identical(trimws(detail), "")) {
    return(stage_label)
  }

  paste0(stage_label, ": ", detail)
}

paintr_complete_expr_safe <- function(paintr_obj, input, envir = parent.frame()) {
  tryCatch(
    {
      complete_result <- paintr_complete_expr(paintr_obj, input, envir = envir)
      list(
        ok = TRUE,
        stage = "complete",
        message = NULL,
        code_text = complete_result$code_text,
        complete_expr_list = complete_result$complete_expr_list,
        eval_env = complete_result$eval_env,
        condition = NULL,
        plot = NULL
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        stage = "complete",
        message = paintr_format_runtime_message("complete", e),
        code_text = NULL,
        complete_expr_list = NULL,
        eval_env = NULL,
        condition = e,
        plot = NULL
      )
    }
  )
}

paintr_get_plot_safe <- function(runtime_result, envir = parent.frame()) {
  if (!isTRUE(runtime_result$ok)) {
    return(runtime_result)
  }

  plot_env <- runtime_result$eval_env
  if (is.null(plot_env)) {
    plot_env <- envir
  }

  tryCatch(
    {
      runtime_result$plot <- paintr_get_plot(
        runtime_result$complete_expr_list,
        envir = plot_env
      )
      runtime_result
    },
    error = function(e) {
      runtime_result$ok <- FALSE
      runtime_result$stage <- "plot"
      runtime_result$message <- paintr_format_runtime_message("plot", e)
      runtime_result$condition <- e
      runtime_result$plot <- NULL
      runtime_result
    }
  )
}

paintr_build_runtime <- function(paintr_obj, input, envir = parent.frame()) {
  runtime_result <- paintr_complete_expr_safe(paintr_obj, input, envir = envir)
  paintr_get_plot_safe(runtime_result, envir = envir)
}

paintr_error_ui <- function(message) {
  if (is.null(message) || identical(trimws(message), "")) {
    return(NULL)
  }

  tags$div(
    style = paste(
      "margin-top: 12px;",
      "margin-bottom: 12px;",
      "padding: 12px;",
      "border: 1px solid #c62828;",
      "border-radius: 4px;",
      "background-color: #fff3f3;",
      "color: #7f1d1d;"
    ),
    tags$strong("Error"),
    tags$div(
      style = "white-space: pre-wrap; margin-top: 6px;",
      message
    )
  )
}

expr_text_server <- function(paintr_obj, input) {
  assert_that(class(paintr_obj) == 'paintr_obj')

  paintr_processed_expr_list <- paintr_obj[['expr_list']]
  unfolded_id_list <- unlist(paintr_obj[['id_list']])
  keywords_list <- paintr_obj[['keywords_list']]
  index_path_list <- paintr_obj[['index_path_list']]

  for (id in unfolded_id_list) {

    id_domain <- unlist(strsplit(id, "\\+"))[1]
    paintr_processed_expr_list[[id_domain]] <-
      enexpr_replace_keywords(paintr_processed_expr_list[[id_domain]],
                              keywords_list[[id_domain]][[id]],
                              index_path_list[[id_domain]][[id]],
                              input[[!!id]])
  }

  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_null)
  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_emptycall2)

  code_text_list <- lapply(paintr_processed_expr_list, expr_text)
  code_text <- do.call(paste, c(unname(code_text_list), sep = ' +\n  '))

  return(code_text)

}

check_remove_null <- function(x) {
  if(is.null(x)) return(NULL)

  x <- x[!sapply(x, is.null )]

  if(length(x) == 0) return(NULL)

  x
}

get_shiny_template <- function() {
  shiny_template <- c(
    "source(here('R/paintr2_func.R'))",
    "source(here('R/ui_function.R'))",
    "library(shiny)",
    "library(shinyWidgets)",
    "",
    "input_formula <- $formula_text$",
    "",
    "ui <- fluidPage(",
    "",
    "  # Application title",
    "  titlePanel(\"ggpaintr demo\"),",
    "",
    "  # Sidebar with a slider input for number of bins",
    "  sidebarLayout(",
    "    sidebarPanel(",
    "      uiOutput(\"controlPanel\"),",
    "      actionButton(\"draw\", \"click to draw the plot\"),",
    "    ),",
    "",
    "    # Show a plot of the generated distribution",
    "    mainPanel(",
    "      plotOutput(\"outputPlot\"),",
    "      uiOutput(\"outputError\"),",
    "      verbatimTextOutput(\"outputCode\")",
    "    )",
    "  )",
    ")",
    "",
    "server <- function(input, output, session) {",
    "",
    "  session$userData$paintr <- reactiveValues(obj = list(NULL))",
    "  session$userData$paintr$obj <- ggpaintr:::paintr_formula(input_formula)",
    "",
    "  observe({",
    "    req(session$userData$paintr$obj)",
    "    session$userData$paintr$var_ui_list <-",
    "      ggpaintr:::output_embed_var(input, output, session$userData$paintr$obj)",
    "  })",
    "",
    "  output$controlPanel <- renderUI({",
    "    req(session$userData$paintr$obj)",
    "    column(12, ggpaintr:::paintr_get_tab_ui(session$userData$paintr$obj))",
    "  })",
    "",
    "  observe({",
    "",
    "    req(session$userData$paintr$obj)",
    "    runtime_result <- ggpaintr:::paintr_build_runtime(",
    "      session$userData$paintr$obj,",
    "      input",
    "    )",
    "",
    "    output$outputPlot <- renderPlot({",
    "      if (!isTRUE(runtime_result[[\"ok\"]])) {",
    "        plot.new()",
    "        return(invisible(NULL))",
    "      }",
    "",
    "      runtime_result[[\"plot\"]]",
    "    })",
    "",
    "    output$outputError <- renderUI({",
    "      if (isTRUE(runtime_result[[\"ok\"]])) {",
    "        return(NULL)",
    "      }",
    "",
    "      ggpaintr:::paintr_error_ui(runtime_result[[\"message\"]])",
    "    })",
    "",
    "    output$outputCode <- renderText({",
    "      runtime_result[[\"code_text\"]]",
    "    })",
    "",
    "  }) %>% bindEvent(input$draw)",
    "}",
    "",
    "shinyApp(ui, server)"
  )

}

generate_shiny <- function(paintr_obj, var_ui, output_file,
                           style = TRUE) {
  shiny_text <- get_shiny_template()
  formula_text <- paintr_obj$formula_text
  formula_text <- gsub("\\\\", "\\\\\\\\", formula_text)
  formula_text <- gsub("\"", "\\\\\"", formula_text)
  formula_text <- paste0("\"", formula_text, "\"")
  shiny_text <- stringr::str_replace(
    shiny_text,
    "\\$formula_text\\$",
    function(...) formula_text
  )

  writeLines(shiny_text, output_file)
  if (style) {
    styler::style_file(output_file)
  }

}

expr_plot_eval <- function(.expr, envir = parent.frame()) {
  p_expr <- eval(.expr, envir = envir)
  p_list <- unlist(break_sum(p_expr))
  p_list_clean <- lapply(p_list, expr_remove_emptycall2)

  return(p_list_clean)
}

get_path <- function(x,
                     target = c("input"),
                     current_path = numeric(),
                     result = list()) {
  for (i in seq_along(x)) {
    new_path <- c(current_path, i)
    if (is.call(x[[i]])) {
      result <- get_path(x[[i]], target, new_path, result)
    } else if (is.symbol(x[[i]])) {
      if (as_string(x[[i]]) %in% target) {
        result <- c(result, list(new_path))
      }
    }
  }
  return(result)
}

get_id_path <- function(input_path) {
  lapply(input_path, function(.p) {
    .p[length(.p)] <- .p[length(.p)] + 1
    .p
  })
}

get_caller_path <- function(input_path) {
  lapply(input_path, function(.p) {
    if (length(.p) > 1 && .p[length(.p)-1] > 1) {
      .p <- .p[-length(.p)]
      .p[length(.p)] <- 1
      .p
    } else {
      stop(eval(call2(paste, "path:", !!!.p)))
    }

  })
}

is_empty_input <- function(x) {
  is.null(x) || is.na(x) || (x == "")
}

expr_remove_empty_input <- function(.expr,
                                    input,
                                    current_path = numeric()) {

  # all names with '+checkbox'
  all_checkboxes <- names(input)[which(
    stringr::str_detect(names(input), "\\+checkbox")
  )]
  checkbox_results <- sapply(all_checkboxes, function(.p) input[[.p]])
  # all layers with FALSE
  layers_to_be_removed <- all_checkboxes[which(!checkbox_results)]
  for (layer in rev(layers_to_be_removed)) {
    target_call <- str_remove(layer, "\\+checkbox") # remove '+checkbox'
    dup <- 1
    if(str_detect(target_call, "\\-")) { # if the layer name is duplicated
      tt <- str_split_1(target_call, "\\-")
      target_call <- tt[1]
      dup <- as.numeric(tt[2])
    }
    # get expr path for the target
    target_path <- get_path(.expr, target = target_call)[[dup]]
    call_path <- target_path[-length(target_path)]
    expr_pluck(.expr, call_path) <- NULL # set it to NULL
  }

  input_path <- get_path(.expr)
  id_path <- get_id_path(input_path)
  ids <- lapply(id_path, function(.p) expr_pluck(.expr, .p))
  caller_path <- get_caller_path(input_path)
  caller <- lapply(caller_path, function(.p) expr_pluck(.expr, .p))

  for (i in length(input_path):1) {
    if (as_string(caller[[i]]) == "[[") {
      if (is_empty_input(input[[ids[[i]]]])) {
        expr_pluck(.expr, caller_path[[i]][-length(caller_path[[i]])]) <- NULL
      }
    } else if (as_string(caller[[i]]) == "parse_expr") {
      if (is_empty_input(input[[ids[[i]]]])) {
        expr_pluck(.expr, caller_path[[i]]) <- NULL
        expr_pluck(.expr, caller_path[[i]]) <- NULL

        if (is.null(expr_pluck(.expr, caller_path[[i]]))) {
          expr_pluck(.expr, caller_path[[i]][-((length(caller_path[[i]])-2):length(caller_path[[i]]))]) <- NULL
        } else {
          expr_pluck(.expr,
                     caller_path[[i]][-((length(caller_path[[i]])-3):length(caller_path[[i]]))]) <-
            expr_pluck(.expr, caller_path[[i]][-length(caller_path[[i]])])
        }
      }
    } else if (is_empty_input(input[[ids[[i]]]])) {
      expr_pluck(.expr, input_path[[i]][-length(input_path[[i]])]) <- NULL
    } else {
      message("Invalid index path. Fail to remove empty inputs.")
    }
  }

  return(.expr)
}


foo_ui <- function() {
  'ui <- fluidPage(

  titlePanel("ggpaintr demo"),

  sidebarLayout(
    sidebarPanel(
      $text_ui$,
      actionButton("draw", "click to draw the plot"),
    ),

    mainPanel(
      plotOutput("outputPlot")
    )
  )
)'
}

foo <- function() {
  'library(shiny)
library(shinyWidgets)

# Please load your data first

ui <- fluidPage(

  # Application title
  titlePanel("ggpaintr demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      $text_ui$,
      actionButton("draw", "click to draw the plot"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("outputPlot")
    )
  )
)

server <- function(input, output) {


  observe({

    output$outputPlot <- renderPlot({
      $text_server$
    })

  }) %>% bindEvent(input$draw)
}

shinyApp(ui, server)'
}
