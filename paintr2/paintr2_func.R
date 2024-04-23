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
    # browser()
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

enexpr_replace_keywords <- function(.expr, keyword, index_path, input_item) {
  switch_keywords(
    keyword,
    var = handle_server_expr_var(.expr, index_path, enexpr(input_item)),
    num = handle_server_expr(.expr, index_path, enexpr(input_item)),
    text = handle_server_expr(.expr, index_path, enexpr(input_item)),
    expr = handle_server_expr(.expr, index_path, enexpr(input_item)),
    upload = handle_server_expr(.expr, index_path, enexpr(input_item)),
    handle_unknown(.expr, keyword)
  )
}

handle_server_expr <- function(.expr, index_path, input_item) {
  expr_pluck(.expr, index_path) <- input_item

  .expr
}

handle_server_expr_var <- function(.expr, index_path, input_item) {
  expr_pluck(.expr, index_path) <- expr(.data[[!!input_item]])

  .expr
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

paintr_formula <- function(formula) {
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
    function(k_l, id_l, p_l) {
      purrr::pmap(list(k_l, id_l, p_l), generate_ui_individual)
    }
  )

  result <- list(
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

  # tab_list <- unname(purrr::map2(ui_list, names(ui_list),
  #                                function(ui, nn) do.call(tabPanel, c(nn, unname(ui)))))
  # tab_ui <- do.call(tabsetPanel, tab_list)
  #
  # return(tab_ui)
}

tab_wrap_ui <- function(ui_list) {
  assert_that(!is.null(names(ui_list)))

  tab_list <- unname(purrr::map2(ui_list, names(ui_list),
                                 function(ui, nn) do.call(tabPanel, c(nn, unname(ui)))))
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

paintr_complete_expr <- function(paintr_obj, input) {
  assert_that(class(paintr_obj) == 'paintr_obj')

  paintr_processed_expr_list <- paintr_obj[['expr_list']]
  unfolded_id_list <- unlist(paintr_obj[['id_list']])
  keywords_list <- paintr_obj[['keywords_list']]
  index_path_list <- paintr_obj[['index_path_list']]

  for (id in unfolded_id_list) {

    id_domain <- unlist(strsplit(id, "\\+"))[1]
    paintr_processed_expr_list[[id_domain]] <-
      expr_replace_keywords(paintr_processed_expr_list[[id_domain]],
                            keywords_list[[id_domain]][[id]],
                            index_path_list[[id_domain]][[id]],
                            input[[id]])
  }

  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_null)
  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_emptycall)

  code_text_list <- lapply(paintr_processed_expr_list, expr_text)
  code_text <- do.call(paste, c(unname(code_text_list), sep = ' +\n  '))

  # paintr_obj[['complete_expr_list']] <- paintr_processed_expr_list
  # paintr_obj[['code_text']] <- code_text
  #
  # return(paintr_obj)

  return(list(
    complete_expr_list = paintr_processed_expr_list,
    code_text = code_text
  ))

}

paintr_get_plot <- function(plot_expr_list) {

  plot_list <- lapply(plot_expr_list, eval)

  p <- plot_list[[1]]

  for (i in 2:length(plot_list)) p <- p + plot_list[[i]]

  return(p)

}

expr_text_server <- function(paintr_obj, input) {
  assert_that(class(paintr_obj) == 'paintr_obj')

  paintr_processed_expr_list <- paintr_obj[['expr_list']]
  unfolded_id_list <- unlist(paintr_obj[['id_list']])
  keywords_list <- paintr_obj[['keywords_list']]
  index_path_list <- paintr_obj[['index_path_list']]

  for (id in unfolded_id_list) {

    # browser()

    id_domain <- unlist(strsplit(id, "\\+"))[1]
    paintr_processed_expr_list[[id_domain]] <-
      enexpr_replace_keywords(paintr_processed_expr_list[[id_domain]],
                              keywords_list[[id_domain]][[id]],
                              index_path_list[[id_domain]][[id]],
                              input[[!!id]])
  }

  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_null)
  paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_emptycall)

  code_text_list <- lapply(paintr_processed_expr_list, expr_text)
  code_text <- do.call(paste, c(unname(code_text_list), sep = ' +\n  '))

  return(code_text)

  # return(list(
  #   complete_expr_list = paintr_processed_expr_list,
  #   code_text = code_text
  # ))

}

check_remove_null <- function(x) {
  if(is.null(x)) return(NULL)

  x <- x[!sapply(x, is.null )]

  if(length(x) == 0) return(NULL)

  x
}

get_shiny_template <- function() {
  shiny_template <- c(
    "library(shiny)",
    "library(shinyWidgets)",
    "",
    "# Please load your data first",
    "",
    "ui <- fluidPage(",
    "",
    "  # Application title",
    "  titlePanel(\"ggpaintr demo\"),",
    "",
    "  # Sidebar with a slider input for number of bins",
    "  sidebarLayout(",
    "    sidebarPanel(",
    "      $text_ui$,",
    "      actionButton(\"draw\", \"click to draw the plot\"),",
    "    ),",
    "",
    "    # Show a plot of the generated distribution",
    "    mainPanel(",
    "      plotOutput(\"outputPlot\")",
    "    )",
    "  )",
    ")",
    "",
    "server <- function(input, output) {",
    "",
    "",
    "  observe({",
    "",
    "    p <- $text_server$",
    "",
    "    output$outputPlot <- renderPlot({",
    "      p",
    "    })",
    "",
    "  }) %>% bindEvent(input$draw)",
    "}",
    "",
    "shinyApp(ui, server)"
  )

}

generate_shiny <- function(patinr_obj, var_ui, output_file,
                           style = TRUE) {

  ui_list <- patinr_obj$ui_list
  updated_ui_list <- var_ui_replacement(ui_list, var_ui)

  text_ui <- expr_text_tab_ui(updated_ui_list)
  text_server <- expr_text_server(patinr_obj)

  shiny_text <- get_shiny_template()
  shiny_text <- stringr::str_replace(
    shiny_text,
    "\\$text_ui\\$",
    text_ui
  )
  shiny_text <- stringr::str_replace(
    shiny_text,
    "\\$text_server\\$",
    text_server
  )

  writeLines(shiny_text, output_file)
  if (style) {
    styler::style_file(output_file)
  }

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




