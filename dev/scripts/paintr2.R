library(tidyverse)
library(rlang)
library(lobstr)
library(ggpaintr)
library(assertthat)

library(shiny)
library(shinyWidgets)

source("preconsideration/metaprogramming/paintr2_func.R")

data("mpg")

data <- mpg

input_formula <-
  "mpg %>% mutate(displ = displ + 1) %>% ggplot(aes(x = var + 1, y = var)) +
   geom_point() +
   labs(x = text, y = text, title = text)"

input_formula <-
  "ggplot(data, aes(x = var + 1, y = var)) +
   geom_point(data, aes(color = var), size = num, color = text) +
   geom_bar(position = select, stat = select) +
   labs(x = text, y = text, title = text, tags = text) +
   scale_x_discrete(breaks = expr, values = char) +
   coord_flip(switch, xlim = text, ylim = text)"

input_formula <-
  "ggplot(data, aes(x = var + 1, y = var)) +
   geom_point(data, aes(color = var), size = num, color = text) +
   labs(x = text, y = text, title = text, tags = text) +
   coord_flip(switch, xlim = text, ylim = text)"


#########################
input_formula <-
  "ggplot(mpg, foo(aes(x = log(var) + 1, y = var))) +
   geom_point(aes(color = var), size = num, color = text) +
   geom_point(color = text) +
   labs(x = text, y = text, title = text, tags = text, text)"

input_formula <-
  "ggplot(mpg, aes(x = var + 1, y = var)) +
   geom_point(aes(color = var), size = num, color = text) +
   geom_point(color = text) +
   labs(x = text, y = text, title = text, tags = text) +
   scale_x_continuous(breaks = expr)"

input_formula <-
  "ggplot(data = mpg, aes(x = var + 1, y = var)) +
   geom_point(aes(color = var), size = num, color = text) +
   geom_point(color = text) +
   labs(x = text, y = text, title = text, tags = text)"

paintr_expr <- parse_expr(input_formula)
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



# UI generated


input_list <- unlist(unname(keywords_list))
input_list[[2]] <- 'displ + 2'
input_list[[3]] <- 'cty'
input_list[4] <- list(NULL)
# input_list[3] <- 'drv'
input_list[[5]] <- NA
input_list[-c(2,3,4,5)] <- ""
input_list[[7]] <- "blue"
# input_list[[11]] <- "c(1,2,3)"


input_list

paintr_processed_expr_list <- paintr_expr_list
for (id in names(input_list)) {
  # browser()
  id_domain <- unlist(strsplit(id, "\\+"))[1]
  # check id domain
  paintr_processed_expr_list[[id_domain]] <-
    expr_replace_keywords(paintr_processed_expr_list[[id_domain]],
                          keywords_list[[id_domain]][[id]],
                          index_path_list[[id_domain]][[id]],
                          input_list[[id]])
}

paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_null)
paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_emptycall)

paintr_processed_expr_list

assign("abc", mpg)

paintr_plot_list <- lapply(paintr_processed_expr_list, eval)

p <- paintr_plot_list[[1]]
for (i in 2:length(paintr_plot_list)) p <- p + paintr_plot_list[[i]]
p

# do.call(ggplot2:::`+.gg`, paintr_plot_list[[1]])


ggplot(mpg, aes(x = .data[["displ"]] + 1, y = .data[["cty"]])) +
  geom_point(aes(color = .data[["drv"]])) +
  geom_point() +
  labs()


ccc <- c("b","QQ","a","A","bb")

for(ch in ccc) cat(ch,":", switch(EXPR = ch, a =, A = 1, b = 2:3, ch),"\n")






















