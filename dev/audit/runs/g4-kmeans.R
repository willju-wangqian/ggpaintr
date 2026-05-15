source("dev/audit/runs/_gallery_helpers.R", local = TRUE)
library(dplyr); library(tidyr); library(purrr); library(broom)
ptr_app(
  'tibble(k = expr(shared = "k")) |>
     mutate(
       kmeans  = map(k, function(.k) kmeans(
         scale(iris[, colvars]),
         centers = .k, nstart = 25)),
       glanced = map(kmeans, broom::glance)
     ) |>
     unnest(glanced) |>
     ggplot(aes(k, tot.withinss)) +
       geom_line(color = "gray50") +
       geom_point(size = num, color = "#0072B2") +
       scale_x_continuous(breaks = expr(shared = "k")) +
       labs(title = text, x = text, y = text) +
       theme_minimal()'
)
