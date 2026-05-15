library(palmerpenguins); library(dplyr); library(tidyr); library(purrr); library(broom)
ptr_app(
  'penguins |>
     tidyr::drop_na() |>
     group_by(species) |>
     tidyr::nest() |>
     mutate(
       fit  = map(data, function(.d) lm(expr, data = .d)),
       tidy = map(fit,  function(.f) broom::tidy(.f, conf.int = TRUE))
     ) |>
     unnest(tidy) |>
     filter(term == text) |>
     ggplot(aes(estimate, species, color = species)) +
       geom_vline(xintercept = 0, color = "gray60", linetype = 2) +
       geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                       size = num) +
       scale_color_brewer(palette = "Dark2") +
       labs(title = text, x = text, y = NULL) +
       theme_minimal() +
       theme(legend.position = "none")'
)
