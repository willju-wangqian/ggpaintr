suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
  library(dplyr)
  library(shiny)
})

formula <- 'ggplot(
  data = mtcars |> filter(cyl == num) |> head(num),
  mapping = aes(x = var, y = var, color = var)
) +
  geom_point(
    data = iris |> filter(Species == text) |> head(num),
    mapping = aes(x = var, y = var, size = var)
  ) +
  geom_smooth(
    data = diamonds |> sample_n(num) |> filter(price > num),
    mapping = aes(x = var, y = var),
    method = text,
    se = bool
  ) +
  geom_text(
    data = mpg |> head(num),
    mapping = aes(x = var, y = var, label = var)
  ) +
  facet_wrap(facets = var, ncol = num) +
  labs(title = text, subtitle = text, x = text, y = text) +
  coord_cartesian(xlim = c(num, num), ylim = c(num, num)) +
  scale_color_brewer(palette = text) +
  theme_minimal()'

cat("=== parsing ===\n")
obj <- ptr_parse_formula(formula)
cat("Layers:", paste(names(obj$expr_list), collapse = ", "), "\n")
cat("Data-pipeline layers:", paste(names(obj$data_pipeline_info), collapse = ", "), "\n\n")

cat("=== launching app on port 4321 ===\n")
options(shiny.port = 4321, shiny.host = "127.0.0.1")
ptr_app(formula)
