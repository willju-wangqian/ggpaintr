library(broom)
ptr_app(
  'broom::augment(lm(expr, data = mtcars)) |>
     ggplot(aes(x = var, y = var)) +
       geom_hline(yintercept = 0, color = "gray60", linetype = 2) +
       geom_point(alpha = num, size = num, color = "#D55E00") +
       geom_smooth(method = "loess", se = FALSE, span = num) +
       labs(title = text, x = text, y = text) +
       theme_minimal()'
)
