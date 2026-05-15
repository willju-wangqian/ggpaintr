library(dplyr)
roll_mean <- function(x, n) as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))
ptr_app(
  'economics |>
     mutate(roll = roll_mean(var(shared = "y"), num)) |>
     ggplot(aes(date, var(shared = "y"))) +
       geom_line(color = "gray70") +
       geom_line(aes(y = roll), color = "#0072B2", linewidth = num) +
       labs(title = text, x = text, y = text) +
       theme_minimal()'
)
