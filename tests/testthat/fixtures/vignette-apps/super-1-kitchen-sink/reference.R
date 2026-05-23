# Doc-only reference (ADR 0013 D3). Not parsed by tests.
# The all-defaults equivalent of super-1-kitchen-sink/app.R with every
# placeholder replaced by its seeded default.
ggplot(
  mtcars |>
    dplyr::filter(hp >= 75) |>
    dplyr::mutate(adj = mpg / wt),
  aes(x = mpg, y = adj, color = cyl)
) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", linewidth = 1) +
  geom_line(linewidth = 1) +
  facet_wrap(vars(cyl)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(title = "Title", subtitle = NULL)
