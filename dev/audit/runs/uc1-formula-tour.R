ptr_app("
ggplot(iris, aes(x = var, y = var, color = var)) +
  geom_point(size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
