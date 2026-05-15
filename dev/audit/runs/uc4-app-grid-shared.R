ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = var, y = Sepal.Length, fill = Species)) + geom_boxplot()",
    "ggplot(iris, aes(x = var, y = Sepal.Width, fill = Species)) + geom_violin()"
  ),
  shared_ui = list()
)
