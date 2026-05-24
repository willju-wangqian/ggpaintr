# Doc-only reference (ADR 0013 D3). Not parsed by tests.
# All-defaults equivalent of the ggpaintr super-app-2b fixture: the same
# ggplot expression rendered statically, with every placeholder replaced by
# its default literal. Useful for `diff app.R reference.R` reviews.
library(ggplot2)

ggplot(iris,
       aes(x = Sepal.Length,
           y = Sepal.Width,
           color = Species)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", linewidth = 1, alpha = 0.3) +
  geom_rug(data = mtcars, aes(x = mpg, y = wt)) +
  facet_wrap(vars(Species)) +
  labs(title = "ppSample + splice + layer-upload")
