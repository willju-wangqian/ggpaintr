library(ggdist)
ptr_app(
  "ggplot(data = mtcars, aes(x = factor(cyl), y = var)) +
     stat_halfeye(adjust = num,
                  justification = num,
                  .width = num,
                  point_colour = NA,
                  slab_alpha = num) +
     geom_boxplot(width = num, outlier.shape = NA) +
     coord_flip() +
     labs(title = text)"
)
