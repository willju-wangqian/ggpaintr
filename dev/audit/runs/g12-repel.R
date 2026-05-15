library(ggrepel)
mt <- data.frame(mtcars, model = rownames(mtcars))
ptr_app(
  "ggplot(data = mt, aes(x = var, y = var, label = var)) +
     geom_point(size = num, color = 'steelblue') +
     geom_text_repel(size = num,
                     max.overlaps = num,
                     box.padding = num) +
     labs(title = text)",
  envir = environment()
)
