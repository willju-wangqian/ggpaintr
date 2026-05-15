custom_text <- ptr_ui_text(list(
  shell = list(
    title       = list(label = "Iris explorer"),
    draw_button = list(label = "Render")
  ),
  params = list(
    x     = list(var  = list(label = "X variable")),
    title = list(text = list(label = "Plot heading"))
  )
))
ptr_app(
  "ggplot(iris, aes(x = var, y = var, color = var)) + geom_point() + labs(title = text)",
  ui_text = custom_text
)
