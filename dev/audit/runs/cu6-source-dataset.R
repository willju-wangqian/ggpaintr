ptr_clear_placeholder()
ptr_define_placeholder_source(
  keyword       = "dataset",
  build_ui      = function(node, label = NULL, ...) {
    shiny::selectInput(node$id, label = label %||% "Built-in dataset",
                choices = c("iris", "mtcars", "diamonds", "economics"))
  },
  resolve_data  = function(value, node, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    switch(value,
           iris      = iris,
           mtcars    = mtcars,
           diamonds  = ggplot2::diamonds,
           economics = ggplot2::economics,
           NULL)
  },
  copy_defaults = list(label = "Built-in dataset for {param}")
)
ptr_app("ggplot(dataset, aes(var, var)) + geom_point()")
