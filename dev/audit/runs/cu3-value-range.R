ptr_clear_placeholder()
ptr_define_placeholder_value(
  keyword       = "range",
  build_ui      = function(node, label = NULL, ...) {
    shiny::sliderInput(node$id, label = label %||% "Range",
                min = 0, max = 100, value = c(0, 100), step = 0.1)
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Range for {param}")
)
ptr_app("ggplot(mtcars, aes(mpg, hp)) + geom_point() + xlim(range)")
