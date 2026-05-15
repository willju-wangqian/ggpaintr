# Register the custom placeholders the gallery references (range, colvars).
# palette and color are registered in their respective sections inline.
ptr_clear_placeholder()

ptr_define_placeholder_value(
  keyword = "range",
  build_ui = function(node, label = NULL, ...) {
    shiny::sliderInput(node$id, label = label %||% "Range",
                       min = -100, max = 100, value = c(0, 50), step = 0.1)
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Range for {param}")
)

ptr_define_placeholder_consumer(
  keyword = "colvars",
  build_ui = function(node, cols = character(), label = NULL,
                      selected = character(0), ...) {
    shiny::selectInput(node$id, label = label %||% "Columns",
                       choices = cols, selected = intersect(selected, cols),
                       multiple = TRUE)
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))
  },
  copy_defaults = list(label = "Columns for {param}")
)
