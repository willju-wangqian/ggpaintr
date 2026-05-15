ptr_clear_placeholder()
ptr_define_placeholder_consumer(
  keyword       = "colvars",
  build_ui      = function(node, cols = character(), label = NULL,
                           selected = character(0), ...) {
    shiny::selectInput(node$id, label = label %||% "Columns",
                choices = cols, selected = intersect(selected, cols),
                multiple = TRUE)
  },
  resolve_expr  = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))
  },
  copy_defaults = list(label = "Columns for {param}")
)
ptr_app("mtcars |> dplyr::select(colvars) |>
        ggplot(aes(x = var, y = var)) + geom_point()")
