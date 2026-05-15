ptr_clear_placeholder()
ptr_define_placeholder_consumer(
  keyword       = "numrange",
  build_ui      = function(node, cols = character(), data = NULL,
                           label = NULL, ...) {
    num_cols <- cols[vapply(data[cols], is.numeric, logical(1))]
    col      <- num_cols[1] %||% ""
    rng      <- if (nzchar(col)) range(data[[col]], na.rm = TRUE) else c(0, 1)
    shiny::sliderInput(node$id, label = label %||% "Range",
                min = rng[1], max = rng[2], value = rng)
  },
  resolve_expr  = function(value, ...) {
    if (is.null(value)) return(NULL)
    rlang::call2("c", value[1], value[2])
  },
  copy_defaults = list(label = "Range for {param}")
)
ptr_app("
mtcars |>
  dplyr::filter(mpg >= numrange(shared = 'n')[1],
                mpg <= numrange(shared = 'n')[2]) |>
  ggplot(aes(x = var, y = var)) +
  geom_point()
")
