source("dev/audit/runs/_gallery_helpers.R", local = TRUE)
library(dplyr); library(broom)

ptr_define_placeholder_value(
  keyword       = "palette",
  build_ui      = function(node, label = NULL, ...) {
    shiny::selectInput(node$id, label = label %||% "ColorBrewer palette",
      choices = c("Dark2","Set1","Set2","Set3","Paired","Accent"),
      selected = "Dark2")
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value) || identical(value, "")) return(NULL)
    as.character(value)
  },
  copy_defaults = list(label = "Palette for {param}")
)

do_pca <- function(d, cols) {
  broom::augment(prcomp(d[, cols], scale. = TRUE), d)
}

ptr_app(
  'iris |>
     do_pca(colvars) |>
     ggplot(aes(.fittedPC1, .fittedPC2, color = Species)) +
       stat_ellipse(level = num(shared = "n"), linewidth = num) +
       geom_point(alpha = num, size = num) +
       scale_color_brewer(palette = palette) +
       labs(title = paste0("Iris in PC space, with ",
                           as.character(100 * num(shared = "n")),
                           "% ellipses"),
            x = text, y = text) +
       theme_minimal() +
       theme(legend.position = "bottom")'
)
