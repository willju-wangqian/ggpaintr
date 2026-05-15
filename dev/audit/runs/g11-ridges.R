library(ggridges)
ptr_app(
  "ggplot(data = lincoln_weather, aes(x = var, y = var, fill = ..x..)) +
     geom_density_ridges_gradient(scale = num,
                                  rel_min_height = num,
                                  bandwidth = num) +
     scale_fill_viridis_c(option = 'C') +
     labs(title = text)"
)
