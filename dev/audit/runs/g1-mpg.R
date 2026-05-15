source("dev/audit/runs/_gallery_helpers.R", local = TRUE)
ptr_app(
  "ggplot(data = mpg, aes(x = var, y = var, color = var)) +
     geom_point(alpha = num, size = num) +
     geom_smooth(method = 'loess', se = FALSE, span = num) +
     facet_wrap(~ var) +
     scale_color_brewer(palette = 'Set2') +
     labs(title = text, x = text, y = text) +
     coord_cartesian(xlim = range, ylim = range)"
)
