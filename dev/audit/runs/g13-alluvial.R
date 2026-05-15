library(ggalluvial)
ptr_app(
  "ggplot(data = vaccinations,
          aes(x = survey, stratum = response, alluvium = subject,
              y = freq, fill = var)) +
     geom_flow(alpha = num) +
     geom_stratum() +
     scale_x_discrete(expand = c(0.1, 0.1)) +
     labs(title = text)"
)
