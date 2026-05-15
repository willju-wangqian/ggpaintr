source("dev/audit/runs/_gallery_helpers.R", local = TRUE)
library(ggpcp); data(flea, package = "GGally")
ptr_app(
  "ggplot(data = flea |>
                  pcp_select(colvars) |>
                  pcp_scale(method = 'uniminmax') |>
                  pcp_arrange(),
          mapping = aes_pcp()) +
     geom_pcp_axes() +
     geom_pcp(aes(colour = species)) +
     coord_cartesian(xlim = range, ylim = range)"
)
