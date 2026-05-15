library(dplyr)
ptr_app(
  "mpg |>
     dplyr::filter(displ > num) |>
     dplyr::group_by(class) |>
     dplyr::filter(dplyr::n() > num) |>
     dplyr::ungroup() |>
     ggplot(aes(var, var, color = class)) +
     geom_point(alpha = num) +
     labs(title = text)"
)
