library(tsibble)
library(tsibbletalk)
library(dplyr)
tourism_shared <- tourism_monthly %>%
  # Comment out the next line to run the full example
  filter(State %in% c("Tasmania", "Western Australia")) %>%
  mutate(Region = stringr::str_replace(Region, "Australia's ", "WA's ")) %>%
  as_shared_tsibble(spec = (State / Region) * Purpose)

p_l <- plotly_key_tree(tourism_shared, height = 800, width = 800)

library(ggplot2)
p_tr <- tourism_shared %>%
  ggplot(aes(x = Month, y = Trips)) +
  geom_line(aes(group = Region), alpha = .5, linewidth = .4) +
  facet_wrap(~ Purpose, scales = "free_y") +
  scale_x_yearmonth(date_breaks = "5 years", date_labels = "%Y")

library(feasts)
tourism_feat <- tourism_shared %>%
  features(Trips, feat_stl)
p_br <- tourism_feat %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(aes(group = Region), alpha = .8, size = 2)

library(plotly)
subplot(p_l,
        subplot(
          ggplotly(p_tr, tooltip = "Region", width = 700),
          ggplotly(p_br, tooltip = "Region", width = 700),
          nrows = 2),
        widths = c(.4, .6)) %>%
  highlight(dynamic = TRUE)





{
  ggplot(ppUpload(iris), aes(x = ppVar(Sepal.Length),
                     y = ppVar(Sepal.Width),
                     color = ppVar(Species))) +
      geom_point()
} |>
  ptr_app()
