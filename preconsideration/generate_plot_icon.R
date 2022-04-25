library(palmerpenguins)
library(tidyverse)
library(RColorBrewer)
library(here)

i_am("preconsideration/generate_plot_icon.R")

data("penguins")

my_theme_icon <- theme_minimal() +
  theme(axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        legend.position="none"
  )

p_boxplot <- penguins %>% ggplot()  +
  geom_boxplot(aes(x=species, y=bill_depth_mm, fill = species)) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB")) +
  my_theme_icon

png(here("preconsideration", "saved_plot_icon", "boxplot.png"),
    width = 236, height = 150, units = "px")
print(p_boxplot)
dev.off()

p_bar <- penguins %>% ggplot()  +
  geom_bar(aes(x=species, fill = species)) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB")) +
  my_theme_icon
penguins %>% ggplot()  +
  geom_bar(aes(x=species, fill = species)) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB")) +
  eval(rlang::parse_expr("labs(x = 'abc')"))

rlang::parse_expr("labs(x = 'abc')")

eval(expr(sym("labs(x = 'abc')")))

png(here("preconsideration", "saved_plot_icon", "barplot.png"),
    width = 236, height = 150, units = "px")
print(p_bar)
dev.off()


eval(rlang::parse_expr(
  'penguins %>% ggplot()  +
  geom_bar(aes(x=species, fill = species)) +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB"))'
))



color1 <- brewer.pal(11, "RdBu")[9]
color2 <- brewer.pal(11, "RdBu")[3]

penguins %>% ggplot()  +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color = (bill_length_mm))) +
  # scale_color_gradient(low = color1, high = color2)
  do.call(scale_color_gradient, list(low = color1, high=color2))

RColorBrewer::display.brewer.all()
RColorBrewer::display.brewer.pal(3, "Set2")

brewer.pal(3, "RdYlBu")
display.brewer.pal(9, "PuBu")
brewer.pal(5, "RdYlBu")[1]







