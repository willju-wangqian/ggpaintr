library(hexSticker)
library(ggplot2)
library(magick)


palette_img<-image_read("www/img/palette.png")
palette_img1<-image_read("www/img/palette1.png")

logo<-sticker(
  subplot=palette_img,
  package="ggpaintr",
  s_width=1.2,
  s_height=1.2,
  s_x = 1,
  s_y= 0.75,
  p_size = 19,
  p_color = "white",
  h_fill="#99CCFF",
  h_color="#FFB266",
  h_size= 3,
  spotlight = T,
  l_x = 1.5,
  l_y = 1,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.7
)
logo





