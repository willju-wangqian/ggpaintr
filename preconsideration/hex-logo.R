library(hexSticker)
library(ggplot2)

## logo based on cybird
cybird_img<-image_read("cybird.png")

logo<-sticker(
        subplot=cybird_img,
        package="ggpaintr",
        s_width=1,
        s_height=1,
        s_x = 1,
        s_y= 0.75,
        p_size = 19,
        p_color = "black",
        h_fill="#CCCCFF",
        h_color="hotpink",
        h_size= 2,
        url="https://github.com/willju-wangqian/ggpaintr",
        u_size=2.5,
        spotlight = T,
        l_x = 1.5,
        l_y = 1,
        l_width = 3,
        l_height = 3,
        l_alpha = 0.8
        )
logo




## logo based on a ggplot2 plot


data("midwest", package = "ggplot2")

# Scatterplot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
  geom_point(aes(col=state, size=popdensity)) +
  geom_smooth(method="loess", se=F) +
  xlim(c(0, 0.1)) +
  ylim(c(0, 500000)) +
  labs(
    title="The Dream Plot",
    col="Z-variable")+guides(size = "none")

p <- gg + theme_bw()+theme_transparent()+xlab(NULL)+ylab(NULL)+ theme(legend.position="none")

logo2<-sticker(
  p,
  s_x=1,
  s_y=.75,
  s_width=1.0,
  s_height=0.8,
  package="ggpaintr",
  p_size = 19,
  p_color = "black",
  h_fill="#CCCCFF",
  h_color="hotpink",
  h=3,
  url="https://github.com/willju-wangqian/ggpaintr",
  u_size=2.5,
  spotlight = T,
  l_x = 1.5,
  l_y = 1,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.5
)
logo2


