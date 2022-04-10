library(hexSticker)
library(ggplot2)



data("ChickWeight")


p <- ChickWeight%>%
  ggplot(aes(x = Time, y = weight, color=Diet)) + geom_boxplot()

p <- p + theme_void()+ theme_transparent()


logo<-sticker(
        p,
        # subplot=cybird_img,
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

