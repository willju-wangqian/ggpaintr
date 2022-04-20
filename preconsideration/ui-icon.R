


df<-iris


scatter<-df%>%filter(Sepal.Length> "5" & Sepal.Length<"6.5")%>%
  ggplot(aes(x=Sepal.Width,y=Sepal.Length, color=Species))+xlim(1, 6)+ylim(4.5, 8)+
  geom_point(aes(size=Sepal.Width,shape=Species))+theme_bw()+scale_color_brewer(palette="Set2")



df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))


line<-ggplot(df2, aes(x=dose, y=len, group=supp)) +
  geom_line(aes(color=supp,linetype=supp, size=0.1))+ylim(0, 50)+
  geom_point(aes(color=supp))+theme_bw()+
  scale_color_brewer(palette="Dark2")

line
