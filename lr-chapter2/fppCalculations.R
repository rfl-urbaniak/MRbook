library(ggplot2)
library(ggthemes)


rmp9 <- 10e-9
rmp3 <- 10e-3


fpp <- seq(0,0.05, by = 0.001)


lr9 <- 1/(rmp9 + (fpp * (1-rmp9)))
lr3 <- 1/(rmp3 + (fpp * (1-rmp3)))


ggplot()+  geom_line(aes(x=fpp,y=lr9))+
  geom_line(aes(x=fpp,y=lr3), lty =2, color = "orange")+
  geom_hline(yintercept =16.8, lty =2 , col = "skyblue")+
  theme_tufte()+ylim(c(0,400))



