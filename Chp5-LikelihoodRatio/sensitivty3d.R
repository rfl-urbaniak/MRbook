library(tidyverse)
library(ggthemes)
library(plot3D)

EifH <- 1
EifNH <- .1
H <- .1
E <- EifH * H + EifNH * (1-H)
E
BF <- EifH/E
BF

H2 <- .2
E2 <- EifH * H2 + EifNH * (1-H2)
E2
BF2 <- EifH/E2
BF2


pH <- seq(0,.05, by = 0.001)
EifNH <- seq(0,.05, by = 0.001)
EifH <- 1
options <- expand.grid(pH = pH, EifNH = EifNH)
options$E <- EifH * options$pH + options$EifNH * (1-options$pH)
options$BF <- EifH/options$E
options <- options[-1,]

scatter3D(options$pH,options$EifNH,options$BF,pch=3,cex=0.3,byt="g",alpha=0.8,theta=50, phi=8,ticktype="simple",xlab="P(H)", ylab="P(E|~H)",zlab="Bayes Factor",main="Bayes factor as a function of prior and P(E|~H)",colvar=NULL, zlim = c(0,250))



options$pH






ggplot()+  geom_line(aes(x=fpp,y=lr9))+
  geom_line(aes(x=fpp,y=lr3), lty =2, color = "orange")+
  geom_hline(yintercept =16.8, lty =2 , col = "skyblue")+
  theme_tufte()+ylim(c(0,400))