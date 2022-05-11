library(reshape2)

#355 with sample size of 40, 100, 200


plotDistro <- function(distro, distroList) {
  plot <-  ggplot()+theme_tufte()+xlab("parameter values")+
    ylab("probability")+theme(plot.title.position = "plot")+ylim(0,1)+
    ggtitle(paste(entPropTable$distributions[distroList]))+
    geom_line(aes(x = ps,y = distro))+annotate("text",
                                               x = ps[which(distro == max(distro))][1],
                                               y = max(distro) * 1.14,
                                               label = paste("h =",round(entPropTable$hSeq[distroList],3), ", w = ",
                                                             round(entPropTable$entPropSeq[distroList],3)), size = 2)+
    ylim(c(0,1.2 * max(distro)))
  return(plot)
}

ps <- seq(0,1,by = 0.01)

db40 <- dbeta(ps,11,31)
db101 <- dbeta(ps,26,76)
db201 <- dbeta(ps,51,151)

carpets <- data.frame(ps, db40, db101, db201)
carpetsLong <- melt(carpets, id.vars = ps, variable.name = "n")

ggplot(carpetsLong)+geom_line(aes(x = ps, y = value, group = n,  lty = n, color = n))+xlab(expression(theta))+ 
  ylab("density")+theme(plot.title.position = "plot")+theme_tufte(base_size = 12)+
  theme(legend.position="none")+labs(title = "Uncertainty about fiber frequency", subtitle = "(beta distributions)")+
  annotate("text", x = rep(.35, 3), y = c(12.5, 9, 5.5), label = paste("n = ", c(200, 100, 40), sep = ""))


