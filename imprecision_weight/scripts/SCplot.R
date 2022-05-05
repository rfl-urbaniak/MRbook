
setwd("imprecision_weight/")
source("/scripts/SCdistro.R")
require(ggplot2)
require(gridExtra)
require(bezier)


GuiltPriorPlotGrob <- ggplotGrob(GuiltPriorPlot)

AsidsPriorPlotGrob <- ggplotGrob(AsidsPriorPlot+theme_tufte(base_size = 7))


graphviz.plot(SallyClarkDAG)

ggplot(data.frame(a=1)) + xlim(1, 40) + ylim(1, 60)+theme_void()+
  annotation_custom(AsidsPriorPlotGrob, xmin = 8, xmax = 16, ymin = 50, ymax = 60)+
  geom_label(aes(label = "Bcause", x = 25, y = 41),
              size = 3 )+
  geom_label(aes(label = "Acause", x = 12, y = 48),
            size = 3 )+
  geom_curve(aes(x = 12, y = 49.5, xend = 25, yend = 42.5), curvature = -.15,size = .3,
             arrow = arrow(length = unit(.015, "npc")))







  annotation_custom(gg2, xmin = 11, xmax = 19, ymin = 21, ymax = 29) +
  annotation_custom(gg3, xmin = 11, xmax = 19, ymin = 12, ymax = 20) +
  annotation_custom(gg4, xmin = 1, xmax = 9, ymin = 10, ymax = 18) +
  annotation_custom(gg5, xmin = 1, xmax = 9, ymin = 1, ymax = 9) +
  annotation_custom(gg6, xmin = 11, xmax = 19, ymin = 1, ymax = 9) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = list(x = c(9, 10, 10, 11), y = c(27, 27, 25, 25)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = list(x = c(9, 10, 10, 11), y = c(27, 27, 18, 18)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = list(x = c(15, 15, 12, 9), y = c(12, 11, 11, 11)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = list(x = c(15, 15, 12, 9), y = c(12, 11, 11, 9)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, p = list(x = c(15, 15, 12, 12), y = c(12, 10.5, 10.5, 9)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"), type = "closed")) +
  theme(rect = element_blank(),
        line = element_blank(),
        text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm"))