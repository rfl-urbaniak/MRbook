
setwd("imprecision_weight/")
#source("/scripts/SCdistro.R")
SCprobsFinal <- readRDS("datasets/SCprobsFinal.rds")
#source("scripts/SCplotCPTs.R")

require(ggplot2)
require(gridExtra)
#require(bezier)


attach(SCprobsFinal)


AsidsPriorPlotGrob <- ggplotGrob(AsidsPriorPlot+theme_tufte(base_size = 7))
BcauseSidsIfAsidsPlotGrob <- ggplotGrob(BcauseSidsIfAsidsPlot+theme_tufte(base_size = 7))
BcauseSidsIfAmurderPlotGrob<- ggplotGrob(BcauseSidsIfAmurderPlot+theme_tufte(base_size = 7)) 

AbruisingIfSidsPlotGrob <- ggplotGrob(AbruisingIfSidsPlot+theme_tufte(base_size = 7)) 
AbruisingIfMurderPlotGrob <- ggplotGrob(AbruisingIfMurderPlot+theme_tufte(base_size = 7)) 


AdiseaseIfSidsPlotGrob <- ggplotGrob(AdiseaseIfSidsPlot+theme_tufte(base_size = 7)) 
AdiseaseIfMurderPlotGrob <- ggplotGrob(AdiseaseIfMurderPlot+theme_tufte(base_size = 7)) 


BbruisingIfSidsPlotGrob <- ggplotGrob(BbruisingIfSidsPlot+theme_tufte(base_size = 7)) 
BbruisingIfMurderPlotGrob <- ggplotGrob(BbruisingIfMurderPlot+theme_tufte(base_size = 7)) 


BdiseaseIfSidsPlotGrob <- ggplotGrob(BdiseaseIfSidsPlot+theme_tufte(base_size = 7)) 
BdiseaseIfAmurderPlotGrob <- ggplotGrob(BdiseaseIfAmurderPlot+theme_tufte(base_size = 7)) 





ggplot(data.frame(a=1)) + xlim(1, 40) + ylim(1, 60)+theme_void()+
  annotation_custom(AsidsPriorPlotGrob, xmin = 9, xmax = 15, ymin = 49, ymax = 59)+
  geom_label(aes(label = "Bcause", x = 25, y = 41),
              size = 3 )+
  geom_label(aes(label = "Acause", x = 12, y = 48),
            size = 3 )+
  geom_curve(aes(x = 13.5, y = 48.2, xend = 25, yend = 42.5), curvature = -.18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  annotation_custom(BcauseSidsIfAsidsPlotGrob, xmin = 17, xmax = 23, ymin = 47, ymax = 57)+
  annotation_custom(BcauseSidsIfAmurderPlotGrob, xmin = 15, xmax = 21, ymin = 37, ymax = 47)+
  geom_label(aes(label = "Abruising", x = 2, y = 41),
             size = 3 )+
  geom_curve(aes(x = 10.5, y = 48.2, xend = 2, yend = 42.5), curvature = .2,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  annotation_custom(AbruisingIfSidsPlotGrob, xmin = 2, xmax = 8, ymin = 47, ymax = 57)+
  annotation_custom(AbruisingIfMurderPlotGrob, xmin = 5, xmax = 11, ymin = 37, ymax = 47)+
  geom_label(aes(label = "Adisease", x = 6, y = 21),
             size = 3 )+
  geom_curve(aes(x = 13, y = 46.2, xend = 6, yend = 23), curvature = -.45,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  annotation_custom(AdiseaseIfSidsPlotGrob, xmin = 4.5, xmax = 10.5, ymin = 27, ymax = 37)+
  annotation_custom(AdiseaseIfMurderPlotGrob, xmin = 9, xmax = 15, ymin = 17.5, ymax = 27.5)+
  geom_label(aes(label = "Bbruising", x = 14, y = 12),
             size = 3 ) +
  geom_curve(aes(x = 24, y = 39.5, xend = 15.5, yend = 13.5), curvature = -.2,size = .3,
                                    arrow = arrow(length = unit(.015, "npc")))+
  annotation_custom(BbruisingIfSidsPlotGrob, xmin = 15.5, xmax = 21.5, ymin = 25, ymax = 35)+
  annotation_custom(BbruisingIfMurderPlotGrob, xmin = 18.5, xmax = 24.5, ymin = 10, ymax = 20)+
  geom_label(aes(label = "Bdisease", x = 33, y = 18),
             size = 3 )  +
  geom_curve(aes(x = 26, y = 39.5, xend = 33, yend = 19.5), curvature = -.2,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  annotation_custom(BdiseaseIfSidsPlotGrob, xmin = 31, xmax = 37, ymin = 27, ymax = 37)+
  annotation_custom(BdiseaseIfAmurderPlotGrob, xmin = 24.5, xmax = 30.5, ymin = 20, ymax = 30)













  