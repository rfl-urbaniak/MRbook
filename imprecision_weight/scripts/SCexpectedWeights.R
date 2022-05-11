


SCprobsFinal <- readRDS("datasets/SCprobsFinal.rds")


head(SCprobsFinal)

attach(SCprobsFinal)


head(SCprobsFinal)


#Abruising
AbruisingExp <- expectedFromSample(AbruisingPrior)

AbruisingNExp <- 1 - AbruisingExp

GuiltAbruisingP <- distroFromSamples(GuiltAbruising)
GuiltPriorP <- distroFromSamples(GuiltPrior)
GuiltAbruisingPlot <- plotPosterior(GuiltAbruisingP, GuiltAbruising, "Guilt (Abruising)",
              prior = GuiltPriorP )
wDeltaAbrusing <- wDelta(GuiltAbruisingP, GuiltPriorP)


GuiltAbruisingNoP <- distroFromSamples(GuiltAbruisingNo)
GuiltAbruisingNoPlot <- plotPosterior(GuiltAbruisingNoP, GuiltAbruisingNo, "Guilt (AbruisingNo)",
              prior = GuiltPriorP )
wDeltaAbruisingNo <- wDelta(GuiltAbruisingNoP, GuiltPriorP)



weightExpectedAbruising <- AbruisingExp * wDeltaAbrusing + AbruisingNExp * wDeltaAbruisingNo

weightExpectedAbruising


AdiseaseExp <- expectedFromSample(AdiseasePrior)
AdiseaseNExp <- 1 - AdiseaseExp


GuiltAdiseaseP <- distroFromSamples(GuiltAdisease)
plotPosterior(GuiltAdiseaseP, GuiltAdisease, "Guilt (Adisease)",
              prior = GuiltPriorP )
wDeltaAdisease <- wDelta(GuiltAdiseaseP, GuiltPriorP)


GuiltAdiseaseNoP <- distroFromSamples(GuiltAdiseaseNo)
plotPosterior(GuiltAdiseaseNoP, GuiltAdiseaseNo, "Guilt (AdiseaseNo)",
              prior = GuiltPriorP )
wDeltaAdiseaseNo <- wDelta(GuiltAdiseaseNoP, GuiltPriorP)



weightExpectedAdisease <- AdiseaseExp * wDeltaAdisease + AdiseaseNExp * wDeltaAdiseaseNo

weightExpectedAbruising
weightExpectedAdisease


head(SCprobsFinal)


(AbruisingPriorPlot <-  plotSample(AbruisingPrior, "Abruising prior", subtitle = "" ))
AbruisingPriorPlotG <- ggplotGrob(AbruisingPriorPlot+theme_tufte(base_size = 7))



(AbruisingNoPriorPlot <-  plotSample(1-AbruisingPrior, "AbruisingNo prior", subtitle = "" ))
AbruisingNoPriorPlotG <- ggplotGrob(AbruisingNoPriorPlot+theme_tufte(base_size = 7))


GuiltAbruisingPlotG <- ggplotGrob(GuiltAbruisingPlot+theme_tufte(base_size = 7)+labs(subtitle = ""))
GuiltAbruisingNoPlotG <-  ggplotGrob(GuiltAbruisingNoPlot+theme_tufte(base_size = 7)+labs(subtitle = ""))



SCweightsPlot <- ggplot(data.frame(a=1)) + xlim(1, 20) + ylim(10, 60)+theme_void()+
  geom_label(aes(label = "Abruising", x = 10, y = 58),
             size = 3 )+
  annotation_custom(AbruisingPriorPlotG, xmin = 1, xmax = 9, ymin = 45, ymax = 55)+
  annotation_custom(AbruisingNoPriorPlotG, xmin = 11, xmax = 19, ymin = 45, ymax = 55)+
  geom_curve(aes(x = 8.5, y = 58, xend = 5, yend = 55), curvature = .18,size = .3,
                                             arrow = arrow(length = unit(.015, "npc")))+
  geom_curve(aes(x = 11.5, y = 58, xend = 15, yend = 55), curvature = -.18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_label(aes(label = paste("Exp(Abruising) = ", round(AbruisingExp,3), sep = ""), x = 6, y = 43),
             size = 3 )+
  geom_label(aes(label = paste("Exp(AbruisingNo) = ", 1-round(AbruisingExp,3), sep = ""), x = 15, y = 43),
             size = 3 )+
  annotation_custom(GuiltAbruisingPlotG, xmin = 1, xmax = 9, ymin = 30, ymax = 40)+
  geom_curve(aes(x = 1, y = 48, xend = 1, yend = 38), curvature = .18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  annotation_custom(GuiltAbruisingNoPlotG, xmin = 11,  xmax = 19, ymin = 30, ymax = 40)+
  geom_curve(aes(x = 19, y = 48, xend = 19, yend = 38), curvature = -.18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_label(aes(label = paste("wDelta(GuiltAbruising) = ", wDeltaAbrusing, sep = ""), x = 5, y = 28),
             size = 3 )+
  geom_label(aes(label = paste("wDelta(GuiltAbruisingNo) = ", wDeltaAbruisingNo, sep = ""), x = 16, y = 28),
             size = 3 )+
  geom_label(aes(label = paste("E(wDelta(Abruising)) = ", round(weightExpectedAbruising,3), sep = ""), x = 10, y = 18),
             size = 3 )+
  geom_curve(aes(x = 4, y = 26.5, xend = 8, yend = 18), curvature = .18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_curve(aes(x = 16, y = 26.5, xend = 12, yend = 18), curvature = -.18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_curve(aes(x = 9, y = 44, xend = 9, yend = 20), curvature = -.15,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_curve(aes(x = 11, y = 44, xend = 11, yend = 20), curvature = .15,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_label(aes(label = paste("E(wDelta(Adisease)) = ", round(weightExpectedAdisease,3), sep = ""), x = 10, y = 14),
             size = 3 )





SCweightsPlot 
