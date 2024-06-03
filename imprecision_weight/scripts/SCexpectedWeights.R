
#SCprobsFinal <- readRDS("datasets/SCprobsFinal.rds")

#head(SCprobsFinal)

#attach(SCprobsFinal)




#Abruising
AbruisingExp <- expectedFromSample(AbruisingPrior)
AbruisingNExp <- 1 - AbruisingExp

GuiltAbruisingP <- distroFromSamples(GuiltAbruising)
GuiltPriorP <- distroFromSamples(GuiltPrior)
GuiltAbruisingPlot <- plotPosterior(GuiltAbruisingP, GuiltAbruising, "Guilt (Abruising)",
              prior = GuiltPriorP, subsize = 7 )
wPropAbsAbrusing <- abs(1-weightProp(GuiltAbruisingP, GuiltPriorP))


GuiltAbruisingNoP <- distroFromSamples(GuiltAbruisingNo)
GuiltAbruisingNoPlot <- plotPosterior(GuiltAbruisingNoP, GuiltAbruisingNo, "Guilt (AbruisingNo)",
              prior = GuiltPriorP, subsize = 7 )
wPropAbsAbruisingNo <- abs(1 - weightProp(GuiltAbruisingNoP, GuiltPriorP))

weightExpectedAbruising <- AbruisingExp * wPropAbsAbrusing +
                        AbruisingNExp * wPropAbsAbruisingNo


AbruisingLineExp <- c(AbruisingExp,wPropAbsAbrusing, AbruisingNExp,
                      wPropAbsAbruisingNo, weightExpectedAbruising)



AdiseaseExp <- expectedFromSample(AdiseasePrior)
AdiseaseNExp <- 1 - AdiseaseExp


GuiltAdiseaseP <- distroFromSamples(GuiltAdisease)
plotPosterior(GuiltAdiseaseP, GuiltAdisease, "Guilt (Adisease)",
              prior = GuiltPriorP, subsize = 7 )
wPropAbsAdisease <- abs(1 - weightProp(GuiltAdiseaseP, GuiltPriorP))


GuiltAdiseaseNoP <- distroFromSamples(GuiltAdiseaseNo)
plotPosterior(GuiltAdiseaseNoP, GuiltAdiseaseNo, "Guilt (AdiseaseNo)",
              prior = GuiltPriorP, subsize = 7 )
wPropAbsAdiseaseNo <- abs(1-weightProp(GuiltAdiseaseNoP, GuiltPriorP))


weightExpectedAdisease <- AdiseaseExp * wPropAbsAdisease + AdiseaseNExp * wPropAbsAdiseaseNo



AdiseaseLineExp <- c(AdiseaseExp,wPropAbsAdisease, AdiseaseNExp,
                      wPropAbsAdiseaseNo, weightExpectedAdisease)


expCalculations <- data.frame(abruising = AbruisingLineExp, adisease = AdiseaseLineExp 
)


rownames(expCalculations) <- c("E(Pr((TRUE))", "proportional (absolute value) if TRUE", "E(Pr(FALSE))", 
                               "proportional (absolute value) if FALSE", "E(|proportional|)")



(AbruisingPriorPlot <-  plotSample(AbruisingPrior, "Abruising prior", subtitle = "" ))
AbruisingPriorPlotG <- ggplotGrob(AbruisingPriorPlot+theme_tufte(base_size = 7))



(AbruisingNoPriorPlot <-  plotSample(1-AbruisingPrior, "AbruisingNo prior", subtitle = "" ))
AbruisingNoPriorPlotG <- ggplotGrob(AbruisingNoPriorPlot+theme_tufte(base_size = 7))


GuiltAbruisingPlotG <- ggplotGrob(GuiltAbruisingPlot+theme_tufte(base_size = 7)+labs(subtitle = ""))
GuiltAbruisingNoPlotG <-  ggplotGrob(GuiltAbruisingNoPlot+theme_tufte(base_size = 7)+labs(subtitle = ""))



SCweightsPlot <- ggplot(data.frame(a=1)) + xlim(1, 20) + ylim(15, 60)+theme_void()+
  geom_label(aes(label = "Abruising", x = 10, y = 58),
             size = 3 )+
  annotation_custom(AbruisingPriorPlotG, xmin = 1, xmax = 9, ymin = 45, ymax = 55)+
  annotation_custom(AbruisingNoPriorPlotG, xmin = 11, xmax = 19, ymin = 45, ymax = 55)+
  geom_curve(aes(x = 8.5, y = 58, xend = 5, yend = 55), curvature = .18,size = .3,
                                             arrow = arrow(length = unit(.015, "npc")))+
  geom_curve(aes(x = 11.5, y = 58, xend = 15, yend = 55), curvature = -.18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_label(aes(label = paste("E(Pr(Abruising)) = ", round(AbruisingExp,3), sep = ""), x = 6, y = 43),
             size = 3 )+
  geom_label(aes(label = paste("E(Pr((AbruisingNo)) = ", 1-round(AbruisingExp,3), sep = ""), x = 15, y = 43),
             size = 3 )+
  annotation_custom(GuiltAbruisingPlotG, xmin = 1, xmax = 9, ymin = 30, ymax = 40)+
  geom_curve(aes(x = 1, y = 48, xend = 1, yend = 38), curvature = .18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  annotation_custom(GuiltAbruisingNoPlotG, xmin = 11,  xmax = 19, ymin = 30, ymax = 40)+
  geom_curve(aes(x = 19, y = 48, xend = 19, yend = 38), curvature = -.18,size = .3,
             arrow = arrow(length = unit(.015, "npc")))+
  geom_label(aes(label = paste("proportional = ", round(weightProp(GuiltAbruisingP, GuiltPriorP),3), sep = ""), x = 5, y = 28),
             size = 3 )+
  geom_label(aes(label = paste("proportional = ", round(weightProp(GuiltAbruisingNoP, GuiltPriorP),3), sep = ""), x = 16, y = 28),
             size = 3 )+
  annotation_custom(tableGrob(round(expCalculations,3), 
                              theme = ttheme_minimal(base_size = 6.5)),
  xmin=2, xmax=18, ymin=11, ymax=28)

