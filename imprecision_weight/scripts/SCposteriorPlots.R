library(tidyverse)
library(gridExtra)
library(kableExtra)

library(magrittr)
library(ggplot2)
library(ggpubr)
library(ggExtra)
library(ggthemes)
library(latex2exp)
th <- theme_tufte(base_size = 20)



n <- 1000
ps <- seq(0,1,  length.out =n)




SCdensities <- grid.arrange(GuiltPriorPlot, GuiltABbruisingPlot, GuiltABbruisingNoDiseasePlot, GuiltABbruisingDiseaseAPlot, ncol =2)

SCdensities

GuiltPriorP <- distroFromSamples(GuiltPrior)
priorPlotP <- plotPosterior(GuiltPriorP, GuiltPrior, "Guilt (prior)", prior = GuiltPriorP)

GuiltABbruisingP <- distroFromSamples(GuiltABbruising)
GuiltABbruisingPlotP <- plotPosterior(GuiltABbruisingP, GuiltABbruising, "Guilt (ABbruising)", prior = GuiltPriorP )

GuiltABbruisingNoDiseaseP <- distroFromSamples(GuiltABbruisingNoDisease)
GuiltABbruisingNoDiseasePlotP <- plotPosterior(GuiltABbruisingNoDiseaseP, GuiltABbruisingNoDisease, "Guilt (ABbruisingNoDisease)",
                                                prior = GuiltPriorP )

GuiltABbruisingDiseaseA <- GuiltABbruisingDiseaseA[!is.na(GuiltABbruisingDiseaseA)]
GuiltABbruisingDiseaseAP <- distroFromSamples(GuiltABbruisingDiseaseA)
GuiltABbruisingDiseaseAPlotP <- plotPosterior(GuiltABbruisingDiseaseAP,
                                               GuiltABbruisingDiseaseA, "Guilt (ABbruisingDiseaseA)",prior = GuiltPriorP)


SCguiltPlotP <- grid.arrange(priorPlotP+theme_tufte(base_size = 9), 
                             GuiltABbruisingPlotP+theme_tufte(base_size = 9),
                             GuiltABbruisingNoDiseasePlotP+theme_tufte(base_size = 9),
                             GuiltABbruisingDiseaseAPlotP+theme_tufte(base_size = 9), ncol =2)




