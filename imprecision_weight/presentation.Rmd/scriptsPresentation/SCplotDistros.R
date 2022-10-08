

GuiltPriorPlot <- plotSample(GuiltPrior, "GuiltPrior",
                             paste("median =", 
                                   round(median(GuiltPrior),2), ", 89%HPDI = ", round(HPDI(GuiltPrior),2)[1],"-",round(HPDI(GuiltPrior),2)[2],
                                   sep = "") )+xlim(0,1)





AbruisingPriorPlot <- plotSample(AbruisingPrior, "AbruisingPrior",
                             paste("median =", 
                                   round(median(AbruisingPrior),2), ", 89%HPDI = ", 
                                   round(HPDI(AbruisingPrior),2)[1],"-",
                                   round(HPDI(AbruisingPrior),2)[2], sep = "") )



AbruisingNoPriorPlot <- plotSample((1-AbruisingPrior), "AbruisingNoPrior",
                                 paste("median =", 
                                       round(median((1-AbruisingPrior)),2), ", 89%HPDI = ", 
                                       round(HPDI((1-AbruisingPrior)),2)[1],"-",
                                       round(HPDI((1-AbruisingPrior)),2)[2], sep = "") )



AdiseasePriorPlot <- plotSample(AdiseasePrior, "AdiseasePrior",
                                 paste("median =", 
                                       round(median(AdiseasePrior),2), ", 89%HPDI = ", 
                                       round(HPDI(AdiseasePrior),2)[1],"-",
                                       round(HPDI(AdiseasePrior),2)[2], sep = "") ) 



AdiseaseNoPriorPlot <- plotSample((1-AdiseasePrior), "AdiseaseNoPrior",
                                   paste("median =", 
                                         round(median((1-AdiseasePrior)),2), ", 89%HPDI = ", 
                                         round(HPDI((1-AdiseasePrior)),2)[1],"-",
                                         round(HPDI((1-AdiseasePrior)),2)[2], sep = "") )



#AbruisingPrior <- SCprobsFinal$AbruisingPrior


GuiltAbruisingPlot <- plotSample(GuiltAbruising, "GuiltAbruising",
                                 paste("median =", 
                                       round(median(GuiltAbruising),2), ", 89%HPDI = ", round(HPDI(GuiltAbruising),2)[1],"-",
                                       round(HPDI(GuiltAbruising),2)[2], sep = "") )







GuiltABbruisingPlot <- plotSample(GuiltABbruising, "GuiltABbruising",
                                  paste("median =", 
                                        round(median(GuiltABbruising),2), ", 89%HPDI = ", round(HPDI(GuiltABbruising),2)[1],"-",
                                        round(HPDI(GuiltABbruising),2)[2], sep = "") )






GuiltABbruisingNoDiseasePlot <- plotSample(GuiltABbruisingNoDisease, "GuiltABbruisingNoDisease",
                                           paste("median =", 
                                                 round(median(GuiltABbruisingNoDisease),2), ", 89%HPDI = ", round(HPDI(GuiltABbruisingNoDisease),2)[1],"-",
                                                 round(HPDI(GuiltABbruisingNoDisease),2)[2], sep = "") )



GuiltABbruisingDiseaseAPlot <- plotSample(GuiltABbruisingDiseaseA, "GuiltABbruisingDiseaseA",
                                           paste("median =", 
                                                 round(median(GuiltABbruisingDiseaseA, na.rm=TRUE),2), ", 89%HPDI = ", 
                                                 round(HPDI(GuiltABbruisingDiseaseA),2)[1],"-",
                                                 round(HPDI(GuiltABbruisingDiseaseA),2)[2], sep = "") )


