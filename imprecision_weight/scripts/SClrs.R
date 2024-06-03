



SCprobsFinal <- readRDS("datasets/SCprobsFinal.rds")


head(SCprobsFinal)

attach(SCprobsFinal)

AbruisingLR <- AbruisingIfMurder / AbruisingIfSids


(AbruisingLRPlot <- plotSample(AbruisingLR,title = "LR: Abruising",  paste("median =", 
                                                            round(median(AbruisingLR),2), ", 89%HPDI = ", 
                                                            round(HPDI(AbruisingLR),2)[1],"-",round(HPDI(AbruisingLR),2)[2],
                                                            sep = "") )+xlim(0,30))


AdiseaseLR <- AdiseaseIfMurder / AdiseaseIfSids


(AdiseaseLRPlot <- plotSample(AdiseaseLR,title = "LR: Adisease",  paste("median =", 
                                                                           round(median(AdiseaseLR, na.rm = TRUE),2), ", 89%HPDI = ", 
                                                                           round(HPDI(AdiseaseLR),2)[1],"-",round(HPDI(AdiseaseLR),2)[2],
                                                                           sep = "") )+xlim(0,3))




(SClrPlot <- grid.arrange(AbruisingLRPlot, AdiseaseLRPlot, ncol =2))


