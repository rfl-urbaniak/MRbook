#source("scripts/SCdistro.R")
#file.edit("scripts/SCdistro.R")
#SCprobsFinal <- readRDS("datasets/SCprobsFinal.rds")


attach(SCprobsFinal)

AsidsPriorPlot <- plotSample(AsidsPrior, "Asids prior", paste("Beta(9,1), median =", round(median(AsidsPrior),2), sep = "") )

AbruisingIfSidsPlot <- plotSample(AbruisingIfSids, "AbruisingIfSids", 
                                  paste("Norm(.02,.04), median =", round(median(AbruisingIfSids),2), sep = "") )


AbruisingIfMurderPlot <- plotSample(AbruisingIfMurder, "AbruisingIfMurder",
                                    paste("Beta(5,30), median =", round(median(AbruisingIfMurder),2), sep = "") )

AdiseaseIfSidsPlot <- plotSample(AdiseaseIfSids, "AdiseaseIfSids",
                                 paste("Norm(.07,.3), median =", round(median(AdiseaseIfSids),2), sep = "") )

AdiseaseIfMurderPlot <- plotSample(AdiseaseIfMurder, "AdiseaseIfMurder",
                                   paste("Norm(.002,.05), median =", round(median(AdiseaseIfMurder),2), sep = "") )

BbruisingIfSidsPlot <- plotSample(BbruisingIfSids, "BbruisingIfSids",
                                  paste("Norm(.03,.08), median =", round(median(BbruisingIfSids),2), sep = "") )

BbruisingIfMurderPlot <- plotSample(BbruisingIfMurder, "BbruisingIfMurder",
                                    paste("Norm(.03,.08), median =", round(median(BbruisingIfMurder),2), sep = "") )

BdiseaseIfSidsPlot <- plotSample(BdiseaseIfSids, "BdiseaseIfSids",
                                 paste("Norm(.03,.08), median =", round(median(BdiseaseIfSids),2), sep = "") )

BdiseaseIfAmurderPlot <- plotSample(BdiseaseIfAmurder, "BdiseaseIfAmurder",
                                    paste("Norm(.002,.05), median =", round(median(BdiseaseIfAmurder),2), sep = "") )

BcauseSidsIfAsidsPlot <- plotSample(BcauseSidsIfAsids, "BcauseSidsIfAsids",
                                    paste("Norm(.002,.05), median =", round(median(BcauseSidsIfAsids),2), sep = "") )

BcauseSidsIfAmurderPlot <- plotSample(BcauseSidsIfAmurder, "BcauseSidsIfAmurder",
                                      paste("Norm(.002,.05), median =", round(median(BcauseSidsIfAmurder),2), sep = "") )
