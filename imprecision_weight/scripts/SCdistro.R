library(bnlearn)
library(Rgraphviz)
library(gRain)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)

plotDistroPlain <- function(distro, title, mult = 1.2) {
  plot <-  ggplot()+theme_tufte()+xlab("parameter values")+
    ylab("probability")+theme(plot.title.position = "plot")+
    ggtitle(title)+
    geom_line(aes(x = ps,y = distro))+
    ylim(c(0,mult * max(distro)))
  return(plot)
}


setwd("imprecision_weight/")





source("scripts/cptCreate.R")


#create SC DAG
#define the structure of the Sally Clark BN
SallyClarkDAG <- model2network("[Abruising|Acause][Adisease|Acause][Bbruising|Bcause][Bdisease|Bcause][Acause][Bcause|Acause][NoMurdered|Acause:Bcause][Guilty|NoMurdered]")
SCdag <- model2network("[Abruising|Acause][Adisease|Acause][Bbruising|Bcause][Bdisease|Bcause][Acause][Bcause|Acause][NoMurdered|Acause:Bcause][Guilty|NoMurdered]")


#plot 
graphviz.plot(SallyClarkDAG)


#CPTs as used in Fenton & al.
AcauseProb <-prior.CPT("Acause","SIDS","Murder",0.921659)
AbruisingProb <- single.CPT("Abruising","Acause","Yes","No","SIDS","Murder",0.01,0.05)
AdiseaseProb <- single.CPT("Adisease","Acause","Yes","No","SIDS","Murder",0.05,0.001)
BbruisingProb <- single.CPT("Bbruising","Bcause","Yes","No","SIDS","Murder",0.01,0.05)
BdiseaseProb <- single.CPT("Bdisease","Bcause","Yes","No","SIDS","Murder",0.05,0.001)
BcauseProb <- single.CPT("Bcause","Acause","SIDS","Murder","SIDS","Murder",0.9993604,1-0.9998538)

#E goes first; order: last variable through levels, second last, then first
NoMurderedProb <- array(c(0, 0, 1, 0, 1, 0, 0,1,0,1,0,0), dim = c(3, 2, 2),dimnames = list(NoMurdered = c("both","one","none"),Bcause = c("SIDS","Murder"), Acause = c("SIDS","Murder")))

#this one is definitional
GuiltyProb <-  array(c( 1,0, 1,0, 0,1), dim = c(2,3),dimnames = list(Guilty = c("Yes","No"), NoMurdered = c("both","one","none")))

# Put CPTs together
SallyClarkCPTfenton <- list(Acause=AcauseProb,Adisease = AdiseaseProb,
                      Bcause = BcauseProb,Bdisease=BdiseaseProb,
                      Abruising = AbruisingProb,Bbruising = BbruisingProb,
                      NoMurdered = NoMurderedProb,Guilty=GuiltyProb)

# join with the DAG to get a BN
SCfenton <- custom.fit(SallyClarkDAG,SallyClarkCPTfenton)







#building samples: general tools
n <- 1000
ps <- seq(0,1,  length.out =n)


distroNorm <- function(mean, sigma){
distro <-   dnorm(ps, mean, sigma)
distro <- distro/sum(distro)
return(distro)
}

sampleNorm <- function(mean, sigma){
  sample(ps, size = 1e4, replace = TRUE,prob = distroNorm(mean,sigma))
}


distroBeta <- function(a, b){
  distro <-   dbeta(ps, a, b)
  distro <- distro/sum(distro)
  return(distro)
}


sampleBeta <- function(a, b){
  sample(ps, size = 1e4, replace = TRUE,prob = distroBeta(a,b))
}


plotSample <- function (sample, title, subtitle){
  ggplot()+theme_tufte()+xlab(expression(theta))+
    ylab("density")+theme(plot.title.position = "plot")+
    ggtitle(title)+
    geom_density(aes(x = sample ))+
    labs(title = title, subtitle = subtitle)
  }


distroFromSamples <- function (samples){
distro <-  density(samples, n = 1000)$y
distro <- distro/sum(distro)
return(distro)
}



#now defining the DF

set.seed(66)
AsidsPrior <- sampleBeta(8,1)
AsidsPriorPlot <- plotSample(AsidsPrior, "Asids prior", paste("Beta(9,1), median =", round(median(AsidsPrior),2), sep = "") )

AbruisingIfSids <- sampleNorm(.02,.04)
AbruisingIfSidsPlot <- plotSample(AbruisingIfSids, "AbruisingIfSids", 
                                  paste("Norm(.02,.04), median =", round(median(AbruisingIfSids),2), sep = "") )

AbruisingIfMurder <- sampleBeta(5,30)
AbruisingIfMurderPlot <- plotSample(AbruisingIfMurder, "AbruisingIfMurder",
                                    paste("Beta(5,30), median =", round(median(AbruisingIfMurder),2), sep = "") )
AbruisingIfMurderPlot


AdiseaseIfSids <- sampleNorm(.07,.3)
AdiseaseIfSidsPlot <- plotSample(AdiseaseIfSids, "AdiseaseIfSids",
                                    paste("Norm(.07,.3), median =", round(median(AdiseaseIfSids),2), sep = "") )


AdiseaseIfMurder <- sampleNorm(.002,.05)
AdiseaseIfMurderPlot <- plotSample(AdiseaseIfMurder, "AdiseaseIfMurder",
                                 paste("Norm(.002,.05), median =", round(median(AdiseaseIfMurder),2), sep = "") )


BbruisingIfSids <- sampleNorm(.02,.04)
BbruisingIfSidsPlot <- plotSample(BbruisingIfSids, "BbruisingIfSids",
                                   paste("Norm(.03,.08), median =", round(median(BbruisingIfSids),2), sep = "") )


BbruisingIfMurder <- sampleBeta(6,4)
BbruisingIfMurderPlot <- plotSample(BbruisingIfMurder, "BbruisingIfMurder",
                                  paste("Norm(.03,.08), median =", round(median(BbruisingIfMurder),2), sep = "") )



BdiseaseIfSids <- sampleNorm(.07,.3)
BdiseaseIfSidsPlot <- plotSample(BdiseaseIfSids, "BdiseaseIfSids",
                                    paste("Norm(.03,.08), median =", round(median(BdiseaseIfSids),2), sep = "") )


BdiseaseIfAmurder   <- sampleNorm(.002,.05)
BdiseaseIfAmurderPlot <- plotSample(BdiseaseIfAmurder, "BdiseaseIfAmurder",
                                   paste("Norm(.002,.05), median =", round(median(BdiseaseIfAmurder),2), sep = "") )


BcauseSidsIfAsids  <- sampleBeta(15,4)
BcauseSidsIfAsidsPlot <- plotSample(BcauseSidsIfAsids, "BcauseSidsIfAsids",
                                   paste("Norm(.002,.05), median =", round(median(BcauseSidsIfAsids),2), sep = "") )

BcauseSidsIfAmurder <- sampleNorm(.0015,.04)
BcauseSidsIfAmurderPlot <- plotSample(BcauseSidsIfAmurder, "BcauseSidsIfAmurder",
                                   paste("Norm(.002,.05), median =", round(median(BcauseSidsIfAmurder),2), sep = "") )

BcauseSidsIfAmurderPlot



SCprobsDF <- data.frame(AsidsPrior = AsidsPrior, 
                        AbruisingIfSids = AbruisingIfSids, 
                        AbruisingIfMurder = AbruisingIfMurder, 
                        AdiseaseIfSids = AdiseaseIfSids,
                        AdiseaseIfMurder = AdiseaseIfMurder,
                        BbruisingIfSids = BbruisingIfSids,
                        BbruisingIfMurder = BbruisingIfMurder,
                        BdiseaseIfSids = BdiseaseIfSids,
                        BdiseaseIfAmurder = BdiseaseIfAmurder,
                        BcauseSidsIfAsids = BcauseSidsIfAsids,
                        BcauseSidsIfAmurder = BcauseSidsIfAmurder) 

SCBNs <- list()

nrow(SCprobsDF)

startTime <- Sys.time()
for(i in 1: nrow(SCprobsDF)){
AcauseP <-prior.CPT("Acause","SIDS","Murder", SCprobsDF$AsidsPrior[i])
AbruisingP <- single.CPT("Abruising","Acause","Yes","No","SIDS","Murder",SCprobsDF$AbruisingIfSids[i],SCprobsDF$AbruisingIfMurder[i])
AdiseaseP <- single.CPT("Adisease","Acause","Yes","No","SIDS","Murder",SCprobsDF$AdiseaseIfSids[i],SCprobsDF$AdiseaseIfMurder[i])
BbruisingP <- single.CPT("Bbruising","Bcause","Yes","No","SIDS","Murder", SCprobsDF$BbruisingIfSids[i],SCprobsDF$BbruisingIfMurder[i])
BdiseaseP <- single.CPT("Bdisease","Bcause","Yes","No","SIDS","Murder",SCprobsDF$BdiseaseIfSids[i],SCprobsDF$BdiseaseIfAmurder[i])
BcauseP <- single.CPT("Bcause","Acause","SIDS","Murder","SIDS","Murder", SCprobsDF$BcauseSidsIfAsids[i],SCprobsDF$BcauseSidsIfAmurder[i])

#E goes first; order: last variable through levels, second last, then first
NoMurderedP <- array(c(0, 0, 1, 0, 1, 0, 0,1,0,1,0,0), dim = c(3, 2, 2),dimnames = list(NoMurdered = c("both","one","none"),Bcause = c("SIDS","Murder"), Acause = c("SIDS","Murder")))

#this one is definitional
GuiltyP <-  array(c( 1,0, 1,0, 0,1), dim = c(2,3),dimnames = list(Guilty = c("Yes","No"), NoMurdered = c("both","one","none")))

# Put CPTs together
SCcpt <- list(Acause=AcauseP,Adisease = AdiseaseP,
                            Bcause = BcauseP,
                            Bdisease=BdiseaseP,
                            Abruising = AbruisingP,
                            Bbruising = BbruisingP,
                            NoMurdered = NoMurderedP,
                            Guilty=GuiltyP)

# join with the DAG to get a BN
SCBNs[[i]] <- custom.fit(SCdag,SCcpt)
}
endTime <- Sys.time()
time <- endTime - startTime


time


#priors
startTime <- Sys.time()
GuiltPrior <- numeric(length(SCBNs))
i <- 1
for (i in 1:length(SCBNs)){
SCJN <- compile(as.grain(SCBNs[[i]]))
GuiltPrior[i] <- querygrain(SCJN, node = "Guilty")[[1]][1]
}
endTime <- Sys.time()
time <- endTime - startTime



GuiltPriorPlot <- plotSample(GuiltPrior, "GuiltPrior",
                          paste("Norm(median =", 
                   round(median(GuiltPrior),2), ", 89%HPDI = ", round(HPDI(GuiltPrior),2)[1],"-",round(HPDI(GuiltPrior),2)[2], sep = "") )

GuiltPriorPlot


#bruising on A
startTime <- Sys.time()
GuiltAbruising <- numeric(length(SCBNs))
for (i in 1:length(SCBNs)){
SCJN <- compile(as.grain(SCBNs[[i]]))
SCJNA <- setEvidence(SCJN,nodes = c("Abruising"), states = c("Yes"))
GuiltAbruising[i] <- querygrain(SCJNA, node = "Guilty")[[1]][1]
}
endTime <- Sys.time()
time <- endTime - startTime

time



GuiltAbruisingPlot <- plotSample(GuiltAbruising, "GuiltAbruising",
                             paste("median =", 
                                   round(median(GuiltAbruising),2), ", 89%HPDI = ", round(HPDI(GuiltAbruising),2)[1],"-",
                                   round(HPDI(GuiltAbruising),2)[2], sep = "") )


GuiltAbruisingPlot


#bruising on both
startTime <- Sys.time()
GuiltABbruising <- numeric(length(SCBNs))
for (i in 1:length(SCBNs)){
  SCJN <- compile(as.grain(SCBNs[[i]]))
  SCJNAB <- setEvidence(SCJN,nodes = c("Abruising", "Bbruising"), states = c("Yes","Yes"))
  GuiltABbruising[i] <- querygrain(SCJNAB, node = "Guilty")[[1]][1]
}
endTime <- Sys.time()
time <- endTime - startTime

time


GuiltABbruisingPlot <- plotSample(GuiltABbruising, "GuiltABbruising",
                                 paste("median =", 
                                       round(median(GuiltABbruising),2), ", 89%HPDI = ", round(HPDI(GuiltABbruising),2)[1],"-",
                                       round(HPDI(GuiltABbruising),2)[2], sep = "") )


GuiltABbruisingPlot




#Also, no disease
startTime <- Sys.time()
GuiltABbruisingNoDisease <- numeric(length(SCBNs))
for (i in 1:length(SCBNs)){
  SCJN <- compile(as.grain(SCBNs[[i]]))
  SCJNABNoDisease <- setEvidence(SCJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"),
                        states = c("Yes","Yes", "No", "No"))
  GuiltABbruisingNoDisease[i] <- querygrain(SCJNABNoDisease, node = "Guilty")[[1]][1]
}
endTime <- Sys.time()
time <- endTime - startTime

time



GuiltABbruisingNoDiseasePlot <- plotSample(GuiltABbruisingNoDisease, "GuiltABbruisingNoDisease",
                                  paste("median =", 
                                        round(median(GuiltABbruisingNoDisease),2), ", 89%HPDI = ", round(HPDI(GuiltABbruisingNoDisease),2)[1],"-",
                                        round(HPDI(GuiltABbruisingNoDisease),2)[2], sep = "") )


GuiltABbruisingNoDiseasePlot


#now, with disease in Child A
startTime <- Sys.time()
GuiltABbruisingDiseaseA <- numeric(length(SCBNs))
for (i in 1:length(SCBNs)){
  SCJN <- compile(as.grain(SCBNs[[i]]))
  SCJNABDiseaseA <- setEvidence(SCJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"),
                                 states = c("Yes","Yes", "Yes", "No"))
  GuiltABbruisingDiseaseA[i] <- querygrain(SCJNABDiseaseA, node = "Guilty")[[1]][1]
}
endTime <- Sys.time()
time <- endTime - startTime

time




GuiltABbruisingDiseaseAPlot <- plotSample(GuiltABbruisingDiseaseA, "GuiltABbruisingDiseaseA",
                                           paste("median =", 
                                                 round(median(GuiltABbruisingDiseaseA, na.rm=TRUE),2), ", 89%HPDI = ", 
                                                 round(HPDI(GuiltABbruisingDiseaseA),2)[1],"-",
                                                 round(HPDI(GuiltABbruisingDiseaseA),2)[2], sep = "") )


GuiltABbruisingDiseaseAPlot











