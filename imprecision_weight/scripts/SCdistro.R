library(bnlearn)
library(Rgraphviz)
library(gRain)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)




#setwd("imprecision_weight/")





#source("scripts/cptCreate.R")


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




#evidential stages with Fenton's BN

SCfJN <- compile(as.grain(SCfenton))

priorFenton <- querygrain(SCfJN, node = "Guilty")[[1]][1]

SCfJNAbruising <- setEvidence(SCfJN, nodes = c("Abruising"), states = c("Yes"))
(AbruisingFenton <- querygrain(SCfJNAbruising, node = "Guilty")[[1]][1])

SCfJNAbruisingBbruising <- setEvidence(SCfJN, nodes = c("Abruising","Bbruising"),
                                states = c("Yes","Yes"))

(AbruisingBbruisingFenton <- querygrain(SCfJNAbruisingBbruising, node = "Guilty")[[1]][1])

SCfJNAbruisingBbruisingNoDisease <- setEvidence(SCfJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"),
                                         states = c("Yes","Yes", "No", "No"))
(AbruisingBbruisingFentonNoDiseaseFenton <-   querygrain(SCfJNAbruisingBbruisingNoDisease, node = "Guilty")[[1]][1])


SCfJNAbruisingBbruisingDiseaseA <- setEvidence(SCfJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"), 
                                               states = c("Yes","Yes", "Yes", "No"))
(AbruisingBbruisingFentonDiseaseAFenton <- querygrain(SCfJNAbruisingBbruisingDiseaseA, node = "Guilty")[[1]][1])


SCfentonTable <- data.frame(stage = factor(c("prior", "bruising in A", "bruising in both",
                                      "bruising in both, no disease", "bruising in both, disease on A only"),
                                      levels = c("prior", "bruising in A", "bruising in both",
                                                 "bruising in both, no disease", "bruising in both, disease on A only")),
                            probability = c(priorFenton,AbruisingFenton,AbruisingBbruisingFenton,
                                            AbruisingBbruisingFentonNoDiseaseFenton,AbruisingBbruisingFentonDiseaseAFenton))



ggplot(SCfentonTable) + geom_point(aes(x = stage, y = probability, size = probability))+
  scale_x_discrete(limits=rev, expand = c(0, 2)) +coord_flip()+theme_tufte(base_size = 14)+ scale_size(guide="none")+
  theme(plot.title.position = "plot")+ggtitle("Impact of evidence according to Fenton's BN for the Sally Clark case") +
  geom_text(aes(x = stage, y= probability, label= round(probability,2) ,hjust=-.45, vjust=-.4), size  = 4)+
  scale_y_continuous(breaks = seq(0,.7, by =.1), limits = c(0,.8))



#building samples: general tools
n <- 1000
ps <- seq(0,1,  length.out =n)





#now defining the DF

set.seed(66)
AsidsPrior <- sampleBeta(8,1)
AbruisingIfSids <- sampleNorm(.02,.04)
AbruisingIfMurder <- sampleBeta(5,30)
AbruisingIfMurderPlot


AdiseaseIfSids <- sampleNorm(.07,.3)

AdiseaseIfMurder <- sampleNorm(.002,.05)


BbruisingIfSids <- sampleNorm(.02,.04)


BbruisingIfMurder <- sampleBeta(6,4)



BdiseaseIfSids <- sampleNorm(.07,.3)


BdiseaseIfAmurder   <- sampleNorm(.002,.05)


BcauseSidsIfAsids  <- sampleBeta(15,4)

BcauseSidsIfAmurder <- sampleNorm(.0015,.04)




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
timeBuildingBNs <- endTime - startTime


timeBuildingBNs


#adding probabilities to DF

GuiltPrior <- numeric(length(SCBNs))


AbruisingPrior <- numeric(length(SCBNs))
AdiseasePrior <- numeric(length(SCBNs))

GuiltAbruising <- numeric(length(SCBNs))
GuiltAbruisingNo <- numeric(length(SCBNs))

GuiltAdisease <- numeric(length(SCBNs))
GuiltAdiseaseNo <- numeric(length(SCBNs))

GuiltABbruising <- numeric(length(SCBNs))
GuiltABbruisingNoDisease <- numeric(length(SCBNs))

GuiltABbruisingDiseaseA <- numeric(length(SCBNs))



startTime <- Sys.time()
for (i in 1:length(SCBNs)){
  

  SCJN <- compile(as.grain(SCBNs[[i]]))
GuiltPrior[i] <- querygrain(SCJN, node = "Guilty")[[1]][1]
AbruisingPrior[i] <- querygrain(SCJN, node = "Abruising")[[1]][1]
AdiseasePrior[i] <- querygrain(SCJN, node = "Adisease")[[1]][1]


#Abruising Yes
SCJNA <- setEvidence(SCJN,nodes = c("Abruising"), states = c("Yes"))
GuiltAbruising[i] <- querygrain(SCJNA, node = "Guilty")[[1]][1]

GuiltAbruising[i]

#Abruising No
SCJNANo <- setEvidence(SCJN,nodes = c("Abruising"), states = c("No"))
GuiltAbruisingNo[i] <- querygrain(SCJNANo, node = "Guilty")[[1]][1]


#Adisease Yes
SCJNAd <- setEvidence(SCJN,nodes = c("Adisease"), states = c("Yes"))
GuiltAdisease[i] <- querygrain(SCJNAd, node = "Guilty")[[1]][1]

#Adisease No
SCJNAdNo <- setEvidence(SCJN,nodes = c("Adisease"), states = c("No"))
GuiltAdiseaseNo[i] <- querygrain(SCJNAdNo, node = "Guilty")[[1]][1]


#Bruising in both
SCJNAB <- setEvidence(SCJN,nodes = c("Abruising", "Bbruising"), states = c("Yes","Yes"))
GuiltABbruising[i] <- querygrain(SCJNAB, node = "Guilty")[[1]][1]

#Bruising in both no disease
SCJNABNoDisease <- setEvidence(SCJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"),
                               states = c("Yes","Yes", "No", "No"))

GuiltABbruisingNoDisease[i] <- querygrain(SCJNABNoDisease, node = "Guilty")[[1]][1]

#bruising in bith disease in first
SCJNABDiseaseA <- setEvidence(SCJN, nodes = c("Abruising","Bbruising","Adisease","Bdisease"),
                              states = c("Yes","Yes", "Yes", "No"))

GuiltABbruisingDiseaseA[i] <- querygrain(SCJNABDiseaseA, node = "Guilty")[[1]][1]
}
endTime <- Sys.time()
timeProbs <- endTime - startTime


timeProbs

saveRDS(SCBNs, "datasets/SCBNs.rds")



SCprobsFinal <- cbind(SCprobsDF,GuiltPrior, AbruisingPrior, AdiseasePrior,
                      GuiltAbruising,  GuiltAbruisingNo, GuiltAdisease,  
                      GuiltAdiseaseNo, GuiltABbruising,
                      GuiltABbruisingNoDisease,
                      GuiltABbruisingDiseaseA)


saveRDS(SCprobsFinal, "datasets/SCprobsFinal.rds")









