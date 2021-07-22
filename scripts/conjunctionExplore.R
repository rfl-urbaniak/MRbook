library(ggplot2)
library(ggthemes)
library(plot3D)

getwd() #this should be the project directory

conjunctionTable <- readRDS(file = "datasets/conjunctionTable.RDS")

nrow(conjunctionTable)
colnames(conjunctionTable)
attach(conjunctionTable)


#how often is the joint bf below each individual Bfs?
mean(BFABs < BFAs & BFABs < BFBs)

mean(BFABs<BFAs)
mean(BFABs<BFBs)


conjunctionTable$maxBF <- pmax(BFAs, BFBs)

conjunctionTable$BFdifs <- conjunctionTable$BFABs - conjunctionTable$maxBF 

ggplot(conjunctionTable)+geom_histogram(aes(x=BFdifs), bins= 90)+
  xlim(c(-10,10))




#how often is the joint LR below each individual LR?
mean(LRABs < LRAs & LRABs < LRBs)

mean(LRABs<LRAs)
mean(LRABs<LRBs)


conjunctionTable$maxLR <- pmax(LRAs, LRBs)

conjunctionTable$LRdifs <- conjunctionTable$LRABs - conjunctionTable$maxLR 

ggplot(conjunctionTable)+geom_histogram(aes(x=LRdifs), bins= 80)+
  xlim(c(-10,10))





#now let's restrict this to positive LR support, 
#that is, we want LRAs and LRBs >1

positiveLR <- conjunctionTable[conjunctionTable$LRAs > 1 & conjunctionTable$LRBs > 1,]

nrow(positiveLR)

#note the joint support is positive
mean(positiveLR$LRABs > 1)

#is it always higher than the minumum?


#however, it is not always higher than the max
ggplot(positiveLR)+geom_histogram(aes(x=positiveLR$LRdifs), bins= 80)+
  xlim(c(-10,10))


mean(positiveLR$LRdifs>0)



#is joint LR always between the individual LRs?
mean((conjunctionTable$LRABs > conjunctionTable$LRAs &
       conjunctionTable$LRABs < conjunctionTable$LRBs)|
       (conjunctionTable$LRABs < conjunctionTable$LRAs &
          conjunctionTable$LRABs > conjunctionTable$LRBs))
#no!

mean((conjunctionTable$LRABs < conjunctionTable$LRAs) &
       (conjunctionTable$LRABs < conjunctionTable$LRBs))
#


#is joint LR always between the individual LRs for positive support?
mean((positiveLR$LRABs > positiveLR$LRAs &
        positiveLR$LRABs < positiveLR$LRBs)|
       (positiveLR$LRABs < positiveLR$LRAs &
          positiveLR$LRABs > positiveLR$LRBs))

#also no!

mean((positiveLR$LRABs < positiveLR$LRAs) &
       (positiveLR$LRABs < positiveLR$LRBs))
#yes!



#if LR the same, being above minimum is above the both of them



#now, similarly, let's restrict this to positive BF support, 

positiveBF <- conjunctionTable[conjunctionTable$BFAs > 1 &
                                 conjunctionTable$BFBs > 1,]

nrow(positiveBF)

#note the joint support is positive
mean(positiveBF$BFABs > 1)

mean(positiveBF$BFdifs > 0)

#however, note BF is also always higher than the max
ggplot(positiveBF)+geom_histogram(aes(x=positiveBF$BFdifs), bins= 80)+
  xlim(c(-100,100))

mean(positiveBF$BFdifs>0)



#is joint LR sensitive to some priors?












