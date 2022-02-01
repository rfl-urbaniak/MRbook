library(tidyverse)

con <- readRDS("datasets/conjunctionTable3(jointEvidence).RDS")
conDep <- readRDS("datasets/conjunctionTableDep3(fixedEvidence).RDS")
con2Dep <- readRDS("datasets/doubleDependencyTable3(fixedEvidence).RDS")

#first BN1
colnames(con)

con$maxLRab <- pmax(con$LRAabs, con$LRBabs)
con$minLRab <- pmin(con$LRAabs, con$LRBabs)
con$LRdifsMax <- con$LRABs - con$maxLRab 
con$LRdifsMin <- con$LRABs - con$minLRab 

conDep$maxLRab <- pmax(conDep$LRAabs, conDep$LRBabs)
conDep$minLRab <- pmin(conDep$LRAabs, conDep$LRBabs)
conDep$LRdifsMax <- conDep$LRABs - conDep$maxLRab 
conDep$LRdifsMin <- conDep$LRABs - conDep$minLRab 

con2Dep$maxLRab <- pmax(con2Dep$LRAabs, con2Dep$LRBabs)
con2Dep$minLRab <- pmin(con2Dep$LRAabs, con2Dep$LRBabs)
con2Dep$LRdifsMax <- con2Dep$LRABs - con2Dep$maxLRab 
con2Dep$LRdifsMin <- con2Dep$LRABs - con2Dep$minLRab 




LRabFails <- con %>% filter(LRAabs > LRABs & LRBabs > LRABs )
nrow(LRabFails)/nrow(con)
#!!!
#If no assumption about the direction of support is made, 
#around 12% of the time  (around twice less often if the usual LRs are used), 
#the separate LRs with fixed evidence are both greater than the joint LR.
LRabFails2 <- conDep %>% filter(LRAabs > LRABs & LRBabs > LRABs )
nrow(LRabFails2)/nrow(conDep)
## The frequency goes slightly up to around 14\% if we switch to BN2.
LRabFails3 <- con2Dep %>% filter(LRAabs > LRABs & LRBabs > LRABs )
nrow(LRabFails3)/nrow(con2Dep)
## and is only .4\% higher if additionally we allow for the dependency between the items of evidence.





colnames(con)

plotLRabindAbove <- con %>% filter(LRAabs > 1 & LRBabs > 1) %>% ggplot( aes(x = LRdifsMax))+
  geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(LR[AB] - max,"(",LR[A,ab], ", ", LR[B,ab],")")))+ggtitle("Compared to  the maximum")+
  labs(subtitle = expression(paste("Assuming ",  LR[Aab], ", ",   LR[Bab] > 1)))+xlim(c(-10,3))




plotLRabindAbove2 <- con %>% filter(LRAabs > 1 & LRBabs > 1) %>% ggplot( aes(x = LRdifsMin))+
  geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(LR[AB] - min,"(",LR[Aab], ", ", LR[Bab],")")))+ggtitle("Compared to  the minimum")+
  labs(subtitle = expression(paste("Assuming ",  LR[Aab], ", ",   LR[Bab] > 1)))+xlim(c(-1,15))

plotLRabindAbove2




aboveLR <-  con %>% filter(LRAabs > 1 & LRBabs > 1)
aboveLR2 <-  conDep %>% filter(LRAabs > 1 & LRBabs > 1)
aboveLR3 <-  con2Dep %>% filter(LRAabs > 1 & LRBabs > 1)

mean(aboveLR$LRABs > aboveLR$minLR & aboveLR$LRABs < aboveLR$maxLR)
mean(aboveLR2$LRABs > aboveLR2$minLR & aboveLR2$LRABs < aboveLR2$maxLR)
mean(aboveLR3$LRABs > aboveLR3$minLR & aboveLR3$LRABs < aboveLR3$maxLR)

## !!!
## Around 73\% of joint likelihoods (75% for BN2, 70\% for BN3) are between the individual ones, 
mean(aboveLR$LRABs < aboveLR$minLR)
mean(aboveLR2$LRABs < aboveLR2$minLR)
mean(aboveLR3$LRABs < aboveLR3$minLR)


## no joint LR is below the minimal for BN1, but already 1.2\% for BN2 and 2\% for BN3.


mean(aboveLR$LRABs > aboveLR$LRAabs & aboveLR$LRABs > aboveLR$LRBabs)
mean(aboveLR2$LRABs > aboveLR2$LRAabs & aboveLR2$LRABs > aboveLR2$LRBabs)
mean(aboveLR3$LRABs > aboveLR3$LRAabs & aboveLR3$LRABs > aboveLR3$LRBabs)

##!! ## around 27\% of the time, the joint LR is strictly greater than both of the individual LRs with evidence fixed for BN1, 
##23\% for BN2, and 27\% for BN3.




