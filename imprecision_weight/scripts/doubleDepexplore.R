library(tidyverse)


doubleTable <-  readRDS(file = "datasets/doubleDependencyTable.RDS")

str(doubleTable)

doubleTable$maxBF <- pmax(doubleTable$BFAs, doubleTable$BFBs)
doubleTable$minBF <- pmin(doubleTable$BFAs, doubleTable$BFBs)
doubleTable$BFdifsMax <- doubleTable$BFABs - doubleTable$maxBF
doubleTable$BFdifsMin <- doubleTable$BFABs - doubleTable$minBF

doubleTable$maxLR <- pmax(doubleTable$LRAs, doubleTable$LRBs)
doubleTable$minLR <- pmin(doubleTable$LRAs, doubleTable$LRBs)
doubleTable$LRdifsMax <- doubleTable$LRABs - doubleTable$maxLR 
doubleTable$LRdifsMin <- doubleTable$LRABs - doubleTable$minLR 


positiveBF <- doubleTable %>% filter (BFAs >1 & BFBs > 1 )



nrow(focus)/nrow(positiveBF)

focus <- doubleTable %>% filter (BFAs >1 & BFBs > 1 & BFABs < BFAs & BFABs < BFBs & LRABs < LRAs & LRABs < LRBs)



plotBFfailure <- positiveBF %>% ggplot( aes(x = BFdifsMin))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(BF[AB] - min,"(",BF[A], ", ", BF[B],")")))+ggtitle("Joint BFs compared to  the minimum")+
  labs(subtitle = expression(paste("Assuming ",  BF[A], ", ",   BF[B] > 1)))+xlim(c(-3,3))

plotLRfailure <- positiveBF %>% ggplot( aes(x = LRdifsMin))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(LR[AB] - min,"(",LR[A], ", ", LR[B],")")))+ggtitle("Joint LRs compared to  the minimum")+
  labs(subtitle = expression(paste("Assuming ",  LR[A], ", ",   LR[B] > 1)))+xlim(c(-3,3))

plotLRfailure


