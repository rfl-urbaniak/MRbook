library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plot3D)
library(plotly)


old <- theme_set(theme_tufte())

getwd() #this should be the project directory

conjunctionTable2 <- readRDS(file = "datasets/conjunctionTable2raf.RDS")



nrow(conjunctionTable2)
colnames(conjunctionTable2)
attach(conjunctionTable2)


#how often is the joint bf below each individual Bfs?
mean(BFABs < BFAs & BFABs < BFBs)





conjunctionTable2 


scatter3D(conjunctionTable2$BFAs,conjunctionTable2$BFBs,conjunctionTable2$BFABs,pch=3,cex=0.3,byt="g",alpha=0.8,theta=50, phi=8,xlab= "BF(A)", ylab="BF(B)",zlab="BF(AB)",main="Joint Bayes factor as a function of individual Bayes factors", zlim = c(0,2), xlim = c(0, 2), ylim = c(0,2),cex.main =0.8)

BFfails <- conjunctionTable2 %>% filter(BFAs > BFABs & BFBs > BFABs ) 
 scatter3D(BFfails$BFAs,BFfails$BFBs,BFfails$BFABs,pch=3,cex=0.3, colvar = NULL, box = TRUE, grid = TRUE,theta=50, phi = 10, axis.ticks = TRUE, ticktype= "detailed",xlab= "BF(A)", ylab="BF(B)",zlab="BF(AB)",main="Cases in which BF(AB) < BF(A), BF(B) (frequency=.25)")

LRfails <- conjunctionTable2 %>% filter(LRAs > LRABs & LRBs > LRABs ) 
 scatter3D(LRfails$LRAs,LRfails$LRBs,LRfails$LRABs,pch=3,cex=0.3, colvar = NULL, 
box = TRUE, grid = TRUE,theta=50, phi = 10, axis.ticks = TRUE, ticktype= "detailed",xlab= "LR(A)", ylab="LR(B)",zlab="LR(AB)",main="Cases in which LR(AB) < LR(A), LR(B) (frequency=.125)")




mean(BFABs<BFAs)
mean(BFABs<BFBs)


# does the multiplicative claim  hold?
mean(BFABs == BFAs * BFBs)

conjunctionTable2$BFAtimesBs <- conjunctionTable2$BFAs * conjunctionTable2$BFBs

mult2 <- lm(data = conjunctionTable2, formula =  BFABs ~ BFAtimesBs)

cor(conjunctionTable2$BFAtimesBs, conjunctionTable2$BFABs)

summary(mult2)

ggplot(conjunctionTable2, aes(y = BFABs, x = (BFAs * BFBs)))+geom_point(alpha= 0.2, size = .5)+
  xlab(expression(paste(BF[A], "*", BF[B] )))+
  ylab(expression(paste(BF[AB])))+
  ggtitle("Multiplicative claim fails DAG2")+labs(subtitle = "Pearson's correlation coefficient = .95")

       
       # YES IN BN1, no in BN2?


# choose the maximal individual BF
conjunctionTable2$maxBF <- pmax(BFAs, BFBs)
# choose the minimal individual BF
conjunctionTable2$minBF <- pmin(BFAs, BFBs)

conjunctionTable2$BFdifsMax <- conjunctionTable2$BFABs - conjunctionTable2$maxBF 

conjunctionTable2$BFdifsMin <- conjunctionTable2$BFABs - conjunctionTable2$minBF 



conjunctionTable2$maxLR <- pmax(conjunctionTable2$LRAs, conjunctionTable2$LRBs)
conjunctionTable2$minLR <- pmin(conjunctionTable2$LRAs, conjunctionTable2$LRBs)
conjunctionTable$LRdifsMax <- conjunctionTable2$LRABs - conjunctionTable2$maxLR 
conjunctionTable2$LRdifsMin <- conjunctionTable2$LRABs - conjunctionTable2$minLR 

#now suppose the antecendents are false, is joint BF still higher?
plotBFindBelow <- conjunctionTable2 %>% filter(BFAs < 1 & BFBs < 1) %>% ggplot( aes(x = BFdifsMin))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(BF[AB] - min,"(",BF[A], ", ", BF[B],")")))+ggtitle("Distance of joint BF from the minimal individual BFs for BN2")+
  labs(subtitle = expression(paste("Assuming ",  BF[A], ", ",   BF[B] < 1)))+xlim(c(-.3,.1))


plotBFindBelow



# joint BF is still above max if BFs are above 1
plotBFindAbove <- conjunctionTable2 %>% filter(BFAs > 1 & BFBs > 1) %>% ggplot( aes(x = BFdifsMax))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(BF[AB] - max,"(",BF[A], ", ", BF[B],")")))+ggtitle("Distance of joint BF from the maximal individual BFs for BN1")+
  labs(subtitle = expression(paste("Assuming ",  BF[A], ", ",   BF[B] > 1)))+xlim(c(-1,3))

plotBFindAbove





mean(BFAs < 1 & BFBs < 1 & BFABs > BFAs & BFABs > BFBs)



#how often is the joint LR below each individual LR?
mean(LRABs < LRAs & LRABs < LRBs)



mean(LRABs<LRAs)
mean(LRABs<LRBs)



conjunctionTable2$maxLR <- pmax(LRAs, LRBs)
conjunctionTable2$minLR <- pmin(LRAs, LRBs)


conjunctionTable2$LRdifsMax <- conjunctionTable2$LRABs - conjunctionTable2$maxLR 
conjunctionTable2$LRdifsMin <- conjunctionTable2$LRABs - conjunctionTable2$minLR 


ggplot(conjunctionTable2)+geom_histogram(aes(x=LRdifsMax), bins= 80)+
  xlim(c(-10,10))


#ALICJA: VISUALISE DISTRIBUTION FAIL

 
# MARCELLO: DIFFERENCE BETWEEN COMBINED LR AND MAX INDIVIDUAL LR IS OFTEN NEGATUVE


# MARCELLO'S ADDITION: DIFFERENCE BETWEEN COMBINED LR AND MIN OF THE INDIVIDUAL LR




ggplot(conjunctionTable2)+geom_histogram(aes(x=LRdifsMin), bins= 80)+
  xlim(c(-1,10))
# MARCELLO: DIFFERENCE BETWEEN COMBINED LR AND MIN INDIVIDUAL LR IS OFTEN POSITIVE
#R: yeah, like 12% is below
mean(conjunctionTable2$LRABs < conjunctionTable2$LRAs & conjunctionTable2$LRABs < conjunctionTable2$LRBs )












plotLRindBelow2 <- conjunctionTable2 %>% filter(LRAs < 1 & LRBs < 1) %>% ggplot( aes(x = LRdifsMin))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(LR[AB] - min,"(",LR[A], ", ", LR[B],")")))+ggtitle("Compared to the the minimum")+
  labs(subtitle = expression(paste("Assuming ",  LR[A], ", ",   LR[B] < 1, " ", (DAG2))))

plotLRindBelow2


plotLRindBelow2 <- conjunctionTable %>% filter(LRAs < 1 & LRBs < 1) %>% ggplot( aes(x = LRdifsMax))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(LR[AB] - max,"(",LR[A], ", ", LR[B],")")))+ggtitle("Compared to the the maximum")+
  labs(subtitle = expression(paste("Assuming ",  LR[A], ", ",   LR[B] < 1)))


#belowLR <- conjunctionTable %>% filter(LRAs < 1 & LRBs < 1)
#mean(belowLR$LRABs > belowLR$minLR & belowLR$LRABs < belowLR$maxLR)


LRbelowPlot <- ggarrange(plotLRindBelow,plotLRindBelow2, ncol = 2)

plotLRindAbove <- conjunctionTable %>% filter(LRAs > 1 & LRBs > 1) %>% ggplot( aes(x = LRdifsMax))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(LR[AB] - max,"(",LR[A], ", ", LR[B],")")))+ggtitle("Compared to  the maximum")+
  labs(subtitle = expression(paste("Assuming ",  LR[A], ", ",   LR[B] > 1)))+xlim(c(-10,3))


plotLRindAbove2 <- conjunctionTable %>% filter(LRAs > 1 & LRBs > 1) %>% ggplot( aes(x = LRdifsMin))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(LR[AB] - min,"(",LR[A], ", ", LR[B],")")))+ggtitle("Compared to  the minimum")+
  labs(subtitle = expression(paste("Assuming ",  LR[A], ", ",   LR[B] > 1)))+xlim(c(-1,15))

#aboveLR <-  conjunctionTable %>% filter(LRAs > 1 & LRBs > 1)
#mean(aboveLR$LRABs > aboveLR$minLR & aboveLR$LRABs < aboveLR$maxLR)



LRabovePlot <- ggarrange(plotLRindAbove,plotLRindAbove2, ncol = 2)









#now let's restrict this to positive LR support, 
#that is, we want LRAs and LRBs >1

positiveLR <- conjunctionTable2[conjunctionTable2$LRAs > 1 & conjunctionTable2$LRBs > 1,]

nrow(positiveLR)

#note the joint support is positive
mean(positiveLR$LRABs > 1)

#is it always higher than the minumum?

# MARCELLO'S ADDITION BELOW - CODE FOR THIS QUESTION WAS MISSING WHEN I WAS READING 

ggplot(positiveLR)+geom_histogram(aes(x=positiveLR$LRdifsMin), bins= 80)+
  xlim(c(-10,10))

mean(positiveLR$LRdifsMin>0)
# MARCELLO: COMBINED LR IS ALWAYS GREATER THAN MINIMUM INDIVIDUAL LR FOR POSITIVE LR

#however, it is not always higher than the max
ggplot(positiveLR)+geom_histogram(aes(x=positiveLR$LRdifs), bins= 80)+
  xlim(c(-10,10))

mean(positiveLR$LRdifs>0)
# MARCELLO: IN ABOUT 29% OF CASES COMBINED LR EXCEEDS THE MAX INDIVIDUAL LR

# MARCELLO:  **ADDITIONAL QUESTION**
# - CAN WE ANALYZE THOSE CASES OF "positiveLR$LRdifs>0" MORE CAREFULLY? 
# - UNDER WHAT CONDITIONS DOES COMBINED LR EXCEEDS MAX INDIVIDUAL LR?
# - IS IT WHEN THE TWO INDIVIDUAL LRs ARE VERY CLOSE OR THE SAME?

#is joint LR always between the individual LRs?
mean((conjunctionTable2$LRABs > conjunctionTable2$LRAs &
       conjunctionTable2$LRABs < conjunctionTable2$LRBs)|
       (conjunctionTable2$LRABs < conjunctionTable2$LRAs &
          conjunctionTable2$LRABs > conjunctionTable2$LRBs))
#no!

mean((conjunctionTable2$LRABs < conjunctionTable2$LRAs) &
       (conjunctionTable2$LRABs < conjunctionTable2$LRBs))
#


#is joint LR always between the individual LRs for positive support?
mean((positiveLR$LRABs > positiveLR$LRAs &
        positiveLR$LRABs < positiveLR$LRBs)|
       (positiveLR$LRABs < positiveLR$LRAs &
          positiveLR$LRABs > positiveLR$LRBs))
#also no!

# MARCELLO: FOR CLARITY WE SHOULD NOTE THAT JOINT LR IS NOT ALWAYS BETWEEN INDIVIDUAL LR
# BECAUSE SOMETIMES JOINT LR EXCEEDS BOTH THE INDIVIDUAL ONES
# NOT BECAUSE SOMETIMES JOINT LR IS BELOW BOTH OF THE INDIVIDUAL ONES

mean((positiveLR$LRABs < positiveLR$LRAs) &
       (positiveLR$LRABs < positiveLR$LRBs))
#yes!

# MARCELLO: THE ABOVE MEANS THAT JOINT LR IS *NEVER* BELOW BOTH THE INDIVIDUAL ONES
# IN OTHER WORDS, JOINT LR IS ALWAYS ABOVE THE SMALLEST OF THE INDIVIDUAL LR

#if LR the same, being above minimum is above the both of them

#now, similarly, let's restrict this to positive BF support, 

positiveBF <- conjunctionTable2[conjunctionTable2$BFAs > 1 &
                                 conjunctionTable2$BFBs > 1,]

nrow(positiveBF)

#note the joint support is positive
mean(positiveBF$BFABs > 1)

mean(positiveBF$BFdifs > 0)

#however, note BF is also always higher than the max
ggplot(positiveBF)+geom_histogram(aes(x=positiveBF$BFdifs), bins= 80)+
  xlim(c(-1,10))
mean(positiveBF$BFdifs>0)



# MARCELLO'S COMMENT. THIS SEEMS TO BE A DIFFERENCE BETWEEN JOINT LR AND JOINT BF
# JOINT BF IS ALWAYS HIGHER THAN POSITIVE INDIVIDUAL BF
# INSTEAD JOINT LR IS NOT ALWAYS HIGHER THAN POSITIVE INDIVIDUAL BF
# WHAT EXPLAINS THE DIFFERENCE BETWEEN JOINT BF AND JOINT LR?

# MARCELLO: **ADDITIONAL QUESTION**
# - CAN WE DO A COMPARISON BETWEEN BF AND LR?
# - E.G. CAN WE COMPARE HOW AN INDIVIDUAL BF CHANGES AS A FUNCTION OF VARIOUS PARAMETERS
# - AND COMPARE THOSE CHANGES TO THE CHANGES TO INDIVIDUAL LR DUE TO THE SAME PARAMETERS?
# - CAN WE DO THE SAME FOR JOINT LR AND JOINT BF?

#is joint LR sensitive to some priors?


# MARCELLO: COMMENTS ON THE **BEST NETWORK TO REPRESENT CONJUNCTION**
# OPTION 1: ARROWS INCOMING INTO A&B NODE: a<---A--->A&B<---B---->b
# OPTION 2: ARROWS OUTCOMING FROM A&B NODE: a<---A<---A&B--->B---->b
# OPTION 3: OPTION 1 PLUS ARROW BETWEEN A AND B
# OPTION 4: OPTION 2 PLUS ARROW BETWEEN A AND B
# THERE ARE NOT OTHER SENSIBLE OPTIONS, RIGHT?
# OPTION 1 CAPTURES THE MEANING OF &, BUT ALSO FORCES INDEPENDENCE BETWEEN A AND B
# OPTION 2 FAILS TO CAPTURE MEANING OF &, BUT ALLOWS FOR DEPENDENCE
# OPTION 3 HAS THE BEST OF BOTH OPTIONS 1 AND 2, SO IT MIGHT BE THE BEST OPTION OVERALL
# OPTION 4 FAILS TO CAPTURE & JUST LIKE OPTION 2
# SO OPTION 3 WINS?


# MARCELLO: **ADDITIONAL QUESTION**
# SINCE OPTION 3 ABOVE SEEMS THE MOST GENERAL NETWORK TO REPRESENT CONJUCTION
# CAN WE ASK THE SAME QUESTIONS IN THIS SIMULATION USING OPTION 3 AS NETWORK?

# MARCELLO: **ADDITIONAL QUESTION -- GENERALIZATION 1**
# CAN WE SAMPLE OVER A CLASS OF BAYESIAN NETWORKS GRAPHS?
# SAY WE DECIDE THAT "A-->a" and "B-->b", BUT LEAVE OPEN THE ARROWS BETWEEN A AND B
# SO WE SHOULD ASK THE SAME QUESTIONS AS ABOVE, BUT ACROSS THAT CLASS OF GRAPHS.
# SPECIFICALLY, HERE WE SOULD HAVE TWO POSSIBLE GRAHS STRUCTURES:
# 1. a<---A--->B---->b
# 2. a<---A<---B---->b
# COULD WE EXPLORE THE SAME QUESTIONS IN THIS SIMULATION USING THE TWO NETWORKS ABOVE
# BY THE WAY, WOULD THESE TWO GRAPHS BE EQUIVALENT?
# BUT IN THEORY WE COULD EXPLORE LARGER CLASSES OF GRAPHS
# I THINK **IN GENERAL** WE MIGHT WANT TO SYSTEMTIZE THIS PROCESS
# BY HAVING (i) A FIXED SET OF QUESTIONS and (ii) CLASS OF BAYESIAN NETOWKRS


# MARCELLO: **ADDITIONAL QUESTION -- GENERALIZATION 2**
# - WHAT FUNCTIONAL RELANTIOSHIP IS THERE BETWEEN INDIVIDUAL LR AND JOINT LR
# - CAN WE APPROXIMATE/LEARN A SIMPLE OPERATION TO COMBINE INDIVIDUAL LRS  
# - USING THE SIMULATION DATA WE HAVE? (MAYBE USING SOME ML ALGORITHM?)

# MARCELLO: GENERAL COMMENT
# PERHPAS THERE IS AN *INTUITVE, INFORMAL WAY* IN WHICH WE COMBINE EVIDENCE
# MAYBE THIS INTUITVE WAY EMERGES FROM APPLYING A BIG DATA ML ALGORITHM
# AFTER ALL THE GOAL HERE IS TO ARRIVE AT A PLAUSIBLE, NATURAL WAY TO COMBINE EVIDENCE
# SEE ARTICLE "BAYESIAN BRAINS WITHOUT PROBABILITIES"
# https://www.sciencedirect.com/science/article/pii/S1364661316301565


# MARCELLO: **ADDITIONAL QUESTION**
# - TO MAKE SENSE OF THE QUESTIONS ABOVE, 
# - CAN WE PLAY AROUND WITH SIMPLYFYING ASSUMPTIONS?
# - SAY ASSUME THAT
# - INDIVIDUAL LR ARE THE SAME
# - PRIOR PROBABILITIES OF HYPOTHESIS ARE THE SAME OR 0.5 EACH




