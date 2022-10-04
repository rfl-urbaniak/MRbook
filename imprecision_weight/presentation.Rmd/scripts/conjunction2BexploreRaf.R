library(tidyverse)


conjunctionTable2B <-  readRDS(file = "datasets/conjunctionTable2Braf.RDS")
conjunctionTable2B$maxBF <- pmax(conjunctionTable2B$BFAs, conjunctionTable2B$BFBs)
conjunctionTable2B$minBF <- pmin(conjunctionTable2B$BFAs, conjunctionTable2B$BFBs)
conjunctionTable2B$BFdifsMax <- conjunctionTable2B$BFABs - conjunctionTable2B$maxBF
conjunctionTable2B$BFdifsMin <- conjunctionTable2B$BFABs - conjunctionTable2B$minBF

conjunctionTable2B$maxLR <- pmax(conjunctionTable2B$LRAs, conjunctionTable2B$LRBs)
conjunctionTable2B$minLR <- pmin(conjunctionTable2B$LRAs, conjunctionTable2B$LRBs)
conjunctionTable2B$LRdifsMax <- conjunctionTable2B$LRABs - conjunctionTable2B$maxLR 
conjunctionTable2B$LRdifsMin <- conjunctionTable2B$LRABs - conjunctionTable2B$minLR 


head(conjunctionTable2B)


conjunctionPositiveBforA <- conjunctionTable2B %>% filter (BFAs >1)

head(conjunctionPositiveBforA)

mean(conjunctionPositiveBforA$LRAs > 1)


mean(conjunctionPositiveBforA$as >= conjunctionPositiveBforA$aifnAs)




conjunctionPositiveB <- conjunctionTable2B %>% filter (BFAs >1 & BFBs >1)
attach(conjunctionTable2B)


conjunctionPositiveB$





set.seed(109)
sample <- sample_n(conjunctionPositiveB,5)
sample

#BFA: 
0.6572764/0.3543418
#BFBprim 
0.6909435/0.3926414
#BFB: 
0.6909435/0.4250645
#BFAprim fix this
BFAprim  <- 0.6572764/0.3273133
BFAprim
BFB <- 0.6909435/0.4250645
BFAprim * BFB
#seems ok











mean(BFABs >= BFAs)
mean(BFABs >= BFBs)
#the main claim holds
mean(conjunctionPositiveB$BFABs >= conjunctionPositiveB$BFAs)
mean(conjunctionPositiveB$BFABs >= conjunctionPositiveB$BFBs)





#assumptions of second corollary fail
mean(BFAprimes  >= BFAs)
mean(BFBprimes  >= BFBs)
mean(conjunctionPositiveB$BFAprimes >= conjunctionPositiveB$BFAs)
mean(conjunctionPositiveB$BFBprimes >= conjunctionPositiveB$BFBs)



P(a|b) ≤ P(a|A)

mean(conjunctionPositiveB$aifbs < conjunctionPositiveB$aifAs)
mean(conjunctionPositiveB$bifas < conjunctionPositiveB$bifBs)



#assumptions of the first corollary fail, no impact of BFAs
mean(aifbs <= as)
mean(conjunctionPositiveB$aifbs > conjunctionPositiveB$as)

mean(bifas > bs)
mean(conjunctionPositiveB$bifas > conjunctionPositiveB$bs)

mean(conjunctionPositiveB$bifas > conjunctionPositiveB$bs | conjunctionPositiveB$aifbs > conjunctionPositiveB$as)



#bow about BFBprimes <1?

mean(BFBprimes >= 1)
mean(conjunctionPositiveB$BFBprimes >= 1)

mean(BFAprimes >= 1)
mean(conjunctionPositiveB$BFAprimes >= 1)
mean(conjunctionPositiveB$BFAprimes >= 1)

ggplot(conjunctionPositiveB, aes(y = BFAprimes, x = BFAs))+geom_point(size = 0.4)+xlim(c(0,30))+ylim(c(0,30))

ggplot(conjunctionTable2B, aes(y = BFAprimes, x = BFAs))+geom_point(size = 0.4)+xlim(c(0,20))+ylim(c(0,20))

lmbf <- lm(BFAprimes ~ BFAs, data = conjunctionTable2B)
cor(BFAprimes,BFAs)


mult <- BFAs *BFBs

lmbf2 <- lm(BFABs ~ mult, data = conjunctionTable2B)
cor.test(BFABs,mult)




summary(lmbf)

mean(conjunctionPositiveB$aifAs >= conjunctionPositiveB$as)
mean(conjunctionPositiveB$aifAs >= conjunctionPositiveB$aifbs)

ggplot(conjunctionPositiveB, aes(y = aifAs, x = aifbs))+geom_point(size = 0.1)

mean(conjunctionPositiveB$as >=  conjunctionPositiveB$aifbs )

ggplot(conjunctionPositiveB, aes(y = aifbs, x = as))+geom_point(size = 0.1)

mean(conjunctionPositiveB$as >=  conjunctionPositiveB$aifbs | conjunctionPositiveB$bs >=  conjunctionPositiveB$bifas)

ggplot(conjunctionPositiveB, aes(x = aifbs))+geom_histogram()
ggplot(conjunctionPositiveB, aes(x = as))+geom_histogram()

ggplot(conjunctionPositiveB, aes(y = aifAs, x = aifbs))+geom_point(size = 0.1)


mean(conjunctionPositiveB$aifAs >=  conjunctionPositiveB$aifbs )



aA <- conjunctionPositiveB$aifAs
Ab <- conjunctionPositiveB$Aifbs

left <- aA   * Ab

colnames(conjunctionPositiveB)

anA <- conjunctionPositiveB$aifnAs
nAb <- 1 - conjunctionPositiveB$As

right <- anA * nA


ggplot()+geom_histogram(aes(x = left))
ggplot()+geom_histogram(aes(x = right))

mean(left < aA)

sum <- left + right

mean(sum  ==  conjunctionPositiveB$aifbs )





#does the multiplicative result hold?

mean(round(BFABs,2) == round(BFAs * BFBprimes,2))

mean(round(BFABs,2) == round(BFBs * BFAprimes,2))









set.seed(109)
sample <- sample_n(conjunctionPositiveB,5)

sample

#BFA: .93/.8 =- .1.16
#BFBprim .62/.47 = 1.3  #fix this
0.6217664/0.4769257
#BFB: .63/.44 =1.43  ok
0.6217664/0.4425180
#BFAprim fix this
BFAprim  <- 0.9336201/0.8664053 
BFB <- 0.6217664/0.4425180
BFAprim * BFB


ggplot(conjunctionPositiveB, aes(y = BFABs, x = BFAs))+geom_point(size = 0.1)





## Marcello's questions

conjunctionTable2B %>% filter( BFAs > 1 & BFBs >1 & AifBs)






getwd() #this should be the project directory
getwd()
conjunctionTable2dep <- readRDS(file = "datasets/conjunctionTableDepAli2.RDS")


colnames(conjunctionTable2dep)

conjunctionTable2dep$minIndividual <- pmin(conjunctionTable2dep$Aifas, conjunctionTable2dep$Bifbs)
conjunctionTable2dep$diffIndividualMin <- conjunctionTable2dep$ABifabs - conjunctionTable2dep$minIndividual 


positiveDEP <-  conjunctionTable2dep %>% filter(ABifabs > ABs)


plotABindBelow2DEP <-  ggplot(positiveDEP, aes(x = diffIndividualMin))+geom_histogram(aes( y = ..density..), bins = 60)+
  xlab("P(AB|ab) -  min(P(A|a), P(B|b))")+ggtitle("DAG 2, compared to the the minimum (failure rate ca. 60%)")+
  labs(subtitle = expression(paste("Assuming  P(AB|ab) > P(AB)")))+xlim(c(-.7,1))+theme_tufte()


mean(positiveDEP$ABifabs < positiveDEP$Aifas & positiveDEP$ABifabs < positiveDEP$Bifbs )




plotBFindAbove <- conjunctionTable %>% filter(BFAs > 1 & BFBs > 1) %>% ggplot( aes(x = BFdifsMax))+geom_histogram(aes( y = ..density..), bins = 40)+
  xlab(expression(paste(BF[AB] - max,"(",BF[A], ", ", BF[B],")")))+ggtitle("Compared to  the maximum")+
  labs(subtitle = expression(paste("Assuming ",  BF[A], ", ",   BF[B] > 1)))+xlim(c(0,3))













