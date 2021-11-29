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



conjunctionPositiveB <- conjunctionTable2B %>% filter (BFAs >1 & BFBs >1)
attach(conjunctionTable2B)





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


#assumptions of the first corollary fail, no impact of BFAs
mean(aifbs <= as)
mean(conjunctionPositiveB$aifbs <= conjunctionPositiveB$as)

mean(bifas <= bs)
mean(conjunctionPositiveB$bifas <= conjunctionPositiveB$bs)

mean(conjunctionPositiveB$bifas <= conjunctionPositiveB$bs | conjunctionPositiveB$aifbs <= conjunctionPositiveB$as)

#bow about BFBprimes <1?

mean(BFBprimes >= 1)
mean(conjunctionPositiveB$BFBprimes >= 1)

mean(BFAprimes >= 1)
mean(conjunctionPositiveB$BFAprimes >= 1)

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





















