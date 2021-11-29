library(bnlearn)
library(Rgraphviz)
library(gRain)
source("scripts/CptCreate.R")


conjunctionDAG2 <- model2network("[a|A][b|B][AB|A:B][A][B|A]")

graphviz.plot(conjunctionDAG2)

set.seed(123)
n <- 10000

As <- numeric(n)
Bs <- numeric(n)
BifAs <- numeric(n)
BifnAs <- numeric(n)
aifAs <- numeric(n)
aifnAs <- numeric(n)
bifBs <- numeric(n)
bifnBs <- numeric(n)
as <- numeric(n)
bs <- numeric(n)
abs <- numeric(n)
aifbs <- numeric(n)
bifas <- numeric(n)
abifABs <- numeric(n)
abIfnABs <- numeric(n)
ABs <- numeric(n)
ABifabs <- numeric(n)
BFAs <- numeric(n)
BFBs <- numeric(n)
BFABs <-  numeric(n)
BFAprimes <- numeric(n)
BFBprimes <- numeric(n)
LRAs <- numeric(n)
LRBs <- numeric(n)
LRABs <- numeric(n)



for(i in 1:n){
  As[i] <- runif(1,0,1)
  
  BifAs[i] <- runif(1,0,1)
  BifnAs[i] <- runif(1,0,1)
  
  aifAs[i] <-runif(1,0,1)
  aifnAs[i] <- runif(1,0,1)
  bifBs[i] <-runif(1,0,1)
  bifnBs[i] <- runif(1,0,1)
  
  
  AProb <-prior.CPT("A","1","0",As[i])
  BProb <-  single.CPT("B","A","1","0","1","0",BifAs[i],BifnAs[i])
  aProb <- single.CPT("a","A","1","0","1","0",aifAs[i],aifnAs[i])
  bProb <- single.CPT("b","B","1","0","1","0",bifBs[i],bifnBs[i])
  
  
  ABProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                  dim = c(2, 2, 2),
                  dimnames = list(AB = c("1","0"),
                                  B = c("1","0"), 
                                  A = c("1","0")))
  
  
  conjunctionCPT <- list(A = AProb, B = BProb, 
                         a = aProb, b = bProb, AB = ABProb)
  
  
  conjunctionBN <- custom.fit(conjunctionDAG2,conjunctionCPT)
  
  
  
  #graphviz.chart(conjunctionBN,type="barprob", scale = c(0.7,1.3),
  #               main = "Marginal probabilities in a conjunction BN")
  
  
  conjunctionJN <- compile(as.grain(conjunctionBN))
  
  as[i] <- querygrain(conjunctionJN, node = "a")[[1]][[1]]
  bs[i] <- querygrain(conjunctionJN, node = "b")[[1]][[1]]
  
  
  Bs[i] <- querygrain(conjunctionJN, node = "B")[[1]][[1]]
  
  abs[i] <- querygrain(conjunctionJN, node = c("a","b"), 
                       type = "joint")[1,1]
  
  conjunctionJNa <- setEvidence(conjunctionJN, nodes = c("a"), 
                               states = c("1"))
  bifas[i] <-   querygrain(conjunctionJNa, node = c("b"))[[1]][1]

  conjunctionJNb <- setEvidence(conjunctionJN, nodes = c("b"), 
                                states = c("1"))
  aifbs[i] <-   querygrain(conjunctionJNb, node = c("a"))[[1]][1]
  
  
  conjunctionJNAB <- setEvidence(conjunctionJN, nodes = c("A", "B"), 
                                 states = c("1", "1"))
  
  abifABs[i] <- querygrain(conjunctionJNAB, node = c("a","b"), 
                           type = "joint")[1,1]
  
  
  BFAs[i] <- aifAs[i]/as[i]
  BFBs[i] <- bifBs[i]/bs[i]
  
  BFAprimes[i] <- aifAs[i]/aifbs
  BFBprimes[i] <- bifBs[i]/bifas
  
  BFABs[i] <- abifABs[i]/abs[i]
  
  LRAs[i] <- aifAs[i]/aifnAs[i]
  LRBs[i] <- bifBs[i]/bifnBs[i]
  
  
  #now for joint likelihood, P(a^b|A ^B) is easy and already done, but
  # P(a^b | ~(A&B)) will be calculated using Bayes:
  # (1-P(A^B |a^b))P(a^b)/[1 - P(A^B)]
  conjunctionJNab <- setEvidence(conjunctionJN, nodes = c("a", "b"), 
                                 states = c("1", "1"))
  
  ABifabs[i] <- querygrain(conjunctionJNab, node = c("A","B"), 
                           type = "joint")[1,1]
  
  
  ABs[i] <- querygrain(conjunctionJN, node = c("AB"))[[1]][1]
  
  abIfnABs[i] <- ((1- ABifabs[i]) * abs[i])/(1-ABs[i])
  
  
  LRABs[i] <- abifABs[i] / abIfnABs[i]
}



conjunctionTable2raf <- data.frame(As,Bs,aifAs,aifnAs,bifBs,bifnBs,
                               as, bs, abs, aifbs, bifas, abifABs,abIfnABs,ABs,
                               ABifabs,BFAs,BFBs, BFABs, BFAprimes, BFBprimes,
                               LRAs, LRBs, LRABs)



saveRDS(conjunctionTable2raf, file = "datasets/conjunctionTable2Braf.RDS")




