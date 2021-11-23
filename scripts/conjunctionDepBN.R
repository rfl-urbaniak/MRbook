library(bnlearn)
library(Rgraphviz)
library(gRain)
source("scripts/CptCreate.R")

# define the network visually
conjunctionDAG <- model2network("[a|A][b|B][AB|A:B][A][B|A]")
graphviz.plot(conjunctionDAG)

# set seed and number of iterations
set.seed(321)
n <- 100000

# initialize the variables
As <- numeric(n)
Bs <- numeric(n)

aifAs <- numeric(n)
aifnAs <- numeric(n)

# added here dependence for B when A and not A
BifAs <- numeric(n)
BifnAs <- numeric(n)

bifBs <- numeric(n)
bifnBs <- numeric(n)
as <- numeric(n)
bs <- numeric(n)
abs <- numeric(n)
abifABs <- numeric(n)
abIfnABs <- numeric(n)
ABs <- numeric(n)
ABifabs <- numeric(n)

# should this be changed? 
BFAs <- numeric(n)
BFBs <- numeric(n)
BFABs <-  numeric(n)

LRAs <- numeric(n)
LRBs <- numeric(n)
LRABs <- numeric(n)

# run a loop per each case - add dependence

for(i in 1:n){
  As[i] <- runif(1,0,1)
  # Bs[i] <- runif(1,0,1)
  
  aifAs[i] <-runif(1,0,1)
  aifnAs[i] <- runif(1,0,1)
  bifBs[i] <-runif(1,0,1)
  bifnBs[i] <- runif(1,0,1)
  
  # ok
  BifAs[i] <- runif(1,0,1)
  BifnAs[i] <- runif(1,0,1)
  
  
  AProb <-prior.CPT("A","1","0",As[i])
  
  # ok
  # BProb <- prior.CPT("B","1","0",Bs[i])
  
  BProb <- single.CPT("B","A","1","0","1","0",BifAs[i],BifnAs[i])
  
  aProb <- single.CPT("a","A","1","0","1","0",aifAs[i],aifnAs[i])
  bProb <- single.CPT("b","B","1","0","1","0",bifBs[i],bifnBs[i])
  
  
  ABProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                  dim = c(2, 2, 2),
                  dimnames = list(AB = c("1","0"),
                                  B = c("1","0"), 
                                  A = c("1","0")))
  
  
  # does it have to be changed?
  conjunctionCPT <- list(A = AProb, B = BProb, 
                         a = aProb, b = bProb, AB = ABProb)
  
  
  conjunctionBN <- custom.fit(conjunctionDAG,conjunctionCPT)
  
  
  
  #graphviz.chart(conjunctionBN,type="barprob", scale = c(0.7,1.3),
  #               main = "Marginal probabilities in a conjunction BN")
  
  
  conjunctionJN <- compile(as.grain(conjunctionBN))
  
  as[i] <- querygrain(conjunctionJN, node = "a")[[1]][[1]]
  bs[i] <- querygrain(conjunctionJN, node = "b")[[1]][[1]]
  
  abs[i] <- querygrain(conjunctionJN, node = c("a","b"), 
                       type = "joint")[1,1]
  
  
  conjunctionJNAB <- setEvidence(conjunctionJN, nodes = c("A", "B"), 
                                 states = c("1", "1"))
  
  abifABs[i] <- querygrain(conjunctionJNAB, node = c("a","b"), 
                           type = "joint")[1,1]
  
  # todo: read the chapter again and Scutari
  
  # should it be changed? 
  BFAs[i] <- aifAs[i]/as[i]
  BFBs[i] <- bifBs[i]/bs[i]
  
  BFABs[i] <- abifABs[i]/abs[i]
  
  # should it be changed? 
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

conjunctionTableAli <- data.frame(As,Bs,aifAs,aifnAs,bifBs,bifnBs,
                               as, bs, abs, abifABs,abIfnABs,ABs,
                               ABifabs,BFAs,BFBs, BFABs,
                               LRAs, LRBs, LRABs)



conjunctionTableAli

saveRDS(conjunctionTableAli, file = "conjunctionTableDepAli.RDS")








