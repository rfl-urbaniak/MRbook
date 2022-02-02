library(bnlearn)
library(Rgraphviz)
library(gRain)
source("scripts/CptCreate.R")





#need to add these things:
#LRABab = P(ab | AB)   /   P(ab| ~AB)
#LRAab = P(ab |A) / P(ab|~ A)    abifAs/ abifnAs
#LRBab = P(ab |B) / P(ab | ~B)

conjunctionDAG <- model2network("[a|A][b|B][AB|A:B][A][B]")

graphviz.plot(conjunctionDAG)

set.seed(123)
n <- 10000

As <- numeric(n)
Bs <- numeric(n)
aifAs <- numeric(n)
aifnAs <- numeric(n)
bifBs <- numeric(n)
bifnBs <- numeric(n)
as <- numeric(n)
bs <- numeric(n)
abs <- numeric(n)
abifABs <- numeric(n)
abIfnABs <- numeric(n)
Aifas <- numeric(n)
Bifbs <- numeric(n)
ABs <- numeric(n)
ABifabs <- numeric(n)
BFAs <- numeric(n)
BFBs <- numeric(n)
BFABs <-  numeric(n)
LRAs <- numeric(n)
LRBs <- numeric(n)
LRABs <- numeric(n)


#this is needed for  LR with joint evidence fixed
abifAs <- numeric(n)
abifnAs <- numeric(n)
abifBs <- numeric(n)
abifnBs <- numeric(n)
LRAabs <- numeric(n)
LRBabs <- numeric(n)



for(i in 1:n){
As[i] <- runif(1,0,1)
Bs[i] <- runif(1,0,1)

aifAs[i] <-runif(1,0,1)
aifnAs[i] <- runif(1,0,1)
bifBs[i] <-runif(1,0,1)
bifnBs[i] <- runif(1,0,1)


AProb <-prior.CPT("A","1","0",As[i])
BProb <- prior.CPT("B","1","0",Bs[i])
aProb <- single.CPT("a","A","1","0","1","0",aifAs[i],aifnAs[i])
bProb <- single.CPT("b","B","1","0","1","0",bifBs[i],bifnBs[i])


ABProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                dim = c(2, 2, 2),
                dimnames = list(AB = c("1","0"),
                                B = c("1","0"), 
                                A = c("1","0")))


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

conjunctionJNa <-  setEvidence(conjunctionJN, nodes = c("a"), 
                               states = c("1"))
Aifas[i]  <- querygrain(conjunctionJNa, node = "A")[[1]][[1]] 

conjunctionJNb <-  setEvidence(conjunctionJN, nodes = c("b"), 
                               states = c("1"))
Bifbs[i]  <- querygrain(conjunctionJNa, node = "B")[[1]][[1]] 




conjunctionJNAB <- setEvidence(conjunctionJN, nodes = c("A", "B"), 
                               states = c("1", "1"))

abifABs[i] <- querygrain(conjunctionJNAB, node = c("a","b"), 
                     type = "joint")[1,1]


BFAs[i] <- aifAs[i]/as[i]
BFBs[i] <- bifBs[i]/bs[i]

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

#this adds individual LR with fixed joint evidence
conjunctionJNA <-  setEvidence(conjunctionJN, nodes = c("A"), 
                               states = c("1"))
abifAs[i] <- querygrain(conjunctionJNA, node = c("a","b"), 
              type = "joint")[1,1]

conjunctionJNnA <-  setEvidence(conjunctionJN, nodes = c("A"), 
                               states = c("0"))

abifnAs[i] <- querygrain(conjunctionJNnA, node = c("a","b"), 
                        type = "joint")[1,1]

LRAabs[i] <- abifAs[i]/abifnAs[i]


conjunctionJNB <-  setEvidence(conjunctionJN, nodes = c("B"), 
                               states = c("1"))
abifBs[i] <- querygrain(conjunctionJNB, node = c("a","b"), 
                        type = "joint")[1,1]

conjunctionJNnB <-  setEvidence(conjunctionJN, nodes = c("B"), 
                                states = c("0"))

abifnBs[i] <- querygrain(conjunctionJNnB, node = c("a","b"), 
                         type = "joint")[1,1]

LRBabs[i] <- abifBs[i]/abifnBs[i]





}

conjunctionTable <- data.frame(As,Bs,aifAs,aifnAs,bifBs,bifnBs,
                             as, bs, Aifas, Bifbs, abs, abifABs,abIfnABs,ABs,
                             ABifabs,BFAs,BFBs, BFABs,
                             LRAs, LRBs, LRAabs, LRBabs, LRABs)



conjunctionTable

nrow(conjunctionTable)

getwd()
saveRDS(conjunctionTable, file = "datasets/conjunctionTable3(jointEvidence).RDS")




