library(bnlearn)
library(Rgraphviz)
library(gRain)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)

library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path)) 


source("cptCreate.R")



tenantDAG <- model2network("[Bathroom][Singing|Bathroom]") 



graphviz.plot(tenantDAG)


#priors stage 1
pL1 <- .1
pB1 <- .1 
pO1 <- .1 

bathroomPrior1 <- array(c(pL1, pB1, 1-(pL1 + pB1)), dim = 3, dimnames = list(c("L", "B", "N")))


bathroomLV1 <- c("L", "B", "N")
singingLV <- c("1", "0")
singingProb1 <- array(c(0.2, 0.8, 0.2, 0.8, 0, 1), dim = c(2, 3), 
                dimnames = list(Singing = singingLV, Bathroom = bathroomLV1))

singingProb1

tenantProbs1 <- list(Bathroom= bathroomPrior1, Singing = singingProb1)


tenantBN1 <- custom.fit(tenantDAG,tenantProbs1)


graphviz.chart(tenantBN1, type = "barprob")



bathroomLV2 <- c("L", "B", "O", "N")

bathroomPrior2 <- array(c(pL1, pB1, pO1, (1-(pL1 + pB1 + pO1))), dim = 4, dimnames = list(c("L", "B", "O", "N")))


singingProb2 <- array(c(0.2, 0.8, 0.2, 0.8, 0.2, 0.8, 0, 1), dim = c(2, 4), 
                      dimnames = list(Singing = singingLV, Bathroom = bathroomLV2))


singingProb2

tenantProbs2 <- list(Bathroom= bathroomPrior2, Singing = singingProb2)


tenantBN2 <- custom.fit(tenantDAG,tenantProbs2)

graphviz.chart(tenantBN2, type = "barprob")






tenantJN1 <-  compile(as.grain(tenantBN1))
tenantJN2 <-  compile(as.grain(tenantBN2))


tenantJN1S <- setEvidence(tenantJN1,nodes = c("Singing"), states = c("1"))
tenantJN2S <- setEvidence(tenantJN2,nodes = c("Singing"), states = c("1"))

querygrain(tenantJN1S, node = "Bathroom")
querygrain(tenantJN2S, node = "Bathroom")


#what's conservative? Conditional probs for L and B, perhaps equal priors









