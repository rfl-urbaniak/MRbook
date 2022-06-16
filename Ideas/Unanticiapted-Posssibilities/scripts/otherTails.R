library(bnlearn)
library(Rgraphviz)
library(gRain)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)

library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path)) 





totDAG <- model2network("[Outcome][Image|Outcome]") 



totDAG2 <- model2network("[Outcome|Design][Image|Outcome:Design][Design]") 

graphviz.plot(totDAG)

graphviz.plot(totDAG2)


#priors coin
pH <- .5
pT <- .5 

OutcomePrior <- array(c(pH, pT), dim = 2, dimnames = list(c("H", "T")))



imageLV1 <- c("N", "L")
imageLV2 <- c("N", "L", "S")


imageProb1 <-  array(c(1, 0, 0, 1), dim = c(2, 2), 
       dimnames = list(Image = imageLV1, Outcome = c("H", "T")))
 
imageProb1


imageProb2 <-  array(c(1, 0, 0, 0, .9, .1), dim = c(3, 2), 
                     dimnames = list(Image = imageLV2, Outcome = c("H", "T")))

imageProb2

 
  
totProbs1 <- list(Outcome= OutcomePrior, Image = imageProb1)
totProbs2 <- list(Outcome= OutcomePrior, Image = imageProb2)



totBN1 <- custom.fit(totDAG,totProbs1)
totBN2 <- custom.fit(totDAG,totProbs2)



graphviz.chart(totBN1, type = "barprob")
graphviz.chart(totBN2, type = "barprob")



totJN1 <-  compile(as.grain(totBN1))
totJN2 <-  compile(as.grain(totBN2))


querygrain(totJN1, node = "Image")
querygrain(totJN2, node = "Image")


#what's conservative? Conditional probs for L and B, perhaps equal priors









