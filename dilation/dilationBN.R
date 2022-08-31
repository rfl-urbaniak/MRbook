library(bnlearn)
library(gRain)
library(ggplot2)
source("scripts/CptCreate.R")
getwd()

dilationDAG <- model2network("[Red][Even][Equiv|Red:Even]")

redProb <-prior.CPT("Red","1","0",0.5)
equivProb <-  array(c( 1,0,0,1,0,1,1,0), dim = c(2,2,2),dimnames = list(Equiv = c("1","0"),
                                          Red = c("1","0"), Even = c("1","0")))


equivProb

set.seed(55)
n <- 1000
red <- rep(.5,n)
p <- runif(n,0,1)
evenPosterior <- numeric(n)
redPosterior <- numeric(n)


for(i in 1:n){
evenProb <-prior.CPT("Even","1","0",p[i])

dilationCPT <- list(Red=redProb,Even = evenProb, Equiv = equivProb)
dilationBN <- custom.fit(dilationDAG, dilationCPT)
dilationJN <- compile(as.grain(dilationBN))

dilationJNequiv <- setEvidence(dilationJN, nodes = c("Equiv"),
                               states = c("1"))

evenPosterior[i] <- querygrain(dilationJNequiv, node = "Even")[[1]][1]
redPosterior[i] <- querygrain(dilationJNequiv, node = "Red")[[1]][1]
}

dilationDF <- data.frame(red,p,evenPosterior,redPosterior)



head(dilationDF)


plot(evenPosterior)
plot(redPosterior)





ggplot()+geom_density(aes(x = evenPosterior))
ggplot()+geom_density(aes(x = redPosterior))

