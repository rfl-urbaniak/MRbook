library(bnlearn)

getwd()
source("scripts/CptCreate.R")
source("scripts/kableCPTs.R")
source("scripts/dagToDagitty.R")
source("scripts/independenciesToFormulas.R")
source("scripts/conjunctionToConstraints.R")


dag1simpleDAG <- model2network("[a|A][b|B][A][B]")

graphviz.plot(dag1simpleDAG)

dag1extendedDAG <- model2network("[a|A][b|B][AB|A:B][A][B][aA|A:a][bB|B:b][ab|a:b]")


dag1extendedDAGunique <- model2network("[a|A][b|B][C|A:B][A][B][D|A:a][E|B:b][F|a:b]")
#AB:C
#aAD
#bBE
#abF

graphviz.plot(dag1extendedDAG)
graphviz.plot(dag1extendedDAGunique)




ABProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                dim = c(2, 2, 2),
                dimnames = list(AB = c("1","0"),
                                B = c("1","0"), 
                                A = c("1","0")))

aAProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                dim = c(2, 2, 2),
                dimnames = list(aA = c("1","0"),
                                A = c("1","0"), 
                                a = c("1","0")))


bBProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                dim = c(2, 2, 2),
                dimnames = list(bB = c("1","0"),
                                B = c("1","0"), 
                                b = c("1","0")))

abProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                dim = c(2, 2, 2),
                dimnames = list(ab = c("1","0"),
                                a = c("1","0"), 
                                b = c("1","0")))




As <- runif(1,0,1)
Bs <- runif(1,0,1)
aifAs <-runif(1,0,1)
aifnAs <- runif(1,0,1)
bifBs <-runif(1,0,1)
bifnBs <- runif(1,0,1)



AProb <-prior.CPT("A","1","0",As)
BProb <- prior.CPT("B","1","0",Bs)
aProb <- single.CPT("a","A","1","0","1","0",aifAs,aifnAs)
bProb <- single.CPT("b","B","1","0","1","0",bifBs,bifnBs)


dag1extendedCPT <- list(A = AProb, B = BProb, 
                       a = aProb, b = bProb, AB = ABProb,
                       aA = aAProb, bB = bBProb, ab = abProb)


nodes(dag1extendedDAG)

dag1extendedBN <- custom.fit(dag1extendedDAG,dag1extendedCPT)


dag1extendedDagitty <- dagToDagitty(dag1extendedDAG)


dag1extendedIndependenciesMinimal  <- impliedConditionalIndependencies(dag1extendedDagitty)
dag1extendedIndependenciesAll  <- impliedConditionalIndependencies(dag1extendedDagitty, type = "all.pairs")


independencies <- dag1extendedIndependenciesMinimal



cat(unlist(formulas))


graphviz.plot(dag1extendedDAGunique)

conjunction <- "D"
conjunct1 <- "A"
conjunct2 <- "a"



listDAG1 <- list(
      c("C", "A", "B"),
      c("D", "A", "a"), 
      c("E", "B", "b"),
      c("F", "a", "a")
                 )

constraintsDAG1 <- conjunctionsToConstraints(listDAG1)


cat(unlist(constraintsDAG1))

cat(constraints)

d1simple <- dagToDagitty(dag1simpleDAG)
d1simpleIndependencies <- impliedConditionalIndependencies(d1simple) #, type = "all.pairs")
length(d1simpleIndependencies)

d1simpleIndependencies
d1simpleFormulas <- independenciesToFormulas(d1simpleIndependencies)
length(unlist(d1simpleFormulas))

clipr::write_clip(unlist(d1simpleFormulas))





d1unique <- dagToDagitty(dag1extendedDAGunique)
d1uniqueIndependencies  <- impliedConditionalIndependencies(d1unique, type = "all.pairs")
length(d1uniqueIndependencies)
d1formulas <- independenciesToFormulas(d1uniqueIndependencies)
length(d1formulas)

library(clipr)
clipr::write_clip(cat(unlist(d1formulas)))

clipr::write_clip(unlist(d1formulas))



s##need to implement consistency check
##need to remove repeated conjuncts
## need to remove redundant 
#Pr[(A \[And] B) | (a) \[And] \[Not] (A)] == 
#Pr[(A \[And] B) | \[Not] (A)]


contest <- gsub("[()]", "", abnc)
library(stringr)
str_detect(contest, "\\[Not]")
#this is unfinished



independencies[[8]]



