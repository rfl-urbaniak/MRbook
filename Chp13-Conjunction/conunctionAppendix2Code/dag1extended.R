library(bnlearn)

getwd()
source("scripts/CptCreate.R")
source("scripts/kableCPTs.R")
source("scripts/dagToDagitty.R")


dag1extendedDAG <- model2network("[a|A][b|B][AB|A:B][A][B][aA|A:a][bB|B:b][ab|a:b]")



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


independencies <- dag1extendedIndependenciesMinimal


formulas <- list()

i <- 2

triple <- independencies[[i]]

triple$Xs <- paste(unlist(strsplit(triple$X, split = "")), collapse = "\\[And]")
triple$Ys <- paste(unlist(strsplit(triple$Y, split = "")), collapse = "\\[And]")
triple$Zs <- paste(unlist(strsplit(triple$Z, split = "")), collapse = "\\[And]")



a <- c("a","na")
b <- c("b","nb")
c <- c("c","nc")
expand.grid(a =a, b = b, c= c)

abc <- paste("Pr[(", triple$Xs,  ")|(" , triple$Ys, ")\\[And](",  triple$Zs,
      ")] == Pr[(", triple$Xs,  ")|(" , triple$Zs, ")]", sep = "")

nabc <- paste("Pr[\\[Not](", triple$Xs,  ")|(" , triple$Ys, ")\\[And](", 
              triple$Zs,")] == Pr[\\[Not](", triple$Xs,  
              ")|(" , triple$Zs, ")]", sep = "")

anbc <- paste("Pr[(", triple$Xs,  ")|\\[Not](" , triple$Ys, ")\\[And](", 
              triple$Zs, ")] == Pr[(", triple$Xs,  ")|(" ,
              triple$Zs, ")]", sep = "")


nanbc <- paste("Pr[\\[Not](", triple$Xs,  ")|\\[Not](" , triple$Ys,
               ")\\[And](",  triple$Zs,
              ")] == Pr[\\[Not](", triple$Xs,  ")|(" , triple$Zs, ")]", sep = "")


abnc <- paste("Pr[(", triple$Xs,  ")|(" , triple$Ys, ")\\[And] \\[Not](",
              triple$Zs,")] == Pr[(", triple$Xs,  ")|\\[Not](", 
              triple$Zs, ")]", sep = "")

nabnc <- paste("Pr[\\[Not](", triple$Xs,  ")|(" , triple$Ys, ")\\[And] \\[Not](",
              triple$Zs,")] == Pr[(", triple$Xs,  ")|\\[Not](", 
              triple$Zs, ")]", sep = "")

anbnc <- paste("Pr[(", triple$Xs,  ")|\\[Not](" , triple$Ys, ")\\[And] \\[Not](",
               triple$Zs,")] == Pr[(", triple$Xs,  ")|\\[Not](", 
               triple$Zs, ")]", sep = "")

nanbnc <- paste("Pr[\\[Not](", triple$Xs,  ")|\\[Not](" , triple$Ys, ")\\[And] \\[Not](",
               triple$Zs,")] == Pr[\\[Not](", triple$Xs,  ")|\\[Not](", 
               triple$Zs, ")]", sep = "")


cat(paste(c(abc , nabc, anbc, nanbc, abnc, nabnc, 
            anbnc, nanbnc), collapse = ", \n "))




##need to implement consistency check

contest <- gsub("[()]", "", abnc)
library(stringr)
str_detect(contest, "\\[Not]")
#this is unfinished







cat(anbc)

cat(nanbc)


Pr[A | b \[And] a] == Pr[A | a]

triple$Y,

graphviz.plot(dag1extendedDAG)

