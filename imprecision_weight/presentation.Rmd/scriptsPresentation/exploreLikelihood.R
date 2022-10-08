library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plot3D)
library(plotly)


old <- theme_set(theme_tufte())

getwd() #this should be the project directory

# conjunctionTable2BrafRDS
conjunctionTable2 <- readRDS(file = "datasets/conjunctionTableAdditional.RDS")


conjunctionTable2

# LRAs > LRABs
newdata <- subset(conjunctionTable2, LRAs > LRABs)
one_row <- head(newdata, 1)
one_row

As = one_row[1, "0.2875775"] 
BifAs = one_row[1, "BifAs"]
BifnAs = one_row[1, "BifnAs"]
aifAs = one_row[1, "aifAs"]
aifnAs = one_row[1, "aifnAs"]
bifBs = one_row[1, "bifBs"]
bifnBs = one_row[1, "bifnBs"]

AProb <-prior.CPT("A","1","0",As)
BProb <-  single.CPT("B","A","1","0","1","0",BifAs,BifnAs)
aProb <- single.CPT("a","A","1","0","1","0",aifAs,aifnAs)
bProb <- single.CPT("b","B","1","0","1","0",bifBs,bifnBs)


ABProb <- array(c(1, 0, 0, 1, 0, 1, 0,1), 
                dim = c(2, 2, 2),
                dimnames = list(AB = c("1","0"),
                                B = c("1","0"), 
                                A = c("1","0")))


conjunctionCPT <- list(A = AProb, B = BProb, 
                       a = aProb, b = bProb, AB = ABProb)


conjunctionCPT


