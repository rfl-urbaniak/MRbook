library(tidyverse)


conjunctionTable2B <-  readRDS(file = "datasets/doubleDependencyTable.RDS")

str(conjunctionTable2B)

conjunctionTable2B$maxBF <- pmax(conjunctionTable2B$BFAs, conjunctionTable2B$BFBs)
conjunctionTable2B$minBF <- pmin(conjunctionTable2B$BFAs, conjunctionTable2B$BFBs)
conjunctionTable2B$BFdifsMax <- conjunctionTable2B$BFABs - conjunctionTable2B$maxBF
conjunctionTable2B$BFdifsMin <- conjunctionTable2B$BFABs - conjunctionTable2B$minBF

conjunctionTable2B$maxLR <- pmax(conjunctionTable2B$LRAs, conjunctionTable2B$LRBs)
conjunctionTable2B$minLR <- pmin(conjunctionTable2B$LRAs, conjunctionTable2B$LRBs)
conjunctionTable2B$LRdifsMax <- conjunctionTable2B$LRABs - conjunctionTable2B$maxLR 
conjunctionTable2B$LRdifsMin <- conjunctionTable2B$LRABs - conjunctionTable2B$minLR 



focus <- conjunctionTable2B %>% filter (BFAs >1 & BFBs > 1 & BFABs < BFAs & BFABs < BFBs)

focus
