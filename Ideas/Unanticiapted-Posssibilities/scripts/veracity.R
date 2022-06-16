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

# 
# 
# veracityDAG <- model2network("[H][E|H]")


veracityDAGitty  <- dagitty("
    dag{
        H -> E -> R
      }")

coordinates(veracityDAGitty) <- list(x = c(H = 0, E = 1, R = 2), y = c(H = 0, E = 0, R = 0))
drawdag(veracityDAGitty , shapes =  list(H = "c", E = "c", R = "c")) 

#      cex = 1, radius = 7)