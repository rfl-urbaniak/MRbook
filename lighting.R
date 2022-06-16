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



lighting1DAG <- model2network("[H][E|H]")

graphviz.plot(lighting1DAG)


lighting1DAGitty  <- dagitty("
    dag{
        H -> E
      }")


#coordinates(entDAG) <- list( x=c(A = 1, B = 2, x = 2, C = 3, y = 3, D = 4, z = 4),
#                             y=c(A = 2, B = 3, x = 1, C = 2, y = 3, D = 2, z = 3) )

coordinates(lighting1DAGitty) <- list(x = c(H = 1, E = 1), y = c(H = 0, E = .2))
drawdag(lighting1DAGitty , shapes =  list(H = "c", E = "c")) 
        
  #      cex = 1, radius = 7)



lighting2DAGitty  <- dagitty("
    dag{
        H -> E 
        L -> E
      }")

coordinates(lighting2DAGitty) <- list(x = c(H = 0, E = 1, L = 2), y = c(H = 0, E = 1, L = 0))
drawdag(lighting2DAGitty , shapes =  list(H = "c", E = "c", L = "c")) 
