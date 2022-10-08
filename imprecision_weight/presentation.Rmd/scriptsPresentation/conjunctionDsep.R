library(bnlearn)
library(Rgraphviz)
library(gRain)
source("scripts/CptCreate.R")


conjunctionDAGextended <- model2network("[a|A][b|B][AB|A:B][Aa|A:a][A][B]")


conjunctionDAG <- model2network("[a|A][b|B][AB|A:B][A][B|A]")

graphviz.plot(conjunctionDAG)


#Independencies assumed in the Bayes factor proof
dsep(conjunctionDAGextended, x= "A"   , y = "B" )

dsep(conjunctionDAGextended, x= "a"   , y = "B", z = "A" )


#rewrite
P(b|A^a^B) =
  P(b|a ^ B) =
  P(b|B)
dsep(conjunctionDAGextended, x= "b"   , y = "Aa", z = "B")



dsep(conjunctionDAGextended, x= "b"   , y = "a")


conjunctionDAGnoInd <- model2network("[a|A][b|B][Aa|A:a][A|AB][B|AB][AB]")

conjunctionDAGnoInd <- model2network("[a|A][b|B][A|AB][B|AB][AB]")

graphviz.plot(conjunctionDAGnoInd)


dsep(conjunctionDAGnoInd, x = "a", y = "B", z = "A")

dsep(conjunctionDAGnoInd, x = "b", y = "Aa", z = "B")

#note A B become dependent
dsep(conjunctionDAGnoInd, x = "A", y = "B")

dsep(conjunctionDAGnoInd, x = "A", y = "B", z= "AB")



#not used in proof, used in the discussion after, b and a are not independent
dsep(conjunctionDAGnoInd, x = "b", y = "a")





