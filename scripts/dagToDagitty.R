library(bnlearn)
library(dagitty)
library(rethinking)



dagToDagitty <- function (dag){
arcs <- as.data.frame(dag$arcs)
arrows <- character(nrow(arcs))
for(i in 1: nrow(arcs)){
arrows[i] <- paste(arcs[i,]$from, " -> ",   arcs[i,]$to, sep = "")
}
arrowsString <- paste(arrows, collapse=" ")
dagittyString <- paste("dagitty( \"dag {", arrowsString, "}\")", sep = " ")
dagittyObject <- eval(parse(text = dagittyString))
return(dagittyObject)
return(dagittyObject)
}


dagToindependencies <- function (dag){
  
}

# #test 1
# dag1 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
# dagitty1  <- dagitty( "dag {
# A -> E S -> E E -> O E -> R O -> T R -> T
# }")
# dagitty1 == dagToDagitty(dag1)

# #test2
# dag2 <-  model2network("[A][D|A:M][M|A]")
# dagitty2 <- dagitty('dag{ D <- A -> M -> D }')
# dagitty2 == dagToDagitty(dag2)





#par(mfrow=c(1,2))



