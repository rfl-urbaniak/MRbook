library(bnlearn)
library(gRain)
library(dagitty)
library(rethinking)
library(kableExtra)
library(tidyverse)


priorCPT <- function(node, state1, state2, prob1){
  states <- c(state1, state2)
  dimnames <- list(states)
  names(dimnames) <- node 
  array(c(prob1, 1-prob1), dim = 2, dimnames = dimnames)
}

singleCPT <- function(eNode, hNode,
                      eState1 = "1", eState2 = "0",
                      hState1 = "1", hState2 = "0",
                      probEifHS1, probEifHS2){
  eStates <- c(eState1,eState2)
  hStates <- c(hState1,hState2)
  dimnames <- list(eStates,hStates)
  names(dimnames) <- c(eNode,hNode)
  array(
    c(probEifHS1, as.numeric(1-probEifHS1),
      probEifHS2,as.numeric(1-probEifHS2)), 
    dim = c(2,2),
    dimnames = dimnames
  )
}



doubleCPT <- function(eNode, h1Node, h2Node, eState1 = "1" , eState2 = "0", h1State1 = "1", h1State2 = "0", h2State1 = "1", h2State2 = "0", probEifH1S1H2S1, probEifH1S1H2S2, probEifH1S2H2S1,probEifH1S2H2S2){
  eStates <- c(eState1,eState2)
  h1States <- c(h1State1,h2State2)
  h2States <- c(h2State1,h2State2)
  dimnames <- list(eStates,h1States,h2States)
  names(dimnames) <- c(eNode,h1Node,h2Node)
  array(c(
    probEifH1S1H2S1,as.numeric(1-probEifH1S1H2S1),
    probEifH1S2H2S1,as.numeric(1-probEifH1S2H2S1),
    probEifH1S1H2S2,as.numeric(1-probEifH1S1H2S2),
    probEifH1S2H2S2,as.numeric(1-probEifH1S2H2S2)
  ),
  dim = c(2,2,2), dimnames = dimnames)
}




CPkable1 <- function(bn, node){
  parent <- paste(bn,'$',node, '$', 'parents[1]', sep="")
  parent <- eval(parse(text =  parent))
  ref <-  paste(bn,'$',node,'[[4]]', sep="")
  table <- eval(parse(text =  ref))
  table <- round(table,3)  %>% kable(format = "latex",booktabs=T,
                                     #col.names = c(node, "1", "0"),
                                     linesep = "")# %>%   kable_styling(latex_options=c("striped"))
  eval(parse(text = paste('add_header_above(table, c(\"', node, '\",','\"', parent, '\"', '=2), line = FALSE )', sep="")))
}


CPkable2 <- function(bn, node){
  ref <-  paste(bn,'$',node,'[[4]]', sep="")
  parents <- paste(bn,'$',node, '$', 'parents', sep="")
  parents <- eval(parse(text =  parents))
  
  table <- eval(parse(text =  ref)) %>% kable(format = "latex",booktabs=T,
                                              col.names = c(node, "", "", "Pr"), linesep = "") #%>%   kable_styling(latex_options=c("striped"))
  eval(parse(text = paste('add_header_above(table, c(\"\",', '\"', parents[1], '\" =1, \"', parents[2], '\"=1,', '\"\"),line = FALSE)', sep="")))
}






slippery.daggity <- dagitty("
    dag{
        slippery -> interference
        interference -> video
        slippery -> testimony
        slippery -> fall
        slippery -> video
      }")


drawdag(slippery.daggity)



slipperyProb <-prior.CPT("slippery","1","0", .05)
slipperyProb

interferenceProb <- singleCPT("interference","slippery","1","0","1","0",.65,0)
interferenceProb

fallProb <- singleCPT("fall","slippery","1","0","1","0",.1,.5)
fallProb

testimonyProb <- singleCPT("testimony","slippery","1","0","1","0",1,.1)
testimonyProb



videoStates <- c("missing","danger", "safe")
interferenceStates <- c("1", "0")
slipperyStates <- c("1", "0")
dimnames <- list(videoStates,interferenceStates,slipperyStates)

videoProb <- array(c(
 .8, .2, 0, .2, .8, 0, .8, 0, .2,  .2, 0,  .8
),
dim = c(3,2,2), dimnames = dimnames)

videoProb



slipperyDAG <- model2network("[fall|slippery][testimony|slippery][video|interference:slippery][slippery][interference|slippery]")

graphviz.plot(slipperyDAG)


slipperyCPT <- list( slippery = slipperyProb, 
                     interference = interferenceProb, 
                      fall = fallProb, testimony = testimonyProb, 
                      video = videoProb)


slipperyBN <- custom.fit(slipperyDAG,slipperyCPT)



CPkable1("slipperyBN","fall") %>%   
  kable_styling(latex_options=c("striped","HOLD_position")) 

CPkable2("slipperyBN","video") %>%   
  kable_styling(latex_options=c("striped","HOLD_position")) 


slipperyJN <- compile(as.grain(slipperyBN))


querygrain(slipperyJN, nodes = "slippery")

querygrain(slipperyJN, nodes = c("interference", "slippery"))

querygrain(slipperyJN, nodes = c("interference", "slippery"), type = "joint")


slipperyExplicitJN <-  setEvidence(slipperyJN, nodes = c("fall", "testimony"), states = c("1", "1"))


querygrain(slipperyExplicitJN, nodes = c("slippery"))


slipperyAllJN <-  setEvidence(slipperyJN, nodes = c("fall", "testimony", "video"), states = c("1", "1", "missing"))


querygrain(slipperyAllJN, nodes = c("slippery"))


