
# priors
prior.CPT <- function(node, state1, state2, prob1){
  states <- c(state1, state2)
  dimnames <- list(states)
  names(dimnames) <- node 
  array(c(prob1, 1-prob1), dim = 2, dimnames = dimnames)
}

HDprob <- prior.CPT("HD", "1", "0", .5)
HPprob <- prior.CPT("HP", "1", "0", .5)

# double CPT
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

Eprob <- doubleCPT("E", "HD", "HP",  probEifH1S1H2S1 = 0, probEifH1S1H2S2 =.7, probEifH1S2H2S1 =.7 , probEifH1S2H2S2 = .1)


# CPkable0
CPkable0 <- function(bn,node){
  ref <-  paste(bn,'$',node,'[[4]]', sep="")
  
  table <- eval(parse(text =  ref))
  round(table,3)  %>% kable(format = "latex",booktabs=T,
                            col.names = c(node, "Pr"),
                            linesep = "") #%>%   kable_styling(latex_options=c("striped"))
}

# CPkable2
CPkable2 <- function(bn, node){
  ref <-  paste(bn,'$',node,'[[4]]', sep="")
  parents <- paste(bn,'$',node, '$', 'parents', sep="")
  parents <- eval(parse(text =  parents))
  
  table <- eval(parse(text =  ref)) %>% kable(format = "latex",booktabs=T,
                                              col.names = c(node, "", "", "Pr"), linesep = "") #%>%   kable_styling(latex_options=c("striped"))
  eval(parse(text = paste('add_header_above(table, c(\"\",', '\"', parents[1], '\" =1, \"', parents[2], '\"=1,', '\"\"),line = FALSE)', sep="")))
}

STMpr <- list(HD = HDprob, HP = HPprob, E = Eprob)

STMbn <- custom.fit(STMdag, STMpr)

CPkable0("STMbn", "HD")
CPkable0("STMbn", "HP")
CPkable2("STMbn", "E")


