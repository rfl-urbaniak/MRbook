100*(10^-2)


10^{-.8}

10^{-.8}

10^{-2}



/1


EH <- c(0.5, rep(.1,100))
priorH <- rep(1, length(EH))
priorH <- priorH/sum(priorH)

EH1 <- EH[1]
EH2 <- EH[2]
E <- sum(EH * priorH)
BF1 <- EH1 / E
BF1


priorH1orH2 <- priorH[1] + priorH[2]
priorH1orH2

L(H1 v H2) = P(E | H1 v H2) = 
  
P(E ^ ( H1 v H2)) / P( H1 v H2) =
  
P ( (E ^ H1) v (E ^ H2)) =
P  (E ^ H1) + P (E ^ H2)
P(E|H1)P(H1) +  P(E|H2)P(H2)


EandH1 <- EH1 * priorH[1]

EandH2 <- EH2 * priorH[2]

EandH1orH2 <- EandH1 + EandH2

EH1orH2 <- EandH1orH2 / priorH1orH2

BF12 <-EH1orH2/ E 


BF1
BF12


P(H1 | H1 v H2) = .5

P(H1) = priorH[1]



BF <- .5/priorH[1]
BF




