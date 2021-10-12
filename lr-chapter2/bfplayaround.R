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

priorH1orH2 <- priorH[1] + priorH[2]
priorH1orH2

EandH1 <- EH1 * priorH[1]
EandH2 <- EH2 * priorH[2]

EandH1orH2 <- EandH1 + EandH2

EH1orH2 <- EandH1orH2 / priorH1orH2

BF12 <-EH1orH2/ E 


BF1
BF12



