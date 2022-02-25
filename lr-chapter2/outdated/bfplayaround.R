100*(10^-2)


10^{-.8}

10^{-.8}

10^{-2}



/1

Suppose there are  101 distinct cosmological
hypotheses $H_1, \dots, H_{101}$, assumed to be pairwise incompatible and jointly exhaustive, each providing a different physical explanation of astronomical observations represented by $E$. Say they all have equal prior probability $\approx .0099$. The first one is the bing bang hypothesis, and its likelihood ($\pr{E \vert H_1}$) is .5. The likelihood corresponding to other hypotheses is .1 (that is, for $i>1$ we have $\pr{E\vert H} = .1$). 


EH <- c(0.5, rep(.1,100))


priorH <- rep(1, length(EH))
priorH <- priorH/sum(priorH)
priorH

The prior of the evidence is $\sum_{i = 1}^100 \pr{E \vert H_i} \approx .103$, and the Bayes factor for $H_1$ is \nicefrac{\pr{E \vert H_1}}{\pr{E}} \approx 4.809$.

priorH[1]
EH1 <- EH[1]
EH2 <- EH[2]

sum(EH * priorH)
E <- sum(EH * priorH)
E
EH1
BF1 <- EH1 / E
BF1

#Now consider the disjunction $H_1 \vee H_2$, which logically follows from $H_1$. Since the hypotheses are exclusive, its prior is the sum of the two separate priors $\pr{H_1} + pr{H_2} \approx .0198$. Let's derive the likelihood for this disjunction:

\begin{align*}
P(E | H_1 \vee H_2) & = \frac{\pr{E \wedge ( H_1 \vee H_2)} }{\pr{H_1 \vee H_2}} \\
&  = \frac{\pr{(E \et H_1) \vee ( E \et  H_2)} }
{\pr{H_1 \vee H_2}} \\
&  = \frac{\pr{(E \et H_1) + ( E \et  H_2)} }
{\pr{H_1 \vee H_2}}  \\
&  = \frac{\pr{(E \vert H_1)\pr{H_1} + ( E \vert  H_2)\pr{H_2}} }
{\pr{H_1 \vee H_2}} \\
& \approx \frac{.0049 + .00099 }{.0198} \approx  .3
\end{align*}

So, the Bayes factor for the disjunction is $ \approx \nicefrac{.3}{.103} \approx 2.88$, which is less than the Bayes factor for the first hypothesis. 

priorH1orH2 <- priorH[1] + priorH[2]
priorH1orH2

EandH1 <- EH1 * priorH[1]

EandH1


EandH2 <- EH2 * priorH[2]
EandH2


EandH1orH2 <- EandH1 + EandH2


EandH1orH2

EH1orH2 <- EandH1orH2 / priorH1orH2

EH1orH2

BF12 <-EH1orH2/ E 


BF1
BF12







#100 items the same
EH[1]


num1 <- EandH2 * 100
denom1 <- 1 -  priorH[1]
EnH <- num1/denom1
EnH
Lr1 <- EH[1]/EnH
Lr1

priorH[1] == priorH[2]

num2 <- EandH2 * 99
denom2 <- 1 - 2* priorH[2]

lkNeg <- num2/denom2

.3/.1

EH1orH2

LR <- EH1orH2/lkNeg

LR






P(H1 | H1 v H2) = .5

P(H1) = priorH[1]



BF <- .5/priorH[1]
BF


P(H1 | H1 v H2)
/
P(H1 | ~ (H1 v H2))  =0, undefined



