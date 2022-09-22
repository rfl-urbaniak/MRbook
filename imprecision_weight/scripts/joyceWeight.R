library(ggplot2)
library(ggthemes)


#start with a simple case of three hypotheses, ch %in% (0.4, .5, .6) with equal weight


x <- c(.4, .5, .6)
chx <- c(1/3, 1/3, 1/3)

#say the data is 7 heads out of six tosses. 
data <- c(7,10)



#chance of evidence given data
chex <- dbinom(data[1], data[2],  x)

#overall chance of evidence (denumerator)
che <-  sum(chex * chx)

#chance of x given evidence
chxe <- (chex * chx ) / che

#credence in X and in X|E, using Joyce's expected value strategy
cX <- sum (chx * x)
cXe <- sum (chxe * x)

cX
cXe

multiplier <- (x - cX)^2
multiplierE <- (x - cXe)^2

top <-  chxe   * multiplierE

bottom <- chx * multiplier 

top - bottom

weight <- sum ( abs( top - bottom) )

cX
cXe
weight


weightJoyce()

#now as a function
weightJoyce <- function (chanceHypotheses = c(.4, .5, .6),
                         credenceInHypotheses =  c(1/3, 1/3, 1/3),
                         successes = 7,
                         trials = 10){
  
        #chance of evidence given data
        chex <- dbinom(successes, trials,  chanceHypotheses)
        #overall chance of evidence (denumerator)
        che <-  sum(chex * credenceInHypotheses)
        #chance of x given evidence (by Bayes)
        chxe <- (chex * credenceInHypotheses ) / che
        
        #credence in X before and after
        cX <- sum (credenceInHypotheses * chanceHypotheses)
        cXe <- sum (chxe * chanceHypotheses)
        
        multiplier <- (chanceHypotheses - cX)^2
        multiplierE <- (chanceHypotheses - cXe)^2
        
        top <-  chxe   * multiplierE
        bottom <- credenceInHypotheses * multiplier 
        
        weight <- sum ( abs( top - bottom) )
        
        return(list(hypotheses = chanceHypotheses, prior = credenceInHypotheses, posterior =  chxe, 
                    cX = cX, cXe = cXe, weight = weight))  
          }


outOfTenWeightsEqualPriors <- numeric(10)

for(i in seq(1,11, by  = 1)){
  outOfTenWeightsEqualPriors[i] <- weightJoyce(successes = i-1)$weight
}


plot(outOfTenWeightsEqualPriors)



outOfTenWeightsLeftPriors <- numeric(10)

for(i in seq(1,11, by  = 1)){
  outOfTenWeightsLeftPriors[i] <- weightJoyce(credenceInHypotheses = c(.5, 3, .2), successes = i-1)$weight
}


plot(outOfTenWeightsLeftPriors)

ggplot



plot(outOfTenWeightsEqualPriors,outOfTenWeightsLeftPriors)






weightJoyce(successes = 40, trials = 100)


weightJoyce(successes = 40, trials = 100)$weight/
  weightJoyce(successes = 50, trials = 100)$weight



weightJoyce(successes = 2, trials = 100)$weight/weightJoyce(successes = 8, trials = 100)$weight





weightJoyce(successes = , trials = 100)$weight/weightJoyce(successes = 40, trials = 100)$weight






outOf100WeightsEqualPriors <- numeric(101)

for(i in seq(1,101, by  = 1)){
  outOf100WeightsEqualPriors[i] <- weightJoyce(successes = i-1, trials = 100)$weight
}

outOf100WeightsEqualPriors

plot(outOf100WeightsEqualPriors)



outOf100WeightsLeftPriors <- numeric(101)

for(i in seq(1,101, by  = 1)){
  outOf100WeightsLeftPriors[i] <- weightJoyce(credenceInHypotheses = c(.5, 3, .2), successes = i-1, 
                                              trials = 100)$weight
}


outOf100WeightsLeftPriors

plot(outOf100WeightsEqualPriors)

plot(outOf100WeightsLeftPriors)

#what your priors are makes huuuuuge difference to weight, comparison does not make sense
#

ggplot



plot(outOfTenWeightsEqualPriors,outOfTenWeightsLeftPriors)







