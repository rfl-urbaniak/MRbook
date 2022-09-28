library(ggplot2)
library(ggthemes)
library(gridExtra)
library(tidyr)
library(philentropy)

#start with a simple case of three hypotheses, ch %in% (0.4, .5, .6) with equal weight


x <- c(.4, .5, .6)

crx <- c(1/3, 1/3, 1/3)

#say the data is 7 heads out of six tosses. 
data <- c(7,10)

#credence in x Joyce's expected value strategy
cX <- sum (crx * x)

cX




#chance of evidence given data
crex <- dbinom(x = data[1], size = data[2], prob = x)

?dbinom

choose(data[2], data[1]) * x^data[1] * (1-x)^{data[2]-data[1]}

crex

#overall chance of evidence (denumerator)
cre <-  sum(crex * crx)


cre

#chance of x given evidence
crxe <- (crex * crx ) / cre


rbind(crxe,
x,
crxe * x
)


cXe <- sum (crxe * x)


cX
cXe

x - cX
x - cXe


weight <- (x - cX)^2
weightE <- (x - cXe)^2

weight
weightE

cX

top <-  chxe   * multiplierE

bottom <- chx * multiplier 

top - bottom

weight <- sum ( abs( top - bottom) )

cX
cXe
weight


weightJoyce(successes= 7, trials = 10)$weight/
  weightJoyce(successes= 70, trials = 100)$weight

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
        
        return(list(hypotheses = chanceHypotheses, prior = credenceInHypotheses,
                    posterior =  chxe, 
                    cX = cX, cXe = cXe, 
                    multiplierE = multiplierE, 
                     multiplier = multiplier,
                    top = top, bottom  = bottom, 
                    weight = weight))  
          }


outOfTenWeightsLeftPriors <- numeric(10)

for(i in seq(1,11, by  = 1)){
  outOfTenWeightsLeftPriors[i] <- weightJoyce(credenceInHypotheses = c(.5, .3, .2), successes = i-1)$weight
}



outOfTenWeightsEqualPriors <- numeric(10)

for(i in seq(1,11, by  = 1)){
  outOfTenWeightsEqualPriors[i] <- 
    weightJoyce(successes = i-1)$weight
}


outOfTenWeightsEqualPriors[1]/outOfTenWeightsEqualPriors[6]

outOf10df <- data.frame( successes = seq(0,10,1),
  equal = outOfTenWeightsEqualPriors, ".5, .3, .2" = outOfTenWeightsLeftPriors)

outOf10df

names(outOf10df) <- c("successes", "equal", ".5, .3, .2")
names(outOf10df)
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
outOf10dfLong  <- gather(data = outOf10df,
                    key = priors, value = w,
                    "equal", ".5, .3, .2", 
                    factor_key=TRUE)



joyce10  <- ggplot(outOf10dfLong)+geom_point(aes(x = successes,
                                    y = w, color = priors) )+
  scale_x_continuous(breaks = seq(0,10))+theme_tufte(base_size = 14)+ylab("w")+
  xlab("successes in ten trials")+
  scale_y_continuous(breaks = seq(0,0.007, by = .001))+
  labs(title = "Joyce's weights change radically by frequency",
       subtitle = "(equal priors, fixed sample size)")+theme(plot.title.position = "plot")


joyce10






outOf100WeightsEqualPriors <- numeric(101)

for(i in seq(1,101, by  = 1)){
  outOf100WeightsEqualPriors[i] <- 
    weightJoyce(successes = i-1, trials = 100)$weight
}


outOf100WeightsLeftPriors <- numeric(101)

for(i in seq(1,101, by  = 1)){
  outOf100WeightsLeftPriors[i] <- weightJoyce(credenceInHypotheses = c(.5, .3, .2),
                                              successes = i-1, trials  = 100)$weight
}




outOf100df <- data.frame( successes = seq(0,100,1),
                         equal = outOf100WeightsEqualPriors, 
                         ".5, .3, .2" = outOf100WeightsLeftPriors)

outOf100df

names(outOf100df) <- c("successes", "equal", ".5, .3, .2")
names(outOf100df)


outOf100dfLong  <- gather(data = outOf100df,
                         key = priors, value = w,
                         "equal", ".5, .3, .2", 
                         factor_key=TRUE)



joyce100  <- ggplot(outOf100dfLong)+geom_point(aes(x = successes,
                                                 y = w, color = priors) )+
  scale_x_continuous(breaks = seq(0,100, by = 5))+theme_tufte(base_size = 14)+ylab("w")+
  xlab("successes in ten trials")+
  scale_y_continuous(breaks = seq(0,0.01, by = .001))+
  labs(title = "Joyce's weights change radically by frequency",
       subtitle = "(equal priors,   fixed sample size)")+
  theme(plot.title.position = "plot")


joyce100



#now let's say we see 10% for increasing sample size

s <- seq(1,100)
obs <- seq(10, 1000, by = 10)


weightsBySampleSize <- numeric(length(s))
weightsBySampleSizeLeft <- numeric(length(s))


for (i in  1:100){
weightsBySampleSize[i] <- weightJoyce(successes = s[i],
                                      trials = obs[i])$weight
}

for (i in  1:100){
  weightsBySampleSizeLeft[i] <- weightJoyce(successes = s[i],
                                    trials = obs[i],
                                    credenceInHypotheses = c(.5, .3, .2))$weight
}



wbss <- data.frame( "sample size" = obs,
                          equal = weightsBySampleSize, 
                          ".5, .3, .2" = weightsBySampleSizeLeft)

wbss$frequency <- rep(.1, nrow(wbss))

wbss

s2 <- seq(1,500)
obs2 <-2 *s2   

s2

obs2


weightsBySampleSize2 <- numeric(length(s))
weightsBySampleSizeLeft2 <- numeric(length(s))


for (i in  1:500){
  weightsBySampleSize2[i] <- weightJoyce(successes = s2[i],
                                        trials = obs2[i])$weight
}

for (i in  1:500){
  weightsBySampleSizeLeft2[i] <- weightJoyce(successes = s2[i],
                                            trials = obs2[i],
                                            credenceInHypotheses = c(.5, .3, .2))$weight
}


wbssHalf <- data.frame( "sample size" = obs2,
                    equal = weightsBySampleSize2, 
                    ".5, .3, .2" = weightsBySampleSizeLeft2)

wbssHalf$frequency <- rep(.5, nrow(wbssHalf))


names(wbss) <- c("sampleSize", "equal", ".5, .3, .2", "frequency")
names(wbssHalf) <- c("sampleSize", "equal", ".5, .3, .2", "frequency")

wbss

wbssHalf


wbssLong <- gather(data = rbind(wbss, wbssHalf),
                          key = priors, value = w,
                          "equal", ".5, .3, .2", 
                          factor_key=TRUE)


head(wbssLong)

ggplot(wbssLong)+geom_line(aes(x = sampleSize,
                                y = w, color = priors,
                                lty = as.factor(frequency)) )+
  scale_x_continuous(breaks = seq(0,1000, by = 100))+theme_tufte(base_size = 14)+
  ylab("w")+
  xlab("sample size")+
  #  scale_y_continuous(breaks = seq(0,0.01, by = .001))+
  labs(title = "Joyce's weights  can drop with sample size",
       subtitle = "(eventually they stop growing)",
       lty = "observed frequency")+
  theme(plot.title.position = "plot")



ggplot(wbssLong)+geom_point(aes(x = sampleSize,
                                y = w, color = priors,
                                shape = frequency) )+
  scale_x_continuous(breaks = seq(0,1000, by = 10))+theme_tufte(base_size = 14)+
  ylab("w")+
  xlab("sample size")+
  #  scale_y_continuous(breaks = seq(0,0.01, by = .001))+
  labs(title = "Joyce's weights  by sample size",
       subtitle = "")+
  theme(plot.title.position = "plot")


joyceWBSS  <- ggplot(wbssLong)+geom_point(aes(x = sampleSize,
                                                   y = w, color = priors,
                                              shape = frequency) )+
  scale_x_continuous(breaks = seq(0,1000, by = 10))+theme_tufte(base_size = 14)+
  ylab("w")+
  xlab("sample size")+
#  scale_y_continuous(breaks = seq(0,0.01, by = .001))+
  labs(title = "Joyce's weights  by sample size",
       subtitle = "")+
  theme(plot.title.position = "plot")

joyceWBSS


joyce100



#now entropy-based weight for the same example


weightEntropyCustomChances <- function(
      hypotheses = c(.4, .5, .6),
      prior = c(1/3, 1/3, 1/3),
      successes  = 7,
      trials = 10){
      uniform <- rep(1,length(hypotheses))/ sum(rep(1,length(hypotheses)))
      #chance of evidence given data
      likelihood <- dbinom(x = successes, size = trials, prob = hypotheses)
      #overall chance of evidence (denumerator)
      evidence <-  sum(likelihood * prior)
      #chance of x given evidence
      posterior <- (likelihood * prior ) / evidence

      weightOfPrior <- 1 - (H(prior)/H(uniform, unit  = "log2"))
      weightOfPosterior <- 1 - (H(posterior)/H(uniform, unit  = "log2"))

      weightDelta <- weightOfPosterior - weightOfPrior
      return(list(prior = prior, weightOfPrior = weightOfPrior,
              posterior = posterior,
            weightOfPosterior = weightOfPosterior,
            weightDelta = weightOfPosterior - weightOfPrior)
      )
}

i <- 2

weightEntropyCustomChances(successes = 6)


outOfTenEntropyEqualPriors <- numeric(10)

for(i in seq(1,11, by  = 1)){
  outOfTenEntropyEqualPriors[i] <- 
    weightEntropyCustomChances(successes = i-1)$weightDelta
}


outOfTenEntropyEqualPriors

outOfTenEntropyLeftPriors <- numeric(10)

for(i in seq(1,11, by  = 1)){
  outOfTenEntropyLeftPriors[i] <- 
    weightEntropyCustomChances(prior = c(.5, .3, .2), successes = i-1)$weightDelta
}



outOf10entropyDf <- data.frame( successes = seq(0,10,1),
                         equal = outOfTenEntropyEqualPriors, ".5, .3, .2" =
                           outOfTenEntropyLeftPriors)

outOf10entropyDf

names(outOf10entropyDf) <- c("successes", "equal", ".5, .3, .2")

outOf10entropyDfLong  <- gather(data = outOf10entropyDf,
                         key = priors, value = w,
                         "equal", ".5, .3, .2", 
                         factor_key=TRUE)



outOf10Entropy  <- ggplot(outOf10entropyDfLong)+geom_point(aes(x = successes,
                                                 y = w, color = priors) )+
  scale_x_continuous(breaks = seq(0,10))+theme_tufte(base_size = 14)+ylab("W")+
  xlab("successes in ten trials")+
#  scale_y_continuous(breaks = seq(0,0.007, by = .001))+
  labs(title = "Information-theoretic weights",
       subtitle = "(sample size =1)")+theme(plot.title.position = "plot")

outOf10Entropy







#outdated
