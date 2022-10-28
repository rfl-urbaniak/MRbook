library(ggplot2)
library(ggthemes)
library(gridExtra)
library(philentropy)

priorA <- 1
priorB <- 1


set.seed(215)

trueH <- runif(1,0,1) 
trueH

sampleSize <- sample(10:20,size = 1)
sampleSize

testSize <- sampleSize

successes <- rbinom(1, sampleSize, trueH)

successes/sampleSize 

successes

posterior <- function(x) dbeta(x, priorA + successes,
                               priorB + sampleSize - successes)

#weightedPosterior <- function (x) x * posterior(x)
#pointPred <- integrate(weightedPosterior, 0, 1)[[1]]

pointEstimate <- successes/sampleSize
pointEstimate


pointPredictions <-  rbinom( 1e4 , size = testSize , prob = pointEstimate )



pointPlot <- ggplot()+geom_bar(aes(x= pointPredictions))+
  ggtitle("Predictions based on the point estimate")+xlim(0,testSize)


ps <- seq(0,1,length.out = 1001)


posteriorSample <- sample( ps , size=1e4 ,
                           replace=TRUE , posterior(ps)/sum(posterior(ps)))


samplesPlot <- ggplot()+geom_density(aes(x= posteriorSample))+
  ggtitle("Posterior samples")

posteriorPredictions <- rbinom( 1e4 , size=testSize , prob=posteriorSample )

posteriorPlot <- ggplot()+geom_bar(aes(x= posteriorPredictions))+
  ggtitle("Predictions based on the posterior sample")+xlim(0, testSize)


testPredictions <- rbinom( 1e4 , size = testSize , prob = trueH )

testPlot <- ggplot()+geom_bar(aes(x= testPredictions))+
  ggtitle("testPredictions")+xlim(0,testSize)

grid.arrange(testPlot,pointPlot,samplesPlot,posteriorPlot, ncol = 1)






mean(testPredictions >= 10)


mean(pointPredictions >= 10 )


mean(posteriorPredictions >= 10 )






