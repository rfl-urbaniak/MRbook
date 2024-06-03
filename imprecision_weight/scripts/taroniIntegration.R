library(ggplot2)
library(ggthemes)
library(gridExtra)
library(philentropy)

priorA <- 1
priorB <- 1


set.seed(215)
trueH <- runif(1,0,1) 

sampleSize <- sample(10:20,size = 1)
testSize <- sampleSize

set.seed(319)
successes <- rbinom(1, sampleSize, trueH)
pointEstimate <- successes/sampleSize

pointPredictions <-  rbinom( 1e4 , size = testSize , prob = pointEstimate )

ps <- seq(0,1,length.out = 1001)

testPredictions <- rbinom( 1e4 , size = testSize , prob = trueH )

posterior <- function(x) dbeta(x, priorA + successes,
                               priorB + sampleSize - successes)

posteriorSample <- sample( ps , size=1e4 ,
                           replace=TRUE , posterior(ps)/sum(posterior(ps)))



posteriorPredictions <- rbinom( 1e4 , size=testSize , prob=posteriorSample )


trueH
sampleSize
successes
successes/sampleSize 


testPlot <- ggplot()+geom_bar(aes(x= testPredictions, y = ..prop..))+
  ggtitle(paste("Predictions based on the true parameter = ", round(trueH,2), sep = ""))+xlim(0,testSize)+ylab("probability")+xlab("successes")+theme_tufte(base_size = 10)+theme(plot.title.position = "plot")


pointPlot <- ggplot()+geom_bar(aes(x= pointPredictions,y = ..prop..))+
  labs(title = paste("Predictions based on the point estimate = ", round(pointEstimate,2)), subtitle = paste(successes, " successes in ", sampleSize, " observations", sep = ""))+xlim(0,testSize)+ylab("probability")+xlab("successes")+theme_tufte(base_size = 10)+theme(plot.title.position = "plot")


samplesPlot <- ggplot()+geom_density(aes(x= posteriorSample))+
  ggtitle(paste("Posterior sample from beta(", 1+successes, ",", 1+testSize - successes,
                ")", sep = ""))+xlab(
                  "parameter value"
                )+theme_tufte(base_size = 10)+theme(plot.title.position = "plot")

posteriorPlot <- ggplot()+geom_bar(aes(x= posteriorPredictions,y = ..prop..))+
  ggtitle("Predictions based on the posterior sample")+xlim(0, testSize)+ylab("probability")+xlab("successes")+theme_tufte(base_size = 10)+theme(plot.title.position = "plot")


grid.arrange(testPlot,pointPlot,samplesPlot,posteriorPlot, ncol = 1)


table(testPredictions)


testProbs <- table(factor(testPredictions, levels = seq(0,sampleSize)))/1e4
pointProbs <- table(factor(pointPredictions, levels = seq(0,sampleSize)))/1e4
posteriorProbs <- table(factor(posteriorPredictions, levels = seq(0,sampleSize)))/1e4 


kld(testProbs,pointProbs)
kld(testProbs,posteriorProbs)


length(testPredictions)



mean(testPredictions >= 9)
mean(pointPredictions >= 9 )
mean(posteriorPredictions >= 9 )

mean(testPredictions <= 9)
mean(pointPredictions <= 9 )
mean(posteriorPredictions <= 9 )




#now in a loop


klds <- numeric(1000)
for(i in 1:1000){
trueH <- runif(1,0,1) 
sampleSize <- sample(10:25,size = 1)
testSize <- sampleSize
successes <- rbinom(1, sampleSize, trueH)
pointEstimate <- successes/sampleSize
pointPredictions <-  rbinom( 1e4 , size = testSize , prob = pointEstimate )
ps <- seq(0,1,length.out = 1001)
testPredictions <- rbinom( 1e4 , size = testSize , prob = trueH )
posteriorSample <- sample( ps , size=1e4 ,
                           replace=TRUE , posterior(ps)/sum(posterior(ps)))
posteriorPredictions <- rbinom( 1e4 , size=testSize , prob=posteriorSample )
testProbs <- table(factor(testPredictions, levels = seq(0,sampleSize)))/1e4
pointProbs <- table(factor(pointPredictions, levels = seq(0,sampleSize)))/1e4
posteriorProbs <- table(factor(posteriorPredictions, levels = seq(0,sampleSize)))/1e4 
klds[i] <- kld(testProbs,pointProbs) - kld(testProbs,posteriorProbs)
}

mean(klds)
median(klds)

ggplot()+geom_density(aes(x = klds))














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










