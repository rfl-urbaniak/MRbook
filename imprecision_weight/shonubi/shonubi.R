library(ggplot2)
library(ggthemes)
library(ggExtra)
library(gridExtra)
library(ggdogs)
library(reshape2)
library(rethinking)
library(ggcorrplot)
library(dagitty)
library(dplyr)
library(haven)
library(ggcorrplot)
library(bayesrules)
library(dplyr)
library(forcats)
library(stringr)

th <- theme_tufte(base_size = 10) + theme(plot.title.position = "plot")

#SEE
#Statistical Issues in the Application of the Federal Sentencing
#Guidelines in Drug, Pornography, and Fraud Cases
#Alan J. Izenman


#United States v. Shonubi (1997): the defendant was found to have
#swallowed 103 balloons of heroin (with a total weight of427.4 grams) when
#arrested at JFK International Airport. The defendant's
#passport and employment record indicated seven previous overseas trips to Nigeria
#from the United States whose purposes were not satisfactorily explained.
#The main question was how to estimate Q, the total amount smuggled from all eight trips
#- individuals carrying 3000 or more grams of heroin receive 
#larger sentences than those bringing in 1000-3000

# - The first estimate was simple: 8 x 427.4 grams 3419.2 grams, leading to a 
#sentencing range of 151-188 months and a prison term of 151 months

# - The second estimate was much more complicated, with extensive evidentiary 
#hearings and reports by experts. 

#We focus on the JFK airport dataset

#set working folder to source file directory

sh <- read.csv("ShonubiCaseDataset.csv")

#FIRST, STRATEGY WHICH USES POSTERIOR MEANS ONLY


#note there are 107 cases with gross_wt, but no net_wt. Let's first predict
#those, then fill them in to predict weight based on balloons

sum(is.na(sh$net_wt))

netData <- sh %>% select(gross_wt, net_wt)
netData <- netData[complete.cases(netData),]

netModel <- quap(
  alist(
    net_wt ~ dnorm( mu , sigma ) ,
    mu <- m * gross_wt,
    m ~ dnorm(.8,.3) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=netData)


precis(netModel)

#note how the model is pretty certain about where the mean is, but has a
#rather huge sd. If we focus on mean values, we ignore this uncertainty


#suppose we assign nean predictions for the NAs

#extract cases that need predictions
fillNet <- sh %>% filter(!is.na(gross_wt),is.na(net_wt))
netPred <- sim(netModel,fillNet)
fillNet$mean <- apply(netPred, 2, mean)

#now let's plug known net when it's known, predicted mean if it's not
sh$netEstimate <- ifelse( !is.na(sh$net_wt), sh$net_wt, NA )

for (obs in 1:218){
  if (is.na(sh$netEstimate[obs])){
    sh$netEstimate[obs] <- fillNet$mean[fillNet$obs == obs]
  }
}


#now let's us this estimate learn the relation between balloons
#and net weight

balloonsData <- sh %>% select(balloons, netEstimate)
balloonsData <- balloonsData[complete.cases(balloonsData),]


mean(balloonsData$netEstimate)
mean(balloonsData$netEstimate/balloonsData$balloons)
sd(balloonsData$netEstimate/balloonsData$balloons)

dens(balloonsData$netEstimate/balloonsData$balloons)


sd(balloonsData$netEstimate)


set.seed(123)
balloonsModel <-  quap(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- baseline + b + balloons,
    baseline ~ dnorm(400,100) ,
    b ~ dnorm(15,25) ,
    sigma ~ dnorm( 200 , 200 )
  ) , data=balloonsData)


precis(balloonsModel)



#but wait, Shonubi used pretty small balloons



set.seed(123)
balloonsModelShonubi <-  quap(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- baseline + b + balloons,
    baseline ~ dnorm(369,34) ,
    b ~ dnorm(14.78,24.3) ,
    sigma ~ dnorm( 311.68 , 18.41 )
  ) , data= list(netEstimate = 427, balloons = 103))


precis(balloonsModelShonubi)





#so we'll use the predicted number of balloons by taking

#369 + 14.7 * balloons


#we'll use the point estimate from this model once we make a prediction of
#how many balloons Shonubi carried on all trips, we start estimating the 
#numbers of carried baloons in general and then use the result as a prior 
#for shonubi with one observation only, 103 



balloonsLambdaModel <- quap(
  alist(
    balloons ~ dpois( lambda ),
    log(lambda) <- a,
    a ~ dnorm(5,5)
  ), data=balloonsData)


precis(balloonsLambdaModel)

ballonsSim <- sim(balloonsLambdaModel)

sd(ballonsSim)
mean(ballonsSim)



balloonsLambdaShonubiModel <- quap(
  alist(
    balloons ~ dpois( lambda ),
    log(lambda) <- a,
    a ~ dnorm(4.22,.01)
  ), data= list(balloons = 103))

balloonsShonubiSim <- sim(balloonsLambdaShonubi)

mean(balloonsShonubiSim)

mean(sh$balloons, na.rm = TRUE)


68 * 8

#just a bit more than the mean, let's go with the mean prediction


predictedBallonsShonubiMean <- 8 * mean(balloonsShonubiSim)

predictedBallonsShonubiMean

#now let's use the balloons model to predict the total weight

predictedBallonsShonubiMean

shonubiNetPredictionMean <- sim(balloonsModel, data = list(balloons = 546))



mean(shonubiNetPredictionMean)


369 + 14.7 * 546

#this is above 3000 grams







#__________________________________________





set.seed(123)

netData <- sh %>% select(gross_wt, net_wt)
netData <- netData[complete.cases(netData),]
netData

netModel <- ulam(
  alist(
    net_wt ~ dnorm( mu , sigma ) ,
    mu <- m * gross_wt,
    m ~ dnorm(.8,.3) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=netData, log_lik = TRUE )

precis(netModel)


fillNet <- sh %>% filter(!is.na(gross_wt),is.na(net_wt))

netPred <- sim(netModel,fillNet)


data.frame(t(apply(netPred, 2, HPDI)))

fillNet$low <- data.frame(t(apply(netPred, 2, HPDI)))[,1]


sh$netEstimate <- ifelse( !is.na(sh$net_wt), sh$net_wt, NA )

for (obs in 1:218){
  if (is.na(sh$netEstimate[obs])){
    sh$netEstimate[obs] <- fillNet$low[fillNet$obs == obs]
  }
}


sh

ageData <- sh %>% select(netEstimate,age_yrs)
ageData <- ageData[complete.cases(ageData),]


ageModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- baseline + a * age_yrs,
    baseline ~ dnorm(400,100) ,
    a ~ dnorm(1,.5) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=ageData, log_lik = TRUE )



precis(ageModel)


ageData$age_yrs <- standardize(ageData$age_yrs)

ageData$age_yrs


ageModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- baseline + a * age_yrs,
    baseline ~ dnorm(400,100) ,
    a ~ dnorm(1,.5) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=ageData, log_lik = TRUE )


precis(ageModel)




ageGenderData <- sh %>% select(netEstimate,age_yrs, gender)

ageGenderData <- ageGenderData[complete.cases(ageGenderData),]

nrow(ageGenderData)

str(ageGenderData)

ageGenderData$gender

ageGenderData$gender <- ageGenderData$gender + 1

ageGenderModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- baseline + a * age_yrs + g[gender],
    baseline ~ dnorm(400,100) ,
    a ~ dnorm(1,.5) ,
    g[gender] ~ dnorm(1,.5) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=ageGenderData, log_lik = TRUE )


precis(ageGenderModel, depth = 2)


#consider Shonubi's age, 28
tail(standardize(c(sh$gender, 28)), n = 1)


shonubi <- data.frame(age_yrs = 10.68, gender = 1, balloons = 103)

shonubiAgeGenderPred <-  8 * sim(ageGenderModel,data = shonubi)

dens(shonubiAgeGenderPred)


#-------------------

sh

balloonsData <- sh %>% select(balloons, netEstimate)
balloonsData <- balloonsData[complete.cases(balloonsData),]

balloonsModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- baseline + b + balloons,
    baseline ~ dnorm(400,100) ,
    b ~ dnorm(15,5) ,
    sigma ~ dunif( 0 , 20 )
  ) , data=balloonsData, log_lik = TRUE )


precis(balloonsModel)



shonubiBalloonsPred <-  8 * sim(balloonsModel,data = shonubi)


dens(shonubiAgeGenderPred)

dens(shonubiBalloonsPred)


dens(sh$balloons)


mean(sh$balloons, na.rm = TRUE)


max(sh$balloons)

shonubi2 <- data.frame(balloons = 50:110)

shonubiBalloonsPred2 <-  sim(balloonsModel,data = shonubi2)

str(shonubiBalloonsPred2)

shonubiWide <- 8 * c(shonubiBalloonsPred2)

dens(shonubiWide)

mean(shonubiWide > 3000)






