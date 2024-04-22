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

head(sh)

weights <- data.frame(gross = sh$gross_wt,
                      net = sh$net_wt)
weights <- weights[complete.cases(weights), ]

weights_melted <- melt(weights)

ggplot(weights_melted, aes(x= variable, y= value)) + geom_jitter() +
  geom_boxplot()+ th
  

ggplot() + geom_density(aes(x = weights$gross), col= 'skyblue', linewidth = 1)+
  geom_density(aes(x = weights$net), col= 'purple', linewidth = 1)+ th+ 
  geom_vline(xintercept = mean(weights$gross), linetype = "dashed", color = "red")+
  geom_vline(xintercept = mean(weights$net), linetype = "dashed", color = "red")
  
mean(weights$net)


weights_arrDiff <- weights %>% 
  arrange(gross) %>% 
  mutate(diff = gross - net) %>% 
  mutate(proportion = net / gross)

dens(weights_arrDiff$proportion)

ggplot(weights_arrDiff, aes(x= 1:nrow(weights_arrDiff), y= proportion)) + geom_line()+
  geom_smooth()

ggplot(weights_arrDiff, aes(x= diff, y= proportion)) + geom_line()+
  geom_smooth()


#FIRST, STRATEGY WHICH USES POSTERIOR MEANS ONLY


#note there are 107 cases with gross_wt, but no net_wt. Let's first predict
#those, then fill them in to predict weight based on balloons

nrow(sh)
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


#suppose we assign mean predictions for the NAs

#extract cases that need predictions
fillNet <- sh %>% filter(!is.na(gross_wt),is.na(net_wt))
netPred <- sim(netModel,fillNet)

str(netPred)

fillNet$mean <- apply(netPred, 2, mean)

fillNet$mean

#for future reference we will also introduce HPDI low
fillNet$low <- t(apply(netPred, 2, HPDI))[,1]

fillNet$low <- ifelse(fillNet$low < 0, 0, fillNet$low)

head(fillNet)



#now let's plug known net when it's known, predicted mean if it's not
sh$netEstimate <- ifelse( !is.na(sh$net_wt), sh$net_wt, NA )
sh$netEstimateLow <- ifelse( !is.na(sh$net_wt), sh$net_wt, NA )


for (obs in 1:218){
  if (is.na(sh$netEstimate[obs])){
    sh$netEstimate[obs] <- fillNet$mean[fillNet$obs == obs]
    sh$netEstimateLow[obs] <- fillNet$low[fillNet$obs == obs]
  }
}

#now let's us this estimate learn the relation between balloons
#and net weight

balloonsData <- sh %>% select(balloons, netEstimate, netEstimateLow)
balloonsData <- balloonsData[complete.cases(balloonsData),]

mean(balloonsData$netEstimate)
mean(balloonsData$netEstimate/balloonsData$balloons)
sd(balloonsData$netEstimate/balloonsData$balloons)

dens(balloonsData$netEstimate/balloonsData$balloons)


sd(balloonsData$netEstimate)


names(balloonsData)

set.seed(123)
balloonsModel <-  quap(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <-  b * balloons,
    b ~ dnorm(15,25) ,
    sigma ~ dnorm( 200 , 200 )
  ) , data=balloonsData)

precis(balloonsModel)


#for later use and comparison
balloonsModelLow  <-  quap(
  alist(
    netEstimateLow ~ dnorm( mu , sigma ) ,
    mu <-  b * balloons,
    b ~ dnorm(15,25) ,
    sigma ~ dnorm( 200 , 200 )
  ) , data=balloonsData)


precis(balloonsModel)
precis(balloonsModelLow)


#now take the empirical distro of ballons as your prior, 
#train on one example of Shonubi, predict the number of balloons on 
#8000 trips, 
#take the mean, i.e. divide by 1000, that's your point estimate of the total 
#number of balloons he carried


set.seed(123)
balloonsCarriedModel <-  quap(
  alist(
    balloons ~ dnorm( mu , sigma ) ,
    mu <- a,
    a ~ dnorm(70, 50) ,
    sigma ~ dunif(0,50)
  ) , data= balloonsData)

precis(balloonsCarriedModel)


set.seed(123)
balloonsCarriedModelShonubi <-  ulam(
  alist(
    balloons ~ dnorm( mu , sigma ) ,
    mu <- a,
    a ~ dnorm(68, 40) ,
    sigma ~ dunif(0,50)
  ) , data= list(balloons = 103))


precis(balloonsCarriedModelShonubi)


simBaloonsShonubi <- sim(balloonsCarriedModelShonubi)
dens(simBaloonsShonubi)

pointBallonsShonubi <- mean(simBaloonsShonubi) * 8
pointBallonsShonubi

#now use the balloons model to mean-predict net

NetShonubi <- link(balloonsModel, data = list(balloons = 735))
pointNetShonubi <- mean(NetShonubi)

pointNetShonubi
#the predicted value is 4664.925

#1. We're gonna use balloonsModelLow instead of balloonsModel
#2. We're gonna use the whole distro for the number of balloons he carried

allTripsBalloonsDistro <- 8 * simBaloonsShonubi 

allTripsBalloonsDistro <- data.frame(balloons = allTripsBalloonsDistro)

str(allTripsBalloonsDistro)
#ach of this number is going to be associated with a *simulated prediction*

allTripsNetDistro <-  sim(balloonsModel, data  =
                             list(balloons = 
                                    as.vector(allTripsBalloonsDistro$balloons)))

str(allTripsNetDistro )


dens( allTripsNetDistro ) 

mean( allTripsNetDistro)


mean(allTripsNetDistro > 3000)







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






