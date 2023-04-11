#terminal: sudo apt install libmagick++-dev 
#remotes::install_github("R-CoderDotCom/ggdogs@main")

library(ggplot2)
library(ggthemes)
library(ggExtra)
library(gridExtra)
library(ggdogs)
library(reshape2)
library(rethinking)
library(lubridate)
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


#1. in most criminal cases, sentencing is divorced from the trial itself. In a secondary
#phase of a federal criminal proceeding following conviction, the defendant's sen-
#tence is determined by the judge



#2. The practice of sampling to determine the total quantity of illicit drugs seized:
#If all the drugs seized are found in a number of containers, such as plastic baggies,
#then a sampling plan can be devised in a straightforward way.


#3. Anomaly: United States v. Shonubi (1997): the defendant was found to have
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


sh <- read.csv("ShonubiCaseDataset.csv")

#E1. Check the head and names

names(sh)


#E2. What are the raw estimates obtained by multiplying net weights by 8?

rawEstimate <- ____ * ____

#E3. Plot their density using dens()

____


sum(is.na(rawEstimate))


netData <- sh %>% select(gross_wt, net_wt)
netData <- netData[complete.cases(netData),]
netData

#E4. Train a linear model that predicts net_wt by multiplying gross_wt

set.seed(123)
netModel <- ulam(
  alist(
    net_wt ~ dnorm( _____ , sigma ) ,
    mu <- _____  * _____,
    m ~ dnorm(.8,.3) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=netData, log_lik = TRUE )

precis(netModel)




fillNet <- sh %>% filter(!is.na(gross_wt),is.na(net_wt))

#E5. Use sim() and this model to make predictions for the fillNet dataset

netPred <- sim(_____,  ______)


data.frame(t(apply(netPred, 2, HPDI)))
fillNet$low <- data.frame(t(apply(netPred, 2, HPDI)))[,1]


sh$netEstimate <- ifelse( !is.na(sh$net_wt), sh$net_wt, NA )
for (obs in 1:218){
  if (is.na(sh$netEstimate[obs])){
    sh$netEstimate[obs] <- fillNet$low[fillNet$obs == obs]
  }
}


sh

#E6. Select onnly netEstimate and age, only complete cases
ageData <- sh %>% select(___,___)
ageData <- ageData[complete.cases(____),]



#E7. Build a model that predicts netEstimate based on age
ageModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- _______ + _______  * _________,
    baseline ~ dnorm(400,100) ,
    a ~ dnorm(1,.5) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=ageData, log_lik = TRUE )



precis(ageModel)


ageData$age_yrs <- standardize(ageData$age_yrs)

ageData$age_yrs


#E8 Do this again after standardization
ageModel <-  ulam(
  
  )


precis(ageModel)




ageGenderData <- sh %>% select(netEstimate,age_yrs, gender)

ageGenderData <- ageGenderData[complete.cases(ageGenderData),]

nrow(ageGenderData)

str(ageGenderData)

ageGenderData$gender

ageGenderData$gender <- ageGenderData$gender + 1

#E9 Now a model that uses both age and gender (watch out, gender is a categorical variable,
#allow for differing slopes)
ageGenderModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- baseline + a[___] * age_yrs + ____ ,
    baseline ~ dnorm(400,100) ,
    a[gender] ~ dnorm(1,.5) ,
    gb[gender] ~ dnorm(0,100) ,
    sigma ~ dunif( 0 , 150 )
  ) , data=ageGenderData, log_lik = TRUE )


precis(ageGenderModel, depth = 2)


#consider Shonubi's age, 28
tail(standardize(c(sh$gender, 28)), n = 1)


#E10. Make predictions about Shonubi using this model, plot density and see what the 
#stimated probabiltiy of 8 trips totalling below 3kg is
shonubi <- data.frame(age_yrs = ____, gender = ____, balloons = 103)

shonubiAgeGenderPred <-  ____ * sim(ageGenderModel,data = shonubi)

dens(shonubiAgeGenderPred)

mean(_______________________)




balloonsData <- sh %>% select(balloons, netEstimate)
balloonsData <- balloonsData[complete.cases(balloonsData),]


#E11. build a  model that predicts the estimate based on balloons
balloonsModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- ___________________,
    baseline ~ dnorm(200,100) ,
    b ~ dnorm(10,5) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=balloonsData, log_lik = TRUE )


precis(balloonsModel)


#E12 Notice that sigma is at the edge, you should increase it until it isn't 
balloonsModel <-  ulam(
  alist(
    netEstimate ~ dnorm( mu , sigma ) ,
    mu <- _______,
    baseline ~ dnorm(200,100) ,
    b ~ dnorm(10,5) ,
    sigma ~ dunif( 0 , _____)
  ) , data=balloonsData, log_lik = TRUE )


precis(balloonsModel)

#E13. Now make predictions for 8 trips using sim() and multiplying, for now use the shonubi df
shonubiBalloonsPred <-  ____ * sim(_____, _____)


dens(shonubiAgeGenderPred)

dens(shonubiBalloonsPred)


#E14. What's the estimate probability that the total amount is below 3000?

mean(shonubiBalloonsPred < 3000)




dens(sh$balloons)
mean(sh$balloons, na.rm = TRUE)


max(sh$balloons)

shonubi2 <- data.frame(balloons = 50:110)

#E15. Now analogous prediction for the range of 50:110 (with equal probability)
shonubiBalloonsPred2 <-  sim(______, _____________)

str(shonubiBalloonsPred2)

shonubiWide <- 8 * c(shonubiBalloonsPred2)

dens(shonubiWide)

mean(shonubiWide > 3000)





