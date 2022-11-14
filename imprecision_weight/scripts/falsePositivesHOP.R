#install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
#devtools::install_github("rmcelreath/rethinking")
# we recommend running this is a fresh R session or restarting your current session
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


library(ggplot2)
library(rethinking)
library(truncnorm)
library(ggthemes)
library(reshape2)
library(gridExtra)

# In addition---and more importantly---equating the denominator $\pr{\textsf{match} \vert \neg \textsf{source}}$ with the random match probability ignores the risk of false positive matches. This risk is not negligible [@Shaer2016False]. The denominator, in fact, should depend on two sources of error: a false positive match and a coincidental match. These errors are quite distinct. For suppose two individuals---say the perpetrator and the defendant---happen to share the same DNA profile by coincidence. If an expert states that the crime scene sample and the defendant's sample match, this would be a coincidental match, not a false positive match. This risk of error is captured by the random match probability. But if the two samples do not actually match, and yet the expert says that they do, this would count as a false positive match, not a coincidental match. This risk of error is not captured by the random match probability.

#Let's now examine the impact of the  error probabilities FNP and FPP on the likelihood ratio,  holding fixed certain values of the random match probability. Figure \ref{fig:fpplr} shows the impact of error rates  (for values between 0 and .05). 

#!!Random match probabilities are assumed to be in the order of $10^{-9}$ (often reported in the case of two single-source samples over ten or more loci).

ps <- seq(0,1, length.out = 100001)


set.seed(1233)
fpdN <- ifelse(ps > 0 & ps <= .01015, 1, 0) 
fpdN <- fpdN/sum(fpdN)

fpdNsample <- sample(ps, 1e4, replace = TRUE, prob = fpdN)

ggplot()+geom_line(aes(x = ps, y = fpdN))+xlim(0,.02)


ggplot()+geom_density(aes(x = fpdNsample))+xlim(0,.02)

mean(fpdNsample <= .01)




set.seed(1233)
fpdWsample <- sample(ps, 1e4, replace = TRUE,
                     prob = dtruncnorm(ps, a = 0, b = 1, 
                                       mean = 0.0005,
                                       sd = .004))



ggplot()+geom_density(aes(x = fpdWsample))+xlim(0,.04)

mean(fpdWsample <= .01)

max(fpdWsample)


prior <- seq(0,1, by = 0.001)
posterior <- function(prior, rmp = 10e-9, fpp){ 
  return ( prior/(prior + rmp + fpp) )
}



pristinePosterior <- posterior(prior, rmp = 1e-9, fpp = 0)
min(prior[pristinePosterior >= .99])




posteriorN <- list()
posteriorW <- list()
minimaN <- numeric(1e4)
minimaW <- numeric(1e4)


for (s in 1:1e4){
  fppN <-    fpdNsample[s] 
  fppW <-    fpdWsample[s] 
  
  posteriorN[[s]] <- posterior(prior = prior, fpp = fpdNsample[s])
  posteriorW[[s]] <- posterior(prior = prior, fpp = fpdWsample[s])


}


str(posteriorN)


for (s in 1:1e4){
minimaN[s] <- ifelse(sum(posteriorN[[s]]> .99) >0, min(prior[posteriorN[[s]] > .99]), 1)
minimaW[s] <- ifelse(sum(posteriorW[[s]]> .99) >0, min(prior[posteriorW[[s]] > .99]), 1)
}


plot(prior,posteriorN[[5]])


posteriorN[[23]] >.99

min(prior[posteriorN[[4]] >.99])


posteriorNDF <-   do.call(cbind, posteriorN)
posteriorWDF <-   do.call(cbind, posteriorW)

str(posteriorNDF)
str(prior)

length(minimaN)

length(minimaW)

minimasDF <- data.frame(minima = c(minimaN, minimaW),
                        prior = c(rep("narrow", 1e4) , rep("wide", 1e4)) )


minimaPlot <- ggplot(minimasDF)+geom_histogram(aes(x = minima, fill = prior, 
                                                   y = ..count../sum(..count..)), bins = 40,
                                               position = "dodge2")+
  theme_tufte(base_size = 10)+ggtitle("Minimal priors sufficient for posterior >.99")+
  ylab("Second-order probability")+theme(legend.position = c(0.9,.9))



minimaPlot


minimaGrob <- ggplotGrob(minimaNPlot)

posteriorNDFsubset <- as.data.frame(posteriorNDF[,1:300])
posteriorNDFsubset$prior <- prior
posteriorNDFsubsetLong <- melt(posteriorNDFsubset, id.vars = "prior")


posteriorWDFsubset <- as.data.frame(posteriorWDF[,1:300])
posteriorWDFsubset$prior <- prior
posteriorWDFsubsetLong <- melt(posteriorWDFsubset, id.vars = "prior")




alpha = .05
size = .2

posteriorNplot <- ggplot(posteriorNDFsubsetLong, aes(x = prior, y = value, group = variable))+
  geom_line(alpha = alpha, size = size)+
  theme_tufte(base_size = 10)+ylab("posterior")+geom_line(aes(x = prior, y = pristinePosterior), 
                                                          color = "orangered")


posteriorWplot <- ggplot(posteriorWDFsubsetLong, aes(x = prior, y = value, group = variable))+
  geom_line(alpha = alpha, size = size)+
  theme_tufte(base_size = 10)+ylab("posterior")



grid.arrange(posteriorNplot+xlim(0,.2), posteriorWplot+xlim(0,.2),
             minimaPlot)










