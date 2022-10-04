library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)
getwd()
source("scripts/SCfunctions.R")


ps <- seq(0,1, length.out = 1001)
prior <- dbeta(ps,2,4)
prior <- prior/sum(prior)

priorSample <- sample(ps, prob = prior, size = 1e7, replace = TRUE)
  
peIfa <- .3
peIfna <- .2
pe <- peIfa * priorSample + (peIfna * (1-priorSample))
pae <- (peIfa * priorSample)/pe
priorHPDI <- HPDI(priorSample, prob = .89)
paeHPDI <- HPDI(pae, prob = .89)

abusePlot_3_2 <-  ggplot()+theme_tufte(base_size=7)+xlab("parameter values")+
  ylab("density")+theme(plot.title.position = "plot")+
  labs(title = "Prior beta(2,4) and posterior in the abuse case",
         subtitle = "P(E|A)=.3, P(E|~A)=.2")+
  geom_density(aes(x = priorSample), lty =2 )+
  geom_density(aes(x = pae), color = "skyblue" )+
  annotate(geom = "label", 
           label = paste("prior median: ", round(median(priorSample),2), ", 89% HPDI = (", round(priorHPDI[1],2), 
                         ", ", round(priorHPDI[2],2), ")", sep = ""),
           x = median(priorSample), y = 2.2, size = 2.5)+
  annotate(geom = "label", 
           label = paste("posterior median: ", round(median(pae),2), ", 89% HPDI = (", round(paeHPDI[1],2), 
                         ", ", round(paeHPDI[2],2), ")", sep = ""),
           x = median(pae), y = 1.9, size = 2.5)


  
  ## now say it's .3 vs. 0.001
  
peIfa2 <- .3
peIfna2 <- .05
peIfa2 * priorSample
pe2 <- peIfa2 * priorSample + (peIfna2 * (1-priorSample))
pae2 <- (peIfa2 * priorSample)/pe2
priorHPDI <- HPDI(priorSample, prob = .89)
pae2HPDI <- HPDI(pae2, prob = .89)


ggplot()+theme_tufte(base_size=7)+xlab("parameter values")+
  ylab("density")+theme(plot.title.position = "plot")+
  labs(title = "Prior beta(2,4) and posterior in the abuse case",
       subtitle = "P(E|A)=.3, P(E|~A)=.05")+
  geom_density(aes(x = priorSample), lty =2 )+
  annotate(geom = "label", 
           label = paste("prior median: ", round(median(priorSample),2), ", 89% HPDI = (", round(priorHPDI[1],2), 
                         ", ", round(priorHPDI[2],2), ")", sep = ""),
           x = median(priorSample), y = 2.2, size = 2.5)+
    geom_density(aes(x = pae2), color = "skyblue", lty = 1 )+
    annotate(geom = "label", 
             label = paste("posterior median: ", round(median(pae2),2), ", 89% HPDI = (", round(pae2HPDI[1],2), 
                           ", ", round(pae2HPDI[2],2), ")", sep = ""),
             x = median(pae2), y = 2, size = 2.5)
  

  
#now with uncertainty about conditional probabilities
peIfa3 <- rnorm(.3, sd = .03, n = 1e7)
peIfna3 <- rnorm(.05, sd = .01, n = 1e7)
pe3 <- peIfa3 * priorSample + (peIfna3 * (1-priorSample))
pae3 <- (peIfa3 * priorSample)/pe3
pae3HPDI <- HPDI(pae3, prob = .89)


ggplot()+theme_tufte(base_size=7)+xlab("parameter values")+
  ylab("density")+theme(plot.title.position = "plot")+
  labs(title = "Prior beta(2,4) and posterior in the abuse case",
       subtitle = "P(E|A)~N(.3,.03), P(E|~A)~N(.05, .01)")+
  geom_density(aes(x = priorSample), lty =2 )+
  annotate(geom = "label", 
           label = paste("prior median: ", round(median(priorSample),2), ", 89% HPDI = (", round(priorHPDI[1],2), 
                         ", ", round(priorHPDI[2],2), ")", sep = ""),
           x = median(priorSample), y = 2.2, size = 2.5)+
  geom_density(aes(x = pae3), color = "skyblue", lty = 2 )+
  annotate(geom = "label", 
           label = paste("posterior median: ", round(median(pae2),2), ", 89% HPDI = (", round(pae2HPDI[1],2), 
                         ", ", round(pae2HPDI[2],2), ")", sep = ""),
           x = median(pae2), y = 2, size = 2.5)




#now weights

unif <-dbeta(ps,1,1)
unif <- unif/sum(unif)
hunif <- H(unif)

weightAbs(prior)
weightAbs(pae)










