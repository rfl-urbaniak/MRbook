library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)



ps <- seq(0,1, length.out = 1000)
prior <- dbeta(ps,2,4)
prior <- prior/sum(prior)

priorSample <- sample(ps, prob = prior, size = 1e7, replace = TRUE)
  
peIfa <- .3
peIfna <- .2
pe <- peIfa * priorSample + (peIfna * (1-priorSample))
pae <- (peIfa * priorSample)/pe
priorHPDI <- HPDI(priorSample, prob = .89)
paeHPDI <- HPDI(pae, prob = .89)

#abusePlot_3_2 <- 
  
  ggplot()+theme_tufte(base_size=7)+xlab("parameter values")+
  ylab("density")+theme(plot.title.position = "plot")+
  labs(title = "Prior beta(2,4) and posterior in the abuse case",
         subtitle = "P(E|A)=.3, P(E|~A)=.2")+
  geom_density(aes(x = priorSample), lty =2 )+
  geom_density(aes(x = pae), color = "skyblue" )+
  annotate(geom = "label", 
           label = paste("prior median: ", round(median(priorSample),2), ", 89% HPDI = (", round(priorHPDI[1],2), 
                         ", ", round(priorHPDI[2],2), ")", sep = ""),
           x = median(priorSample), y = 2.2, size = 3)+
  annotate(geom = "label", 
           label = paste("posterior median: ", round(median(pae),2), ", 89% HPDI = (", round(paeHPDI[1],2), 
                         ", ", round(paeHPDI[2],2), ")", sep = ""),
           x = median(pae), y = 1.9, size = 3)


  
  ## now say it's .3 vs. 0.001
  
peIfa2 <- .3
peIfna2 <- .001
peIfa2 * priorSample
pe2 <- peIfa2 * priorSample + (peIfna2 * (1-priorSample))
pae2 <- (peIfa2 * priorSample)/pe2
priorHPDI <- HPDI(priorSample, prob = .89)
pae2HPDI <- HPDI(pae2, prob = .89)

  
