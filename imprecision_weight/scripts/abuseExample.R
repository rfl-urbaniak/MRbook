library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)
library(truncnorm)
getwd()
source("imprecision_weight/scripts/SCfunctions.R")


ps <- seq(0,1, length.out = 1001)
prior <- dbeta(ps,2,4)
prior <- prior/sum(prior)

priorSample <- sample(ps, prob = prior, size = 1e7, replace = TRUE)
  



peIfa <- .3
peIfna <- .2
pe <- peIfa * priorSample + (peIfna * (1-priorSample))
pae <- (peIfa * priorSample)/pe
priorHPDI <- round(HPDI(priorSample, prob = .89),3)
paeHPDI <- round(HPDI(pae, prob = .89),3)

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
pae2HPDI <- round(HPDI(pae2, prob = .89),3)


abusePlot_3_05 <- ggplot()+theme_tufte(base_size=7)+xlab("parameter values")+
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
peIfa3 <- rtruncnorm(.3, sd = .1, n = 1e7, a = 0, b = 1)
peIfna3 <- rtruncnorm(.05, sd = .05, n = 1e7, a  = 0, b = 1)
pe3 <- peIfa3 * priorSample + (peIfna3 * (1-priorSample))
pae3 <- (peIfa3 * priorSample)/pe3
pae3HPDI <- round(HPDI(pae3, prob = .89),3)

pae3HPDI

abusePlot_31_0505 <-  ggplot()+theme_tufte(base_size=7)+xlab("parameter values")+
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



peIfa4 <- rtruncnorm(.6, sd = .05, n = 1e7, a = 0, b = 1)
peIfna4 <- rtruncnorm(.01, sd = .03, n = 1e7, a  = 0, b = 1)
pe4 <- peIfa4 * priorSample + (peIfna4 * (1-priorSample))
pae4 <- (peIfa4 * priorSample)/pe4
pae4HPDI <- round(HPDI(pae4, prob = .89),3)



#now weights

unif <-dbeta(ps,1,1)
unif <- unif/sum(unif)
hunif <- H(unif)



paeDistro <- distroFromSamples(samples = pae,   precision = 1001)



pae2Distro <- distroFromSamples(pae2, precision = 1001)

pae3Distro <- distroFromSamples(pae3, precision= 1001)

pae4Distro <- distroFromSamples(pae4, precision= 1001)
  



distribution <- c("prior", ".3/.2", ".3/.05", 
                   "tN(.3,.1)/tN(.05,.05)", 
                   "tN(.6, .05)/tN(.01,.03)")

median <- round(c(median(priorSample), median(pae), median(pae2), median(pae3), 
                median(pae4)),3)


HPDI <- c(paste(priorHPDI[1], ", " ,  priorHPDI[2], sep = "" ),
           paste(paeHPDI[1], ", " ,  paeHPDI[2], sep = "" ),
           paste(pae2HPDI[1], ", " ,  pae2HPDI[2], sep = "" ),
           paste(pae3HPDI[1], ", " ,  pae3HPDI[2], sep = "" ),
           paste(pae4HPDI[1], ", " ,  pae4HPDI[2], sep = "" )
              )



absolute <- round(c(weightAbs(prior), weightAbs(paeDistro),
              weightAbs(pae2Distro), weightAbs(pae3Distro), 
              weightAbs(pae4Distro)),3) 

proportional <- round(absolute/weightAbs(prior) - 1, 3)

delta <-  round(absolute - weightAbs(prior),3)



stats <- rbind(distribution, median, HPDI, absolute,
               proportional, delta)


stats

?tableGrob

plotDistroPlain(paeDistro, multiplier = 4.5)+
  geom_line(aes(x = ps,y = prior), color = "grey", lty = 2)+
  geom_line(aes(x = ps,y = pae2Distro), color = "skyblue")+
  geom_line(aes(x = ps,y = pae3Distro), color = "orangered")+
  geom_line(aes(x = ps,y = pae4Distro), color = "green")+
  annotation_custom(tableGrob(stats, 
                  theme = ttheme_minimal(base_size = 7.5),
                              ),
                    xmin=0.1, xmax=0.7, ymin=0.005, ymax=0.009)+
  theme(plot.title.position = "plot")+
  labs(title ="Prior and  posteriors in the rocking example",
       subtitle = "(with weights and weight shifts)")+
  annotate(geom = "label", 
           label = "prior", x = .25, y = .0024, color = "grey")+
  annotate(geom = "label", 
           label = ".3/.2", x = .41, y = .00215)+
  annotate(geom = "label", 
           label = ".3/.05", x = .8, 
           y = .003, color = "skyblue")+
  annotate(geom = "label", 
           label = "tN(.3,.1)/tN(.05,.05)", x = .9, 
           y = .0022, color = "orangered")+
  annotate(geom = "label", 
           label = "tN(.6,.05)/tN(.01,.03)", x = .9, 
           y = .005, color = "green")






