library(bnlearn)
library(Rgraphviz)
library(gRain)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rethinking)
library(philentropy)



plotDistroPlain <- function(distro, title =  " ", multiplier = 1.2) {
  plot <-  ggplot()+theme_tufte()+xlab("parameter values")+
    ylab("probability")+theme(plot.title.position = "plot")+
    ggtitle(title)+
    geom_line(aes(x = ps,y = distro))+
    ylim(c(0,multiplier * max(distro)))
  return(plot)
}


distroNorm <- function(mean, sigma){
  distro <-   dnorm(ps, mean, sigma)
  distro <- distro/sum(distro)
  return(distro)
}

sampleNorm <- function(mean, sigma){
  sample(ps, size = 1e4, replace = TRUE,prob = distroNorm(mean,sigma))
}


distroBeta <- function(a, b){
  distro <-   dbeta(ps, a, b)
  distro <- distro/sum(distro)
  return(distro)
}


sampleBeta <- function(a, b){
  sample(ps, size = 1e4, replace = TRUE,prob = distroBeta(a,b))
}


plotSample <- function (sample, title, subtitle){
  ggplot()+theme_tufte()+xlab(expression(theta))+
    ylab("density")+theme(plot.title.position = "plot")+
    ggtitle(title)+
    geom_density(aes(x = sample ))+
    labs(title = title, subtitle = subtitle)
}


distroFromSamples <- function (samples, precision = 1001){
  distro <-  density(na.omit(samples), n = precision)$y
  distro <- distro/sum(distro)
  return(distro)
}


kld <- function(p,q) kullback_leibler_distance(p,q, testNA = TRUE, unit = "log2",
                                               epsilon = 0.00001)

unif <-dbeta(ps,1,1)
unif <- unif/sum(unif)
hunif <- H(unif)
weightAbs <- function(X) {1 - ( H(X)/hunif )  }
weightRel <- function(posterior, prior) {1 - ( H(posterior)/H(prior) )  }





plotPosterior <- function(distro, distroS, title, prior){
  subtitle <- paste("adw = ", round(weightAbs(distro),3), ", rdw = ", round(weightRel(distro,prior),3), ", wDelta = ",  
                    abs(round(weightAbs(distro) - weightAbs(prior),3))
                    , ", median =", 
                    round(median(distroS, na.rm = TRUE),2), ", 89%HPDI = ", round(HPDI(distroS),2)[1],"-",round(HPDI(distroS),2)[2], sep = "")
  
  ggplot()+ geom_line(aes(x = ps, y = distro))+theme_tufte()+ylab("probability")+xlab(expression(theta))+
    theme(plot.title.position = "plot")+
    ggtitle(title)+
    labs(title = title, subtitle = subtitle)
}




expectedFromSample <- function (distro) {
  probs <- distroFromSamples(distro)
  expProb <- sum(probs * ps)
  return(expProb)
}


wDelta <- function (distro,prior){
  abs(round(weightAbs(distro) - weightAbs(prior),3))
}






