install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")
# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))




library(ggplot2)
library(rethinking)
library(truncnorm)

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

rmp <- 10e-9




prior <- seq(0,.3, by = 0.001)
priorH0 <- 1-prior

#POSTERIOR IS
posterior <- function(prior, rmp = 10e-9, fpp){ 
  return ( prior/(prior + rmp + fpp) )
}


posteriorN <- list()
posteriorW <- list()
minimaN <- numeric(1e4)
minimaW <- numeric(1e4)


for (s in 1:1e4){
  fppN <-    fpdNsample[s] 
  fppW <-    fpdWsample[s] 
  
  posteriorN[[s]] <- posterior(prior, fpp = fpdNsample[s])
  posteriorW[[s]] <- posterior(prior, fpp = fpdWsample[s])
  
  minimaN[s] <- min(prior[posteriorN[[s]] > .99])
  minimaW[s] <- min(prior[posteriorW[[s]] > .99])
}
posteriorNDF <-   do.call(cbind, posteriorN)
posteriorWDF <-   do.call(cbind, posteriorW)


minimaPlot <- ggplot()+geom_density(aes(x = minima))+
  theme_tufte(base_size = 10)+ggtitle("Minimal priors sufficient for posterior >.99")+xlab("minimal prior")+
  theme(plot.title.position = "plot")

minimaGrob <- ggplotGrob(minimaPlot)

alpha = .3
size = .08
densitiesLinesPlot <- ggplot()+geom_line(aes(x = prior, y = jointPosteriorDF[,1]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,2]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,3]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,4]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,5]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,6]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,7]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,8]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,9]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,10]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,11]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,12]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,13]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,14]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,15]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,16]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,17]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,18]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,19]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,20]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,21]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,22]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,23]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,24]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,25]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,26]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,27]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,28]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,29]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,30]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,31]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,32]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,33]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,34]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,35]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,36]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,37]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,38]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,39]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,40]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,41]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,42]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,43]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,44]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,45]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,46]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,47]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,48]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,49]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,50]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,51]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,52]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,53]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,54]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,55]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,56]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,57]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,58]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,59]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,60]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,61]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,62]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,63]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,64]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,65]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,66]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,67]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,68]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,69]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,70]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,71]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,72]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,73]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,74]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,75]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,76]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,77]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,78]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,79]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,80]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,81]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,82]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,83]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,84]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,85]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,86]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,87]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,88]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,89]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,90]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,91]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,92]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,93]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,94]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,95]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,96]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,97]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,98]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,99]), alpha = alpha, size = size)+
  geom_line(aes(x = prior, y = jointPosteriorDF[,100]), alpha = alpha, size = size)+
  xlim(0,.1)+theme_tufte(base_size = 10)+ylab("posterior")+
  annotation_custom(minimaGrob, xmin = .025, xmax = .1, ymin = 0.01, ymax = 0.8)+
  ggtitle("Posterior vs prior (100 sampled lines)")+
  theme(plot.title.position = "plot")








