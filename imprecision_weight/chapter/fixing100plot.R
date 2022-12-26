library(ggplot2)
library(ggthemes)

#already defined
ps <- seq(0,1,length.out = 1001)

hairA <- 30
hairB <- 1149
hairSamples <- sample(ps, 1e4, replace = TRUE, prob = dbeta(ps, hairA, hairB))

dogA <- 3
dogB <- 79
dogSamples <- sample(ps, 1e4, replace = TRUE, prob = dbeta(ps, dogA, dogB))

jointEvidence <- dogSamples * hairSamples


prior <- seq(0,1, by = 0.001)
priorH0 <- 1-prior

#-----

jointPosterior <- list()
minima <- numeric(1e4)
for (s in 1:1e4){
  lik <- jointEvidence[s] 
  denomin <- lik * priorH0 + prior
  num <-  lik * priorH0
  posterior <- 1- num/denomin
  jointPosterior[[s]] <- posterior      
  minima[s] <- min(prior[posterior > .99])
}
jointPosteriorDF <-   do.call(cbind, jointPosterior)

str(jointPosterior[[1]])

str(jointPosteriorDF)



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
```



\begin{figure}[H]
```{r fig:lines3,echo=FALSE,eval=TRUE,fig.align = "center",cache=TRUE, fig.show = "hold", out.width = "60%", warning = FALSE, message = FALSE}
densitiesLinesPlot
```

\caption{100 lines illustrating the uncertainty about the dependence of the posterior on the prior given aleatory uncertainty about the evidence, with the distribution of the minimal priors required for the posterior to be above .99.}

\label{fig:lines}

\end{figure}


