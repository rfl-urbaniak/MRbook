library(ggplot2)
library(ggthemes)
library(rethinking)
library(bnlearn)
library(gRain)

source("scripts/CptCreate.R")

# carpetMean <- 87/3444
# carpetA <- 88
# carpetB <- 3445
# 
ps <- seq(0,1,length.out = 1001)


hairMean <-  29/1148
hairA <- 30
hairB <- 1149

dogMean <- 2/78
dogA <- 3
dogB <- 79



lik0 <- hairMean * dogMean
prior <- seq(0,.3, by = 0.001)
priorH0 <- 1-prior
denomin <- lik0h * priorH0 + prior
num <-  lik0 * priorH0
posterior <- 1- num/denomin
threshold <- min(prior[posterior > .99])


ggplot()+geom_line(aes(x = prior, y = posterior))+xlim(0,.07)+
  theme_tufte(base_size = 10)+labs(title = "Prior vs. posterior, based on point estimates",
                                   subtitle = "Joint evidence: dog & hair")+
  geom_vline(xintercept = threshold, lty = 2, size =.5, alpha = .7)+
  annotate(geom = "label", label = "posterior > .99", x = .067, y =.95, size = 3)


#carpetSamples <- sample(ps, 1e4, replace = TRUE, prob = dbeta(ps, carpetA, carpetB))
hairSamples <- sample(ps, 1e4, replace = TRUE, prob = dbeta(ps, hairA, hairB))
dogSamples <- sample(ps, 1e4, replace = TRUE, prob = dbeta(ps, dogA, dogB))



#carpetHPDI <- HPDI(carpetSamples, prob  =.9)
#.020, .028
hairHPDI <- HPDI(hairSamples, prob  =.99)
hairHPDI
# 0.017, .032

dogHPDI <- HPDI(dogSamples, prob  =.99)
dogHPDI
# .003, .122

lik0l <- .037 * .103
denominL <- lik0l * priorH0 + prior
numL <-  lik0l * priorH0
posteriorL <- 1- numL/denominL
min(prior[posteriorL > .99])




jointEvidence <- dogSamples * hairSamples





lik0J <- 0.002760
denominJ <- lik0J * priorH0 + prior
numJ <-  lik0J * priorH0
posteriorJ <- 1- numJ/denominJ
min(prior[posteriorJ > .99])





HPDI(jointEvidence, prob = .99)

HPDI(jointEvidence, prob = .9)




densities1Plot <- ggplot()+  
  geom_line(aes(x = ps, y = dbeta(ps, hairA, hairB)), lty  = 2)+
  geom_line(aes(x = ps, y = dbeta(ps, dogA, dogB)), lty = 3)+xlim(0,.15)+
  xlab("probability")+
  ylab("density")+
  theme_tufte(base_size = 10)+
  #  annotate(geom  = "label", label = "carpet", x =  0.045, y = 140)+
  annotate(geom  = "label", label = "hair", x =  0.035, y = 80)+
  annotate(geom  = "label", label = "dog", x =  0.06, y = 15)+
  labs(title = "Conditional densities for  individual items of evidence if the source hypothesis is false")

densities2Plot <- ggplot()+  
  xlab("probability")+
  ylab("density")+
  theme_tufte(base_size = 10)+
  geom_density(aes(x= jointEvidence))+
  geom_vline(xintercept = 0.002760, lty = 2, size = .5)+
  geom_vline(xintercept = 0.000023, lty = 2, size  = .5)+
  geom_vline(xintercept = 0.000144, lty = 3, size = .8)+
  geom_vline(xintercept = 0.001742, lty = 3, size  = .8)+
  labs(title = "Conditional density for joint evidence", 
       subtitle = "(with .99 and .9 HPDIs)")





ggplot()+geom_density(aes(x= jointEvidence))

+

    geom_vline(xintercept = low, size =.5, alpha = .5, lty = 2)+
  geom_vline(xintercept = high, size =.5, alpha = .5, lty  = 2)+
  geom_vline(xintercept = point, size =.7, alpha = 1)+
  theme_tufte(base_size = 10)+xlab("probability")

plot(jointEvidence)



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





jointPosteriorDF[,2]

library(reshape2)
?melt

jointPosteriorDFlong <- melt(jointPosteriorDF[,2:3], id.vars = prior)

head(jointPosteriorDFlong)

ggplot(jointPosteriorDFlong)+geom_line(aes(x = prior, y = jointPosteriorDF, group = Var2))




ggplot()+  #geom_line(aes(x = ps, y = dbeta(ps, carpetA, carpetB)))+
  geom_line(aes(x = ps, y = dbeta(ps, hairA, hairB)), lty  = 2)+
  geom_line(aes(x = ps, y = dbeta(ps, dogA, dogB)), lty = 3)+xlim(0,.15)+
  xlab("probability")+
  ylab("density")+
  theme_tufte(base_size = 8)+
  #  annotate(geom  = "label", label = "carpet", x =  0.045, y = 140)+
  annotate(geom  = "label", label = "suspect", x =  0.047, y = 80)+
  annotate(geom  = "label", label = "dog", x =  0.06, y = 15)+
  labs(title = "Conditional probabilities of hair evidence given the defense hypothesis")







minimaPlot <- ggplot()+geom_density(aes(x = minima))+
  theme_tufte(base_size = 10)+ggtitle("Minimal priors sufficient for posterior >.99")+xlab("minimal prior")

  

minimaGrob <- ggplotGrob(minimaPlot)


alpha = .6
size = .15
ggplot()+geom_line(aes(x = prior, y = jointPosteriorDF[,1]), alpha = alpha, size = size)+
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
  ggtitle("Posterior vs prior (100 sampled lines)")









WayneWilliamsDAG <- model2network("[source][carpet|source][hair|source][dog|source]")



graphviz.plot(WayneWilliamsDAG)




prior.CPT

sourceProb <-prior.CPT("source","1","0",0.01)
carpetProb <- single.CPT(eNode = "carpet", "source", "1", "0", "1", "0", probH1E = 1, probH2E = carpetMean)
dogProb <- single.CPT(eNode = "dog", "source", "1", "0", "1", "0", probH1E = 1, probH2E = dogMean)
hairProb <- single.CPT(eNode = "hair", "source", "1", "0", "1", "0", probH1E = 1, probH2E = hairMean)



WayneWilliamsCPT <- list(source = sourceProb, carpet = carpetProb, dog = dogProb, hair = hairProb)


WayneWilliamsBN <- custom.fit(WayneWilliamsDAG, WayneWilliamsCPT)

WayneWilliamsJT <- compile(as.grain(WayneWilliamsBN))


WayneWilliamsJTh1 <- setEvidence(WayneWilliamsJT, nodes = c("source"), states = c("1"))
WayneWilliamsJTh0 <- setEvidence(WayneWilliamsJT, nodes = c("source"), states = c("0"))


querygrain(WayneWilliamsJTh1, nodes = c("dog", "hair"), type = "joint")


EifH0 <- querygrain(WayneWilliamsJTh0, nodes = c("dog", "hair"), type = "joint")[[1]]

EifH0


point <- dogMean * hairMean

point

#now look at the edges
#uncharitable
low <- dogHPDI[[1]] * hairHPDI[[1]]
#charitable 
high <- dogHPDI[[2]] * hairHPDI[[2]]

low
high


#to get a feel for the posterior if you work with the charitable version
lik0h <- high
prior <- seq(0,.3, by = 0.001)
priorH0 <- 1-prior
denominH <- lik0h * priorH0 + prior
numH <-  lik0h * priorH0
posteriorH <- 1- numH/denominH
min(prior[posteriorH > .99])

lik0l <- low
denominL <- lik0l * priorH0 + prior
numL <-  lik0l * priorH0
posteriorL <- 1- numL/denominL
min(prior[posteriorL > .99])



ggplot()+geom_line(aes(x = prior, y = posteriorH))+
  geom_line(aes(x = prior, y = posteriorL))


#now LRs
#point
1/(dogMean * hairMean)

#low
1/(dogHPDI[[1]] * hairHPDI[[1]])


#high
1/(dogHPDI[[2]] * hairHPDI[[2]])






82 out of 638,992 occupied
homes in Atlanta, or about 1 in 8,000, had carpeting with that fiber.

82/656000

1/8000

Yarn that was sold to a Georgia carpet company, West Point Pepperell, which used it to make a line called Luxaire. 
The color of the fibers found on the bodies, including Nathaniel Cater, matched Luxaire English Olive; this was the type of carpet found in Williams’ home.
Experts estimated that one in approximately 8,000 Atlanta area homes contained Luxaire English Olive carpet.


it was determined that the West
Point Pepperell Corporation of Dalton,
Ga., had manufactured

carpet called "Luxaire," which was
constructed in the same manner as
the Williams carpet. One of the colors
offered in the "Luxaire" line was
called "English Olive," and this color
was the same as that of the Williams
carpet (both visually and by the use of
        discriminating ch
        
        
        
        
        however, it had only
        purchased Wellman 1818 fiber for this
        line during 1970 and 1971
        
        
        
An estimation, to be discussed
later, basec.\ on sales records provided
by the West Point Pepperell Corpora-
tion indicated that there was a very low
chance (117 792) of finding a carpet
like Williams carpet by randomly se-
lecting occupied residences in the At-
lanta area.



Therefore, it was estimated that
a total of 16,397 square yards of
carpet containing the Wellman
1818 fiber and d:Jed English
Olive in color was sold by the
West Point Pepperell Corporation
to retailers in 10 southeastern
States during 1971 and 1972.


(In
1979, existing residential
carpeted floor space in the
United States was estimated at
6.7 billion square yards.)

8y assuming that this carpet
was installed in one room,
averaging 12 feet by 15 feet in
size, per house, and also
assuming that the total sales of
carpet were divided equally
among the 10 southeastern
States, then approximately 82
rooms with this carpet could be
found in the State of Georgia.

#15 square feet to yards = 1.66

16379/17


1091.933 /10

