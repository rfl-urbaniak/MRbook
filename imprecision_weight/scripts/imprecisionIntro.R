library(ggplot2)
library(ggthemes)
library(rethinking)
library(bnlearn)
library(gRain)

source("scripts/CptCreate.R")

carpetMean <- 87/3444
carpetA <- 88
carpetB <- 3445

ps <- seq(0,1,length.out = 1001)


hairMean <-  29/1148
hairA <- 30
hairB <- 1149



dogMean <- 2/78
dogA <- 3
dogB <- 79



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


jointEvidence <- dogSamples * hairSamples


HPDI(jointEvidence, prob = .99)



ggplot()+geom_density(aes(x= jointEvidence))+
  geom_vline(xintercept = low, size =.5, alpha = .5, lty = 2)+
  geom_vline(xintercept = high, size =.5, alpha = .5, lty  = 2)+
  geom_vline(xintercept = point, size =.7, alpha = 1)+
  theme_tufte(base_size = 10)+xlab("probability")

plot(jointEvidence)



jointPosterior <- matrix(ncol = length(prior), nrow = 1e4)
jointPosterior
for (i in 1:1e4)
lik <- jointEvidence[i] 
denomin <- lik * priorH0 + prior
numL <-  lik0l * priorH0
posteriorL <- 1- numL/denominL
min(prior[posteriorL > .99])







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

