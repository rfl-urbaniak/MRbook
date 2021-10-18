#E = at least one coin toss in 1,2 is heads
#H = both tosses 1,2 are heads

EifH <- 1

toss1 <- c("H", "T")
toss2 <- c("H", "T")

twoTosses <- expand.grid(toss1 = toss1, toss2 = toss2)

twoTosses

notH <- twoTosses[!(twoTosses$toss1 == "H" & twoTosses$toss2 == "H"),]
notH

EifNH <- 2/3


#X = third toss is H


LR1 <- EifH / EifNH

LR1


EifHX <- 1

threeTosses <- expand.grid(toss1 = toss1, toss2 = toss2, toss3 = toss2)

threeTosses
notHX <- threeTosses[!(threeTosses$toss1 == "H" & threeTosses$toss2 == "H" & threeTosses$toss3 == "H"),]

notHX$E <- notHX$toss1 == "H" | notHX$toss2 == "H"

EifNHX  <- mean(notHX$E)

EifNHX

EifHX/EifNHX


