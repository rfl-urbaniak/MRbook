library(ggplot2)



#this is about Taroni 2015 dismissal
#he suggests dividing densities to get an LR, and integrating
#question does this make sense, and does this give a result different from sampling

ps <- seq(0,1, length.out = 1001)

dens1 <- function (x) if( (x <= 0 | x>1)) {0
                      } else{
                      ifelse(x <= .5, dunif(x, min = 0, max = .5), .01 )    
                      }
dens1v <- Vectorize(dens1)
dens1vnorm <- function(x) dens1v(x)/integrate(dens1v,0,1)[[1]]


dens2 <- function (x) if( (x <= 0 | x>1)) {0
                    } else{
                    ifelse(x > .5, dunif(x, min = .5, max = 1), 0.01 )    
                    }
dens2v <- Vectorize(dens2)
dens2vnorm <- function(x) dens2v(x)/integrate(dens2v,0,1)[[1]]

lr <- function (x) dens1vnorm(x)/dens2vnorm(x)

lrs <- lr(ps)

ggplot()+geom_line(aes(x = ps, y = lrs))

ggplot()+geom_point(aes(x = ps, y = lrs))+xlim(.49,.51)

integrate(lr, lower = 0, upper = 1)
100.0025


set.seed(654)

d1samples  <- sample(ps, size=10000, replace=TRUE, prob=dens1vnorm(ps))

d2samples  <- sample(ps, size=10000, replace=TRUE, prob=dens2vnorm(ps))

lrsamples <- d1samples/d2samples


ggplot()+geom_point(aes(x = 1:10000, y = d1samples), size = .1, alpha = .2)
ggplot()+geom_point(aes(x = 1:10000, y = d2samples), size = .1, alpha = .2)
ggplot()+geom_point(aes(x = 1:10000, y = lrsamples), size = .1, alpha = .2)


ggplot()+geom_histogram(aes(x = d1samples,y = stat(density)), bins = 20)
ggplot()+geom_histogram(aes(x = d2samples,y = stat(density)), bins = 20)

ggplot()+geom_histogram(aes(x = lrsamples,y = stat(density)), bins = 60)
ggplot()+geom_histogram(aes(x = lrsamples,y = stat(density)), bins = 60)+xlim(0,2)

mean(lrsamples)

mean(lrsamples < 1)

mean(lrsamples < .5)



#now we're gunning for the same mean, different ranges
set.seed(232)
dens1b <- function (x) if( (x <= 0 | x>1)) {0
} else{
  dnorm(x,.05,.5)   
}
dens1vb <- Vectorize(dens1b)
dens1vnormb <- function(x) dens1vb(x)/integrate(dens1vb,0,1)[[1]]
integrate(dens1vnormb, 0, 1)

dens2b <- function (x) if( (x <= 0 | x>1)) {0
} else{
  dnorm(x,.97,.1)   
}
dens2vb <- Vectorize(dens2b)
dens2vnormb <- function(x) dens2vb(x)/integrate(dens2vb,0,1)[[1]]
integrate(dens2vnormb, 0, 1)


lrb <- function (x) dens1vnormb(x)/dens2vnormb(x)
lrsb <- lrb(ps)

ggplot()+geom_line(aes(x = ps, y = lrsb))
ggplot()+geom_point(aes(x = ps, y = lrsb))+xlim(.69,.71)

integrate(lrb, lower = 0, upper = 1)
123895852

set.seed(654)

d1samplesb  <- sample(ps, size=10000, replace=TRUE, prob=dens1vnormb(ps))

d2samplesb  <- sample(ps, size=10000, replace=TRUE, prob=dens2vnormb(ps))

lrsamplesb <- d1samplesb/d2samplesb


ggplot()+geom_point(aes(x = 1:10000, y = d1samplesb), size = .1, alpha = .2)
ggplot()+geom_point(aes(x = 1:10000, y = d2samplesb), size = .1, alpha = .2)
ggplot()+geom_point(aes(x = 1:10000, y = lrsamplesb), size = .1, alpha = .2)


ggplot()+geom_histogram(aes(x = d1samplesb,y = stat(density)), bins = 20)
ggplot()+geom_histogram(aes(x = d2samplesb,y = stat(density)), bins = 20)

ggplot()+geom_histogram(aes(x = lrsamplesb,y = stat(density)), bins = 60)
ggplot()+geom_histogram(aes(x = lrsamplesb,y = stat(density)), bins = 60)+xlim(0,3)


mean(lrsamples)
mean(lrsamplesb)

mean(lrsamples < 1)
mean(lrsamplesb < 1)

mean(lrsamples < .1)
mean(lrsamplesb < .1)


mean(lrsamples < .5)
mean(lrsamplesb < .5)



#still ooking for the minimal value

#now we're gunning for the same mean, different ranges
m1 <- seq(0.2,.5, by = 0.02)
m2 <- seq(.6,.9, by = 0.02)
s1 <- seq(0,.7, by  = 0.02)
s2 <- seq(0, .7, by = 0.02)
opts <- expand.grid(m1,m2,s1,s2)

nrow(opts)

attach(opts)
for(i in 1: nrow(opts)){
  i <- 1
  set.seed(232)
  dens1b <- function (x) if( (x <= 0 | x>1)) {0
  } else{
    dnorm(x,m1[i],s1[i])   
  }
  dens1vb <- Vectorize(dens1b)
  dens1vnormb <- function(x) dens1vb(x)/integrate(dens1vb,0,1)[[1]]
  
  
  dens2b <- function (x) if( (x <= 0 | x>1)) {0
  } else{
    dnorm(x,m2[i],s2[i])   
  }
  dens2vb <- Vectorize(dens2b)
  dens2vnormb <- function(x) dens2vb(x)/integrate(dens2vb,0,1)[[1]]
  
  set.seed(654)
  d1samplesb  <- sample(ps, size=10000, replace=TRUE, prob=dens1vnormb(ps))
  d2samplesb  <- sample(ps, size=10000, replace=TRUE, prob=dens2vnormb(ps))
  lrsamplesb <- d1samplesb/d2samplesb
  
  opts$dist[i] <- abs(mean(lrsamples) - mean(lrsamplesb))
}


# 
# mean(lrsamples < 1)
# mean(lrsamplesb < 1)
# 
# mean(lrsamples < .1)
# mean(lrsamplesb < .1)
# 
# 
# mean(lrsamples < .5)
# mean(lrsamplesb < .5)
# 
## Now for optimization




