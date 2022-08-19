library(plot3D)


lr <- function (fnp, rmp, fpp){
  (1 -fnp)/
    (  ((1-fnp)* rmp)
       +
         (fpp * (1-rmp))    )
}


lrneg <- function (fnp, rmp, fpp){
  (fnp)/
    (  ((fnp)* rmp)
       +
         ((1-fpp) * (1-rmp))    )
}



fpp <- seq(0,0.05, length.out = 200)
fnp <- seq(0,0.05, length.out = 200)
rmp9 <- 10e-9



lrTable <- expand.grid(rmp9 = rmp9,fnp = fnp, fpp = fpp)
head(lrTable)

lrTable$lr <- lr(lrTable$fnp, lrTable$rmp9, lrTable$fpp)
lrTable$lrneg <- lrneg(lrTable$fnp, lrTable$rmp9, lrTable$fpp)


head(lrTable)


?scatter3D



scatter3D(lrTable$fpp,lrTable$fnp,lrTable$lr,
          pch=3,cex=0.2,byt="g",alpha=0.7,theta=40,
          phi=10,xlab="FPP", ylab="FNP",zlab="likelihood ratio", 
          main="Likelihood ratio of incriminating DNA evidence",
          colvar=NULL, zlim = c(0,250),cex.main =1, ticktype = "detailed", nticks = 2,
          border = NA)

scatter3D(lrTable$fpp,lrTable$fnp,lrTable$lrneg,
                          pch=3, cex=.2,byt="g",alpha=0.7,theta=40,
                          phi=10,xlab="FPP", ylab="FNP",zlab="likelihood ratio", 
                          main="Likelihood ratio of exculpatory DNA evidence",
                          colvar=NULL, cex.main =1, ticktype = "detailed", border = NA,
          nticks = 2)





lr9 <-  lr(fnp,rmp9,fpp)
lr3 <-  lr(fnp,rmp3,fpp)

fppTable <- data.frame(fpp,   lr9,  lr3, ref = rep(16.8, length(fpp)))

library(tidyr)
fppTableLong <- gather(fppTable,line,value,c(lr9,lr3,ref), factor_key=TRUE)
