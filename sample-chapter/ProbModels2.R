library("ggplot2")

# Constructing Quadratic Formula
result <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    zeros <- c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    zeros <- -b/(2*a)
  }
  else {zeros <- "There are no real roots."} # third case D<0
print(zeros)
  }


# Constructing delta
delta<-function(a,b,c){
  b^2-4*a*c
}


delta(0.982,-1.982,0.991)

result(0.982,-1.982,0.991)

delta(-0.982,0.982,0)
result(-0.982,0.982,0)

result(-0.955,0.955,0)


lr <- function(x){ (0.991-0.991* x)/( 0.009*x)}

eq <- function(x){-0.982*x^2+0.982*x}


eqfour <- function(x){-0.955*x^2+0.955*x}


#plot(eq(0), type='l')
#plot(eq(0:10), type='l')

png(file="f-gate.png",width = 2000, height = 1195, res=300)
par(mfrow=c(1,1)) 
ggplot(data.frame(x=c(-0.1, 1.1)), aes(x=x)) + stat_function(fun=eq, geom="line") + xlab(expression(pi)) + ylab(expression(f(pi)))
dev.off()



png(file="lre-gate.png",width = 4000, height = 2195, res=600)
par(mfrow=c(1,1)) 
ggplot(data.frame(x=c(0, 1)), aes(x=x)) + stat_function(fun=lr, geom="line") + xlab(expression(pi)) + ylab("LR(E)")
dev.off()

result(-)
result



0.991/0.009



lr(0.1)
lr(0.99)

factored <- function(LG) {0.991 - LG*0.009} 

factored(40)

equation4 <- function(x){
  -factored(4)*x^2 + factored(4) 
}

equation40 <- function(x){
  -factored(40)*x^2 + factored(40) 
}

equation400 <- function(x){
  -factored(400)*x^2 + factored(400) 
}



equation110 <- function(x){
  -factored(110)*x^2 + factored(110) 
}


equation112 <- function(x){
  -factored(112)*x^2 + factored(112) 
}



equation112 <- function(x){
  -factored(111)*x^2 + factored(111) 
}



1/0.009



0.009*110

ggplot(data.frame(x=c(-2, 2)), aes(x=x)) + stat_function(fun=equation110, geom="line") + xlab(expression(pi)) + ylab("LR(E)")


#miller
A <- seq(1,6,1)
B <- seq(1,6,1)
results <- expand.grid(A,B)
names(results) <- c("A","B")

results


# 




