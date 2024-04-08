library(rethinking)
library(philentropy)

# the bag simulation
bias1 <- rnorm(500, 0.5, 0.05)
bias2 <- rnorm(500, 0.3, 0.05)

bag <- c(bias1, bias2)

# sampling
oneCoin <- sample(bag, 1, replace = TRUE)

onekCoins <- sample(bag, 1000, replace = TRUE)
dens(onekCoins)


# priors ------------------------------------------------------------------

# creating 3 distributions that are used in the paper

n <- 1000
ps <- seq(0,1,  length.out =n)
a <- dnorm(ps, .3, .05)
b <- dnorm(ps, .5, .05)
c <- ifelse(ps <= .4, a, b) 

bimodal <- c / sum(c)
bimodalCum <- cumsum(bimodal)
plot(bimodal)

centered <-   dnorm(ps, .4, .05)
centered <- centered/sum(centered)
centeredCum <- cumsum(centered)

aw <- dnorm(ps, .2, .05)
bw <- dnorm(ps, .6, .05)
cw <- ifelse(ps <= .4, aw, bw) 
bimodalWide <- cw / sum(cw)
bimodalWideCum <- cumsum(bimodalWide)


# coin practical ---------------------------------------------------------

# introducing kld and cvm measures

kld <- function(p,q) kullback_leibler_distance(p,q, testNA = TRUE, unit = "log2",
                                               epsilon = 0.00001)

cvm <- function(w,p){
  cumw <-  cumsum(w)
  cump <- cumsum(p)
  dist <- (cump - cumw )^2
  return(sum(dist))
}





# non cum measures --------------------------------------------------------

# each sampled coin bias will be treated as a true chance, from which
# we will calculate the inaccuracy for all 3 dists.
# with 2 methods, in cumulative and non-cumulative scenario



inaccuracy_coins_sample <- function(coins, tested_dist) {
  results_df <- data.frame(Element = numeric(length(coins)),
                           CVM = numeric(length(coins)),
                           KLD = numeric(length(coins)))
  
  for (i in 1:length(coins)) {
    trueDist <- rep(0, length(coins))
    trueCh <- round(coins[i] * 1000) # creating point distributions for coins probs
    trueDist[trueCh] <- 1
    
    # Calculate CVM
    cvm_result <- cvm(tested_dist, trueDist)
    
    # Calculate KLD
    kld_result <- kld(tested_dist, trueDist)
    
    results_df[i, ] <- c(coins[i], cvm_result, kld_result)
  }
  
  return(results_df)
}


df_bimodal <- inaccuracy_coins_sample(onekCoins, bimodal)
df_centered <- inaccuracy_coins_sample(onekCoins, centered)
df_bimodalWide <- inaccuracy_coins_sample(onekCoins, bimodalWide)







# cvm winnings ######

# calculating the 'winners' for cvm, a dist. is a winner when
# it has the lowest inacc. score for a given value

cvm_compDF <-  data.frame(Element = df_bimodal$Element,
                          CVM_bimodal = df_bimodal$CVM,
                          CVM_centered = df_centered$CVM,
                          CVM_bimodalWide = df_bimodalWide$CVM)

cvm_compDF$Winning_Distribution <- apply(cvm_compDF[, c("CVM_bimodal",
                                                        "CVM_centered",
                                                        "CVM_bimodalWide")], 1, function(x) {
                                                          ifelse(x["CVM_bimodal"] == min(x), "bimodal", 
                                                                 ifelse(x["CVM_centered"] == min(x), "centered", 
                                                                        ifelse(x["CVM_bimodalWide"] == min(x), "bimodalWide", NA)))
                                                        })

cvm_compDF$Winning_Distribution <- as.factor(cvm_compDF$Winning_Distribution)

table(cvm_compDF$Winning_Distribution) # the results









# kld winnings ##########

kld_compDF <-  data.frame(Element = df_bimodal$Element,
                          KLD_bimodal = df_bimodal$KLD,
                          KLD_centered = df_centered$KLD,
                          KLD_bimodalWide = df_bimodalWide$KLD)

kld_compDF$Winning_Distribution <- apply(kld_compDF[, c("KLD_bimodal",
                                                        "KLD_centered",
                                                        "KLD_bimodalWide")], 1, function(x) {
                                                          ifelse(x["KLD_bimodal"] == min(x), "bimodal", 
                                                                 ifelse(x["KLD_centered"] == min(x), "centered", 
                                                                        ifelse(x["KLD_bimodalWide"] == min(x), "bimodalWide", NA)))
                                                        })


kld_compDF$Winning_Distribution <- as.factor(kld_compDF$Winning_Distribution)


table(kld_compDF$Winning_Distribution) # the results





# cumulative versions -----------------------------------------------------




inaccuracy_coins_sample_cum <- function(coins, tested_dist) {
  results_df <- data.frame(Element = numeric(length(coins)),
                           CVM = numeric(length(coins)),
                           KLD = numeric(length(coins)))
  
  for (i in 1:length(coins)) {
    trueDist <- rep(0, length(coins))
    trueCh <- round(coins[i] * 1000) # creating point distributions for coins probs
    trueDist[trueCh:length(trueDist)] <- 1
    
    # Calculate CVM
    cvm_result <- cvm(tested_dist, trueDist)
    
    # Calculate KLD
    kld_result <- kld(tested_dist, trueDist)
    
    results_df[i, ] <- c(coins[i], cvm_result, kld_result)
  }
  
  return(results_df)
}



df_bimodalCum <- inaccuracy_coins_sample_cum(onekCoins, bimodal)
df_bimodalWideCum <- inaccuracy_coins_sample_cum(onekCoins, bimodalWideCum)
df_centeredCum <- inaccuracy_coins_sample_cum(onekCoins, centered)

c(mean(df_bimodalCum$CVM), mean(df_bimodalCum$KLD))
c(mean(df_bimodalWideCum$CVM), mean(df_bimodalWideCum$KLD))
c(mean(df_centeredCum$CVM), mean(df_centeredCum$KLD))






# cvm winnnings ####

cvm_compDF_cum <-  data.frame(Element = df_bimodalCum$Element,
                              CVM_bimodal = df_bimodalCum$CVM,
                              CVM_centered = df_centeredCum$CVM,
                              CVM_bimodalWide = df_bimodalWideCum$CVM)

cvm_compDF_cum$Winning_Distribution <- apply(cvm_compDF_cum[, c("CVM_bimodal",
                                                                "CVM_centered",
                                                                "CVM_bimodalWide")], 1, function(x) {
                                                                  ifelse(x["CVM_bimodal"] == min(x), "bimodal", 
                                                                         ifelse(x["CVM_centered"] == min(x), "centered", 
                                                                                ifelse(x["CVM_bimodalWide"] == min(x), "bimodalWide", NA)))
                                                                })

cvm_compDF_cum$Winning_Distribution <- as.factor(cvm_compDF_cum$Winning_Distribution)

table(cvm_compDF_cum$Winning_Distribution) # results




# kld winnings #######



kld_compDF_cum <-  data.frame(Element = df_bimodalCum$Element,
                              KLD_bimodal = df_bimodalCum$KLD,
                              KLD_centered = df_centeredCum$KLD,
                              KLD_bimodalWide = df_bimodalWideCum$KLD)

kld_compDF_cum$Winning_Distribution <- apply(kld_compDF_cum[, c("KLD_bimodal",
                                                                "KLD_centered",
                                                                "KLD_bimodalWide")], 1, function(x) {
                                                                  ifelse(x["KLD_bimodal"] == min(x), "bimodal", 
                                                                         ifelse(x["KLD_centered"] == min(x), "centered", 
                                                                                ifelse(x["KLD_bimodalWide"] == min(x), "bimodalWide", NA)))
                                                                })


kld_compDF_cum$Winning_Distribution <- as.factor(kld_compDF_cum$Winning_Distribution)


table(kld_compDF_cum$Winning_Distribution)  # the results





# comparison of all the winnings ------------------------------------------



table(cvm_compDF$Winning_Distribution)

cvm_compDF_means <- colMeans(cvm_compDF[, 2:4])
print(cvm_compDF_means)


table(kld_compDF$Winning_Distribution)

kld_compDF_means <- colMeans(kld_compDF[, 2:4])
print(kld_compDF_means)


table(cvm_compDF_cum$Winning_Distribution)

cvm_compDF_cum_means <- colMeans(cvm_compDF_cum[, 2:4])
print(cvm_compDF_cum_means)



table(kld_compDF_cum$Winning_Distribution)

kld_compDF_cum_means <- colMeans(kld_compDF_cum[, 2:4])
print(kld_compDF_cum_means)
























