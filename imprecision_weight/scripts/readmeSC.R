#First, build BNs for SC, and get the distributions


source("scripts/SCfunctions.R")
file.edit("scripts/SCfunctions.R")

source("scripts/SCdistro.R")
file.edit("scripts/SCdistro.R")


#prepare plots for the CPTs

source("scripts/SCplotCPTs.R")
file.edit("scripts/SCplotCPTs.R")


#now plot the BN with higher-order CPTs
source("scripts/SCplot.R")
file.edit("scripts/SCplot.R")



#prepare plots for the distributions of interest
source("scripts/SCplotDistros.R")
file.edit("scripts/SCplotDistros.R")


#prepare posterior plots 
source("scripts/SCposteriorPlots.R")
file.edit("scripts/SCposteriorPlots.R")

#calculate and plot LRs for Abruising
source("scripts/SClrs.R")
file.edit("scripts/SClrs.R")

#calculate and illustrate expected weights
source("scripts/SCexpectedWeights.R")
file.edit("scripts/SCexpectedWeights.R")


