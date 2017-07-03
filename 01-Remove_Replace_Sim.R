#===============================================
#
#Simuation model by removing species dispersed by
#large-bodied frugviores and replacing by the other
# species left in the community
#
#===============================================
# script assumes working directory is source file location

rm(list=ls(all=TRUE))
setwd("D:/Research2016/Tree Architecture/WoodDensity/Appendix_Analysis_Codes/") 

#import functions
source("Sim.R")
source("SimBoot.R")

#Percent tolerance for difference of basal area
Tolerance = 0.01 #percent tolerance 1%

All_T <- read.csv("All_T_WSG.csv")

# Consider only tree with dbh>5 cm according to the calculation by Chave et al. (2005)
All_T <- subset(All_T,dbh.C3 >=5)

#function to calculate basal area
BA <- function(dbh){
  return((dbh/2)^2*pi)
}

#AGB function for the mosit tropical forest
AGB <- function(D,W){
  return(W*exp(-1.499+2.148*log(D)+0.207*log(D)^2-0.0281*log(D)^3))
}

#Calucate AGB and add to the main dataframe
All_T$AGB <-mapply(AGB,All_T$dbh.C3,All_T$WSG)

#Calculate Basal area
All_T$BA <- sapply(All_T$dbh.C3,BA)

#*****Defuantion Scenario: no defaunation********* 
resultDefaun = boot(data = All_T, statistic = Sim_Boot, R = 500, SimT = 500 )

#*****Control Scenario: no defaunation*********
resultRandom = boot(data = All_T, statistic = Sim_Boot, R = 500, SimT = 500, type = "Random")


 save.image(file="Results.Rdata")


