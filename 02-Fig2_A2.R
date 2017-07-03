#===============================================
#
#Visualization of both scenario results
#
#===============================================
rm(list=ls(all=TRUE))
setwd("D:/Research2016/Tree Architecture/WoodDensity/Appendix_Analysis_Codes")
library(boot)
load("Results.Rdata")

#--------------------------------------------
#
#Defauantion Scenario
#
#--------------------------------------------
# Confidence intervals, 
CI_Defaun <- boot.ci(resultDefaun, type = "perc", index = 19) #All species
Median_Defaun <- median(resultDefaun$t[,19]) #median of all species
SE_Defaun <- c(Median_Defaun-CI_Defaun$percent[4],Median_Defaun-CI_Defaun$percent[5])

#Figure
#pdf(file=paste("Figures/","Defaun_boot.pdf",sep=""),width=10,height=7)
png(file=paste("Figures/","Defaun_boot.png",sep=""), width=1900,height=1400,res=250)
boxplot(resultDefaun$t[,19],resultDefaun$t[,4],resultDefaun$t[,8],resultDefaun$t[,12],resultDefaun$t[,16],xlab="% Change in AGC",cex.lab=1.3,boxwex=0.7,horizontal = T,outwex=0.3,
        names=c("All","Emer","Cano","Unde","Lowe"),ylim=c(-10,27),notch=T,col=c("red","grey","grey","grey","grey"),cex.lab=1.2)
abline(v=0,lty="dashed")
text(6,0.8,"Median of all strata -2% ",cex=0.8,col="red")
dev.off()

#--------------------------------------------
#
#   Control Scenario: no defaunation
#
#--------------------------------------------
# Confidence intervals
CI_Random <- boot.ci(resultRandom, type = "perc", index = 19) #All species
Median_Random <- median(resultRandom$t[,19]) #median of all species
SE_Random <- c(Median_Random-CI_Random$percent[4],Median_Random-CI_Random$percent[5])

pdf(file=paste("Figures/","Random_boot.pdf",sep=""),width=10,height=7)
#png(file=paste("Figures/","Random_boot.png",sep=""),width=1900,height=1400,res=250)
boxplot(resultRandom$t[,19],resultRandom$t[,4],resultRandom$t[,8],resultRandom$t[,12],resultRandom$t[,16],xlab="% Change in AGC",cex.lab=1.3,boxwex=0.7,horizontal = T,outwex=0.3,
        names=c("All","Emer","Cano","Unde","Lowe"),ylim=c(-0.6,1.2),notch=T,col=c("red","grey","grey","grey","grey"),cex.lab=1.2)
abline(v=0,lty="dashed")
text(14,0.8,"Median of all strata ~ 0.7% ",cex=0.8,col="red")
dev.off()
