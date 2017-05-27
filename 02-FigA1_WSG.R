#' ================================================
#'
#' Plot wood density (Fig. A1)
#'
#'==================================================
library(ggplot2)
rm(list=ls(all=TRUE))

setwd("D:/Research2016/Tree Architecture/WoodDensity/Appendix_Analysis_Codes/") 
load("01-Data.RData")

#Change WSG to the numberic format
All_T_WSG$WSG  <-  as.numeric(as.character(All_T_WSG$WSG))

#Creat figures

Class="1-E";Class_Name="Emergent"
Stra <- subset(All_Disp,stratum==Class)
png(file=paste("Figures/",Class_Name,".png",sep=""),width=1000,height=800,res=200)
qplot(DM, as.numeric(WSG), data=Stra , geom=c("boxplot", "point"), 
      fill=DM, main=Class_Name, xlab="", ylab="Wood density",ylim=c(0.1,1))
dev.off()

Class="2-C";Class_Name="Canopy"
  Stra <- subset(All_Disp,stratum==Class)
  png(file=paste("Figures/",Class_Name,".png",sep=""),width=1000,height=800,res=200)
   qplot(DM, as.numeric(WSG), data=Stra , geom=c("boxplot", "point"), 
        fill=DM, main=Class_Name, xlab="", ylab="Wood density",ylim=c(0.1,1))
  dev.off()
  
  Class="3-U";Class_Name="Understory"
  Stra <- subset(All_Disp,stratum==Class)
  png(file=paste("Figures/",Class_Name,".png",sep=""),width=1000,height=800,res=200)
  qplot(DM, as.numeric(WSG), data=Stra , geom=c("boxplot", "point"), 
        fill=DM, main=Class_Name, xlab="", ylab="Wood density",ylim=c(0.1,1))
  dev.off()

  Class="4-LU";Class_Name="Lower understory"
  Stra <- subset(All_Disp,stratum==Class)
  png(file=paste("Figures/",Class_Name,".png",sep=""),width=1000,height=800,res=200)
  qplot(DM, as.numeric(WSG), data=Stra , geom=c("boxplot", "point"), 
        fill=DM, main=Class_Name, xlab="", ylab="Wood density",ylim=c(0.1,1))
  dev.off()
  