# ================================================
#
# Create Fig1 
#
#==================================================
rm(list=ls(all=TRUE))
setwd("D:/Research2016/Tree Architecture/WoodDensity/Appendix_Analysis_Codes/") 

All_T_WSG <- read.csv("All_T_WSG.csv")
#Change WSG to the numberic format
All_T_WSG$WSG  <-  as.numeric(as.character(All_T_WSG$WSG))

#-----Calculate above ground biomass -----
# AGB function
AGB <- function(D,W){
  return(W*exp(-1.499+2.148*log(D)+0.207*log(D)^2-0.0281*log(D)^3))
}

#Find AGB dbh>=5cm
All_DbhGeq <- subset(All_T_WSG,dbh.C3 >=5)
All_DbhGeq$AGB <- mapply(AGB,All_DbhGeq$dbh.C3,All_DbhGeq$WSG)

#check individuals with NA
#which(is.na(All_DbhGeq$AGB==T))
#Orderand join before combine
E<-subset(All_DbhGeq,Strata=="1-E"); C<-subset(All_DbhGeq,Strata=="2-C"); 
L<-subset(All_DbhGeq,Strata=="4-LU"); U<-subset(All_DbhGeq,Strata=="3-U"); 
G<-subset(All_DbhGeq,Strata=="5-G")

#Sum
E_AGB <-0.001*tapply(E$AGB,E$DM,sum)
C_AGB <-0.001*tapply(C$AGB,C$DM,sum)
L_AGB <-0.001*tapply(L$AGB,L$DM,sum)
U_AGB <-0.001*tapply(U$AGB,U$DM,sum)
G_AGB <-0.001*tapply(G$AGB,G$DM,sum)

#Acutual value of Carbon
C_Starta <- c(sum(E_AGB/2),sum(C_AGB/2),sum(U_AGB/2),sum(L_AGB/2),sum(G_AGB/2))

#----Percent of per total biomass
#Percent for total biomass (dbh>=3 cm) and convert to Carbon
Tot_C <- 0.5*sum(All_DbhGeq$AGB,na.rm=T)/1000
#Percent in each startum
C_Starta/Tot_C

#Create the figure
png(file=paste("Figures/All4_AGB_Geq5cm_Percent.png",sep=""),
    width=1500,height=1800,res=200)
par(mfrow=c(2,2),cex.lab=1.3,cex.axis=1.1)
barplot(100*0.5*E_AGB/Tot_C,ylab="%AGC",xlab="",main="Emergent",ylim=c(0,30),space=0.5)
mtext("a)", side = 2, adj=2, las=1, padj=-12, cex = 1.3)
barplot(100*0.5*C_AGB/Tot_C,ylab="%AGC",xlab="",main="Canopy",ylim=c(0,30),space=0.5)
mtext("b)", side = 2, adj=2, las=1, padj=-12, cex = 1.3)
barplot(100*0.5*U_AGB/Tot_C,ylab="%AGC",xlab="Dispersal modes",main="Understory",ylim=c(0,15),space=0.5)
mtext("c)", side = 2, adj=2, las=1, padj=-12, cex = 1.3)
barplot(100*0.5*L_AGB/Tot_C,ylab="%AGC",xlab="Dispersal modes",main="Lower understory",ylim=c(0,15),space=0.5)
mtext("d)", side = 2, adj=2, las=1, padj=-12, cex = 1.3)
dev.off()




