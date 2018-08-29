rm(list=ls(all=TRUE)) 
setwd("F:/Research2016/Tree Architecture/WoodDensity/Codes/manuscript_Revised/Aug2018/") 
load("Results/01-Data.RData")
library(ggplot2)

#Read the file of all individuals
All_T <- read.csv("Results/All_T_WSG_MoreDM.csv")

# Consider only tree with dbh>5 cm according to the calculation by Chave et al. (2005)
All_T <- subset(All_T,dbh.C3 >=5 & dbh.C3 <185)

#AGB function for the mosit tropical forest from Chave at al. (2005)
AGB <- function(D,W){
  return(W*exp(-1.499+2.148*log(D)+0.207*log(D)^2-0.0281*log(D)^3))
}

#Calucate AGB and add to the main dataframe
All_T$AGB <-mapply(AGB,All_T$dbh.C3,All_T$WSG)

All_T=All_T[!(is.na(All_T$WSG)==T),]
#All AGB, the dbh size >185 may not have been correctly measured
All_AGB=sum(All_T$AGB)

#Create the new feild to reclassfiy into three groups:
#1) P= only primates 2) BT=large-bodied birds and terrrestial mammals
# 3) O = the other species
All_T$DM_3Gr = ifelse(All_T$DM_All=="L" & All_T$DM_Prim=="L","P",
                      ifelse(All_T$DM_All=="L" & All_T$DM_Prim=="O","TB","O"))

# The palette with grey:
cbPalette <- c("#009E73","#D55E00","#000000", "#E69F00","#009E73", "#F0E442","#0072B2", "#D55E00")

tiff(filename = "Results/Fig2_Total_AGB_NoSubFig.tif",height = 900,width = 1200, res=300)
  Df = data.frame(Tot_AGB=sort(0.5*tapply(All_T$AGB,All_T$DM_3Gr,sum)/1000),
                          DM=c("P","TB","O"))
  ggplot(Df, aes(x=DM, y=Tot_AGB,fill=DM)) +
    geom_bar(stat="identity",width=0.7)+
    labs(title= " " ,y="Above-ground C (ton)",x="")+
   scale_y_continuous(limits = c(0, 3100))+
    scale_fill_manual(values=cbPalette[c(1,7,8)])+
    theme(title = element_text(size=10),
          axis.title.y = element_text(size=11),
          axis.text.x = element_text(size=10,face="bold"),
          axis.text.y = element_text(size=9),
          legend.position="none")
dev.off()


