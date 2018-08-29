rm(list=ls(all=TRUE)) 
setwd("F:/Research2016/Tree Architecture/WoodDensity/Codes/manuscript_Revised/Aug2018/") 
load("Results/01-Data.RData")
library(ggplot2)
source("sources/multiplot.r")

#Select only the size for carbon estimation
All_Disp = All_Disp[All_Disp$Max.dbh>=5,]
All_Disp= All_Disp[is.na(All_Disp$DM_All)==F,]

#change WSG to numeric
All_Disp$WSG = round(as.numeric(All_Disp$WSG),digits = 3)

# The palette with grey:
cbPalette <- c("#009E73","#D55E00","#000000", "#E69F00","#009E73", "#F0E442","#0072B2", "#D55E00")

#Create the`function again for plotting
#Maximum Size
Plot_SubFigs = function(DM,DM_title,Ylabel,NumCol){
  P=ggplot(All_Disp, aes(x=DM, y=Max.dbh,fill=DM)) +
    geom_boxplot(width=0.6)+
    labs(title=DM_title,y=Ylabel,x=" ")+
    scale_y_continuous(limits = c(0, 195))+ #The maximum size limited to 185 cm
    scale_fill_manual(values=cbPalette[NumCol])+
    theme(title = element_text(size=14),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(size=14,face="bold"),
          axis.text.y = element_text(size=12),
          legend.position="none")
  return(P)
}

#WSGM_All
Plot_SubFigs_WSG = function(DM,DM_title,Ylabel){
  P=ggplot(All_Disp, aes(x=DM, y=WSG,fill=DM)) +
    geom_boxplot(width=0.6)+
    labs(title=DM_title,y=Ylabel,x=" ")+
    scale_y_continuous(limits = c(0.1, 1))+
    theme(title = element_text(size=14),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(size=14,face="bold"),
          axis.text.y = element_text(size=12),
          legend.position="none")
  return(P)
}

#Change names for visualization
All_Disp$DM_All = ifelse(All_Disp$DM_All=="L","All large","Others")
All_Disp$DM_Prim = ifelse(All_Disp$DM_Prim=="L","Primates","TBO")

#Create a figure
tiff(filename = "Results/Fig3_MaxSize_WSG.tif",height = 2000,width = 2000, res=300)
multiplot(Plot_SubFigs(All_Disp$DM_All,"Maximum size","Dbh (cm)",6:5),
          Plot_SubFigs_WSG(All_Disp$DM_All,"Wood specific gravity",expression(WSG~(g~cm^{-3}))),
          Plot_SubFigs(All_Disp$DM_Prim,"Maximum size "," ",6:5),
          Plot_SubFigs_WSG(All_Disp$DM_Prim,"Wood specific gravity",""),cols=2)
dev.off()

#Save data for t-test
save.image("Results/02-MaxSize.RData")






