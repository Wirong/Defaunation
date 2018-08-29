#----------------------------------------
#
# Visualization of the results
#
#----------------------------------------

rm(list=ls(all=TRUE)) 
setwd("F:/Research2016/Tree Architecture/WoodDensity/Codes/manuscript_Revised/June2018-/") 
load("Results/01-Data.RData")
library(boot)
library(colorspace)

load("Results/SimT200R200_SimWSG_All_PrimTerr.RData")
load("Results/SimT200R200_SimWSG_All_Cont.RData")

#--------------------------------------------------
#Percent of intensity
Per= Frac*100#c(20,40,60,80,100)
#Create band for polygon
Band_Per = c(Per,rev(Per))
#tested data
Num_Intensity =length(Per)
#Result = Res_All
# Create confidence intervals function
Conf_Med = function(Result,Num_Intensity){
  SE_Defaun = data.frame(Low=rep(0,Num_Intensity),Upp=rep(0,Num_Intensity))
  Median_Defaun=0
  i=1
  for(i in 1:Num_Intensity){
    #Result[[i]]$t[,7]
    CI_Defaun <- boot.ci(Result[[i]], type = "perc", index = 10) #All species
    Median_Defaun[i] <- median(Result[[i]]$t[,10]) #median of all species
    SE_Defaun[i,1:2] <- c(CI_Defaun$percent[4],CI_Defaun$percent[5])
  }
  Defaun_Frame <- data.frame(Percent=Per,Med=Median_Defaun,Low=SE_Defaun$Low,Upp=SE_Defaun$Upp)
  return(Defaun_Frame)
}

#Conf_Result = Defaun_Frame; title="Bin3"
Plot_Conf = function(Conf_Result,title,Xlabel,Ylabel,Poly_Color){
  #Creat a plot
  plot(Per,Conf_Result$Med,type="n",cex=3,ylim=c(-3,1),xlim=c(22,98),cex.lab=1.35,
       cex.axis=1.3,xlab=Xlabel,ylab=Ylabel,main=title,cex.main=1.7)
  #Confidence interval band
  polygon(Band_Per,c(Conf_Result$Low,rev(Conf_Result$Upp)),col = Poly_Color,border=NA)
  lines(Per,Conf_Result$Med,lty="dotdash",lwd=3,cex=4,col="tomato")
  abline(h=0,cex=1,lty="dashed",col="gray5")
}

#Apply function
#All
Conf_All=Conf_Med(Res_All,length(Per))
#Primate 
Conf_Prim=Conf_Med(Res_Prim,length(Per))
#Control scenario: not specific only on control 
Conf_Cont=Conf_Med(Res_Cont,length(Per))
#Export the results
Table_Res=data.frame(Conf_All,Conf_Prim,Conf_Cont)
write.csv(Table_Res,"JUne2018-/Simulation_Res.csv",row.names = F)

#-----------------Generate subfigures--------------------------
#x11()
#Use the color pallete,  
col4DM=rainbow_hcl(6)

#png(file=paste("Figures/","Defaun_3Scenarios.png",sep=""), width=3300,height=3000,res=250)
tiff(file=paste("Figures/","Defaun_3Scenarios_Control.tif",sep=""), width=1200,height=2600,res=300)
par(mfrow=c(3,1))
#par(mai=c(0.5,0.8,0.8,0.3))
Plot_Conf(Conf_All,"All species extirpated"," ","% Change in AGC",col4DM[6])
mtext("a)", side = 2, adj=1.5, las=1, padj=-7, cex = 1.3)
#par(mai=c(0.5,0.4,0.8,0.7))
Plot_Conf(Conf_Prim,"Primates extirpated"," ","% Change in AGC ",col4DM[4])
mtext("b)", side = 2, adj=1.5, las=1, padj=-7, cex = 1.3)
#par(mai=c(0.8,0.8,0.5,0.3))
Plot_Conf(Conf_Cont,"Control scenario","%Intensity of defaunation","% Change in AGC",col4DM[5])
mtext("c)", side = 2, adj=1.5, las=1, padj=-7, cex = 1.3)
dev.off() 