rm(list=ls(all=TRUE)) 
setwd("F:/Research2016/Tree Architecture/WoodDensity/Codes/manuscript_Revised/Aug2018/") 

load("Results/01-Data.RData")
library(boot)
library(colorspace)
#Simulation time steps
SimT = 200

#Intensity of defuanation leavel
Frac <- seq(.2:1,by=.2)

#Read file
All_T <- read.csv("All_T_WSG_MoreDM.csv")
 
# Consider only trees with dbh>5 cm according to the calculation of AGB by Chave et al. (2005)
All_T <- subset(All_T,dbh.C3 >=5 & dbh.C3 <185)


#AGB function for the mosit tropical forest from Chave et. al. 2005
AGB <- function(D,W){
  return(W*exp(-1.499+2.148*log(D)+0.207*log(D)^2-0.0281*log(D)^3))
}

#Calucate AGB and add to the main dataframe
All_T$AGB <-mapply(AGB,All_T$dbh.C3,All_T$WSG)

All_T=All_T[!(is.na(All_T$WSG)==T),]
#All AGB, the dbh size >185 may not have been correctly measured
All_AGB=sum(All_T$AGB)

#-------------------------------------
#Function: simulation with respect to two size classes
defauanSimWSG.bins = function(DM1,Bin1,Bin2){ #e.g. DM1= All_Disp$DM_All,etc.
  #Select large-bodied frugivores Index
  L <- which(DM1=="L"& All_T$dbh.C3>=Bin1 & All_T$dbh.C3<Bin2)
  #Species pool left
  NL <- which(DM1=="O"& All_T$dbh.C3>=Bin1 & All_T$dbh.C3<Bin2)
  All_NL = All_T[NL,]
  
  Res=0 #variable to collect a result
  #Simulation
  for(i in 1:SimT){
    #Someties L >NL
    if(length(NL)>=length(L)){
      Sim_Index =sample(length(NL),length(L))
    }else{
      Sim_Index =c(sample(length(NL),length(NL)),sample(length(NL),length(L)-length(NL)))
    }
    #Assumption: Constant tree size
    AGB_Sim = sum(mapply(AGB,All_T$dbh.C3[L],All_NL$WSG[Sim_Index]))+
      sum(mapply(AGB,All_NL$dbh.C3,All_NL$WSG))  
    Res[i]=AGB_Sim 
  }
  return(Res)
}

#Function: the defuanation for all size classes
defauanSimWSG.allbins = function(DM1,indices){
  d=DM1[indices] 
  #The bind size are based on the Geometric series
  Bins=c(5,20,40,80,185)
        #Create variables to record data
    Res=array(0,dim=c(length(Bins)-1,2))
    AGB_Bins_All_Defaun =0;AGB_Bins_All=0
    #Loop for all "Bins
    for(i in 1:(length(Bins)-1)){
      AGB_Bins_All_Defaun[i]=median(defauanSimWSG.bins(DM1,Bins[i],Bins[i+1]))
      AGB_Bins_All[i]=sum(mapply(AGB,All_T$dbh.C3[All_T$dbh.C3>=Bins[i]&All_T$dbh.C3<Bins[i+1]],
                                 All_T$WSG[All_T$dbh.C3>=Bins[i]&All_T$dbh.C3<Bins[i+1]]))
    } 
    #AGB of each size class   
    Res[,1] =AGB_Bins_All_Defaun
    #% of each size class 
    Res[,2]=100*(AGB_Bins_All_Defaun-AGB_Bins_All)/AGB_Bins_All
    #Add AGB all
    Res = rbind(Res,c(sum(Res[1:4,1]),100*(sum(Res[1:4,1])-All_AGB)/All_AGB))
    colnames(Res) =c("Actual","Percent")
     return(Res)
}


#==============================================================
#Type=All_T$DM_All
Scenarios = function(Type){ #e.g. Type = All_T$DM_All
  L<- which(Type=="L")
  Frac <- seq(.2:1,by=.2)
  L_Frac= round(Frac*length(L))
  #create a list to collect data
  Frac_Defuan=paste("S",seq(.2:1,by=.2),sep="")
 # Res_Defaun<-list(Frac_Defuan=seq(.2:1,by=.2),Sim_Res=list(paste("S",seq(.2:1,by=.2),sep="")))
  Res_Defaun <- list() 
   for (i in 1:length(Frac_Defuan)){
      Res_Defaun[Frac_Defuan[i]] =0
      }
  #Simulate by the fractions of defauantion
  for(i in 1:length(Frac)){
    #This DM will be used for the function below 
    All_T$DM <- rep("O",nrow(All_T)) 
    if(i<length(Frac)){ 
      #Remove accordingly to a fraction above
      L_Indices <- L[round(sample(length(L),L_Frac[i]))]
      All_T$DM[L_Indices] <-"L"
    }else{
      #Use the field data
      All_T$DM[L] <-"L"
    }  
   Res_Defaun[[i]] =boot(data=as.factor(All_T$DM),statistic = defauanSimWSG.allbins,R=200)
  }
    return(Res_Defaun)  
}
#All large-bodied frugivores extirpated
Res_All=Scenarios(All_T$DM_All)

#Only priames extirpated
Res_Prim=Scenarios(All_T$DM_Prim)

#Control scenario
Res_Cont=Scenarios(All_T$DM_Cont)


save.image(""Sim_All_Primates_Cont_Constant.RData"")


