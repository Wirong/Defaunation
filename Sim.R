#Create function of defauantion simulation 
Sim_Defaun <- function(data, Stratum, SimT = 10){
  #Select large-bodied frugivores
  L <- subset(data,DM=="L"& Strata==Stratum)
  
  #Species pool left
  NotL <- subset(data,DM!="L"&Strata==Stratum)
  
  #Calculate Total Basal area
  L_TotBA<- sum(L$BA)
  NotL_TotBA <- sum(NotL$BA)
  
  #Total AGB
  L_TotAGB<- sum(L$AGB)
  NotL_TotAGB <- sum(NotL$AGB)
  
  #-----Ramdomly replace withthe same individual removed 
  #Create a matrix to keep AGB and % for SimT simulations times
  Res <- matrix(0,SimT,4)
  for(i in 1:SimT){
    Index <-sample(nrow(NotL),nrow(L),replace=T)
    Samp_TotBA_NotL<-sum(NotL$BA[Index]) #Total basal area 
    
    PerDiff <-(L_TotBA-Samp_TotBA_NotL)/L_TotBA # calcualte percent difference
    
    
    #Declare array to keep values
    Keep <- rep(0,nrow(L)) #The number should not above 2 times
    n=0
    
    
    while(abs(PerDiff)>Tolerance){
      n=n+1
      #sample one individual
      Index_One <- sample(nrow(NotL),1)
      #Keep in the array
      Keep[n] <- Index_One
      if(PerDiff<0){
        Samp_TotBA_NotL <- Samp_TotBA_NotL- NotL$BA[Index_One]
      }else{
        Samp_TotBA_NotL <- Samp_TotBA_NotL+NotL$BA[Index_One]
      }
      PerDiff <-(L_TotBA-Samp_TotBA_NotL)/L_TotBA
    }
    
    #Indices of all individuals replaced
    Indices <- c(Index,Keep)
    
    #Compare AGB
    AGB_Obs <- L_TotAGB+NotL_TotAGB
    AGB_Sim <- sum(NotL$AGB[Indices])+NotL_TotAGB
    
    #Keep Results
    Res[i,1]<-AGB_Sim  #AGB simulated
    Res[i,2]<-AGB_Obs  #AGB observed
    Res[i,3]<-AGB_Sim-AGB_Obs   #AGB simulated-#AGB observed
    Res[i,4]<-((AGB_Sim-AGB_Obs)/AGB_Obs)*100  #% difference between simulated and observed data
  }               
  return(Res)
}



#----------------Removal randomly dispersed species for all species ----------------------------

#Create function 
Sim_Random <- function(data, Stratum, SimT = 10){
  Stra <- subset(data,Strata==Stratum)
  TotBA <- sum(Stra$BA)
  TotAGB <- sum(Stra$AGB)
  #--------Ramdomly replace with the same individuals removed 
  #Create a matrix to keep AGB and %
  Res <- matrix(0,SimT,4)
  for(i in 1:SimT){
    Index <-sample(nrow(Stra),nrow(Stra),replace=T)
    Samp_TotBA<-sum(Stra$BA[Index]) #Total basal area 
    
    PerDiff <-(TotBA-Samp_TotBA)/TotBA
    #Use while loop to calucate difference 
    
    #Declare array to keep values
    Keep <- rep(0,nrow(Stra)) #The number should not above 2 times
    n=0
    while(abs(PerDiff)>Tolerance){
      n=n+1
      #sample one individual
      Index_One <- sample(nrow(Stra),1)
      #Keep in the array
      Keep[n] <- Index_One
      if(PerDiff<0){
        Samp_TotBA <- Samp_TotBA- Stra$BA[Index_One]
      }else{
        Samp_TotBA <- Samp_TotBA+Stra$BA[Index_One]
      }
      PerDiff <-(TotBA-Samp_TotBA)/TotBA
    }
    
    #Indices of all individuals replaced
    Indices <- c(Index,Keep)
    
    #Compare AGB
    AGB_Obs <- TotAGB
    AGB_Sim <- sum(Stra$AGB[Indices])
    
    #Keep Results
    Res[i,1]<-AGB_Sim
    Res[i,2]<-AGB_Obs
    Res[i,3]<-AGB_Sim-AGB_Obs
    Res[i,4]<-((AGB_Sim-AGB_Obs)/AGB_Obs)*100
  }               
  return(Res)
}
