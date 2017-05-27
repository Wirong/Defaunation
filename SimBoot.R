library(boot)
library(matrixStats)

#Create function of defauantion simulation 
Sim_Boot <- function(data, indices, SimT = 10, type = "Defaun"){
  
  dat = data[indices,]
  
  if(type == "Defaun"){
    
  E_Def <- Sim_Defaun(data = dat, Stratum = "1-E", SimT = SimT) 
  C_Def <- Sim_Defaun(data = dat, Stratum = "2-C", SimT = SimT)  
  U_Def <- Sim_Defaun(data = dat, Stratum = "3-U", SimT = SimT) 
  LU_Def <- Sim_Defaun(data = dat, Stratum = "4-LU", SimT = SimT) 

  }else if(type == "Random"){
    
  E_Def <- Sim_Random(data = dat, Stratum = "1-E", SimT = SimT) 
  C_Def <- Sim_Random(data = dat, Stratum = "2-C", SimT = SimT)  
  U_Def <- Sim_Random(data = dat, Stratum = "3-U", SimT = SimT) 
  LU_Def <- Sim_Random(data = dat, Stratum = "4-LU", SimT = SimT) 

  }else stop("type must be either Defaun or Random")
  
  #All % impact
  Tot_Sim <- E_Def[,1]+C_Def[,1]+U_Def[,1]+LU_Def[,1]
  Tot_Obs <- E_Def[,2]+C_Def[,2]+U_Def[,2]+LU_Def[,2]
  All_Stra <- 100*(Tot_Sim- Tot_Obs)/Tot_Obs
    
  return(c(colMedians(E_Def), colMedians(C_Def), colMedians(U_Def), colMedians(LU_Def), 
           median(Tot_Sim), median(Tot_Obs), median(All_Stra) ))
}


#test = boot(data = All_T, statistic = Sim_Boot, R = 10, SimT = 10 )


