#======================================================
#
# t-test of the maximum height and WSG between
# large-bodied and other frugivores
#
#======================================================
rm(list=ls(all=TRUE)) 
setwd("F:/Research2016/Tree Architecture/WoodDensity/Codes/manuscript_Revised/June2018-/") 
load("Results/02-MaxSize.RData")

#Function to do statistical tests
H_WSG_Ttest= function(DM,Dat){
  DM_Col= which(names(All_Disp)==DM) #Index of a column of dispersal mode
  #Number of species in each type of agent
  NumSpp=table(All_Disp[,DM_Col])
  #Stra <- subset(All_Disp,stratum==Stratum & WSG <0.85)
  Ftest=var.test(as.numeric(Dat)~as.factor(All_Disp[,DM_Col]))
  #var.test(as.numeric(All_Disp$WSG)~as.factor(All_Disp$DM_All))
  #t-test with equal variance
  if(Ftest$p.value >0.05){
    t=t.test(as.numeric(Dat)~as.factor(All_Disp[,DM_Col]),var.equal=T)
    #text(1.5,67,paste("t-test: P=",round(t$p.value,digits=2),", equal var", sep=""),cex=0.9)
  }else{
    t=t.test(as.numeric(Dat)~as.factor(All_Disp[,DM_Col]),var.equal=F) 
    #text(1.5,67,paste("t-test: P=",round(t$p.value,digits=2),", non-equal var", sep=""),cex=0.9)
  }
  return(list(t,Ftest,NumSpp))
}
#Use the function
t_test_DM_All_Size =H_WSG_Ttest("DM_All",All_Disp$Max.dbh)
t_test_DM_Prim_Size =H_WSG_Ttest("DM_Prim",All_Disp$Max.dbh)
t_test_DM_All_WSG =H_WSG_Ttest("DM_All",All_Disp$WSG)
t_test_DM_Prim_WSG =H_WSG_Ttest("DM_Prim",All_Disp$WSG)

#Create a table for excel 
Combined=data.frame(All_Size=c(t_test_DM_All_Size[[1]]$statistic,t_test_DM_All_Size[[1]]$p.value,
          t_test_DM_All_Size[[2]]$statistic,t_test_DM_All_Size[[2]]$p.value),
          #Primates
          Primates_Height=c(t_test_DM_Prim_Size[[1]]$statistic,t_test_DM_Prim_Size[[1]]$p.value,
          t_test_DM_Prim_Size[[2]]$statistic,t_test_DM_Prim_Size[[2]]$p.value),
          #WSG
          All_WSG=c(t_test_DM_All_WSG[[1]]$statistic,t_test_DM_All_WSG[[1]]$p.value,
                       t_test_DM_All_WSG[[2]]$statistic,t_test_DM_All_WSG[[2]]$p.value),
          #Primates
          Primates_WSG=c(t_test_DM_Prim_WSG[[1]]$statistic,t_test_DM_Prim_WSG[[1]]$p.value,
                            t_test_DM_Prim_WSG[[2]]$statistic,t_test_DM_Prim_WSG[[2]]$p.value))
          rownames(Combined)=c("t","P (t-test)"," F"," P (F-test)")
    Combined = round(Combined,digits = 3)
    
  #paste("(L=",t_test_DM_PrimTerr[[3]][1],
write.csv(Combined,"Results/t_test_Size_WSG.csv")
