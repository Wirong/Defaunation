# ================================================
#This code is for the manuscript entitled
#"Defaunation of large-bodied animals reduces carbon 
#storage in a tropical forest of Southeast Asia"
# Manipulate data, join dataset of 
#strata and seed dispersers
# developed by Wirong Chanthorn 2018
#
#'==================================================
rm(list=ls(all=TRUE))
setwd("F:/Research2016/Tree Architecture/WoodDensity/Codes/manuscript_Revised/Aug2018/") #******change directery
source("sources/Abbrev.r") #This is a function to make an abbreviated name

#-----------------------------------
# Read all data (and X,Y) of the third census in 2010
All_T <- read.csv("Data/Cleaned_Tree_2010.csv") 
All_T$X0 <- All_T$X-300 # The first colum of the plot is 15,
#thus we have to substract by 300

#Correct Mangsp to Mangdu
All_T$mnemonic<- as.character(All_T$mnemonic) #convert to characters
All_T$mnemonic[which(All_T$mnemonic=="MANGSP")] <- "MANGDU" #change to the new name
#"Chionanthus microstigma" change to Olea brachiata
All_T$mnemonic[which(All_T$mnemonic=="CHIOMI")] <- "OLEABR"
All_T$mnemonic[which(All_T$Sci_Name=="Chionanthus microstigma")] <- "Olea brachiata"
#read the metadata of full scientitific name
SppList <- read.csv("Data/Mnemonic.csv") 
SppList$Sci_Name <- paste(as.character(SppList$genus),as.character(SppList$species),sep=" ")

#Match mnemonic
mat4mnemonic<-function(mnemonic){
  if(which(as.character(SppList$mnemonic)%in%as.character(mnemonic)==T)){
    return(as.character(SppList[as.character(SppList$mnemonic)%in%as.character(mnemonic),6]))
  }else{
    return(NA)
  }
}

#Add full scientifc names
All_T$Sci_Name <- sapply(All_T$mnemonic,mat4mnemonic)

#Remove tree ferns and monocots (palms, Pandanus) from this analysis
All_T = All_T[!(All_T$Sci_Name%in%c("Cyathea latebrosa","Livistona jenkinsiana ","Pandanus bifidus ")),]

# Read the metadata of WSG & seed dispersal modes
All_Disp <- read.csv("Data/MST_Combined_Meta_Jan2017.csv")

#remove STERLA because there was not in any census data  
All_Disp <- All_Disp[which(All_Disp$mnemonic!="STERLA"),]

#Nephelium has very low chance to be dispersed by terrestial frugivores (e.g. deer), and they tend to be seed predators
All_Disp$Terr.Mamm[All_Disp$Species=="Nephelium melliferum"] <- ""

#WSG from Khao Yai from Patcharin's project (unpublished data) in secondary forests 
WSG_KY = read.csv("Data/Patcharin_WSG_data.csv")
#find median
WSG_KY2 <-tapply(WSG_KY$WSG,WSG_KY$Species,median)
#Join to the main dataset
  All_Disp$spwood[All_Disp$Species%in%names(WSG_KY2)]=WSG_KY2

#Add more species to large-bodied dispesal modes
#because species with larger seed size have very little chance
#to be dispersed by smaller animals
#information of seed size comes from Kitamura et al. 2002 (see the full referene in the paper)
AddSpp<- c("Aglaia lawii", "Dysoxylum cyrtobotryum", "Polyalthia simiarum","Prunus javanica", "Eugenia siamensis")

#So we removed those species from the smaller birds
All_Disp$small.bird[All_Disp$Species%in%AddSpp] <- rep("",length(AddSpp))
#------------------------------------------
#Function for classification  
#Classify seed dispersal modes without any redundancy between 
#Tree dispesed by large-bodied frugivores and the others
#L means the defauantion of large bodied frugrivores clasffied
#O means the others excluding L (but including wind-dispersed species)
#
#------------------------------------------
  #All large frugivores defaunated: 
  #gibbons, macaques, hornbills, and terristial mammals
  # ** terrestrial mammals completely overlap with the rest
  Class_DM_All <- function(Indiv){
    if((Indiv$Gibbon=="x"|Indiv$Monkey=="x"|Indiv$Hornbill=="x")&Indiv$small.bird!="x"){
      return("L")
      }else{
      return("O")
    }
  }

  #High-chance, only defuanation of primates
  Class_DM_Prim <- function(Indiv){
  if((Indiv$Gibbon=="x"|Indiv$Monkey=="x")&Indiv$Hornbill!="x"&
     Indiv$small.bird!="x"& Indiv$Terr.Mamm!="x"){
    return("L")
    }else{
    return("O") 
    }
  } 
  
  #Apply the function above here and add to the main data
  for(i in 1:nrow(All_Disp)){
    All_Disp$DM_All[i]<- Class_DM_All(All_Disp[i,])
    All_Disp$DM_Prim[i]<- Class_DM_Prim(All_Disp[i,])
  }

#Assign WSG for species with no WSG data of species or genus  
  All_Disp$WSG <- ifelse(is.na(All_Disp$spwood)==F,as.character(All_Disp$spwood),
                       ifelse(is.na(All_Disp$genwood)==F,as.character(All_Disp$genwood),
                              as.character(All_Disp$famwood)))

#write.csv(All_Disp,"Aug2018-/Results/All_Disp_Scenarios.csv",row.names = F)

#Match mnemonic
  mat4disp<-function(mnemonic_census){
    if(length(which(as.character(All_Disp$mnemonic)%in%as.character(mnemonic_census)))>0){
      return(which(as.character(All_Disp$mnemonic)%in%as.character(mnemonic_census)))
    }else{
      return(NA)    
    }
  }

#Get indices
Indices<- sapply(All_T$mnemonic,mat4disp)

#Make a dataframe
Query<-data.frame(All_Disp$mnemonic[Indices],All_Disp$WSG[Indices],All_Disp$stratum[Indices],
                  All_Disp$DM_All[Indices],All_Disp$DM_Prim[Indices])
 colnames(Query) <- c("mnemonic_Disp","WSG","Strata","DM_All","DM_Prim")

 #Add the selected data to the main data
All_T_WSG <- cbind(All_T,Query)

#Some species are unknown, which do not exist in the datbase
#Thus we manually added genus WSG
#change to character first
All_T_WSG$WSG = as.numeric(as.character(All_T_WSG$WSG))

#Check the name
#All_T_WSG$Sci_Name[is.na(All_T_WSG$WSG)==T]

#Add
All_T_WSG$WSG[All_T_WSG$Sci_Name=="Symplocos sp1"]= 0.536183
All_T_WSG$WSG[All_T_WSG$Sci_Name=="Ardisia quinguegona "]= 0.597818
All_T_WSG$WSG[All_T_WSG$Sci_Name=="Ixora nigricans "]= 0.597818
#To assingn the same letter "L" for the control scenario: 
#randomly insert "L" to the whole community and conserv 
#with the same numbers of individual dispersed by all large-bodied frugivores
NumL = which(All_T_WSG$DM_All=="L")

#Create a new column of the control scenario
All_T_WSG$DM_Cont = "O"
#Randomly insert "L" as the same number as that trees dispersed by L
Rand_L = sample(nrow(All_T_WSG),length(NumL), replace=F)
All_T_WSG$DM_Cont[Rand_L] = "L"

#write.csv(All_T_WSG,"Results/All_T_WSG_MoreDM.csv",row.names = F) #More species added
save.image("Results/01-Data.RData")

