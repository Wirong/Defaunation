# Function for making the mnemonic
Abbrev <-function(dat){
  Gen_Spp<- (unlist(strsplit(as.character(dat),split=" ")))
 return(paste(substr(Gen_Spp[1],1,3),substr(Gen_Spp[2],1,3),sep=""))
}