## This code has the purspose to update the species codes from the 
## species composition files. 
## Here we also do calculations for the coverages 

rm(list=ls())
## Call your downloaded packages
library(tidyverse)
library(data.table)
library(janitor)

## If you run the code by your Google Drive Desktop using a PC 
#setwd("G:/My Drive/FolderTrabajoPersonal/Loma Experiment/SpeciesCompositionLoma/20182018CSSsppComp")

#Mike's species list with updated codes
updatedlist <- read.csv("mikes.update_CSS_2009_2018.csv")

updatedlist %>% 
  count(SpeciesCode)%>%
  filter(n>1)
##################################################
#Updated historic data set
data2018 <- read.csv("DOE_LRS_Data_SpeciesComp_Entered_Updated2018.csv")# Call original 

#Transpose the data set to be able to join and add new species codes
data2018_transpose <- as.data.frame(t(as.matrix(data2018)))

#Make row name as an actual column to allow merging in the next step
data2018_trans.v2 <- setDT(data2018_transpose, keep.rownames = TRUE)[]%>%
  rename(SpeciesCode = rn)# rename to have same column names 
  
#Join histoic with updated names
New2018 <- left_join(data2018_trans.v2, updatedlist, by="SpeciesCode")%>%
  relocate(NewSpeciesCode, .after = SpeciesCode)%>%
  relocate(Full_Scientific_Name, .after = NewSpeciesCode)
  
New2018.v2 <- as.data.frame(t(as.matrix(New2018)))

colnames(New2018.v2)=New2018.v2[c(2),]# uses row names to become column names
rownames(New2018.v2) <- 1:nrow(New2018.v2) # renames row names to numbers


New2018.v3 <-  New2018.v2%>%
  clean_names(., "none") %>% #Clean names to avoid error
  slice(4:99)%>% #Keep rows with data
  rename(Year = "NA",
        Plot_ID = "NA_2",
         Water_Treatment = "NA_3",
         Nitrogen_Treatment= "NA_4",
        #Continuation= "NA_5", 
        Subplot= "NA_5",
         BareGround= "NA_6",
         LitterThatch= "NA_7")

#Here we save the new file
write.table(New2018.v3,"UpdatedFiles/DOE_LRS_Updated_SppComp_2018.csv",quote=F,row.names=F,sep=",",na="") # save daily table




