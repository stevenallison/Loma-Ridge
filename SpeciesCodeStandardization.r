##This file updates the species code files
##and keep only the unique species codes.
##Then it pastes in a new column the 6 species code names. This file will be use
##to update all species databases.

################################################################################
rm(list=ls())
## Call your downloaded packages
library(tidyverse)

## If you run the code by your Google Drive Desktop using a PC 
setwd("G:/My Drive/FolderTrabajoPersonal/Loma Experiment/SpeciesCompositionLoma/20092018GLsppComp")

#makes one master file by merging each year per species list 
Allspp <- list.files(path = "SppLists", pattern = "DOE_LRG",ignore.case = TRUE, full.names = T)%>% # makes a list per year
map_df(~read_csv(., col_types = cols(.default = "c"))) %>% # takes all list to a master dataframe
  unique() #keep only unique codes

mikes<- read.csv("SppLists/Historical_Species_Key_For_Goulden_Data_LRGCPCC.csv") # calls Mike's historic species keys

new.list <- read.csv("SppLists/LomaGLSpeciesComp_QC_2020.csv")# Calls current species list

# rename columns to match new.list object to be able to merge both files based on 
# full species name
New.Mikes <- mikes %>%
  rename(SpeciesCode = Code, Full_Scientific_Name = Name, New_Name = New.Name)%>%
  relocate(Type, .after = New_Name)%>%
  select(c(1:4))

# Using the speciescode we join the file that has all the species from 2009-2018
# with the file that has full species names
full <- left_join(Allspp, New.Mikes, by="SpeciesCode")

# We save that new file
write.csv(full, "DOE_LRG_SppList_2009_2019.csv", row.names=F)

## Join full historic list with updated codes from Kimball
## usign full species names
mikes.update <- left_join(full, new.list, by="Full_Scientific_Name")
write.csv(mikes.update, "mikes.update_2009_2018.csv", row.names=F)



