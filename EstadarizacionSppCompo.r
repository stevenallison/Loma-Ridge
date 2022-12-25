
###This documentation shows the code used to modified,add and update some 
###columns based on the most recent years formats for species composition data.
###Here we also created unique species list which will be use for other codes

################################################################################
rm(list=ls())
## Call and downloaded packages
library(tidyverse)

## Code below can be use if you run this code using your Google Drive Desktop using a PC 
setwd("G:/My Drive/FolderTrabajoPersonal/Loma Experiment/SpeciesCompositionLoma/20092018GLsppComp") # Sets directory

spp2009<-read_csv("DOE_LRG_Data_SpeciesComp_2009.csv")# Call a file

# Code to add and modify columns
spp2009.v2 <- spp2009 %>%
mutate(Year = "2009") %>% #Adds year
relocate(Year, .before = PlotFull)%>% #Move year column from last to first
rename(Plot_ID = PlotFull, Water_Treatment = PPT,Nitrogen_Treatment=N)%>% #rename columns to match recente years
rename(BareGround = bare, LitterThatch  = Litter)%>%
relocate(BareGround, .after = Subplot)%>%
relocate(LitterThatch, .after = BareGround)%>%
mutate_all(~replace(., is.na(.), 0))%>% # add zeros where there are NA
mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>% # Add a Water treatment column and fill rows with information based on whether plot code has an A, X or R
mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>% # Add a Nitrogen treatment column and fill rows with information based on whether plot code has an N or X
mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))

## Here we are creating a list of unique species codes that were collected 
## on the specified year.
list2009 <- spp2009 %>%
  select(AMME:VUMY) %>%
  pivot_longer(AMME:VUMY, names_to="SpeciesCode")%>%
  select(SpeciesCode)%>%
  unique()

# Save species list as a csv file
write.csv(list2009, "DOE_LRG_SppList_2009.csv", row.names=F)

# Save species composition file as a csv file
write.csv(spp2009.v2, "DOE_LRG_Data_SpeciesComp_2009.csv", row.names=F)

################################################################################
rm(list=ls())
## Call and downloaded packages
library(tidyverse)

## Code below can be use if you run this code using your Google Drive Desktop using a PC 
setwd("G:/My Drive/FolderTrabajoPersonal/Loma Experiment/SpeciesCompositionLoma/20092018GLsppComp") # Sets directory

spp2009<-read_csv("DOE_LRG_Data_SpeciesComp_2009.csv")# Call a file

# Code to add and modify columns
spp2009.v2 <- spp2009 %>%
mutate(Year = "2009") %>% #Adds year
relocate(Year, .before = PlotFull)%>% #Move year column from last to first
rename(Plot_ID = PlotFull, Water_Treatment = PPT,Nitrogen_Treatment=N)%>% #rename columns to match recente years
rename(BareGround = bare, LitterThatch  = Litter)%>%
relocate(BareGround, .after = Subplot)%>%
relocate(LitterThatch, .after = BareGround)%>%
mutate_all(~replace(., is.na(.), 0))%>% # add zeros where there are NA
mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>% # Add a Water treatment column and fill rows with information based on whether plot code has an A, X or R
mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>% # Add a Nitrogen treatment column and fill rows with information based on whether plot code has an N or X
mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))

## Here we are creating a list of unique species codes that were collected 
## on the specified year.
list2009 <- spp2009 %>%
  select(AMME:VUMY) %>%
  pivot_longer(AMME:VUMY, names_to="SpeciesCode")%>%
  select(SpeciesCode)%>%
  unique()

# Save species list as a csv file
write.csv(list2009, "DOE_LRG_SppList_2009.csv", row.names=F)

# Save species composition file as a csv file
write.csv(spp2009.v2, "DOE_LRG_Data_SpeciesComp_2009.csv", row.names=F)

