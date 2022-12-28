## This file allow calculating diversity and species richness
## for the costal sage shrublands data set
rm(list=objects()) 
setwd("G:/Shared drives/Microbes and Global Change/Loma Ridge/Experiments/Ecosystem manipulation/Data/Plant_Species_Composition/UpdatedFilesCSS/Older_Datasets")
setwd("C:/Users/Dilys Vela/Downloads")
## Functions from tidyr: 
# pivot_longer: convert many columns into variable/value pairs; akin to melt in reshape (previous gather in tidyr)
# pivot_wider: convert variable/value pairs into columns; akin to cast in reshape (previously spread in tidyr)

# Load the tidyverse
library(tidyverse)
library(vegan)
#################################################################### 
## Part 1: Call your file                                         ##
####################################################################
## Read in the raw data

sp2020 <- read_csv("Loma_CSS_Species_Comp_Combined.csv")

sp2020.2 <- sp2020 %>%
   filter(Year == "2020")

sp2021.2 <- sp2020 %>%
  filter(Year == "2021")

sp2022.2 <- sp2020 %>%
  filter(Year == "2022")
#setwd("C:/Users/Dilys Vela/Documents/UCI/MicrobiomesClimateChange/LTREB/codeR")

d.long<-sp2022.2%>%
  # Unselected elements below will change
  #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% #remove those codes for unknown
  #make sure that there is a match for species name in the lien below
  pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="Percentage")%>%
  # NEW CODE: group_by tells R which columns to summarize by
  group_by(Plot_ID, Water_Treatment, Nitrogen_Treatment, SpeciesCode)%>%
  # NEW CODE: with summarize you specify the new column name and how to summarize the data for that column
  mutate(SppPresence = replace(Percentage, Percentage > 0, 1))%>%
  summarize(SpNumb=sum(SppPresence, na.rm=T))%>% 
  mutate(Year=2022)%>%
  data.frame()

## calculating spp richness and diversity
## Note the '.' is telling R to use all the columns in the working data frame for the calculation
sp.div.ID<-  d.long %>%  #this line tells R to start with data frame d
  pivot_wider(names_from = SpeciesCode, values_from = SpNumb)%>%
  select(Plot_ID, Year, Water_Treatment, Nitrogen_Treatment)

all.sp.div<-  d.long %>%  #this line tells R to start with data frame d
  pivot_wider(names_from = SpeciesCode, values_from = SpNumb)%>%
  #make sure the spcodenames below matches the first and last spp name in the dataframe all.sp.div
  select(ACMAME:VROLIN) %>% #now just select the species data columns
  replace(is.na(.), 0) %>%
  mutate(spec.rich=specnumber(.)) %>%
  select(spec.rich)%>% #selects just the new diversity indices
  bind_cols(.,select(sp.div.ID,Plot_ID, Year, Water_Treatment, Nitrogen_Treatment))%>%  #This brings back in the plot.key column that was dropped earlier
  relocate(Plot_ID, Year, Water_Treatment, Nitrogen_Treatment, .before = spec.rich) %>%
  data.frame()


#########################################################################
###### Calculate numbers or Diversity calculations
TotalPerPlot <-sp2022.2%>%
  # Unselected elements below will change
  #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% #remove those codes for unknown
  select(SALMEL:SALAUS)%>%
  mutate(TotalPerPlot = rowSums(.))
  
d.long.div<-sp2022.2%>%
  # Unselected elements below will change
  #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% #remove those codes for unknown
  select(SALMEL:SALAUS)%>%
  mutate(across(SALMEL:SALAUS)/100)%>% 
  mutate(across(SALMEL:SALAUS)/TotalPerPlot$TotalPerPlot)%>%#Replace values to use in H diversity calculations
  #select(SALMEL:FESMYU) %>% #  
  mutate(H.div=diversity(.))%>%
  select(H.div)%>% #selects just the new diversity indices
  bind_cols(.,all.sp.div)%>%  #This brings back in the plot.key column that was dropped earlier
  relocate(Plot_ID, Year, Water_Treatment, Nitrogen_Treatment, .before = H.div) %>%
  data.frame()

joinall <- d.long.div %>%
  bind_cols(.,select(sp2022.2,Plot_ID, Native,NonNative, NativeForb, NativeShrub,NonNativeForb,NonNativeGrass,NativeGrass))  #This brings back in the plot.key column that was dropped earlier
  data.frame()
  
  write.csv(joinall, file="Responsevariables_CSS_2022.csv", row.names=FALSE,quote=TRUE)
  
## Call species list
setwd("G:/Shared drives/Microbes and Global Change/Loma Ridge/Experiments/Ecosystem manipulation/Data/Plant_Species_Composition/SpeciesListCodes")

sp <- read_csv("Spp_List_Updated_CSS_2009_2015.csv")

sp.2 <-sp %>%
  select(NewSpeciesCode,Native.Non.Native, Functional.Group)%>%
  rename(SpeciesCode = NewSpeciesCode)

##  Total cover per native or non-native
sp.cover.nat <-  sp2020.2 %>%  #this line tells R to start with data frame d
  #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% 
  pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="CoverPer")%>%
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  group_by(Plot_ID,Native.Non.Native)%>%
  filter(Native.Non.Native=="Native")%>%
  summarize(CoverPerNat=sum(CoverPer, na.rm=T))
  data.frame()

SpPerNonNat <-  sp2020.2 %>%  #this line tells R to start with data frame d
    # Unselected elements below will change
    #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% 
    #pivot_wider(names_from = SpeciesCode, values_from = SpHitsAbun)
    pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="CoverPer")%>%
    left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
    group_by(Plot_ID,Native.Non.Native)%>%
    filter(Native.Non.Native=="Non-Native")%>%
    summarize(CoverPerNonNat=sum(CoverPer, na.rm=T))%>%
    data.frame()
    
    (-UNKNOWN,-UNKNOWN_2,-UNKNOWN_3,-UNKNOWN_4,-UNKNOWN_5,-UNKNOWN_6,
      -UNKNOWN_7,-UNKNOWN_8)
    
##  Total cover per native or non-native and functional group
cover.natfun <-  sp2020.2 %>%  #this line tells R to start with data frame d
      # Unselected elements below will change
      #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% 
      pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="CoverPer")%>%
      left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
      filter(Native.Non.Native=="Native")%>%
      filter(Functional.Group == "Forb" | Functional.Group == "Perennial forb"|Functional.Group == "Annual forb")%>%
      mutate(Functional.Group = fct_recode(Functional.Group, 
                                         "Forb" = "Perennial forb"))%>%
      group_by(Plot_ID, Functional.Group)%>%
      summarize(CoverPerNatFor=sum(CoverPer, na.rm=T))%>%
      select(Plot_ID, CoverPerNatFor)%>%
      data.frame()

cover.natfun2 <-  sp2020.2 %>%  #this line tells R to start with data frame d
  # Unselected elements below will change
  #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% 
  pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="CoverPer")%>%
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  filter(Native.Non.Native=="Native")%>%
  filter(Functional.Group == "Shrub")%>%
  group_by(Plot_ID, Functional.Group)%>%
  summarize(CoverPerNatShrub=sum(CoverPer, na.rm=T))%>%
  select(Plot_ID, CoverPerNatShrub)%>%
  data.frame()

cover.natfun3 <-  sp2020.2 %>%  #this line tells R to start with data frame d
  # Unselected elements below will change
 # select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% 
  pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="CoverPer")%>%
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  filter(Native.Non.Native=="Native")%>%
  filter(Functional.Group == "Grass")%>%
  group_by(Plot_ID, Functional.Group)%>% 
  summarize(CoverPerNatGrass=sum(CoverPer, na.rm=T))%>%
  select(Plot_ID,CoverPerNatGrass)%>%
  data.frame()

all.native <- sp.cover.nat %>% 
  left_join(cover.natfun, by=c('Plot_ID'='Plot_ID'))%>%
  left_join(cover.natfun2, by=c('Plot_ID'='Plot_ID'))%>%
  left_join(cover.natfun3, by=c('Plot_ID'='Plot_ID'))

##################################################################### 
##############  NON-NATIVE SPECIES
##  Total cover per native or non-native and functional group
cover.nonnatfun <-  sp2020.2 %>%  #this line tells R to start with data frame d
  # Unselected elements below will change
  #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% 
  pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="CoverPer")%>%
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  filter(Native.Non.Native=="Non-Native")%>%
  filter(Functional.Group == "Forb")%>%
  group_by(Plot_ID, Functional.Group)%>%
  summarize(CoverPerNonNatFor=sum(CoverPer, na.rm=T))%>%
  select(Plot_ID, CoverPerNonNatFor)%>%
  data.frame()

cover.nonnatfun2 <-  sp2020.2 %>%  #this line tells R to start with data frame d
  # Unselected elements below will change
  #select(-UNKFORB,-UNKFORB_2,-UNKGRASS,-UNKGRASS_2)%>% 
  pivot_longer(SALMEL:SALAUS, names_to="SpeciesCode", values_to="CoverPer")%>%
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  filter(Native.Non.Native=="Non-Native")%>%
  filter(Functional.Group == "Grass")%>%
  group_by(Plot_ID, Functional.Group)%>%
  summarize(CoverPerNonNatGrass=sum(CoverPer, na.rm=T))%>%
  select(Plot_ID, CoverPerNonNatGrass)%>%
  data.frame()

all.non.native <- SpPerNonNat %>% 
  left_join(cover.nonnatfun, by=c('Plot_ID'='Plot_ID'))%>%
  left_join(cover.nonnatfun2, by=c('Plot_ID'='Plot_ID'))
 
covers.all <- all.native %>% 
  left_join(all.non.native, by=c('Plot_ID'='Plot_ID'))%>%
  select(-Native.Non.Native.x,-Native.Non.Native.y)%>%
  data.frame()


### Join all calculted variables
all.calculations <- d.long.div %>% 
  left_join(covers.all, by=c('Plot_ID'='Plot_ID'))
  
write.csv(all.calculations, file="Responsevariables_CSS_20.csv", row.names=FALSE,quote=TRUE)

