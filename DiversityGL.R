## This code helps species richness and species diversity
## for the historical grassland dataset
rm(list=objects()) 
#If working with Google Drive
setwd("G:/Shared drives/Microbes and Global Change/Loma Ridge/Experiments/Ecosystem manipulation/Data/Plant_Species_Composition/UpdatedFilesGL")

#If working locally
setwd("C:/Users/Dilys Vela/Documents/UCI/MicrobiomesClimateChange/LTREB/codeR/UpdatedFilesGL/UpdatedFilesGL")


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

sp2009 <- read_csv("DOE_LRG_Updated_SppComp_2009.csv")

head(sp2009)

sp2009 <- sp2009 %>%
  mutate(PlotNum=substr(Plot_ID, 1, 3) )%>%
filter(Subplot=="P")

## Frequency tables to see how many rows/sampling units are present for  treatments
ftable(sp2009[,c("Year", "Water_Treatment", "Nitrogen_Treatment")]) 

## Call species list
setwd("G:/Shared drives/Microbes and Global Change/Loma Ridge/Experiments/Ecosystem manipulation/Data/Plant_Species_Composition/SpeciesListCodes")

#sp <- read_csv("Spp_List_GL_2020_2022.csv")
sp <- read_csv("Spp_List_Update_GL_2009_2019.csv")

sp.2 <-sp %>%
select(NewSpeciesCode,Native.Non.Native, Functional.Group)%>%
  rename(SpeciesCode = NewSpeciesCode)

## Summarize species hits/abundance per treatment and species
## here hits that are separated in permantnet plot plut seed add
## are add up
d.long<-sp2009%>%
  # Unselected elements below will change
  select(-UNKGRASS, -UNKSHRUB,-UNKFORB)%>% #remove those codes for unknown
  #make sure that there is a match for species name in the lien below
  pivot_longer(AMSMEN:FESMYU, names_to="SpeciesCode", values_to="Abundance")%>%
  # NEW CODE: group_by tells R which columns to summarize by
  group_by(Plot_ID, Water_Treatment, Nitrogen_Treatment, SpeciesCode)%>%
  # NEW CODE: with summarize you specify the new column name and how to summarize the data for that column
  summarize(SpHitsAbun=sum(Abundance, na.rm=T))%>% # here all the hits separated in P and S become one.
  mutate(Year=2009)%>%
  data.frame()
  
#d.response<-d.long%>%
  
##  Total cover per native or non-native and functional group
cover.natfun<-d.long %>%
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  filter(Native.Non.Native=="Native")%>%
  #mutate(Functional.Group = fct_recode(Functional.Group, 
  #                                   "forb"="Forb",
  #                                "perennial.forb"="perennial forb"))%>%
  #filter(Functional.Group == "forb" | Functional.Group == "perennial.forb")%>%
  group_by(Plot_ID, Functional.Group)%>%
  summarize(group_abund=sum(SpHitsAbun, na.rm=T))%>%
  pivot_wider(names_from = Functional.Group, values_from = group_abund)%>%
  # 102 represents the number of points intersect used to get hit numbers
  # 100 to convert to percentage
  # mutate(CoverPerNatFor = forb/102 * 100)%>% 
  #mutate(CoverPerNatPerFor = perennial.forb/102 * 100)%>%
  #mutate(CoverPerNatGrass = grass/102 * 100)%>%
  mutate(CoverPerNatShrub = shrub/102 * 100)%>%
  #rename(ForbhitsNat = forb, PennForbhitsNat = perennial.forb 
  #GrashitsNat = grass,
  #ShrshitsNat = shrub
  #      ) %>%
  mutate(Year=2009)%>%
  data.frame()

setwd("C:/Users/Dilys Vela/Documents/UCI/MicrobiomesClimateChange/LTREB/codeR")
write.csv(cover.natfun, "shurb_GL_2012.csv", row.names=F)
#############################

##  Total cover per native or non-native
sp.cover.nat <-  d.long %>%  #this line tells R to start with data frame d
  #pivot_wider(names_from = SpeciesCode, values_from = SpHitsAbun)
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  group_by(Plot_ID,Native.Non.Native)%>%
  summarize(group_abund=sum(SpHitsAbun, na.rm=T))%>%
  pivot_wider(names_from = Native.Non.Native, values_from = group_abund)%>%
  rename(NonNative = "Non-Native")%>%
  mutate(CoverPerNat = Native/102 * 100)%>%
  mutate(CoverPerNonNat = NonNative/102 * 100)%>%
  rename(NatHits = Native, NonnatHits = NonNative) %>%
  data.frame()

 
cover.nonnatfun<-d.long %>%
  left_join(sp.2, by=c('SpeciesCode'='SpeciesCode'))%>% # add species attributes
  filter(Native.Non.Native=="Non-Native")%>%
  group_by(Plot_ID, Functional.Group)%>%
  summarize(group_abund=sum(SpHitsAbun, na.rm=T))%>%
  pivot_wider(names_from = Functional.Group, values_from = group_abund)%>%
  # 102 represents the number of points intersect used to get hit numbers
  # 100 to convert to percentage
  mutate(CoverPerNonNatFor = forb/102 * 100)%>% 
  mutate(CoverPerNonNatGrass = grass/102 * 100)%>%
  rename(ForbhitsNonnat = forb, GrashitsNonnat = grass) %>%
  data.frame()

## calculating diversity
## Note the '.' is telling R to use all the columns in the working data frame for the calculation
sp.div.ID<-  d.long %>%  #this line tells R to start with data frame d
  pivot_wider(names_from = SpeciesCode, values_from = SpHitsAbun)%>%
  select(Plot_ID, Year, Water_Treatment, Nitrogen_Treatment)
  
all.sp.div<-  d.long %>%  
  pivot_wider(names_from = SpeciesCode, values_from = SpHitsAbun)%>%
  #make sure the spcodenames below matches the first and last spp name in the dataframe all.sp.div
  select(AMSMEN:VICVIL) %>% #now just select the species data columns
  mutate(spec.rich=specnumber(.)) %>%
  mutate(H.div=diversity(.))%>% #create a new data column for the calculation of Shannon's Diversity
  #create a new data column for species richness
  mutate(Even=H.div/log(spec.rich)) %>% #create a new data column for species evenness
  mutate(S.div=diversity(., "inv")) %>% #create a new data column for Simpson's diversity - D2
  select(H.div,spec.rich,Even,S.div)%>% #selects just the new diversity indices
  bind_cols(.,select(sp.div.ID,Plot_ID, Year, Water_Treatment, Nitrogen_Treatment))%>%  #This brings back in the plot.key column that was dropped earlier
  relocate(Plot_ID, Year, Water_Treatment, Nitrogen_Treatment, .before = H.div) %>%
  data.frame()

#### Bind all the dataframes

all.frame <- bind_cols(all.sp.div,sp.cover.nat, cover.natfun, cover.nonnatfun) %>%
          # You may need to change the names below 
          select(-Plot_ID...9,-Plot_ID...14,-Plot_ID...19)%>%
          rename(Plot_ID = Plot_ID...1) %>%
          data.frame()

setwd("G:/My Drive/FolderTrabajoPersonal/LTREB/SummaryData")
write.csv(all.frame, "Responsevariables_GL_2021.csv", row.names=F)
