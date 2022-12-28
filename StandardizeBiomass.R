###This code stardardize column names, and rearrange columns orders. 

################################################################################
rm(list=ls())
## Call your downloaded packages
library(tidyverse)

## Set working directory from Dilys' Google Drive Desktop from PC
setwd("G:/My Drive/FolderTrabajoPersonal/Loma Experiment/Biomass/CSS")

## Read a file at a time to make changes
# Biomass 2008-2009
BIO0809<- read.csv("DOE_LRS_Data_Biomass_Updated_2008_2009.csv")

# Renamed columns 
BIO0809.2 <- BIO0809 %>%
  mutate(Year = "2008")  %>%
  rename(Plot_ID = "Plot",
       Per.Sage = "X.Sage",
       Per.Forb= "X.Forb",
       Per.Grass= "X.Grass",
       Per.Litter= "X.Litter",
       Per.Bare= "X.Bare",
       Per.Live= "X.Live",
       Biomass = "Biomass..g.",
       Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO0809.2$Water_Treatment  = str_sub(BIO0809.2$Plot_ID,5,5) 
BIO0809.2$Nitrogen_Treatment = str_sub(BIO0809.2$Plot_ID,6,6) 

BIO0809.2 <- BIO0809.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
               Per.Bare,Per.Live,Biomass,Litter,
               Biomass.Area), ~replace_na(., 0))


BIO0809$PlotNumber = str_sub(BIO0809$Plot_ID,2,3) 
BIO0809$Treatment = str_sub(BIO0809$Plot_ID,4,6) 


BIO0809.2 <- BIO0809 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "WBS;SP")%>%
  rename(Frame = Location.Numb) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO0809.2,"DOE_LRS_Data_Biomass_Updated_2008_2009.csv", row.names=F)

# Biomass 2009-2010
BIO0910<- read.csv("DOE_LRS_Data_Biomass_Updated_2009_2010.csv")

BIO0910$PlotNumber = str_sub(BIO0910$Plot_ID,2,3) 
BIO0910$Treatment = str_sub(BIO0910$Plot_ID,4,6) 

BIO0910.2 <- BIO0910 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "SP;TN")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO0910.2,"DOE_LRS_Data_Biomass_Updated_2009_2010.csv", row.names=F)

# Renamed columns 
BIO0910.2 <- BIO0910 %>%
  mutate(Year = "2009")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO0910.2$Water_Treatment  = str_sub(BIO0910.2$Plot_ID,5,5) 
BIO0910.2$Nitrogen_Treatment = str_sub(BIO0910.2$Plot_ID,6,6) 

BIO0910.2 <- BIO0910.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
               Per.Bare,Per.Live,Biomass,Litter,
               Biomass.Area), ~replace_na(., 0))

write.csv(BIO0910.2,"DOE_LRS_Data_Biomass_Updated_2009_2010.csv", row.names=F)

# Biomass 2010-2011
BIO1011<- read.csv("DOE_LRS_Data_Biomass_Updated_2010_2011.csv")

BIO1011$PlotNumber = str_sub(BIO1011$Plot_ID,2,3) 
BIO1011$Treatment = str_sub(BIO1011$Plot_ID,4,6) 

BIO1011.2 <- BIO1011 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "SP;GW")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1011.2,"DOE_LRS_Data_Biomass_Updated_2010_2011.csv", row.names=F)

# Renamed columns 
BIO1011.2 <- BIO1011 %>%
  mutate(Year = "2010")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1011.2$Water_Treatment  = str_sub(BIO1011.2$Plot_ID,5,5) 
BIO1011.2$Nitrogen_Treatment = str_sub(BIO1011.2$Plot_ID,6,6) 

BIO1011.2 <- BIO1011.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
               Per.Bare,Per.LECO,Per.Live,Biomass,Litter,
               Biomass.Area), ~replace_na(., 0))


write.csv(BIO1011.2,"DOE_LRS_Data_Biomass_Updated_2010_2011.csv", row.names=F)

# Biomass 2011-2012
BIO1112<- read.csv("DOE_LRS_Data_Biomass_Updated_2011_2012.csv")

BIO1112$PlotNumber = str_sub(BIO1112$Plot_ID,2,3) 
BIO1112$Treatment = str_sub(BIO1112$Plot_ID,4,6) 

BIO1112.2 <- BIO1112 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "SP;RD")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1112.2,"DOE_LRS_Data_Biomass_Updated_2011_2012.csv", row.names=F)

# Renamed columns 
BIO1112.2 <- BIO1112 %>%
  mutate(Year = "2011")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1112.2$Water_Treatment  = str_sub(BIO1112.2$Plot_ID,5,5) 
BIO1112.2$Nitrogen_Treatment = str_sub(BIO1112.2$Plot_ID,6,6) 

BIO1112.2 <- BIO1112.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
                 Per.Bare,Per.LECO,Per.Live,Biomass,Litter,
                 Biomass.Area), ~replace_na(., 0))

write.csv(BIO1112.2,"DOE_LRS_Data_Biomass_Updated_2011_2012.csv", row.names=F)

# Biomass 2012-2013
BIO1213 <- read.csv("DOE_LRS_Data_Biomass_Updated_2012_2013.csv")

BIO1213$PlotNumber = str_sub(BIO1213$Plot_ID,2,3) 
BIO1213$Treatment = str_sub(BIO1213$Plot_ID,4,6) 

BIO1213.2 <- BIO1213 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "SP;PA")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1213.2,"DOE_LRS_Data_Biomass_Updated_2012_2013.csv", row.names=F)

# Renamed columns 
BIO1213.2 <- BIO1213 %>%
  mutate(Year = "2012")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1213.2$Water_Treatment  = str_sub(BIO1213.2$Plot_ID,5,5) 
BIO1213.2$Nitrogen_Treatment = str_sub(BIO1213.2$Plot_ID,6,6) 

BIO1213.2 <- BIO1213.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
                 Per.Bare,Per.LECO,Per.Live,Biomass,Litter,
                 Biomass.Area), ~replace_na(., 0))

write.csv(BIO1213.2,"DOE_LRS_Data_Biomass_Updated_2012_2013.csv", row.names=F)

# Biomass 2013-2014
BIO1314 <- read.csv("DOE_LRS_Data_Biomass_Updated_2013_2014.csv")

BIO1314$PlotNumber = str_sub(BIO1213$Plot_ID,2,3) 
BIO1314$Treatment = str_sub(BIO1213$Plot_ID,4,6) 

BIO1314.2 <- BIO1314 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "PA")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1314.2,"DOE_LRS_Data_Biomass_Updated_2013_2014.csv", row.names=F)

# Renamed columns 
BIO1314.2 <- BIO1314 %>%
  mutate(Year = "2013")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1314.2$Water_Treatment  = str_sub(BIO1314.2$Plot_ID,5,5) 
BIO1314.2$Nitrogen_Treatment = str_sub(BIO1314.2$Plot_ID,6,6) 

BIO1314.2 <- BIO1314.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
                 Per.Bare,Per.LECO,Per.Live,Biomass,Litter,
                 Biomass.Area), ~replace_na(., 0))

write.csv(BIO1314.2,"DOE_LRS_Data_Biomass_Updated_2013_2014.csv", row.names=F)

# Biomass 2014-2015
BIO1415 <- read.csv("DOE_LRS_Data_Biomass_Updated_2014_2015.csv")

BIO1415$PlotNumber = str_sub(BIO1415$Plot_ID,2,3) 
BIO1415$Treatment = str_sub(BIO1415$Plot_ID,4,6) 

BIO1415.2 <- BIO1415 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "SP;RB;LM;AV")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1415.2,"DOE_LRS_Data_Biomass_Updated_2014_2015.csv", row.names=F)

# Renamed columns 
BIO1415.2 <- BIO1415 %>%
  mutate(Year = "2014")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1415.2$Water_Treatment  = str_sub(BIO1415.2$Plot_ID,5,5) 
BIO1415.2$Nitrogen_Treatment = str_sub(BIO1415.2$Plot_ID,6,6) 

BIO1415.2 <- BIO1415.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
                 Per.Bare,Per.LECO,Per.Live,Biomass,Litter,
                 Biomass.Area), ~replace_na(., 0))

write.csv(BIO1415.2,"DOE_LRS_Data_Biomass_Updated_2014_2015.csv", row.names=F)

# Biomass 2015-2016
BIO1516 <- read.csv("DOE_LRS_Data_Biomass_Updated_2015_2016.csv")

BIO1516$PlotNumber = str_sub(BIO1516$Plot_ID,2,3) 
BIO1516$Treatment = str_sub(BIO1516$Plot_ID,4,6) 

BIO1516.2 <- BIO1516 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "SP")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1516.2,"DOE_LRS_Data_Biomass_Updated_2015_2016.csv", row.names=F)

# Renamed columns 
BIO1516.2 <- BIO1516 %>%
  mutate(Year = "2015")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1516.2$Water_Treatment  = str_sub(BIO1516.2$Plot_ID,5,5) 
BIO1516.2$Nitrogen_Treatment = str_sub(BIO1516.2$Plot_ID,6,6) 

BIO1516.2 <- BIO1516.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
                 Per.Bare,Per.LECO,Per.Live,Biomass,Litter,
                 Biomass.Area), ~replace_na(., 0))

write.csv(BIO1516.2,"DOE_LRS_Data_Biomass_Updated_2015_2016.csv", row.names=F)

# Biomass 2016-2017
BIO1617 <- read.csv("DOE_LRS_Data_Biomass_Updated_2016_2017.csv")

BIO1617$PlotNumber = str_sub(BIO1617$Plot_ID,2,3) 
BIO1617$Treatment = str_sub(BIO1617$Plot_ID,4,6) 

BIO1617.2 <- BIO1617 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "CW")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1617.2,"DOE_LRS_Data_Biomass_Updated_2016_2017.csv", row.names=F)

# Renamed columns 
BIO1617.2 <- BIO1617 %>%
  mutate(Year = "2016")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1617.2$Water_Treatment  = str_sub(BIO1617.2$Plot_ID,5,5) 
BIO1617.2$Nitrogen_Treatment = str_sub(BIO1617.2$Plot_ID,6,6) 

BIO1617.2 <- BIO1617.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
                 Per.Bare,Per.LECO,Per.Live,Biomass,Litter,
                 Biomass.Area), ~replace_na(., 0))

write.csv(BIO1617.2,"DOE_LRS_Data_Biomass_Updated_2016_2017.csv", row.names=F)

# Biomass 2017-2018
BIO1718 <- read.csv("DOE_LRS_Data_Biomass_Updated_2017_2018.csv")

BIO1718$PlotNumber = str_sub(BIO1718$Plot_ID,2,3) 
BIO1718$Treatment = str_sub(BIO1718$Plot_ID,4,6) 

BIO1718.2 <- BIO1718 %>%
  mutate(PlotType = "CSS")%>%
  mutate(Team = "CW")%>%
  rename(Frame = Location) %>%
  rename(LiveBiomass = Biomass) %>%
  rename(LitterMass = Litter) %>%
  rename(Date = Date.Collected) %>%
  relocate(Frame, .before = Per.Sage)%>%
  relocate(Plot_ID, .before = Frame) %>%
  relocate(Treatment, .before = Plot_ID) %>%
  relocate(PlotNumber, .before = Treatment) %>%
  relocate(PlotType, .before = PlotNumber) %>%
  relocate(Team, .before = PlotType) %>%
  relocate(Date, .before = Team) %>%
  relocate(Date.Weighed, .after = Notes) %>%
  relocate(Water_Treatment, .after = Frame) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) 

write.csv(BIO1718.2,"DOE_LRS_Data_Biomass_Updated_2017_2018.csv", row.names=F)

# Renamed columns 
BIO1718.2 <- BIO1718 %>%
  mutate(Year = "2017")  %>%
  rename(Plot_ID = "Plot",
         Per.Sage = "X.Sage",
         Per.Forb= "X.Forb",
         Per.Grass= "X.Grass",
         Per.Litter= "X.Litter",
         Per.Bare= "X.Bare",
         Per.Live= "X.Live",
         Per.LECO = "X.LECO",
         Biomass = "Biomass..g.",
         Litter = "Litter..g.")

# Create new columns for treatment by extracting letters from Plot_ID 
BIO1718.2$Water_Treatment  = str_sub(BIO1718.2$Plot_ID,5,5) 
BIO1718.2$Nitrogen_Treatment = str_sub(BIO1718.2$Plot_ID,6,6) 

BIO1718.2 <- BIO1718.2 %>%
  relocate(Year, .before = Plot_ID)%>%
  relocate(Water_Treatment, .after = Date.Weighed) %>%
  relocate(Nitrogen_Treatment, .after = Water_Treatment) %>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'A', 'Added', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'X', 'Ambient', Water_Treatment))%>%
  mutate(Water_Treatment = ifelse(Water_Treatment == 'R', 'Reduced', Water_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'X', 'Ambient', Nitrogen_Treatment))%>%
  mutate(Nitrogen_Treatment = ifelse(Nitrogen_Treatment == 'N', 'Added', Nitrogen_Treatment))%>%
  mutate_at(vars(Per.Sage, Per.Forb, Per.Grass,Per.Litter,
                 Biomass.Area), ~replace_na(., 0))

write.csv(BIO1718.2,"DOE_LRS_Data_Biomass_Updated_2017_2018.csv", row.names=F)
