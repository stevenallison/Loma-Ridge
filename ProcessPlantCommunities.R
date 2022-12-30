## Load packages
library(googledrive)
library(tidyverse)
library(vegan)

# Read in data from Google Drive; will be asked to authorize access
# Need to specify shared drive to avoid accessing local duplicates
# Some csv files are in standard UTF-8 but others are Windows encoded
GL.2009 <- drive_get("DOE_LRG_Updated_SppComp_2009.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

GL.2010 <- drive_get("DOE_LRG_Updated_SppComp_2010.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2011 <- drive_get("DOE_LRG_Updated_SppComp_2011.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2012 <- drive_get("DOE_LRG_Updated_SppComp_2012.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2013 <- drive_get("DOE_LRG_Updated_SppComp_2013.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2014 <- drive_get("DOE_LRG_Updated_SppComp_2014.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2015 <- drive_get("DOE_LRG_Updated_SppComp_2015.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2016 <- drive_get("DOE_LRG_Updated_SppComp_2016.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

# Missing 2017 dataset

GL.2018 <- drive_get("DOE_LRG_Updated_SppComp_2018.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2019 <- drive_get("DOE_LRG_Updated_SppComp_2019.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2020 <- drive_get("DOE_LRG_Updated_SppComp_2020.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

GL.2021 <- drive_get("DOE_LRG_Updated_SppComp_2021.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 


GL.2009.long <- GL.2009 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2010.long <- GL.2010 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2011.long <- GL.2011 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2012.long <- GL.2012 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2013.long <- GL.2013 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2014.long <- GL.2014 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2015.long <- GL.2015 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2016.long <- GL.2016 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2018.long <- GL.2018 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2019.long <- GL.2019 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2020.long <- GL.2020 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.2021.long <- GL.2021 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

GL.long <- rbind(GL.2009.long,GL.2010.long,GL.2011.long,GL.2012.long,GL.2013.long,GL.2014.long,
                 GL.2015.long,GL.2016.long,GL.2018.long,GL.2019.long,GL.2020.long,GL.2021.long) %>%
  filter(Subplot=="P")


CSS.2009 <- drive_get("DOE_LRS_Updated_SppComp_2009.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

CSS.2010 <- drive_get("DOE_LRS_Updated_SppComp_2010.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

CSS.2011 <- drive_get("DOE_LRS_Updated_SppComp_2011.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

CSS.2012 <- drive_get("DOE_LRS_Updated_SppComp_2012.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

CSS.2013 <- drive_get("DOE_LRS_Updated_SppComp_2013.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

CSS.2014 <- drive_get("DOE_LRS_Updated_SppComp_2014.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

CSS.2015 <- drive_get("DOE_LRS_Updated_SppComp_2015.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

CSS.2018 <- drive_get("DOE_LRS_Updated_SppComp_2018.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)


CSS.2009.long <- CSS.2009 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.2010.long <- CSS.2010 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.2011.long <- CSS.2011 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.2012.long <- CSS.2012 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.2013.long <- CSS.2013 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.2014.long <- CSS.2014 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.2015.long <- CSS.2015 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.2018.long <- CSS.2018 %>%
  pivot_longer(names_to = "Species.Code", values_to = "Hits",
               cols = !c(Year,Plot_ID,Subplot,Water_Treatment,TreatedWater,Nitrogen_Treatment,TreatedNitrogen))

CSS.long <- rbind(CSS.2009.long,CSS.2010.long,CSS.2011.long,CSS.2012.long,CSS.2013.long,CSS.2014.long,
                 CSS.2015.long,CSS.2018.long) %>%
  filter(Subplot=="P")

# Input species list
Species.list <- drive_get("SpeciesListLoma.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

# When adding data from a new year, need to check the species list against the codes in the data
# setdiff() returns species codes in the new data that are not in the existing list
# Add codes and info to SpeciesListLoma.csv as needed
Updated.species <- levels(factor(rbind(GL.long,CSS.long)$Species.Code))
setdiff(Updated.species,Species.list$Species.Code)
