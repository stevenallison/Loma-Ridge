## Load packages
library(googledrive)
library(tidyverse)
library(readxl)

# Read in data from Google Drive (will be asked to authorize access)
# Need to specify shared drive to avoid accessing local duplicates
# Most csv files are in standard UTF-8 but others may be Windows encoded

#####################################
# Species composition data processing
#####################################

# Retrieve raw data from shared Google Drive: grassland
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

GL.2022 <- drive_get("DOE_LRG_Updated_SppComp_2022.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

# Retrieve raw data from shared Google Drive: CSS
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

drive_get("2020 Loma CSS Species Comp_QC.xlsx", shared_drive = "Microbes and Global Change") %>%
  drive_download(path = 'CSS.2020.xlsx', overwrite = TRUE)

CSS.2020 <- read_excel("CSS.2020.xlsx",sheet="Species Comp Data") %>%
  slice(-1) %>%
  select(Plot_ID,Subplot=`Plot Type`,Species.Code=Code,Cover=`Pecent Cover`) %>%
  mutate(Year = 2020) %>%
  mutate(Plot_ID = str_remove_all(Plot_ID,"_")) %>%
  mutate(Cover = as.numeric(str_remove_all(Cover,"<")))

drive_get("2021 Loma CSS Species Comp_QC.xlsx", shared_drive = "Microbes and Global Change") %>%
  drive_download(path = 'CSS.2021.xlsx', overwrite = TRUE)

CSS.2021 <- read_excel("CSS.2021.xlsx",sheet="Species Comp Data") %>%
  slice(-1) %>%
  select(Plot_ID,Subplot=`Plot Type`,Species.Code=Code,Cover=`Pecent Cover`) %>%
  mutate(Year = 2021) %>%
  mutate(Plot_ID = str_remove_all(Plot_ID,"_")) %>%
  mutate(Cover = as.numeric(str_remove_all(Cover,"<")))

CSS.2022 <- drive_get("DOE_LRS_Updated_SppComp_2022.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

# Reformat data to long table: grassland
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

GL.2022.long <- GL.2022 %>%
  select(Plot_ID,Species.Code=Code,ground.cover) %>%
  mutate(Species.Code = case_when(Species.Code == "0" ~ ground.cover, TRUE ~ Species.Code)) %>% # no-hit recorded as zero; convert to ground cover type
  select(-ground.cover) %>%
  group_by(Plot_ID,Species.Code) %>%
  summarize(Hits=n()) %>% 
  mutate(Year=2022,Subplot="P",Water_Treatment=NA,TreatedWater=NA,Nitrogen_Treatment=NA,TreatedNitrogen=NA) # add variables to be compatible with prior years

# Merge grassland datasets across years
GL.long <- rbind(GL.2009.long,GL.2010.long,GL.2011.long,GL.2012.long,GL.2013.long,GL.2014.long,
                 GL.2015.long,GL.2016.long,GL.2018.long,GL.2019.long,GL.2020.long,GL.2021.long,
                 GL.2022.long) %>%
  mutate(Cover = Hits*100/102) # Adjust cover to total number of point intercepts


# Reformat data to long table: CSS
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

CSS.2022.long <- CSS.2022 %>%
  select(Plot_ID,Species.Code=Code,Hits=Percent.Cover) %>%
  mutate(Year=2022,Subplot="P",Water_Treatment=NA,TreatedWater=NA,Nitrogen_Treatment=NA,TreatedNitrogen=NA) # add variables to be compatible with prior years

# Merge CSS datasets across years
CSS.long <- rbind(CSS.2009.long,CSS.2010.long,CSS.2011.long,CSS.2012.long,CSS.2013.long,CSS.2014.long,
                 CSS.2015.long,CSS.2018.long,CSS.2022.long) %>%
  mutate(Cover = Hits)


# Input key to plot IDs
PlotTreatments <- drive_get("PlotTreatments.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

# Different IDs used in first year of experiment
PlotTreatments2007 <- drive_get("PlotTreatments2007.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) 

# Combine the GL and CSS vegetation data
# Add plot information and average across subplots
veg <- (rbind(GL.long,CSS.long)) %>%
  select(Year,Plot_ID,Subplot,Species.Code,Cover) %>%
  rbind(CSS.2020, CSS.2021) %>%
  mutate(Plot_ID = str_replace(Plot_ID,"GL","G")) %>%
  left_join(PlotTreatments) %>%
  group_by(Ecosystem,Year,Water,Nitrogen,Treated_2015_2020,Plot_ID,Species.Code) %>%
  summarize(Cover = mean(Cover))

# Input species list
Species.list <- drive_get("SpeciesListLoma.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.)

# When adding data from a new year, need to check the species list against the codes in the data
# setdiff() returns species codes in the new data that are not in the existing list
# Add codes and info to SpeciesListLoma.csv as needed
Updated.species <- levels(factor(veg$Species.Code))
setdiff(Updated.species,Species.list$Species.Code)

# merge cover data with species attributes
veg.species <- veg %>%
  left_join(Species.list)

write_csv(veg.species,"veg.communities.csv")

#####################################
# Biomass data processing
#####################################

# Retrieve raw data by year from shared Google Drive
GL.Biomass.2007 <- drive_get("DOE_GL_Data_Biomass_Updated_2006_2007.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Biomass,Area=Area.m2) %>%
  mutate(Year=2007,Frame=1,Per.Grass=NA,Per.Forb=NA,Per.Bare=NA,Per.Litter=NA,Per.Live=NA,LitterMass=NA) %>%
  mutate(Plot_ID = str_replace(Plot_ID,"GR([1-9])([LR])","G0\\1\\2")) %>%
  mutate(Plot_ID = str_replace(Plot_ID,"GR","G"))

GL.Biomass.2008 <- drive_get("DOE_GL_Data_Biomass_Updated_2007_2008.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame=Sample,Per.Grass,Per.Forb,Per.Bare,Biomass,Area=Area.m2) %>%
  mutate(Year=2008,Per.Litter=NA,Per.Live=NA,LitterMass=NA)

GL.Biomass.2009 <- drive_get("DOE_GL_Data_Biomass_Updated_2008_2009.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Biomass=BiomassSum,LitterMass,Area=Area.m2) %>%
  mutate(Year=2009,Per.Litter=NA,Per.Live=NA)

GL.Biomass.2010 <- drive_get("DOE_GL_Data_Biomass_Updated_2009_2010.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb=Per.Forb.Other,Per.Bare,Biomass=BiomassSum,LitterMass,Area=Area.m2) %>%
  mutate(Year=2010,Per.Litter=NA,Per.Live=NA)

GL.Biomass.2011 <- drive_get("DOE_GL_Data_Biomass_Updated_2010_2011.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2011)

GL.Biomass.2012 <- drive_get("DOE_GL_Data_Biomass_Updated_2011_2012.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2012)

GL.Biomass.2013 <- drive_get("DOE_GL_Data_Biomass_Updated_2012_2013.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID=Plot,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2013)

GL.Biomass.2014 <- drive_get("DOE_GL_Data_Biomass_Updated_2013_2014.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2014)

GL.Biomass.2015 <- drive_get("DOE_GL_Data_Biomass_Updated_2014_2015.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2015)

GL.Biomass.2016 <- drive_get("DOE_GL_Data_Biomass_Updated_2015_2016.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2016)

GL.Biomass.2017 <- drive_get("DOE_GL_Data_Biomass_Updated_2016_2017.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2017) %>%
  filter(Frame==1)

GL.Biomass.2018 <- drive_get("DOE_GL_Data_Biomass_Updated_2017_2018.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2018) %>%
  filter(Frame==1)

GL.Biomass.2019 <- drive_get("DOE_GL_Data_Biomass_Updated_2018_2019.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2019) %>%
  filter(Frame %in% c(1,2))

GL.Biomass.2020 <- drive_get("DOE_GL_Data_Biomass_Updated_2019_2020.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID=PlotID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2020,Per.Live=NA)

GL.Biomass.2021 <- drive_get("DOE_GL_Data_Biomass_Updated_2020_2021.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID=Plot.ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2021,Per.Live=NA) %>%
  mutate(LitterMass = str_replace_na(LitterMass,0))

GL.Biomass.2022 <- drive_get("DOE_GL_Data_Biomass_Updated_2021_2022.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID=Plot.ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Biomass,LitterMass=Litter.mass..g.,Area=Area.m2) %>%
  filter(!is.na(Biomass)) %>%
  mutate(Year=2022,Per.Live=NA) %>%
  mutate(LitterMass = str_replace_na(LitterMass,0))


CSS.Biomass.2009 <- drive_get("DOE_LRS_Data_Biomass_Updated_2008_2009.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2009)

CSS.Biomass.2010 <- drive_get("DOE_LRS_Data_Biomass_Updated_2009_2010.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2010)

CSS.Biomass.2011 <- drive_get("DOE_LRS_Data_Biomass_Updated_2010_2011.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2011)

CSS.Biomass.2012 <- drive_get("DOE_LRS_Data_Biomass_Updated_2011_2012.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2012)

CSS.Biomass.2013 <- drive_get("DOE_LRS_Data_Biomass_Updated_2012_2013.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2013)

CSS.Biomass.2014 <- drive_get("DOE_LRS_Data_Biomass_Updated_2013_2014.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2014)

CSS.Biomass.2015 <- drive_get("DOE_LRS_Data_Biomass_Updated_2014_2015.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2015)

CSS.Biomass.2016 <- drive_get("DOE_LRS_Data_Biomass_Updated_2015_2016.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Year=2016)

CSS.Biomass.2017 <- drive_get("DOE_LRS_Data_Biomass_Updated_2016_2017.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  filter(Frame==1) %>%
  mutate(Year=2017)

CSS.Biomass.2018 <- drive_get("DOE_LRS_Data_Biomass_Updated_2017_2018.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,Biomass,LitterMass,Area=Area.m2) %>%
  filter(Frame==1) %>%
  mutate(Year=2018)

# Missing 2019

CSS.Biomass.2020 <- drive_get("DOE_LRS_Data_Biomass_Updated_2019_2020.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Per.Live=NA,Year=2020) %>%
  mutate(Biomass = as.numeric(str_replace(Biomass,"missing","0"))) # change "missing" into 0

CSS.Biomass.2021 <- drive_get("DOE_LRS_Data_Biomass_Updated_2020_2021.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID=Plot.ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Biomass,LitterMass,Area=Area.m2) %>%
  mutate(Per.Live=NA,Year=2021) %>%
  mutate(LitterMass = str_replace_na(LitterMass,0))

CSS.Biomass.2022 <- drive_get("DOE_LRS_Data_Biomass_Updated_2021_2022.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(Plot_ID=Plot.ID,Frame,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Biomass,LitterMass=Litter.mass..g.,Area=Area.m2) %>%
  mutate(Per.Live=NA,Year=2022) %>%
  filter(!is.na(Biomass)) %>%
  mutate(LitterMass = str_replace_na(LitterMass,0))

# Merge biomass data across years
GL.Biomass <- rbind(GL.Biomass.2007,GL.Biomass.2008,GL.Biomass.2009,GL.Biomass.2010,GL.Biomass.2011,GL.Biomass.2012,
                    GL.Biomass.2013,GL.Biomass.2014,GL.Biomass.2015,GL.Biomass.2016,GL.Biomass.2017,GL.Biomass.2018,
                    GL.Biomass.2019,GL.Biomass.2020,GL.Biomass.2021)

CSS.Biomass <- rbind(CSS.Biomass.2009,CSS.Biomass.2010,CSS.Biomass.2011,CSS.Biomass.2012,CSS.Biomass.2013,CSS.Biomass.2014,
                     CSS.Biomass.2015,CSS.Biomass.2016,CSS.Biomass.2017,CSS.Biomass.2018,CSS.Biomass.2020,CSS.Biomass.2021)

# Combine grassland and CSS datasets, merge with plot data, and tidy dataframe
Biomass <- rbind(GL.Biomass,CSS.Biomass) %>%
  mutate(across(everything(),~str_remove(.,"<"))) %>%
  mutate(across(c(Year,Biomass,Area,Per.Grass,Per.Forb,Per.Bare,Per.Litter,Per.Live,LitterMass),as.numeric)) %>%
  mutate(Biomass.per.area = Biomass/Area) %>%
  mutate(Litter.per.area = LitterMass/Area) %>%
  left_join(rbind(PlotTreatments,PlotTreatments2007)) # Some plot IDs appear to have been different in 2007

write_csv(Biomass,"veg.biomass.csv")


#####################################
# Notes
#####################################

## Resolved issues ##
# G11RRX mislabeled as G11RXX in 2020, 2021
# Plot ID should start with S for CSS/Shrubland and G for Grassland (not CSS or GL as in 2020-21)
# S48LXX mislabeled as S48LAX in 2021
# 2011 CSS cover data are 1/3 of previous analyses
# 2009, 2010 CSS cover data are 2/3 of previous analyses
# Need to locate DOE_LRS_Data_SpeciesComp_Entered_Updated2018.csv
# And determine if different from https://docs.google.com/spreadsheets/d/1YuONxw0scacz-sgwCo4IQBOJWGKtDhlY

## Unresolved issues ##
# 2012 cover data match previous datasets but somewhat higher than Kimball et al 2014
# 2020 CSS data is missing S48RXN
# Need the 2022 data
# Looks like starting in 2020, CSS data collection involved a separate ground cover estimation that adds up to 100%
# Suggest to not use "<" symbols in numeric data columns
# Suggest not to leave zero values as blank, "missing", or NA; be consistent
