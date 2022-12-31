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
  mutate(Ecosystem = "Grassland") %>%
  mutate(Cover = Hits*100/102)


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
  mutate(Ecosystem = "CSS") %>%
  mutate(Cover = Hits)

# Combine the GL and CSS ecosystem data and average across subplots
veg <- (rbind(GL.long,CSS.long)) %>%
  group_by(Ecosystem,Year,Water_Treatment,Nitrogen_Treatment,TreatedWater,TreatedNitrogen,Plot_ID,Species.Code) %>%
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

# compute cover for native/non-native by plot
native.cover <- veg.species %>%
  filter(!Native.Non.Native %in% c("Stem","Unknown")) %>%
  group_by(Ecosystem,Year,Water_Treatment,Nitrogen_Treatment,TreatedWater,TreatedNitrogen,Plot_ID,Native.Non.Native) %>%
  summarize(Native.Cover = sum(Cover)) %>%
  pivot_wider(names_from = Native.Non.Native, values_from = Native.Cover)

# compute cover for functional groups by plot
functional.cover <- veg.species %>%
  filter(!Functional.Group %in% c("Stem","Unknown")) %>%
  group_by(Ecosystem,Year,Water_Treatment,Nitrogen_Treatment,TreatedWater,TreatedNitrogen,Plot_ID,Functional.Group) %>%
  summarize(Funct.Cover = sum(Cover)) %>%
  pivot_wider(names_from = Functional.Group, values_from = Funct.Cover)

# compute cover for native/non-native and functional group by plot
native.functional.cover <- veg.species %>%
  filter(!Native.Non.Native %in% c("Stem","Unknown")) %>%
  mutate(Native.Functional = interaction(Native.Non.Native,Functional.Group,sep = " ")) %>%
  group_by(Ecosystem,Year,Water_Treatment,Nitrogen_Treatment,TreatedWater,TreatedNitrogen,Plot_ID,Native.Functional) %>%
  summarize(Native.Funct.Cover = sum(Cover)) %>%
  pivot_wider(names_from = Native.Functional, values_from = Native.Funct.Cover)

# compute diversity indices by plot
veg.diversity <- veg.species %>%
  filter(!Native.Non.Native %in% c("Stem","Unknown","Litter","Bare ground")) %>%
  group_by(Ecosystem,Year,Water_Treatment,Nitrogen_Treatment,TreatedWater,TreatedNitrogen,Plot_ID) %>%
  summarize(Richness = specnumber(Cover), Shannon.diversity = diversity(Cover), Simpson.diversity = diversity(Cover, "simpson")) %>%
  mutate(Evenness = Shannon.diversity/log(Richness))

# merge all plot level metrics
veg.metrics <- native.cover %>%
  left_join(functional.cover) %>%
  left_join(native.functional.cover) %>%
  left_join(veg.diversity) %>%
  select(-`Bare ground Bare ground`,-`Litter Litter`)

# compute means and standard errors for all plot level metrics by ecosystem, year, and treatment
std.error <- function(x) sd(x)/sqrt(length(x))
veg.means <- ungroup(veg.metrics) %>%
  filter(!(Year %in% c(2015,2016,2017,2018,2019,2020) & TreatedWater == 0)) %>%
  select(-TreatedWater,-TreatedNitrogen,-Plot_ID) %>%
  group_by(Ecosystem,Year,Water_Treatment,Nitrogen_Treatment) %>%
  summarize(across(everything(),list(mean = mean, se = std.error)))

pdf("Graphics/NativeCover.pdf",width = 10,height = 6)
ggplot(veg.means, aes(x=Year, y=(Native_mean), color=Water_Treatment, 
              group = Water_Treatment, linetype = Water_Treatment, shape = Water_Treatment)) + 
  geom_errorbar(aes(ymin=(Native_mean-Native_se), ymax=(Native_mean+Native_se)), width=.1, lty=1, show.legend = F) +
  geom_line() +
  geom_point(size = 2) +
  labs(color = "Water_Treatment",
       linetype = "Water_Treatment",
       shape = "Water_Treatment",
       y = "Mean native cover (%)") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position="right", 
        legend.title = element_text(size=14),
        legend.key.width= unit(1.5, 'cm'),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(Nitrogen_Treatment~Ecosystem)
dev.off()

