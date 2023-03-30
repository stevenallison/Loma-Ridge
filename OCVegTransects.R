#Compiling OC Veg Data
#Sarah Kimball/Steve Allison

#load necessary packages
library(tidyverse)
library(googledrive)

# input species list
meta.species <- drive_get("SpeciesListOCVeg.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>% 
  select(Code, Nativity)

#input the % cover data
data <- drive_get("OC_TX_To2022_ReadyToAnalyze_Feb3.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>% 
  filter(Passive_Restoration == "N") #filter out passive restoration transects prior to analysis

#multiple values by 2 to go from pts intercepted to percent cover
data.percent.cover <- data %>% mutate(across(ACMAME:UnkForb,~.x*2))

data.percent.cover.sprich <- data.percent.cover %>% 
  pivot_longer(ACMAME:VULMYU, names_to = "Code", values_to = "Count") %>% 
  mutate(PresAbs = case_when(Count >0 ~ 1, Count == 0 ~ 0)) %>% 
  inner_join(meta.species) %>%
  group_by(Tn_Yr, Nativity) %>%
  summarize(NatSpRich = sum(PresAbs)) %>% filter(Nativity == "Native")

data.percent.cover <- data.percent.cover %>% 
  full_join(data.percent.cover.sprich)
names(data.percent.cover)

# function to compute standard error
std.error <- function(x,na.rm=T) sd(x,na.rm)/sqrt(sum(!is.na(x)))

#calculate means and SE for metrics
data.sum <- data.percent.cover %>%
  group_by (Year, Veg_Comm) %>% 
  summarize(across(c(Native,NativeGrass,NativeShrub,NonNativeGrass,NonNativeForb,NatSpRich,
                     Min.Fire.Return.Interval.Prior.to.Sample.Year,
                     Fire.Frequency._Total.Fires.since.1914.Prior.to.Sample.Year,
                     ADEFAS,C,B),list(mean = mean, se = std.error))) %>%
  mutate(LowNNGrass_mean = 1-NonNativeGrass_mean) %>%
  mutate(LowNNForb_mean = 1-NonNativeForb_mean) %>%
  ungroup()

#subsetting css and gl for ltreb proposal
ltreb.sum <- data.sum %>% filter(Veg_Comm != "CHAP", Year != 2007) %>%
  mutate(Veg_Comm = str_replace(Veg_Comm,"GL","Grassland"))

# LTREB Plots
png("Graphics/Transects.png",width = 8,height = 4,units = "in",res=300)
ggplot(ltreb.sum, aes(x = Year, y = NativeShrub_mean, group=Veg_Comm, color=Veg_Comm, shape=Veg_Comm)) + 
  geom_errorbar(aes(ymin = NativeShrub_mean-NativeShrub_se, ymax = NativeShrub_mean+NativeShrub_se),width = .1) +
  geom_line() +
  geom_point(size = 2) +
   labs(x = "Year",
       y = "Native shrub cover (%)") +
  scale_color_manual(values=c('coral1','black')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position=c(0.3,0.85), 
        legend.title = element_blank(),
        legend.key.width= unit(1.5, 'cm'),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dev.off()
