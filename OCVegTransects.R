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
ltreb.sum <- data.sum %>% filter(Veg_Comm != "CHAP")

# LTREB Plots
#replace meanNative and seNative with variables from ltreb.sum

ltreb.sum %>% 
  ggplot(aes(x = Year, 
             y = Native_mean), group=Veg_Comm) +
  geom_line(linetype="dashed", color='#00BA38') +
  geom_point(size=2, color='#00BA38') +
  facet_grid(.~Veg_Comm) +
  geom_errorbar(aes(ymin = Native_mean-Native_se, 
                    ymax = Native_mean+Native_se),
                width = .1, color='#00BA38') +
  labs(x = "Year",
       y = "Combined Cover of All Native Veg") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
