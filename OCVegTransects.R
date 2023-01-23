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
data <- drive_get("OC_TX_To2022_ReadyToAnalyze_Nov1.csv", shared_drive = "Microbes and Global Change") %>%
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
#calculate means and SE for metrics
data.sum <- data.percent.cover %>% group_by (Year, Veg_Comm) %>% 
  summarise(meanNative = mean(Native), 
            seNative = sd(Native)/sqrt(length(Native)), 
            meanNativeGrass = mean(NativeGrass), 
            seNativeGrass = sd(NativeGrass)/sqrt(length(NativeGrass)), 
            meanNativeShrub = mean(NativeShrub), 
            seNativeShrub = sd(NativeShrub)/sqrt(length(NativeShrub)), 
            meanLowNNGrass = 1-(mean(NonNativeGrass)), 
            seNonNativeGrass = sd(NonNativeGrass)/sqrt(length(NonNativeGrass)), 
            meanLowNNForb = 1-(mean(NonNativeForb)), 
            seNonNativeForb = sd(NonNativeForb)/sqrt(length(NonNativeForb)),
            meanNativeSpRich = mean(NatSpRich), 
            seNativeSpRich = sd(NatSpRich)/sqrt(length(NatSpRich)),
            meanMinFireReturn = mean(Min.Fire.Return.Interval.Prior.to.Sample.Year), 
            seMinFireReturn = sd(Min.Fire.Return.Interval.Prior.to.Sample.Year)/sqrt(length(Min.Fire.Return.Interval.Prior.to.Sample.Year)),
            meanFireFrequency = mean(Fire.Frequency._Total.Fires.since.1914.Prior.to.Sample.Year), 
            seFireFrequency = sd(Fire.Frequency._Total.Fires.since.1914.Prior.to.Sample.Year)/sqrt(length(Fire.Frequency._Total.Fires.since.1914.Prior.to.Sample.Year)),
            meanADEFAS = mean(ADEFAS), 
            seADEFAS = sd(ADEFAS)/sqrt(length(ADEFAS)),
            meanCrust = mean(C), 
            seCrust = sd(C)/sqrt(length(C)),
            meanBare = mean(B), 
            seBare = sd(B)/sqrt(length(B)),
  ) %>% ungroup()

#subsetting css and gl for ltreb proposal

ltreb.sum <- data.sum %>% filter(Veg_Comm != "CHAP")

# LTREB Plots
#replace meanNative and seNative with variables from ltreb.sum

ltreb.sum %>% 
  ggplot(aes(x = Year, 
             y = meanNative), group=Veg_Comm) +
  geom_line(linetype="dashed", color='#00BA38') +
  geom_point(size=2, color='#00BA38') +
  facet_grid(.~Veg_Comm) +
  geom_errorbar(aes(ymin = meanNative-seNative, 
                    ymax = meanNative+seNative),
                width = .1, color='#00BA38') +
  labs(x = "Year",
       y = "Combined Cover of All Native Veg") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))