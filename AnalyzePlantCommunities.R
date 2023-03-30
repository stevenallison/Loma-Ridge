
library(googledrive)
library(tidyverse)
library(vegan)
library(gridExtra)

Annual.precip <- drive_get("AnnualPrecipLoma.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  rename(Year=WaterYear) %>%
  pivot_longer(cols = !Year, names_to = "Water", values_to = "Water.input")

veg.species <- read.csv("veg.communities.csv")

# compute cover for native/non-native by plot
native.cover <- veg.species %>%
  filter(!Native.Non.Native %in% c("Stem","Unknown")) %>%
  group_by(Ecosystem,Year,Water,Nitrogen,Treated_2015_2020,Plot_ID,Native.Non.Native) %>%
  summarize(Native.Cover = sum(Cover)) %>%
  pivot_wider(names_from = Native.Non.Native, values_from = Native.Cover, values_fill = 0) %>%
  mutate(Native.Rel.Ab = Native/(Native+`Non-Native`))

# compute cover for functional groups by plot
functional.cover <- veg.species %>%
  filter(!Functional.Group %in% c("Stem","Unknown")) %>%
  group_by(Ecosystem,Year,Water,Nitrogen,Treated_2015_2020,Plot_ID,Functional.Group) %>%
  summarize(Funct.Cover = sum(Cover)) %>%
  pivot_wider(names_from = Functional.Group, values_from = Funct.Cover, values_fill = 0)

# compute cover for native/non-native and functional group by plot
native.functional.cover <- veg.species %>%
  filter(!Native.Non.Native %in% c("Stem","Unknown")) %>%
  mutate(Native.Functional = interaction(Native.Non.Native,Functional.Group,sep = " ")) %>%
  group_by(Ecosystem,Year,Water,Nitrogen,Treated_2015_2020,Plot_ID,Native.Functional) %>%
  summarize(Native.Funct.Cover = sum(Cover)) %>%
  pivot_wider(names_from = Native.Functional, values_from = Native.Funct.Cover, values_fill = 0)

# compute diversity indices by plot
veg.diversity <- veg.species %>%
  filter(!Native.Non.Native %in% c("Stem","Unknown","Litter","Bare ground")) %>%
  group_by(Ecosystem,Year,Water,Nitrogen,Treated_2015_2020,Plot_ID) %>%
  summarize(Richness = specnumber(Cover), Shannon.diversity = diversity(Cover), Simpson.diversity = diversity(Cover, "simpson")) %>%
  mutate(Evenness = Shannon.diversity/log(Richness))

# merge all plot level metrics
veg.metrics <- native.cover %>%
  left_join(functional.cover) %>%
  left_join(native.functional.cover) %>%
  left_join(veg.diversity) %>%
  select(-`Bare ground Bare ground`,-`Litter Litter`)

# compute means and standard errors for all plot level metrics by ecosystem, year, and treatment
std.error <- function(x,na.rm=T) sd(x,na.rm)/sqrt(sum(!is.na(x)))
veg.means <- ungroup(veg.metrics) %>%
  filter(!(Year %in% c(2015,2016,2017,2018,2019,2020,2021) & Treated_2015_2020 == 0)) %>%
  select(-Treated_2015_2020,-Plot_ID) %>%
  group_by(Ecosystem,Year,Water,Nitrogen) %>%
  summarize(across(everything(),list(mean = mean, se = std.error),na.rm=T)) %>%
  left_join(Annual.precip)

# read in biomass dataset
Biomass <- read.csv("veg.biomass.csv")

# compute biomass means and merge with precip inputs
Biomass.means <- Biomass %>%
  filter(!(Year %in% c(2015,2016,2017,2018,2019,2020,2021) & Treated_2015_2020 == 0)) %>%
  select(-Treated_2015_2020,-Frame) %>%
  group_by(Ecosystem,Year,Water,Nitrogen,Plot_ID) %>%
  summarize(across(everything(),mean,na.rm=T)) %>%
  select(-Plot_ID,-LitterMass,-Biomass,-Area) %>%
  summarize(across(everything(),list(mean = mean, se = std.error),na.rm=T)) %>%
  left_join(Annual.precip)

biomass.water.model <-
nls(Biomass.per.area_mean~a*Water.input/(b+Water.input),
    filter(Biomass.means,Ecosystem=="Grassland" & Nitrogen=="Ambient"),start=list(a=500,b=300))

# Plot native cover in CSS
png("Graphics/NativeCover.png",width = 8,height = 6,units = "in",res=300)
ggplot(veg.means, aes(x=Year, y=(Native_mean), color=Water, 
                      group = Water, linetype = Water, shape = Water)) + 
  geom_errorbar(aes(ymin=(Native_mean-Native_se), ymax=(Native_mean+Native_se)), width=.1, lty=1, show.legend = F) +
  geom_line() +
  geom_point(size = 2) +
  labs(color = "Water",
       linetype = "Water",
       shape = "Water",
       y = "Native cover (%)") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position=c(0.15,0.35), 
        legend.title = element_text(size=14),
        legend.key.width= unit(1.5, 'cm'),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(Ecosystem~Nitrogen)
dev.off()

# Plot native relative abundance
png("Graphics/NativeRelAb.png",width = 8,height = 6,units = "in",res=300)
ggplot(veg.means, aes(x=Year, y=(Native.Rel.Ab_mean), color=Water, 
                      group = Water, linetype = Water, shape = Water)) + 
  geom_errorbar(aes(ymin=(Native.Rel.Ab_mean-Native.Rel.Ab_se), ymax=(Native.Rel.Ab_mean+Native.Rel.Ab_se)), width=.1, lty=1, show.legend = F) +
  geom_line() +
  geom_point(size = 2) +
  labs(color = "Water",
       linetype = "Water",
       shape = "Water",
       y = "Native relative abundance") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position=c(0.25,0.65), 
        legend.title = element_text(size=14),
        legend.key.width= unit(1.5, 'cm'),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(Ecosystem~Nitrogen)
dev.off()

# Plot native shrub cover in CSS
png("Graphics/NativeShrubCover.png",width = 8,height = 4,units = "in",res=300)
ggplot(filter(veg.means,Ecosystem=="CSS"), aes(x=Year, y=(`Native Shrub_mean`), color=Water, 
                      group = Water, linetype = Water, shape = Water)) + 
  geom_errorbar(aes(ymin=(`Native Shrub_mean`-`Native Shrub_se`), ymax=(`Native Shrub_mean`+`Native Shrub_se`)), width=.1, lty=1, show.legend = F) +
  geom_line() +
  geom_point(size = 2) +
  labs(color = "Water",
       linetype = "Water",
       shape = "Water",
       y = "Native shrub cover (%)") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position=c(0.5,0.78), 
        legend.title = element_text(size=12),
        legend.key.width= unit(1, 'cm'),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(~Nitrogen)
dev.off()

# Plot diversity versus water input
png("Graphics/ShannonWater.png",width = 8,height = 6,units = "in",res=300)
ggplot(veg.means, aes(x=Water.input, y=(Shannon.diversity_mean), color=Water, 
                          group = Water, shape = Water, label = Year)) + 
  geom_errorbar(aes(ymin=(Shannon.diversity_mean-Shannon.diversity_se), ymax=(Shannon.diversity_mean+Shannon.diversity_se)), width=.1, lty=1, show.legend = F) +
  geom_point(size = 2) +
  geom_text(size = 1,hjust=-0.25) +
  labs(color = "Water",
       shape = "Water",
       y = "Shannon diversity",
       x = "Water input (mm)") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position=c(0.4,0.65), 
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(Ecosystem~Nitrogen)
dev.off()


# Plot biomass with precip inputs
biomass.plot <- 
  ggplot(filter(Biomass.means,Ecosystem=="Grassland",Nitrogen=="Ambient"), aes(x=Year, y=(Biomass.per.area_mean), color=Water, 
                          group = Water, linetype = Water, shape = Water)) + 
  geom_errorbar(aes(ymin=(Biomass.per.area_mean-Biomass.per.area_se), ymax=(Biomass.per.area_mean+Biomass.per.area_se)), width=.1, lty=1, show.legend = F) +
  geom_line() +
  geom_point(size = 2) +
  labs(color = "Water",
       linetype = "Water",
       shape = "Water",
       y = "Biomass (g/m^2)") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position="none", 
        legend.title = element_text(size=14),
        legend.key.width= unit(1.5, 'cm'),
        legend.text = element_text(size=12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

precip.plot <-
  ggplot(filter(Biomass.means,Ecosystem=="Grassland",Nitrogen=="Ambient"),
       aes(x=Year, y=Water.input, color=Water, group = Water, linetype = Water, shape = Water)) + 
  geom_line() +
  geom_point(size = 2) +
  labs(color = "Water",
       linetype = "Water",
       shape = "Water",
       y = "Water input (mm)") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position=c(0.55,0.77), 
        legend.title = element_text(size=12),
        legend.key.width= unit(1.5, 'cm'),
        legend.text = element_text(size=10),
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  expand_limits(y=0)

# Plot biomass versus water input png("Graphics/BiomassWater.png",width = 8,height = 4,units = "in",res=300)
water.response <- 
  ggplot(filter(Biomass.means,Ecosystem=="Grassland" & Nitrogen=="Ambient"),
         aes(x=Water.input, y=Biomass.per.area_mean, color=Water, group = Water, shape = Water)) + 
  stat_smooth(aes(x=Water.input, y=Biomass.per.area_mean),
              method = 'nls', formula = 'y~a*x/(b+x)',
              method.args = list(start=c(a=500,b=300)), se=FALSE,
              inherit.aes = FALSE, color = "black") +
  geom_errorbar(aes(ymin=(Biomass.per.area_mean-Biomass.per.area_se), ymax=(Biomass.per.area_mean+Biomass.per.area_se)), width=.1, lty=1, show.legend = F) +
  geom_point(size = 2) +
  labs(color = "Water",
       shape = "Water",
       y = "Biomass (g/m^2)",
       x = "Water input (mm)") +
  scale_color_manual(values=c('#619CFF','#00BA38','#F8766D')) +
  theme_bw(base_size=16) +
  theme(plot.title = element_text(hjust=0, size=18),
        axis.text.y=element_text(size=14),
        axis.text.x=element_text(size=14),
        axis.title.y=element_text(size=18),
        axis.title.x=element_text(size=18),
        legend.position="none", 
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

biomass.precip <- arrangeGrob(biomass.plot, precip.plot, water.response, ncol=1, nrow=3, heights = c(3,3,3))
ggsave("Graphics/Biomass.png", device = "png", biomass.precip, width = 5, height = 10)

