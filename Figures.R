## This code is to create the timeline figures for the response variables
## for diversity and richness 

rm(list=objects()) 
setwd("C:/Users/Dilys Vela/Documents/UCI/MicrobiomesClimateChange/LTREB/codeR")

#library(tidyverse)
library(ggplot2)
#library(plyr)
library(ggthemes)

# need to track down this file and how it was generated
AllVariables <- read.csv("AllYearsResponsevariables.csv") 


############################################
### FUNCTION TO CALCULATE ERROR LINES

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
########################################################################################
### All treatments Average
### Here in "measurevar" you can change the variable of interest
### "groupvars" allows you to separate graph by habitat and yeat
tgc <- summarySE(data=AllVariables, measurevar="spec.rich", groupvars=c("Habitat", "Year"))

pd <- position_dodge(0.1) # move them .05 to the left and right

tiff('SpeceisRichness.tiff', units="in", width=8, height=5, res=300, compression = 'lzw')

ggplot(tgc, aes(x=Year, y=spec.rich , colour=Habitat)) + 
  geom_errorbar(aes(ymin=spec.rich -se, ymax=spec.rich +se), width=.2, position=pd) +
  #geom_point(aes(ymin=spec.rich-se, ymax=spec.rich+se))+
  #geom_linerange(aes(ymin=spec.rich-se, ymax=spec.rich+se))+
  geom_line(position=pd, size=0.8) +
  #theme_classic(base_size=22)+
  geom_point(position=pd, size=2)+
  labs(y=expression("Mean combined species richness per year"), 
       x=expression("Year"),
       colour="Habitat")+
  geom_rangeframe(colour = "black") +
  geom_vline(xintercept = 2020, linetype="dashed",size=1)+
  theme_tufte()+
  scale_x_continuous(name ="Years", breaks=c(2009,2010,2011,2012,2013,2014,2015,
                                             2016,2017,2018,2019,2020,2021,2022))

dev.off()

########################################################################################
#### Separating water treatment and Nitrogen treatment

tgc <- summarySE(data=AllVariables, measurevar="H.div", groupvars=c("Habitat", "Year", "Water_Treatment","Nitrogen_Treatment"))

pd <- position_dodge(0.1) # move them .05 to the left and right

tiff('AllShannon-WeinerTreatments.tiff', units="in", width=10, height=5, res=300, compression = 'lzw')

ggplot(tgc, aes(x=Year, y=H.div , colour=Water_Treatment)) + 
  geom_errorbar(aes(ymin=H.div-se, ymax=H.div +se), width=.2, position=pd) +
  geom_point(aes(shape = factor(Nitrogen_Treatment)),size = 3)+
   geom_line(position=pd, size=0.8) +
  labs(y=expression("Mean combined Shannon-Weiner (H) diversity per year"), 
       x=expression("Year"),
       colour="Water treatment",
       shape="Nitrogen treatment")+
  geom_rangeframe(colour = "black") +
  geom_vline(xintercept = 2020.3, linetype="dashed",size=1)+
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  theme_tufte()+
  theme(axis.text.x = element_text(size = 8,angle=90, hjust=1))+     
  facet_wrap(~Habitat,scales = "free") +
  scale_x_continuous(name ="Years", breaks=c(2009,2010,2011,2012,2013,2014,2015,
                                             2016,2017,2018,2019,2020,2021,2022))
  

dev.off()
