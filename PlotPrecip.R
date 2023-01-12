library(tidyr)
library(dplyr)
library(ggplot2)
library(googledrive)

# Water year is defined by the January of that wet season
# E.g. Water Year 2020 goes from 2019-10-01 to 2020-09-30
PrecipRecord <- drive_get("FullPrecipLoma.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  mutate(Day = as.Date(Day,tryFormats=c("%Y-%m-%d","%m/%d/%Y"))) %>%
  mutate(Year = as.numeric(format(Day,"%Y"))) %>%
  mutate(Month = as.numeric(format(Day,"%m"))) %>%
  mutate(WaterYear = ifelse(Month %in% 1:9,Year,Year+1)) %>%
  group_by(WaterYear) %>%
  mutate(CumAmbient = cumsum(Ambient), CumReduced = cumsum(Reduced), CumAdded = cumsum(Added))

# Compare to CEB weather station dataset
# CEBPrecipLoma <- read.csv("Outputs/DailyCEB.csv") %>%
#  mutate(Day = as.Date(Day)) %>%
#  mutate(Year = as.numeric(format(Day,"%Y"))) %>%
#  mutate(Month = as.numeric(format(Day,"%m"))) %>%
#  mutate(WaterYear = ifelse(Month %in% 1:9,Year-1,Year))

# In the next two plots below, red indicates events that were excluded in the drought manipulation
# In other words, red shows precip that fell in the ambient but not drought plots
xlim <- as.Date(c("2006-10-01","2021-09-30"))
pdf("Outputs/LomaPrecip.pdf",height=6,width=30)
par(cex.axis=1.5,cex.lab=2,font.lab=2,lwd=1.5,tcl=0.4,las=1,mgp=c(3,0.5,0),mar=c(3,5,2,2)+0.1)
plot(x=PrecipRecord$Day,y=PrecipRecord$Added,xlim=xlim,ylab="Water input (mm)",xlab=NA,type="n")
arrows(PrecipRecord$Day,0,PrecipRecord$Day,PrecipRecord$Added,length=0,angle=90,code=2,lwd=0.5,col="cyan")
# arrows(CEBPrecipLoma$Day+0.4,0,CEBPrecipLoma$Day+0.4,CEBPrecipLoma$Ambient+0.4,length=0,angle=90,code=2,lwd=0.5,col="green")
arrows(PrecipRecord$Day,0,PrecipRecord$Day,PrecipRecord$Ambient,length=0,angle=90,code=2,lwd=0.5,col="red")
arrows(PrecipRecord$Day,0,PrecipRecord$Day,PrecipRecord$Reduced,length=0,angle=90,code=2,lwd=0.5)
legend("topright",c("Ambient","Excluded","Added"),lty=1,col=c("black","red","cyan"),cex=1.5)
# legend("topright",c("Ambient","CEB Ambient","Excluded","Added"),lty=1,col=c("black","green","red","cyan"),cex=1.5)
dev.off()

## This subsets per water year to help with visualzation 
PrecipRecordset <- filter(PrecipRecord, Day >= "2020-10-01" & Day <= "2021-09-30")

xlim <- as.Date(c("2020-10-01","2021-09-30"))
pdf("Outputs/LomaPrecipWaterYear2020.pdf",height=6,width=6)
par(cex.axis=1.5,cex.lab=2,font.lab=2,lwd=1.5,tcl=0.4,las=1,mgp=c(3,0.5,0),mar=c(3,5,2,2)+0.1)
plot(x=PrecipRecordset$Day,y=PrecipRecordset$Added,xlim=xlim,ylab="Water input (mm)",xlab=NA,type="n")
arrows(PrecipRecordset$Day,0,PrecipRecordset$Day,PrecipRecordset$Added,length=0,angle=90,code=2,lwd=0.5,col="cyan")
arrows(PrecipRecordset$Day,0,PrecipRecordset$Day,PrecipRecordset$Ambient,length=0,angle=90,code=2,lwd=0.5,col="red")
arrows(PrecipRecordset$Day,0,PrecipRecordset$Day,PrecipRecordset$Reduced,length=0,angle=90,code=2,lwd=0.5)
legend("topright",c("Ambient","Excluded","Added"),lty=1,col=c("black","red","cyan"),cex=1.5)
dev.off()


pdf("Outputs/LomaPrecipCumulative.pdf",height=6,width=30)
d <- ggplot(PrecipRecord, aes(Day, CumAdded)) + geom_line(color="cyan") + ylab("Water input (mm)") +
  geom_line(aes(y=CumReduced), color="red") +
  geom_line(aes(y=CumAmbient))
d + facet_grid(~WaterYear, scales = "free_x")
dev.off()

