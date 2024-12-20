################################################################################
####### Script used to clean raw data that were downloaded from the   ##########
####### OC http://hydstra.ocpublicworks.com/web.htm 			            ##########
####### This code cleans and calculates daily precipitation 	        ##########
####### You may need to adjust names accordingly. 					          ##########
####### You will use the files in the folder GaugeData that are   	  ##########
####### in the Microbes and Global Change drive 					            ##########
####### Files in the folder GaugeData are not clean. Do not use 	    ##########
####### files in this folder to get precipitation daily				        ##########
################################################################################

## Call your downloaded packages
library(googledrive)
library(dplyr)

# Input Hicks Canyon OC Public Works data
Precip.Hicks.raw <- drive_get("GaugeDataToClean/HICKS_CYN_2224.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.,stringsAsFactors = F,header = T)

Precip.Hicks <- Precip.Hicks.raw %>%
  slice(-1:-3) %>% #Remove first rows. You may need to adjust
  rename(Date = Time) %>%
  mutate(Gauge = as.numeric(HICKS_CYN)) %>% #Reformating the column that has the ambient precipitation
  select(c(Date,Gauge)) %>%
  mutate(Date = as.POSIXct(Date,format="%H:%M:%S %m/%d/%Y",tz="America/Los_Angeles")) %>% # Formatting to dates in Pacific timezone
  mutate(Day = format(Date,format="%m/%d/%Y",tz="")) %>%
  mutate(Precipitation = Gauge*25.4) %>% # Convert inches to mm
  mutate(Source = "Hicks_Canyon") %>% #Make sure the source format matched the format for current clean files
  select(c(Date,Precipitation,Source,Day)) %>%
  filter(!is.na(Date)) %>% #Remove NA
  mutate(Units = "mm")

# Save this file. This file will need to be uploaded to the "RawData" folder 
# Make sure the name includes "CleanRaw" otherwise it won't be identified by the R script
write.table(Precip.Hicks,"CleanRawHicks2224.csv",quote=F,row.names=F,sep=",",na="")

# The following code processes data from other sources, namely the Loma UCI weather station #############################################
# Input Loma weather station data to process (from manual download)
Precip.UCI.raw.man <- drive_get("GaugeDataToClean/CR1000_RAWS3_Dat_30Min Aug 20-24.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.,stringsAsFactors = F,header = T)

Precip.UCI.man <- Precip.UCI.raw.man %>%
  rename(Date = TIMESTAMP) %>%
  slice(-(1:2)) %>%
  mutate(Precipitation = as.numeric(Rain_mm_Tot)) %>% #Reformatting the column that has the ambient precipitation
  select(c(Date,Precipitation)) %>%
  mutate(Date = as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles")) %>% # Formatting to dates in Pacific timezone
  mutate(Day = format(Date,format="%m/%d/%Y",tz="")) %>%
  filter(!is.na(Date)) %>% #Remove NA
  mutate(Units = "mm")  %>%
  mutate(Source = "EastLomaUCIRAWS") %>% #Make sure the source format matched the format for current clean files
  arrange(Date) %>%
  select(c(Date,Precipitation,Source,Day,Units))

write.table(Precip.UCI.man,"Outputs/CleanRaw_CR1000_RAWS3_Dat_30Min Aug 20-24.csv",quote=F,row.names=F,sep=",",na="")

# Input Loma weather station data to process (downloaded from RAWS site)
Precip.UCI.raw <- drive_get("GaugeDataToClean/2022-09-30 EastLomaUCIRAWS.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.,stringsAsFactors = F,header = T)

Precip.UCI <- Precip.UCI.raw %>%
  rename(Date = Timestamp) %>%
  mutate(Precipitation = as.numeric(Rain_mm)) %>% #Reformatting the column that has the ambient precipitation
  select(c(Date,Precipitation)) %>%
  mutate(Date = as.POSIXct(Date,format="%m/%d/%Y, %I:%M:%S %p",tz="America/Los_Angeles")) %>% # Formatting to dates in Pacific timezone
  mutate(Day = as.character(Date,format="%m/%d/%Y",tz="")) %>%
  filter(!is.na(Date)) %>% #Remove NA
  mutate(Units = "mm")  %>%
  mutate(Source = "EastLomaUCIRAWS") %>% #Make sure the source format matched the format for current clean files
  arrange(Date) %>%
  select(c(Date,Precipitation,Source,Day,Units))
  
write.table(Precip.UCI,"Outputs/2022-09-30 EastLomaUCIRAWSClean.csv",quote=F,row.names=F,sep=",",na="")


# Legacy code for converting gauge readings to Pacific time zone and day to a standard date format
# Already applied to gauge data from 2013-14
Precip2 <- read.csv("INPUT_FILE.csv",stringsAsFactors = F) %>%
  mutate(Date = as.POSIXct(Date,format="%Y-%m-%d %H:%M:%S",tz="GMT")) %>% # Dates were recorded in UTC
  mutate(Day = format(Date,format="%m/%d/%Y",tz="America/Los_Angeles")) %>%
  mutate(Date = format(Date,format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles")) # Converting to dates in Pacific timezone

# Make sure the name includes "CleanRaw" otherwise it won't be identified by the R script
write.table(Precip2,"OUTPUT_FILE.csv",quote=F,row.names=F,sep=",",na="")

##Calculate Daily sum
DailyPrecipLoma <- Precip %>% #assign a new name to our clean raw data
  filter(Precipitation>0) %>% # Only keep non-zero measurements
  group_by(Day) %>% # Group by day for easy calculation and visualization 
  summarize(rawdata = sum(Precipitation)) %>% # Calculate daily sums
  rename(AmbientPrecip = rawdata) %>%
  mutate(Units = "mm") %>%
  mutate(Source = "Hicks_Canyon")

write.table(DailyPrecipLoma,"FILENAME.csv",quote=F,row.names=F,sep=",",na="") # save daily table

