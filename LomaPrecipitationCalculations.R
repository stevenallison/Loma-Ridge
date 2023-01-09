################################################################################
####### This code uses all clean files to make one master file        ##########                          
####### Every new year that precipitation is collected should         ##########
####### be added to the same folder in the Microbes and Global Change ##########
####### drive.                                                        ##########
################################################################################

## Call your downloaded packages
library(tidyr)
library(purrr)
library(readr)
library(dplyr)

################################################################################
## Give you access to the folder in Microbes and Global Change
## that contain all standardize clean files, called "RawData" . 
## In this folder each "CleanRaw" file corresponds to a year.
## This folder also has the watering file and the drought dates file.
################################################################################

################################################################################
## Call all your clean raw files to be part of hourly.files to create an      ##
## object to perform analyses.                                                ##
## All columns for each file need to have the same name and must be on the    ##
## same order for list.files to work properly.                                ##
## NOTE: in the function "path" we call your local directory where            ##
## all csv_files were stored.                                                 ##
################################################################################

# Create a list with all "CleanRaw" files which are use to create a dataframe
# Every single files that you want to determine daily precipitation needs to
# have "CleanRaw" to be added to this data frame
filenames <- drive_get("RawData", shared_drive = "Microbes and Global Change") %>%
  drive_ls(pattern = "CleanRaw")

AllPrecip.raw <- NULL
for(i in filenames$name) {
  AllPrecip.raw <- drive_get(i, shared_drive = "Microbes and Global Change") %>%
    drive_read_string(encoding="UTF-8") %>%
    read.csv(text=.) %>%
    rbind(AllPrecip.raw)
}

AllPrecip <- AllPrecip.raw %>%
  filter(Precipitation>0) %>% # keep non-zero measurements
  mutate(Day = as.Date(Day,format="%m/%d/%Y")) %>%
  arrange(Day)

# Some daily values were calculated using data from more than one source
# We will create a extra column to retain that information to show sources.
# We will create "Source1" and "Source2". 
# NA in Source2 means that there was not a second source of precipitation 
# for that day. The WARNING it is for those rows that did not have
# a value to add in Source2.
GaugeSource <- AllPrecip %>%
  group_by(Day) %>% # group rows by days
  summarize(Source = paste(unique(sort(Source)), collapse = " ")) %>% # Collects all unique sources
  separate(Source, c("Source1", "Source2"), " ", extra = "merge" ) # creates a column per source

DailyPrecip <- AllPrecip %>%
  group_by(Day) %>% # group rows by days
  summarize(Ambient = sum(as.numeric(Precipitation))) %>% # Calculate daily sums
  inner_join(GaugeSource,by="Day") # join source.name with precipitation by day

################################################################################
#### Here we add the dates that plots were closed                             ##
################################################################################

# Use the actual name of the file since there is only one. 
# Make sure the updated file is the name in the code below
# Create consecutive dates for the range of time that a plot was closed
# We also need to create a column called Reduced to add 0 as precipitation.
# Join Closures object with DailyPrecip object
# Note: This object will have new dates that correspond to Closures
AmbientReduced <- drive_get("ShelterClosureDates.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%
  select(c(ClosureStart,ClosureEnd)) %>%
  mutate(ClosureStart = as.Date(ClosureStart,format="%m/%d/%Y")) %>% #format day
  mutate(ClosureEnd = as.Date(ClosureEnd,format="%m/%d/%Y"))%>% #format day
  mutate(Day = as.Date(ClosureStart,format="%Y-%m-%d")) %>% #same date as closureStart to join
  mutate(Day = purrr::map2(ClosureStart, ClosureEnd, seq, "day")) %>% #create sequence
  unnest(Day, keep_empty = TRUE) %>%
  mutate(Reduced = as.numeric(0)) %>% #Create a new numeric column
  select(Day, Reduced) %>%
  full_join(DailyPrecip, by="Day") %>%
  arrange(Day) %>%
  replace_na(list(Ambient = 0)) %>% # Add Zeros for those dates that were added by ExcludedPrecip object
  mutate(Reduced = if_else(is.na(Reduced), Ambient, Reduced)) # Copy precipitation from AmbientPrecip to Drought 

################################################################################
#### Here we add the dates added water information                            ##
################################################################################

# Use the actual name of the file since there is only one. 
# Make sure the updated file is the name in the code below
# Join with ReducedAmbient object
# This will create dates that were not included in ReducedAmbient

FullPrecipLoma <- drive_get("WaterAdditionDates.csv", shared_drive = "Microbes and Global Change") %>%
  drive_read_string(encoding="UTF-8") %>%
  read.csv(text=.) %>%mutate(Date_Added = as.Date(Date_Added,format="%m/%d/%Y")) %>%
  rename(Day = Date_Added) %>%
  select(Day, Water_Added) %>%
  full_join(AmbientReduced, by="Day") %>%
  replace_na(list(Ambient = 0, Reduced = 0, Water_Added = 0)) %>% # Replace missing values with zero
  mutate(Added = Ambient + Water_Added) %>% # Added treatment is sum of ambient plus water added
  filter(Added != 0) %>% # Remove dates with no precip (from closures)
  mutate_at(vars(Reduced, Ambient, Added), ~round(., 3)) %>% #Standardize with three decimal places
  arrange(Day) %>%
  mutate(Units = "mm") %>% #Recreate Units column
  select(Day, Reduced, Ambient, Added, Units, Source1, Source2)

###############################################################################
## Save the two files that you created above
###############################################################################

## Run the one line of code below to save created FullPrecipLoma file
## This object has Precipitation, Drought and Watering
write.table(FullPrecipLoma,"Outputs/FullPrecipLoma.csv",quote=F,row.names=F,sep=",",na="") # save daily table

## Run the one line of code below to save created daily precipitation file
## Only daily precipitation values. 
write.table(DailyPrecip,"Outputs/DailyAmbientPrecipLoma.csv",quote=F,row.names=F,sep=",",na="") # save daily table

