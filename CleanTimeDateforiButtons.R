### R code to clean iButton time and date                              ###
###############################################################################
# Need to update with Google Drive access
rm(list=objects()) 
setwd( "G:/My Drive/FolderTrabajoPersonal/Loma Experiment/iButton/Gabin")
setwd( "G:/My Drive/FolderTrabajoPersonal/Loma Experiment/iButton/raw_data_Zhao")     

########################################################################
### Code to save each excel sheet as csv file
library(readxl)
library(readr)

# Read sheets and use for filenames
sheets <- excel_sheets("2022.xlsm")
filenames <- paste0(sheets, ".csv")

# read_excel only reads a single sheet, so lapply over each sheet name
dats <- lapply(sheets, read_excel, path = "2022.xlsm")

# Save each data frame with different filename using write_csv
lapply(seq_along(dats), function(i) write_csv(dats[[i]], filenames[i]))
########################################################################

########################################################################
#### CODE TO CLEAN AND STANDARDIZE FILES


# Packages we will use
library(tidyverse) # to manipulate the datasets
library(lubridate) # to deal with dates

# File location
setwd( "G:/My Drive/FolderTrabajoPersonal/Loma Experiment/iButton/raw_data_Zhao")     

#If the path is different than your working directory
# you'll need to set full.names = TRUE to get the full
# paths.
my_files <- list.files("2022", full.names=TRUE)

#Further arguments to read.csv can be passed in ...
all_csv <- lapply(my_files, read.csv)


names(all_csv) <- gsub(".csv","",list.files("2022",full.names = FALSE),fixed = TRUE)

#Now you can create a dataset based on each filename
#Need to call each file individually
# Although tedious it will ensure you do not miss any data
df <- as.data.frame(all_csv[["2022-Mar-16_WS2"]])

#df2 <- df[-c(1:29),]#make sure you do not trim data

df2 <- df %>%
  mutate(ReadingStand = parse_date_time(df$Reading,  c("dmy HM", "dmy HMS"), tz="America/Los_Angeles"))%>%
  rename(ReadingOriginal = "Reading",
         Humidity = "Humidity....")%>%
  relocate(ReadingStand, .after = ReadingOriginal)

which(is.na(df2$ReadingStand))

head(df2)

write.csv(df2,"2022-Mar-16_WS2.csv", row.names=F)


#### IF A VALUE SHOWS THIS WARNING MESSAGE (" failed to parse") RUN LINE BELOW 
#which(is.na(df2$ReadingStand))

#df3 <- df %>%
#  mutate(ReadingStand = parse_date_time(df$Reading, c('%d/%m/%Y %H:%M','%d/%m/%Y %H:%M:%S'), exact = TRUE, tz="America/Los_Angeles"))%>%
#  rename(ReadingOriginal = "Reading",
#         Humidity = "Humidity....")%>%
#  relocate(ReadingStand, .after = ReadingOriginal)

