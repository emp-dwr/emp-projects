# Extracting phyto data from EMP datasheets ------------------------------------
# 01/03/2023 -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

# Set working directory --------------------------------------------------------
setwd("./03_Phyto/surface-phyto-study")
getwd()

# Clean workspace --------------------------------------------------------------
rm(list=ls())

# Importing Raw EMP Data -------------------------------------------------------
df_phyto <- read_csv("Phyto Depth Comparison Biovolume Data.csv")

# Clean up column names --------------------------------------------------------
df_phyto <- df_phyto %>% clean_names(case = "big_camel")

df_phyto <- df_phyto %>% rename("GALD" = "Gald1")

# Remove blank columns 
df_phyto <- df_phyto %>% select_if(~ !all(is.na(.)))

# Combine date and time column
df_phyto <- df_phyto %>% unite(DateTime, c("SampleDate","SampleTime"), sep = " ") #, remove = FALSE, na.rm = FALSE)

df_phyto$DateTime <- mdy_hms(df_phyto$DateTime, 
                                 tz = "US/Pacific")

# Remove unneeded columns
df_phyto <- df_phyto %>% select(!c("Factor"))
df_phyto <- df_phyto %>% select(!(Species:BiovolumeIndividualCell))
df_phyto <- df_phyto %>% select(!(VolumeReceivedML:NumberOfFieldsCounted))