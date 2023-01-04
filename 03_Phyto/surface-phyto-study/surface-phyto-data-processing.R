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

# Reorder date/time columns
df_phyto <- df_phyto %>% 
  relocate(DateTime, .before = Region) %>% 
  relocate(Year, .after = DateTime)

# Check if Station Codes are all correct ---------------------------------------
sort(unique(df_phyto$StationCode)) # 28 Total stations, no typos

sort(unique(df_phyto$Genus))

# Summarize data by genus and algal group --------------------------------------

## Summarize by genus
df_phyto_gen <- df_phyto %>%
  group_by(Year, Month, Season, DateTime, StationCode, Region, RegionAbbreviation, AlgalType, Genus) %>%
  summarize(BV.per.mL = sum(BiovolumePerML, na.rm = TRUE)) %>%
  ungroup

## Summarize by algal group
df_phyto_grp <- df_phyto %>%
  group_by(Year, Month, Season, DateTime, StationCode, Region, RegionAbbreviation, AlgalType) %>%
  summarize(BV.per.mL = sum(BiovolumePerML, na.rm = TRUE)) %>%
  ungroup

# Export data for analysis in PRIMER -------------------------------------------

write_csv(df_phyto_gen, file = "df_phyto_gen.csv")
write_csv(df_phyto_grp, file = "df_phyto_grp.csv")



