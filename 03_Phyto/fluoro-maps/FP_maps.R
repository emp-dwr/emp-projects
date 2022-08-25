## Cleaning and joining FluoroProbe and MOPED data, then generating a map
## 7/20/22 TMF

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("./03_Phyto/fluoro-maps")

# Clean workspace
rm(list=ls()) 

## File names 
FP_data <- "Fluoroprobe_SP_Aug22.txt"
MOPED_data <- "MOPED_Export_92_08232022_SP.csv"

## Map settings
time_unit <- "1 minute"

# read in FluoroProbe data
col_names <- names(read_tsv(FP_data, n_max = 0))
df_FP <- read_tsv(FP_data, skip = 2, col_names = col_names)
## Read in MOPED data
df_MOPED <-  read_csv(MOPED_data, skip = 2)


## Remove unnecessary columns
df_FP[6:9] <- NULL
df_FP[7:28] <- NULL

## Clean headers and remove unnecessary data
df_FP <- df_FP %>%
  clean_names(case = "big_camel")

## Rename headers
df_FP <- df_FP %>%
  rename("GreenAlgae" = "GreenAlgae2") %>%
  rename("Cyanobacteria" = "Bluegreen3") %>%
  rename("Diatoms" = "Diatoms4") %>%
  rename("Cryptophytes" = "Cryptophyta5") %>%
  rename("Total.Fluorescence" = "TotalConc")

## Covert to datetime
df_FP$DateTime <- mdy_hms(df_FP$DateTime, 
                          tz = "US/Pacific")

# Round to nearest 1 min and calc average
df_FP <- df_FP %>%
  mutate(DateTime = round_date(DateTime, unit=time_unit)) %>%
  group_by(DateTime) %>%
  summarize_all(~round(mean(., na.rm = TRUE),2))

## Remove vertical data from MOPED
df_MOPED <- df_MOPED %>% filter(Equipment == "EXO Horizontal")

# clean up (code from Sarah Perry)
if(length(unique(df_MOPED$Extension[!is.na(df_MOPED$Extension)])) > 1) stop('ERROR: more than one extension value in wq data')
run_name <- unique(df_MOPED$Extension[!is.na(df_MOPED$Extension)])

## Covert to datetime
df_MOPED <- df_MOPED %>% rename("DateTime" = "TimeStamp")
df_MOPED$DateTime <- mdy_hm(df_MOPED$DateTime, 
                            tz = "US/Pacific")

## Average data to same time interval as FluoroProbe data
df_MOPED_data <- df_MOPED %>%
  subset(select = c(DateTime, Value, Header)) %>%
  mutate(DateTime = round_date(DateTime, unit='1 minute')) %>%
  group_by(Header, DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE)) 

## Average data to same time interval as FluoroProbe data
df_MOPED_GPS <- df_MOPED %>%
  subset(select = c(DateTime, Latitude, Longitude)) %>%
  mutate(DateTime = round_date(DateTime, unit='1 minute')) %>%
  group_by(DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE))

## Pivot MOPED data wide
df_MOPED_l <- left_join(df_MOPED_GPS, df_MOPED_data)
df_MOPED_w <- pivot_wider(df_MOPED_l, names_from = Header, values_from = Value)

## Combine MOPED and FluoroProbe data
df_EMP_data <- left_join(df_MOPED_w, df_FP)

## Filter out data rows with no FP data
df_EMP_data_FP <- df_EMP_data %>% drop_na()



