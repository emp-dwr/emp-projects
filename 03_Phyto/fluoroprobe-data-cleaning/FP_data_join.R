## Join together MOPED and FluoroProbe data

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("./03_Phyto/fluoroprobe-data-cleaning")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

## Time unit selection for averaging data
time_unit <- "1 minute"

#################################################
######## Importing Raw FluoroProbe Data #########
#################################################

# Import FluoroProbe data files
FP_files <- dir(path = "data_FP/", pattern = "\\.txt", full.names = T)
df_FP <- map_dfr(FP_files, ~read_tsv(.x))

# Remove second row of labels
df_FP <- df_FP %>%
  filter(`Date/Time` != "date")

# Select Columns with concentrations
df_FP <- df_FP %>%
  select(`Date/Time`:Cryptophyta...5,`Total conc.`)

# Rename headers
df_FP <- df_FP %>%
  rename("DateTime" = "Date/Time") %>%
  rename("GreenAlgae.ug.L" = "Green Algae...2") %>%
  rename("Cyanobacteria.ug.L" = "Bluegreen...3") %>%
  rename("Diatoms.ug.L" = "Diatoms...4") %>%
  rename("Cryptophytes.ug.L" = "Cryptophyta...5") %>%
  rename("Total.Conc.FP.ug.L" = "Total conc.")

# change FluoroProbe values to numeric
df_FP <- df_FP %>%
  mutate(across(GreenAlgae.ug.L:Total.Conc.FP.ug.L, ~ as.numeric(.x), .keep = 'unused'))

# Convert to datetime
df_FP$DateTime <- mdy_hms(df_FP$DateTime, tz = "US/Pacific")

# Round to nearest 1 min and calc average
df_FP <- df_FP %>%
  mutate(DateTime = round_date(DateTime, unit=time_unit)) %>%
  group_by(DateTime) %>%
  summarize_all(~round(mean(., na.rm = TRUE),2))

###########################################
######## Importing Raw MOPED Data #########
###########################################
# Import FluoroProbe data files
MOPED_files <- dir(path = "data_MOPED/", pattern = "\\.csv", full.names = T)
df_MOPED <- map_dfr(MOPED_files, ~read_csv(.x, skip = 2))

## Remove vertical data from MOPED
df_MOPED <- df_MOPED %>% filter(Equipment == "EXO Horizontal")

## Covert to datetime
df_MOPED <- df_MOPED %>% rename("DateTime" = "TimeStamp")
df_MOPED$DateTime <- mdy_hms(df_MOPED$DateTime, tz = "US/Pacific")

# Select values needed
df_MOPED <- df_MOPED %>% select(Header:Latitude,DateTime:Value)

# Average data to same time interval as FluoroProbe data
df_MOPED_data <- df_MOPED %>%
  subset(select = c(DateTime, Value, Header)) %>%
  mutate(DateTime = round_date(DateTime, unit = time_unit)) %>%
  group_by(Header, DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE)) 

# Average data to same time interval as FluoroProbe data
df_MOPED_GPS <- df_MOPED %>%
  subset(select = c(DateTime, Latitude, Longitude)) %>%
  mutate(DateTime = round_date(DateTime, unit = time_unit)) %>%
  group_by(DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE))

## Pivot MOPED data wide
df_MOPED_l <- left_join(df_MOPED_GPS, df_MOPED_data)
df_MOPED_w <- pivot_wider(df_MOPED_l, names_from = Header, values_from = Value)

## Combine MOPED and FluoroProbe data
df_all <- left_join(df_MOPED_w, df_FP)

