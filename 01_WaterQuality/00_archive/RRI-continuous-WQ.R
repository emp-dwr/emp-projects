## Calculate Average and Std Dev 
## for Water Temp at RRI

library(tidyverse)
library(lubridate)
library(janitor)

setwd("./RRI-continuous-WQ/")
getwd()

rm(list=ls()) #cleans workspace
theme_set(theme_bw())

# Import EMP data files
WQ_files_EMP <- dir(path = "data/", pattern = "\\.csv", full.names = T)

df_RRI_WQ <- map_dfr(WQ_files_EMP, ~read_csv(.x, 
                                             skip = 4,
                                             col_select = c(3,5:8)))

# Clean up column names
df_RRI_WQ <- df_RRI_WQ %>% clean_names(case = "big_camel")

df_RRI_WQ <- df_RRI_WQ %>% 
  rename("DateTime" = "Date") %>%
  rename("QAQCFlag" = "QaqcFlag")

# Replace station name with more commonly used one ("RRI")
# For some reason gsub wasn't working so just deleting that column and adding
# a new one called "RRI"
df_RRI_WQ$StationName <- NULL

df_RRI_WQ <- df_RRI_WQ %>%
  mutate(StationID = "RRI")

df_RRI_WQ <- df_RRI_WQ %>% relocate(StationID, .before = DateTime)

# Create column of data type
df_RRI_WQ <- df_RRI_WQ %>%
  mutate(ReadingType, Analyte = trimws(str_extract(ReadingType, "(?<=\\]).+?(?=\\()")))

# Create column for depth
df_RRI_WQ <- df_RRI_WQ %>%
  mutate(ReadingType, Depth = trimws(str_extract(ReadingType, "(?<=Inst).+?(?=deep)")))

# Remove Reading Type now that it's no longer needed
df_RRI_WQ$ReadingType <- NULL

# Add column for year and month for highlighting data
df_RRI_WQ <- df_RRI_WQ %>% 
  mutate(Year = year(df_RRI_WQ$DateTime)) %>%
  mutate(Month = month(df_RRI_WQ$DateTime, label = T)) %>%
  mutate(Julian = yday(df_RRI_WQ$DateTime))

# Order month in calendar order rather than (default) alphabetical
df_RRI_WQ$Month = factor(df_RRI_WQ$Month, levels = month.abb)

# Reorder date/time columns
df_RRI_WQ <- df_RRI_WQ %>% 
  relocate(Year, .after = DateTime) %>% 
  relocate(Month, .after = DateTime)

# Use years & Julian date as character not number for plotting
df_RRI_WQ <- df_RRI_WQ %>%
  mutate(Year = as.character(df_RRI_WQ$Year)) %>%
  mutate(Julian = as.character(df_RRI_WQ$Julian))
