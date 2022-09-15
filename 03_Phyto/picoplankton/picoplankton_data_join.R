## Cleaning and joining FluoroProbe and MOPED data, then generating a map
## 
## 8/25/22 TMF

library("tidyverse")
library("lubridate")
library("janitor")

# Set working directory
setwd("./03_Phyto/picoplankton")
getwd()

# Clean workspace
rm(list=ls()) 

# Define negate function
`%!in%` = Negate(`%in%`)

# Import list of sample dates and station IDs from BSA -------------------------
df_pico <- read_csv("data/DSC_Picoplankton_Inventory_DWR.csv", col_select = 1:3)

# Remove blank rows
df_pico <-df_pico %>% filter_all(any_vars(!is.na(.)))

## Get dates in the right format
df_pico$SampleDate <- mdy(df_pico$SampleDate)

## Combine date and time column
df_pico <- df_pico %>% unite(DateTime, c("SampleDate","SampleTime"), sep = " ") #, remove = FALSE, na.rm = FALSE)

## Fix dates that don't match between BSA and AEU database (assume AEU is correct)
df_pico$DateTime <- gsub("2021-08-16 10:56:00","2021-08-16 10:50:00",df_pico$DateTime)
df_pico$DateTime <- gsub("2021-10-12 11:07:00","2021-10-13 11:07:00",df_pico$DateTime)
df_pico$DateTime <- gsub("2021-10-12 08:24:00","2021-10-13 08:24:00",df_pico$DateTime)
df_pico$DateTime <- gsub("2021-11-17 10:25:00","2021-11-16 09:48:00",df_pico$DateTime)


## Fix dates that don't match between BSA and EMP database (assume EMP is correct)
df_pico$DateTime <- gsub("2021-09-09 07:25:00","2021-09-10 07:25:00",df_pico$DateTime)
df_pico$DateTime <- gsub("2021-11-06 12:00:00","2021-11-09 12:00:00",df_pico$DateTime)

df_pico$DateTime <- as_datetime(df_pico$DateTime, 
                                 tz = "US/Pacific",
                                 format = c("%Y-%m-%d %H:%M:%OS"))

## Check for missing dates
df_pico %>% filter(is.na(DateTime)) ## No missing dates

## Classify stations as AEU or EMP for data joining
df_pico <- df_pico %>%
  mutate(Study = case_when(StationCode == "LIS" ~ "AEU",
                           StationCode == "STTD" ~ "AEU",
                           StationCode == "SHR" ~ "AEU",
                           TRUE ~ "EMP"))

# Read in AEU's field water quality data ---------------------------------------
df_AEU_WQ_Field <- read_csv("data/WQ_Data_AEU_Field.csv")

# Get dates in the right format
df_AEU_WQ_Field$Date <- mdy(df_AEU_WQ_Field$Date)

## Combine date and time column
df_AEU_WQ_Field <- df_AEU_WQ_Field %>% unite(DateTime, c("Date","Time"), sep = " ") #, remove = FALSE, na.rm = FALSE)

df_AEU_WQ_Field$DateTime <- as_datetime(df_AEU_WQ_Field$DateTime, 
                                tz = "US/Pacific",
                                format = c("%Y-%m-%d %H:%M:%OS"))


# Rename data frame headers to match EMP 
df_AEU_WQ_Field <- df_AEU_WQ_Field %>%
  rename("Secchi" = "secchi") %>%
  rename("WTSurface" = "water.temp") %>%
  rename("DOSurface" = "DO.probe") %>%
  rename("SpCndSurface" = "sp.cond") %>%
  rename("pHSurface" = "pH") %>%
  rename("Microcystis" = "microcyst") %>%
  rename("TurbiditySurface_FNU" = "turb") %>%
  rename("StationCode" = "Station Name") %>%
  select(!("EC"))

# Convert AEU Secchi data in cm (given in meters)
df_AEU_WQ_Field <- df_AEU_WQ_Field %>%
  mutate(Secchi = Secchi*100, .keep = "unused", .after = "Secchi")


## Combine data frames
#df_pico <- left_join(df_pico,df_AEU_WQ_Field)

# See which dates don't join
#test <- test %>%
#  filter(Study == "AEU")

# Find rows with NAs
#df_AEU_NAs <- test %>% filter_all(any_vars(is.na(.))) 

# Correct the dates that don't mesh (assume times in BSA spreadsheet are incorrect)
#sort(df_AEU_NAs$DateTime)

# Import EMP WQ Data -----------------------------------------------------------
df_EMP_WQ <- read_csv("data/WQ_Data_EMP.csv")


# Remove rows with AEU stations
df_EMP_WQ <- df_EMP_WQ %>% 
  filter(Station %!in% c("LIS","STTD","SHR")) %>%
  rename("StationCode" = "Station")

# Select only field water quality data
df_EMP_WQ <- df_EMP_WQ %>%
  select(StationCode, Date, Time, Secchi, Microcystis, SpCndSurface, DOSurface, 
         WTSurface, TurbiditySurface_FNU, pHSurface)

# Get dates in the right format
df_EMP_WQ$Date <- mdy(df_EMP_WQ$Date)

# Combine date and time column
df_EMP_WQ <- df_EMP_WQ %>% unite(DateTime, c("Date","Time"), sep = " ")

df_EMP_WQ$DateTime <- as_datetime(df_EMP_WQ$DateTime, 
                                        tz = "US/Pacific",
                                        format = c("%Y-%m-%d %H:%M:%OS"))

# Combined EMP and AEU data
df_WQ_field <- bind_rows(df_AEU_WQ_Field,df_EMP_WQ)

# Combine WQ data with list of samples from BSA
df_pico <- left_join(df_pico,df_WQ_field)

# Export as CSV files
write.csv(df_pico, file = "picoplankton_field_WQ.csv")

# Import AEU WQ lab data -------------------------------------------------------

# Read in raw data
df_AEU_WQ_Lab <- read_csv("data/WQ_Data_AEU_Lab.csv")

# Select non-duplicate samples
df_AEU_WQ_Lab <- df_AEU_WQ_Lab %>% filter(SampleType == "Normal Sample")

# Replace Bryte code with three-letter code
df_AEU_WQ_Lab$StationNumber <- gsub("B9D82851352", "LIS", df_AEU_WQ_Lab$StationNumber)

# Rename headers
df_AEU_WQ_Lab <- df_AEU_WQ_Lab %>% 
  rename("StationCode" = "StationNumber") %>%
  rename("DateTime" = "CollectionDate")

# Select analytes and dates
df_AEU_WQ_Lab <- df_AEU_WQ_Lab %>% select(StationCode,DateTime:Analyte, Result)

# Pivot wider
test <- pivot_wider(df_AEU_WQ_Lab, names_from = Analyte, values_from = Result)
