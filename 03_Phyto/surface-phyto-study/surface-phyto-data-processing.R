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
df_phyto <- df_phyto %>% 
  unite(DateTime, c("SampleDate","SampleTime"), sep = " ") 

df_phyto$DateTime <- mdy_hms(df_phyto$DateTime, 
                                 tz = "US/Pacific")

# Remove month column from original datasheet b/c of errors --------------------
df_phyto$Month <- NULL

# Add month data from DateTime column ------------------------------------------
df_phyto <- df_phyto %>%
  mutate(Month = month(DateTime, label = T))

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

# Pull seasonal assignment data from datasheet ---------------------------------
seasons <- df_phyto %>% select(Month, Season)
seasons <- unique(seasons)

# Pull regional data from original datset --------------------------------------
regions <- df_phyto %>% select(StationCode, RegionAbbreviation)
regions <- unique(regions)

# Summarize data by genus and algal group --------------------------------------

## Summarize by genus
df_phyto_gen_Surface <- df_phyto %>%
  group_by(DateTime, StationCode, Region, Genus) %>%
  summarize(BV.um3.per.mL = sum(BiovolumePerML, na.rm = TRUE)) %>%
  ungroup

# Import historical EMP data ---------------------------------------------------
load("df_phyto_gen.RData")

df_phyto_gen_EMP <- df_phyto_gen
rm(df_phyto_gen)

# Filter out EMP data from outside range of surface study-----------------------
start.date = ymd_hms("2018-07-07 11:59:00")
end.date = ymd_hms("2019-07-16 11:59:00")

df_phyto_gen_EMP <- df_phyto_gen_EMP %>%
  filter(DateTime >= start.date & DateTime <= end.date)

# Check for the same number of unique sampling events 
str_length(sort(unique(df_phyto$DateTime))) # 334 events
str_length(sort(unique(df_phyto_gen_EMP$DateTime))) # 335 events
# looks like one sampling event was missed for surface phyto. 

# Add column to indicate these are regular samples -----------------------------
# Regular = sampled at 1m depth ------------------------------------------------
df_phyto_gen_EMP <- df_phyto_gen_EMP %>% 
  mutate(SampleType = "Regular", .after = StationCode)

# Surface = sampled with Van Dorn
df_phyto_gen_Surface <- df_phyto_gen_Surface %>% 
  mutate(SampleType = "Surface", .after = StationCode)

# Select same columns in historical data as in surface data --------------------
df_phyto_gen_EMP <- df_phyto_gen_EMP %>%
  select(DateTime:SampleType,Genus,BV.um3.per.mL)

# Combine surface samples with regular EMP samples -----------------------------
df_phyto <- bind_rows(df_phyto_gen_EMP,df_phyto_gen_Surface)

# Confirm correct number of stations -------------------------------------------
sort(unique(df_phyto$StationCode))

# Fix EZ SJR station names
df_phyto$StationCode <- gsub("EZ2 SJR","EZ2-SJR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ6 SJR","EZ6-SJR",df_phyto$StationCode)

# Count again to confirm
sort(unique(df_phyto$StationCode)) # 28 stations (correct)

# Add in data about region abbrevations ----------------------------------------
df_phyto <- left_join(df_phyto, regions)

# Add month data from DateTime column ------------------------------------------
df_phyto <- df_phyto %>%
  mutate(Month = month(DateTime, label = T))

df_phyto <- left_join(df_phyto, seasons)

# Add in algal group data ------------------------------------------------------
df_algal_groups <- read_csv("algal_genus_no_dup.csv")

df_phyto <- left_join(df_phyto, df_algal_groups)

# Reorder columns for export ---------------------------------------------------
df_phyto <- df_phyto %>%
  relocate(BV.um3.per.mL, .after = Group) %>%
  relocate(Group, .before = Genus) %>%
  relocate(Season, .after = DateTime) %>%
  relocate(Month, .after = Season) %>%
  relocate(RegionAbbreviation, .after = Region)

# Plot data to make sure the scale is ~approx correct and not orders of 
# magnitude off
plot <- ggplot(data = df_phyto, aes(x = Month, y = BV.um3.per.mL, color = SampleType)) +
  geom_jitter(width = 0.2, size = 2)

plot

# Summarize by algal group -----------------------------------------------------
df_phyto_grp <- df_phyto %>%
  group_by(Year, Month, Season, DateTime, StationCode, Region, RegionAbbreviation, AlgalType) %>%
  summarize(BV.per.mL = sum(BiovolumePerML, na.rm = TRUE)) %>%
  ungroup

# Export data for analysis in PRIMER -------------------------------------------
write_csv(df_phyto_gen, file = "df_phyto_gen.csv")
write_csv(df_phyto_grp, file = "df_phyto_grp.csv")



