# Load Libraries and Data Files ------------------------------------------------
# Process EMP Phyto data to get surface tow data only for D19
# Calculate correct biovolume using flowmeter equations from Tomo Kurobe
# 9/30/2022

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(janitor)

# Set working directory
setwd("./03_Phyto/MC-surface-tows")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Import EMP data files
phyto_files <- dir(path = "data/", pattern = "\\.csv", full.names = T)

phyto_all <- map_dfr(phyto_files, ~read_csv(.x))

# Clean up column names
phyto <- phyto_all %>% clean_names(case = "big_camel")

# Remove pre-calculated Unit Density and blank columns
phyto <- phyto %>% select(MethodCode:Biovolume10)

# Remove empty rows
phyto <- phyto %>% filter_all(any_vars(!is.na(.)))

# Average all 10 biovolume measurements for each taxon
phyto <- phyto %>% rowwise() %>% mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T))

# Remove Individual Biovolume Columns
phyto <- phyto %>% select(!(Biovolume1:Biovolume10))

# Remove unneeded columns
phyto <- phyto %>% select(!c("MethodCode","BsaTin","DiatomSoftBody","Synonym"))
phyto <- phyto %>% select(!(ColonyFilamentIndividualGroupCode:Shape))
phyto <- phyto %>% select(!(VolumeReceivedML:NumberOfFieldsCounted))

## Fix samples with missing times
## Just replace with 12:00:00 b/c aren't doing any time-based analyses
phyto <- phyto %>% replace_na(list(SampleTime = "12:00:00"))

## Get dates in the right format
## Some are 1/1/14 and others 1/1/2014
phyto$SampleDate <- mdy(phyto$SampleDate)

## Combine date and time column
phyto <- phyto %>% unite(DateTime, c("SampleDate","SampleTime"), sep = " ") #, remove = FALSE, na.rm = FALSE)

phyto$DateTime <- as_datetime(phyto$DateTime, 
                              tz = "US/Pacific",
                              format = c("%Y-%m-%d %H:%M:%OS"))

# Check for missing dates
phyto %>% filter(is.na(DateTime)) ## No missing dates

# Calculate Unit Density & Biovolume Density
phyto <- phyto %>%
  mutate(Units.per.mL = UnitAbundanceNumberOfNaturalUnits * Factor) %>%
  mutate(BV.um3.per.mL= TotalNumberOfCells * BV.Avg * Factor)

## Add column for year and month for highlighting data
phyto <- phyto %>% mutate(Year = year(phyto$DateTime))

phyto <- phyto %>% mutate(Month = month(phyto$DateTime, label = T))

phyto$Year <- as.factor(phyto$Year)

## Remove columns no longer needed
phyto <- phyto %>% 
  select(!(Species:BV.Avg)) %>% 
  select(!(Factor))

# List of stations 
list(unique(phyto$StationCode))

# Filter only D19 and tow stations
phyto <- phyto %>% filter(StationCode %in% c("D19","D19 MC Tow"))

# Confirm station IDs
unique(phyto$StationCode)
table(phyto$StationCode)

sort(unique(phyto$Genus)) ## 24 unique genera

# Add column for year and month for highlighting data
phyto <- phyto %>% mutate(Year = year(phyto$DateTime))
phyto <- phyto %>% mutate(Month = month(phyto$DateTime, label = T))

# Order month in calendar order rather than (default) alphabetical
phyto$Month = factor(phyto$Month, levels = month.abb)

# Units for Density (unit, biovolume) are in per mL, will convert to per L 
phyto <- phyto %>% mutate(across(Units.per.mL:BV.um3.per.mL, ~ .x * 1000,.keep = "unused"))

# Rename headers b/c units are now in L
phyto <- phyto %>% 
  rename("Units.per.L" = "Units.per.mL") %>%
  rename("BV.um3.per.L" = "BV.um3.per.mL")

# Reorder columns
phyto <- phyto %>%
  relocate(Year, .after = DateTime) %>%
  relocate(Month, .after = DateTime)

# Rename FullCode column to something more descriptive
phyto <- phyto %>% rename("SampleType" = "FullCode")

# Distinguish between Regular Samples and Surface Tows
phyto$SampleType <- gsub("E0722B1412","Regular",phyto$SampleType)
phyto$SampleType <- gsub("E0622B1201","Regular",phyto$SampleType)
phyto$SampleType <- gsub("D19 Microcystis Tow","Surface Tow",phyto$SampleType)

# Make sure all stations are named D19
phyto$StationCode <- gsub("D19 MC Tow","D19",phyto$StationCode)

# Remove unneeded columns
phyto <- phyto %>% select(!("DepthM"))

# Subset to just Microcystis data
MC <- phyto %>% filter(Genus == "Microcystis")



# Save data file
save(phyto_gen_EMP, file = "RData/phyto_gen_EMP.RData") 
