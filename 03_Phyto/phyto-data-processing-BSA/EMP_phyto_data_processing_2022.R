# Extracting phyto data from EMP datasheets from BSA for 2022
# 1/10/2023

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")

# Set working directory
setwd("./03_Phyto/phyto-data-processing-BSA/")
getwd()

# Clean workspace
rm(list=ls())

# Import raw data sheets 2022 -
phyto_files_2022 <- dir(path = "data/EMP/2022", 
                        pattern = "\\.csv", 
                        full.names = T)

df_phyto_2022 <- map_dfr(phyto_files_2022, ~read_csv(.x))

# Remove empty rows ------------------------------------------------------------
df_phyto_2022 <- df_phyto_2022 %>% filter_all(any_vars(!is.na(.)))

# Clean up column names
df_phyto_2022 <- df_phyto_2022 %>% clean_names(case = "big_camel")

# Remove data from extra tows at D19
df_phyto_2022 <- df_phyto_2022 %>% 
  filter(FullCode != "Microcystis Tow") %>%
  filter(FullCode != "D19 Microcystis Tow") %>%
  filter(FullCode != "D19-Microcystis Tow")

# Remove extra data from special Frank's Tract sample in May 2022 --------------
df_phyto_2022 <- df_phyto_2022 %>% filter(FullCode != "Frank's Tract")

# Add in historical BSA data to eventually merge with
load("df_phyto.RData")

# Remove GALD columns (won't be used)
df_phyto_2022 <- df_phyto_2022 %>%
  select(!(Gald1:Gald3)) 

# Average all 10 biovolume measurements for each taxon
df_phyto_2022 <- df_phyto_2022 %>% rowwise() %>% 
  mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T)) %>% 
  select(!(Biovolume1:Biovolume10)) # Remove Individual Biovolume Columns

# Move BV Avg data 
df_phyto_2022 <- df_phyto_2022 %>% 
  relocate(BV.Avg, .after = TotalNumberOfCells) %>%
  relocate(DepthM, .after = DepthFt_2) %>%
  relocate(BsaTin, .after = DepthM) %>%
  relocate(DiatomSoftBody, .after = BsaTin) %>%
  relocate(Synonym, .after = DiatomSoftBody)
  
# Select data for further analyses
df_phyto_2022 <- df_phyto_2022 %>%
  select(SampleDate:SampleTime,StationCode,Factor:BV.Avg)

# Combine date and time column
df_phyto_2022 <- df_phyto_2022 %>% unite(DateTime, c("SampleDate","SampleTime"), sep = " ") #, remove = FALSE, na.rm = FALSE)

df_phyto_2022$DateTime <- as_datetime(df_phyto_2022$DateTime, 
                                 tz = "US/Pacific",
                                 format = c("%m/%d/%Y %H:%M:%OS"))


# Check for missing dates
df_phyto_2022 %>% filter(is.na(DateTime)) # No missing dates

# Rename headers to match older data
df_phyto_2022 <- df_phyto_2022 %>%
  rename("TotalCells" = "TotalNumberOfCells") %>%
  rename("UnitAbundance" = "UnitAbundanceNumberOfNaturalUnits")

# Calculate Unit Density & Biovolume Density
df_phyto_2022 <- df_phyto_2022 %>%
  mutate(Units.per.mL = UnitAbundance * Factor) %>%
  mutate(BV.um3.per.mL= TotalCells * BV.Avg * Factor)

# Add column for year and month for highlighting data
df_phyto_2022 <- df_phyto_2022 %>% 
  mutate(Year = year(df_phyto_2022$DateTime)) %>%
  mutate(Month = month(df_phyto_2022$DateTime, label = T))

# Order month in calendar order rather than (default) alphabetical
df_phyto_2022$Month = factor(df_phyto_2022$Month, levels = month.abb)

# Reorder date/time columns
df_phyto_2022 <- df_phyto_2022 %>% 
  relocate(Year, .after = DateTime) %>% 
  relocate(Month, .after = DateTime)

# Convert years to factors for plotting
df_phyto_2022$Year <- as.factor(df_phyto_2022$Year)

# Check EMP site names
sort(unique(df_phyto_2022$StationCode)) ## 28 stations spelled correctly

# Read in CSV with manually-added WoRMS classification
taxa <- read_csv("CSVs/phyto_group_classification.csv")

df_phyto_2022 <- left_join(df_phyto_2022, taxa)

# check if there are any NAs in Groups after the join
table(is.na(df_phyto_2022$Group)) # no NAs

# Code to print out groups with NAs to search for info and add back to table
#Group.NAs <- df_phyto_2022 %>% filter(is.na(Group))
#sort(unique(Group.NAs$Genus))

# Relocate Group column
df_phyto_2022 <- df_phyto_2022 %>% relocate(Group, .before = Taxon)

# Add Region data --------------------------------------------------------------
regions <- read_csv("CSVs/station_regions_EMP.csv")

# Combine region data and phyto data
df_phyto_2022 <- left_join(df_phyto_2022, regions)

# Reorder column
df_phyto_2022 <- df_phyto_2022 %>% relocate(Region, .after = StationCode)

# check if there are any NAs in Region after the join
table(is.na(df_phyto_2022$Region)) # no NAs

# Add column to indicate what study it came from
df_phyto_2022 <- df_phyto_2022 %>% mutate(Study = "EMP", .after = StationCode)

# Remove GALD from older data
df_phyto$GALD <- NULL

# Combine older EMP data with 2022 data ----------------------------------------
df_phyto <- bind_rows(df_phyto_2022, df_phyto)

# Write dataset for sharing
write.csv(df_phyto, file = "df_phyto_2022.csv")

# Filter out data for sharing with SFSU (NZ068 only) ---------------------------
# Select only data from 2019 - 2022
start.date = "2019-01-01 00:00:01"
end.date = "2022-12-31 11:59:59"

df_phyto_SFSU <- df_phyto %>%
  filter(StationCode == "NZ068") %>%
  filter(DateTime >= start.date & DateTime < end.date)

# Remove unneeded data columns
df_phyto_SFSU <- df_phyto_SFSU %>%
  select(DateTime:Region,Group:Species,Units.per.mL:BV.um3.per.mL)

# Write dataset for sharing
save(df_phyto_SFSU, file = "df_phyto_SFSU.RData")
write.csv(df_phyto_SFSU, file = "df_phyto_SFSU.csv")
