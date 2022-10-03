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

# Clean workspace
rm(list=ls()) 

# Set directory for storing plots
output <- "plots"

# Set visual theme in ggplot
theme_set(theme_bw())

# Import EMP data files
phyto_files <- dir(path = "data/", pattern = "\\.csv", full.names = T)

df_phyto_all <- map_dfr(phyto_files, ~read_csv(.x))

# Clean up column names
df_phyto <- df_phyto_all %>% clean_names(case = "big_camel")

# Remove pre-calculated Unit Density and blank columns
df_phyto <- df_phyto %>% select(MethodCode:Biovolume10)

# Remove empty rows
df_phyto <- df_phyto %>% filter_all(any_vars(!is.na(.)))

# Average all 10 biovolume measurements for each taxon
df_phyto <- df_phyto %>% rowwise() %>% mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T))

# Remove Individual Biovolume Columns
df_phyto <- df_phyto %>% select(!(Biovolume1:Biovolume10))

# Remove unneeded columns
df_phyto <- df_phyto %>% select(!c("MethodCode","BsaTin","DiatomSoftBody","Synonym"))
df_phyto <- df_phyto %>% select(!(ColonyFilamentIndividualGroupCode:Shape))
df_phyto <- df_phyto %>% select(!(VolumeReceivedML:NumberOfFieldsCounted))

# Ensure dates in the right format
df_phyto$SampleDate <- mdy(df_phyto$SampleDate)

# Combine date and time column
df_phyto <- df_phyto %>% unite(DateTime, c("SampleDate","SampleTime"), sep = " ") #, remove = FALSE, na.rm = FALSE)

# Correct error in data for June 2022
df_phyto$DateTime <- gsub("2022-06-11 13:45:00","2022-06-21 13:45:00",df_phyto$DateTime)

df_phyto$DateTime <- as_datetime(df_phyto$DateTime, 
                              tz = "US/Pacific",
                              format = c("%Y-%m-%d %H:%M:%OS"))

# Check for missing dates
df_phyto %>% filter(is.na(DateTime)) ## No missing dates

# Calculate Unit Density & Biovolume Density
df_phyto <- df_phyto %>%
  mutate(Units.per.mL = UnitAbundanceNumberOfNaturalUnits * Factor) %>%
  mutate(BV.um3.per.mL= TotalNumberOfCells * BV.Avg * Factor)

## Add column for year and month for highlighting data
df_phyto <- df_phyto %>% mutate(Year = year(df_phyto$DateTime))

df_phyto <- df_phyto %>% mutate(Month = month(df_phyto$DateTime, label = T))

df_phyto$Year <- as.factor(df_phyto$Year)

## Remove columns no longer needed
df_phyto <- df_phyto %>% 
  select(!(Species:BV.Avg)) %>% 
  select(!(Factor))

# List of stations 
list(unique(df_phyto$StationCode))

# Filter only D19 and tow stations
df_phyto <- df_phyto %>% filter(StationCode %in% c("D19","D19 MC Tow"))

# Confirm station IDs
unique(df_phyto$StationCode)
table(df_phyto$StationCode)

sort(unique(df_phyto$Genus)) ## 24 unique genera

# Add column for year and month for highlighting data
df_phyto <- df_phyto %>% mutate(Year = year(df_phyto$DateTime))
df_phyto <- df_phyto %>% mutate(Month = month(df_phyto$DateTime, label = T))

# Order month in calendar order rather than (default) alphabetical
df_phyto$Month = factor(df_phyto$Month, levels = month.abb)

# Units for Density (unit, biovolume) are in per mL, will convert to per L 
df_phyto <- df_phyto %>% mutate(across(Units.per.mL:BV.um3.per.mL, ~ .x * 1000,.keep = "unused"))

# Rename headers b/c units are now in L
df_phyto <- df_phyto %>% 
  rename("Units.per.L" = "Units.per.mL") %>%
  rename("BV.um3.per.L" = "BV.um3.per.mL")

# Reorder columns
df_phyto <- df_phyto %>%
  relocate(Year, .after = DateTime) %>%
  relocate(Month, .after = DateTime)

# Rename FullCode column to something more descriptive
df_phyto <- df_phyto %>% rename("SampleType" = "FullCode")

# Distinguish between Regular Samples and Surface Tows
df_phyto$SampleType <- gsub("E0722B1412","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("E0622B1201","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("D19 Microcystis Tow","Surface Tow",df_phyto$SampleType)

# Make sure all stations are named D19
df_phyto$StationCode <- gsub("D19 MC Tow","D19",df_phyto$StationCode)

# Remove unneeded columns
df_phyto <- df_phyto %>% select(!("DepthM"))

# Subset to just Microcystis data
#MC <- df_phyto %>% filter(Genus == "Microcystis")

# Read in flowmeter data
df_flowmeter <- read.csv(file = "data/flowmeter-data.csv")

# Make sure dates and times are in correct format
df_flowmeter$DateTime <- as_datetime(df_flowmeter$DateTime, 
                              tz = "US/Pacific")

# Combine flowmeter data with surface tow samples
df_phyto_tow <- df_phyto %>% filter(SampleType == "Surface Tow")
df_phyto_reg <- df_phyto %>% filter(SampleType == "Regular")

df_phyto_tow <- left_join(df_phyto_tow, df_flowmeter)

# Calculate distance traveled by flowmeter
# Rotor Constant is 26873

rm(df_flowmeter)

df_phyto_tow <- df_phyto_tow %>%
  mutate(TowDistance = (FlowmeterPost - FlowmeterPre)*26873/999999)

# Calculate volume in liters using Tomo's formulae
# Net diameter = 0.3 meters
df_phyto_tow <- df_phyto_tow %>%
  mutate(TowVolume = pi * (0.15)^2 * TowDistance * 1000)

# Calculate actual concentration in tow samples using C1V1 = C2V2
df_phyto_tow <- df_phyto_tow %>%
  mutate(BV.um3.per.L.Tow = case_when(SampleType == "Surface Tow" ~ BV.um3.per.L * TowVolumeL / TowVolume)) 

df_phyto_tow <- df_phyto_tow %>% select(DateTime:Genus, BV.um3.per.L.Tow)
df_phyto_reg <- df_phyto_reg %>% select(DateTime:Genus, BV.um3.per.L)

df_phyto_tow <- df_phyto_tow %>% rename("BV.um3.per.L" = "BV.um3.per.L.Tow")

# Recombine tow and regular samples
df_phyto <- bind_rows(df_phyto_reg, df_phyto_tow)

rm(df_phyto_reg)
rm(df_phyto_tow)

## Calculate the most abundant taxa for each sample type
df_abund <- df_phyto %>%
  group_by(SampleType, Genus) %>%
  summarize(Mean.BV.per.L = mean(BV.um3.per.L)) %>%
  ungroup()

df_abund <- df_abund %>%
  group_by(SampleType) %>%
  mutate(MeanRelAbund = Mean.BV.per.L/sum(Mean.BV.per.L)) %>%
  ungroup

# Highlight most abundant genera (avg abundance > 1%)
df_abund <- df_abund %>%
  mutate(Type = case_when(MeanRelAbund > 0.01 ~ Genus,
                          TRUE ~ 'Other'))

df_phyto_type <- df_abund %>% select(Genus,Type)

df_phyto_type <- unique(df_phyto_type)

# Read in taxonomy classification
df_classification <- read_csv(file = "phyto_group_classification.csv")

df_phyto_type <- left_join(df_phyto_type,df_classification)

# Combine with complete phyto dataset
df_phyto <- left_join(df_phyto,df_phyto_type, by = "Genus")

rm(df_abund)
rm(df_classification)
rm(df_phyto_type)

# Rearrange columns
df_phyto <- df_phyto %>%
  relocate(Group, .after = Genus) %>%
  relocate(Type, .after = Group)

# Lump together less abundant taxa
df_phyto_RA <- df_phyto %>%
  group_by(DateTime, Year, Month, SampleType, Group, Type) %>%
  summarize(Mean.BV.per.L = mean(BV.um3.per.L)) %>%
  ungroup()

df_phyto_RA <- df_phyto_RA %>%
  group_by(DateTime, Year, Month, SampleType) %>%
  mutate(MeanRelAbund = Mean.BV.per.L/sum(Mean.BV.per.L)*100) %>%
  ungroup

df_phyto_RA <- df_phyto_RA %>%
  group_by(DateTime, Month, SampleType, Type) %>%
  summarize(MeanRelAbund = sum(MeanRelAbund)) %>%
  ungroup()

# Compare taxonomy
bar.plot <- ggplot(df_phyto, aes(x = Month, y = BV.um3.per.L, fill = Genus)) +
  geom_bar(position = "stack",  
           width = 1, 
           stat = "summary", 
           fun = "mean")

bar.plot + 
  facet_wrap(SampleType ~ ., ncol = 1, scale = "free_y")

# Compare taxonomy (re;lative abundance)
bar.plot.RA <- ggplot(df_phyto_RA, aes(x = SampleType, y = MeanRelAbund, fill = Type)) +
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  scale_fill_brewer(palette = "Paired")

bar.plot.RA + 
  facet_wrap(DateTime ~ ., ncol = 1) +
  labs(x = "Sample Type",
       y = "Relative Abundance (%)",
       fill = "Genus",
       title = "Phytoplankton Sample Comparison - D19")

ggsave(path = output,
       filename = "phyto_RA.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5.5,
       width=5.5, 
       dpi="print")

# Save data file
save(df_phyto, file = "RData/phyto.RData") 
