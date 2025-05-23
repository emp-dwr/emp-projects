# Load Libraries and Data Files ------------------------------------------------
# Process EMP Phyto data to get surface tow data only for D19
# Calculate correct biovolume using flowmeter equations from Tomo Kurobe
# Started: 9/30/2022

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(janitor)
library(vegan)

# Set working directory
setwd("C:/R/emp-projects/")
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
df_phyto <- df_phyto %>% rowwise() %>% 
  mutate(BV.Avg = mean(c_across(Biovolume1:Biovolume10), na.rm = T)) %>%
  select(!(Biovolume1:Biovolume10))

# Remove unneeded columns
df_phyto <- df_phyto %>% select(!c("MethodCode","BsaTin","DiatomSoftBody",
                                   "Synonym"))
df_phyto <- df_phyto %>% select(!(ColonyFilamentIndividualGroupCode:Shape))
df_phyto <- df_phyto %>% select(!(VolumeReceivedML:NumberOfFieldsCounted))

# Ensure dates in the right format
df_phyto$SampleDate <- mdy(df_phyto$SampleDate)

# Combine date and time column
df_phyto <- df_phyto %>% unite(DateTime, 
                               c("SampleDate","SampleTime"), 
                               sep = " ")

# Correct error in data for June 2022 & November 2021
df_phyto$DateTime <- gsub("2022-06-11 13:45:00","2022-06-21 13:45:00",
                          df_phyto$DateTime)
df_phyto$DateTime <- gsub("2021-11-19 12:00:00","2021-11-09 12:00:00",
                          df_phyto$DateTime)

df_phyto$DateTime <- as_datetime(df_phyto$DateTime, 
                              tz = "US/Pacific",
                              format = c("%Y-%m-%d %H:%M:%OS"))

# Check for missing dates
df_phyto %>% filter(is.na(DateTime)) # No missing dates

# Calculate Unit Density & Biovolume Density -----------------------------------
df_phyto <- df_phyto %>%
  mutate(Units.per.mL = UnitAbundanceNumberOfNaturalUnits * Factor) %>%
  mutate(BV.um3.per.mL= TotalNumberOfCells * BV.Avg * Factor)

## Add column for year and month for highlighting data
df_phyto <- df_phyto %>% mutate(Year = year(df_phyto$DateTime))

df_phyto <- df_phyto %>% mutate(Month = month(df_phyto$DateTime, label = T))

# Change year to Factor for plotting
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

sort(unique(df_phyto$Genus)) ## 37 unique genera

# Order month in calendar order rather than (default) alphabetical
df_phyto$Month = factor(df_phyto$Month, levels = month.abb)

# Units for Density (unit, biovolume) are in per mL, will convert to per L 
#df_phyto <- df_phyto %>% mutate(across(Units.per.mL:BV.um3.per.mL, ~ .x * 1000,.keep = "unused"))

# Rename headers b/c units are now in L
#df_phyto <- df_phyto %>% 
#  rename("Units.per.L" = "Units.per.mL") %>%
#  rename("BV.um3.per.L" = "BV.um3.per.mL")

# Reorder columns
df_phyto <- df_phyto %>%
  relocate(Year, .after = DateTime) %>%
  relocate(Month, .after = DateTime)

# Rename FullCode column to something more descriptive
df_phyto <- df_phyto %>% rename("SampleType" = "FullCode")

# Distinguish between Regular Samples and Surface Tows
df_phyto$SampleType <- gsub("E0722B1412","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("E0622B1201","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("D19 Microcystis Tow","Surface Tow",
                            df_phyto$SampleType)
df_phyto$SampleType <- gsub("MC TOW", "Surface Tow",df_phyto$SampleType)
df_phyto$SampleType <- gsub("E1121B2010","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("E0522B0940","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("E082B1736","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("Microcystis Tow", "Surface Tow",
                            df_phyto$SampleType)
df_phyto$SampleType <- gsub("D19-Surface Tow", "Surface Tow",
                            df_phyto$SampleType)
df_phyto$SampleType <- gsub("E0922B2033","Regular",df_phyto$SampleType)
df_phyto$SampleType <- gsub("E1022B2288","Regular",df_phyto$SampleType)

# Make sure all SampleType is Regular or Surface Tow
unique(df_phyto$SampleType)

# Make sure all stations are named D19
df_phyto$StationCode <- gsub("D19 MC Tow","D19",df_phyto$StationCode)

# Remove unneeded columns
df_phyto <- df_phyto %>% select(!("DepthM"))

# Read in flowmeter data
df_flowmeter <- read.csv(file = "flowmeter-data.csv")

# Make sure dates and times are in correct format
df_flowmeter$DateTime <- as_datetime(df_flowmeter$DateTime, 
                              tz = "US/Pacific",
                              format = "%m/%d/%Y %H:%M")

# Combine flowmeter data with surface tow samples
df_phyto_tow <- df_phyto %>% filter(SampleType == "Surface Tow")
df_phyto_reg <- df_phyto %>% filter(SampleType == "Regular")

df_phyto_tow <- left_join(df_phyto_tow, df_flowmeter)

# Calculate distance traveled by flowmeter
# Rotor Constant is 26873

rm(df_flowmeter)

df_phyto_tow <- df_phyto_tow %>%
  mutate(TowDistance = (FlowmeterPost - FlowmeterPre)*26873/999999)

# Calculate volume in milliliters using Tomo's formulae
# Net diameter = 0.3 meters
df_phyto_tow <- df_phyto_tow %>%
  mutate(TowVolume.mL = pi * (0.15)^2 * TowDistance * 1000 * 1000)

# Calculate actual concentration in tow samples using C1V1 = C2V2
df_phyto_tow <- df_phyto_tow %>%
  mutate(BV.um3.per.mL.Tow = case_when(SampleType == "Surface Tow" ~ BV.um3.per.mL * (TowVolumeL*1000) / TowVolume.mL)) 

df_phyto_tow <- df_phyto_tow %>% select(DateTime:Genus, BV.um3.per.mL.Tow)
df_phyto_reg <- df_phyto_reg %>% select(DateTime:Genus, BV.um3.per.mL)

df_phyto_tow <- df_phyto_tow %>% rename("BV.um3.per.mL" = "BV.um3.per.mL.Tow")

# Recombine tow and regular samples
df_phyto <- bind_rows(df_phyto_reg, df_phyto_tow)

rm(df_phyto_reg)
rm(df_phyto_tow)

## Calculate the most abundant taxa for each sample type
df_abund <- df_phyto %>%
  group_by(SampleType, Genus) %>%
  summarize(Mean.BV.per.mL = mean(BV.um3.per.mL)) %>%
  ungroup()

df_abund <- df_abund %>%
  group_by(SampleType) %>%
  mutate(MeanRelAbund = Mean.BV.per.mL/sum(Mean.BV.per.mL)) %>%
  ungroup

# Import list of toxigenic genera
tox <- read_csv("toxin_producer_list.csv")

df_abund <- left_join(df_abund, tox)

# Highlight potentially toxic genera
df_abund <- df_abund %>%
  mutate(Toxic = case_when(Toxingenic == "Potential Toxin Producer" ~ "Toxic",
                           TRUE ~ 'Non-toxic'))

df_abund$Toxingenic <- NULL

# Highlight most abundant genera (avg abundance > 1%)
 df_abund <- df_abund %>%
   mutate(Type = case_when(Toxic == 'Toxic' ~ Genus,
                           TRUE ~ 'Non-toxic'))

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

# Remove data from 2021
df_phyto <- df_phyto %>%
  filter(Year != 2021)

# Lump together less abundant taxa
df_phyto_RA <- df_phyto %>%
  group_by(DateTime, Year, Month, SampleType, Type) %>%
  summarize(Mean.BV.per.mL = mean(BV.um3.per.mL)) %>%
  ungroup()

df_phyto_RA <- df_phyto_RA %>%
  group_by(DateTime, Year, Month, SampleType) %>%
  mutate(MeanRelAbund = Mean.BV.per.mL/sum(Mean.BV.per.mL)*100) %>%
  ungroup

df_phyto_RA <- df_phyto_RA %>%
  group_by(DateTime, Month, SampleType, Type) %>%
  summarize(MeanRelAbund = sum(MeanRelAbund)) %>%
  ungroup()

## Re-order genera in "Type" category
type <- c("Aphanizomenon","Planktothrix","Dolichospermum","Microcystis","Oscillatoria","Non-toxic")
df_phyto_RA$Type <- factor(as.character(df_phyto_RA$Type), levels = type)

# Compare taxonomy
bar.plot <- ggplot(df_phyto, aes(x = Month, y = BV.um3.per.mL, fill = Group)) +
  geom_bar(position = "stack",  
           width = 1, 
           stat = "summary", 
           fun = "sum") +
  scale_fill_brewer(palette = "Set1")

bar.plot + 
  facet_wrap(SampleType ~ ., ncol = 1) +
  labs(x = "Month",
       y = "Biovolume Density (um^3 per mL)",
       fill = "Group")

ggsave(path = output,
       filename = "phyto_total_BV.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Compare taxonomy (relative abundance)
bar.plot.RA <- ggplot(df_phyto_RA, aes(x = Month, 
                                       y = MeanRelAbund, 
                                       fill = Type)) +
  geom_bar(position = "stack",  
           width = 0.6, 
           stat = "summary", 
           fun = "mean") +
  scale_fill_brewer(palette = "Dark2")

bar.plot.RA + 
  facet_wrap(SampleType ~ ., ncol = 2) +
  labs(x = "Month",
       y = "Relative Abundance (%)",
       fill = "Genus")

ggsave(path = output,
       filename = "phyto_RA_PTOX.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=4,
       width=7, 
       dpi="print")

# Comparison of PTOX and other taxa --------------------------------------------
df_phyto_comp <- df_phyto_RA %>%
  mutate(ToxicStatus = case_when(Type == "Non-toxic" ~ "Non-toxic",
                                 TRUE ~ "PTOX"))

# RA Proportion
df_phyto_comp <- df_phyto_comp %>%
  group_by(DateTime, Month, SampleType, ToxicStatus) %>%
  summarize(Proportion = sum(MeanRelAbund, na.rm = T)) %>%
  ungroup()

#Total proportion
df_phyto_tot <- df_phyto %>%
  group_by(DateTime, Month, SampleType) %>%
  summarize(Total.BV = sum(BV.um3.per.mL, na.rm = T)) %>%
  ungroup()

df_phyto_tot <- pivot_wider(df_phyto_tot, 
                             names_from = SampleType,
                             values_from = Total.BV)

df_phyto_tot <- df_phyto_tot %>%
  mutate(Prop.Surf = `Surface Tow` / Regular)

# Import EMP historical data for comparison ------------------------------------
load("df_phyto_gen.RData")

df_EMP <- df_phyto_gen
rm(df_phyto_gen)

# Select data from only D19 for analysis
df_EMP <- df_EMP %>%
  filter(StationCode == "D19") %>%
  filter(Year != 2013)

df_EMP <- left_join(df_EMP, tox)

# Highlight potentially toxic genera
df_EMP <- df_EMP %>%
  mutate(Toxic = case_when(Toxingenic == "Potential Toxin Producer" ~ "Toxic",
                           TRUE ~ 'Non-toxic'))

df_EMP$Toxingenic <- NULL

# Highlight most abundant genera (avg abundance > 1%)
df_abund <- df_EMP %>%
  mutate(Type = case_when(Toxic == 'Toxic' ~ Genus,
                          TRUE ~ 'Non-toxic'))

df_phyto_type <- df_abund %>% select(Genus,Type)

df_phyto_type <- unique(df_phyto_type)

# Combine with complete phyto dataset
df_EMP <- left_join(df_EMP,df_phyto_type, by = "Genus")

rm(df_abund)
rm(df_phyto_type)

# Lump together less abundant taxa
df_EMP_tox <- df_EMP %>%
  group_by(DateTime, Year, Month, Group, Type) %>%
  summarize(Mean.BV.per.mL = mean(BV.um3.per.mL)) %>%
  ungroup()

df_EMP_tox <- df_EMP_tox %>%
  group_by(DateTime, Year, Month, Type) %>%
  summarize(Mean.BV.per.mL = sum(Mean.BV.per.mL)) %>%
  ungroup()

# Relative abundance calcs
df_EMP_RA <- df_EMP %>%
  group_by(DateTime, Year, Month, Group, Type) %>%
  summarize(Mean.BV.per.mL = mean(BV.um3.per.mL)) %>%
  ungroup()

df_EMP_RA <- df_EMP_RA %>%
 group_by(DateTime, Year, Month) %>%
 mutate(MeanRelAbund = Mean.BV.per.mL/sum(Mean.BV.per.mL)*100) %>%
 ungroup
 
df_EMP_RA <- df_EMP_RA %>%
 group_by(DateTime, Year, Month, Type) %>%
 summarize(MeanRelAbund = sum(MeanRelAbund)) %>%
 ungroup()

## Re-order genera in "Type" category
type <- c("Aphanizomenon","Cylindrospermopsis","Dolichospermum","Microcystis","Oscillatoria","Non-toxic")
df_EMP_RA$Type <- factor(as.character(df_EMP_RA$Type), levels = type)

# Compare taxonomy (monthly total abundance)
bar.plot.EMP <- ggplot(df_EMP_tox, aes(x = Month, 
                                       y = Mean.BV.per.mL, 
                                       fill = Type)) +
  geom_bar(position = "stack",  
           width = 0.3, 
           stat = "summary", 
           fun = "mean") 

bar.plot.EMP +
  facet_wrap(Year ~ ., ncol = 2) +
  labs()

# Compare taxonomy (relative abundance)
bar.plot.EMP.RA <- ggplot(df_EMP_RA, aes(x = Month, 
                                         y = MeanRelAbund, 
                                         fill = Type)) +
  geom_bar(position = "stack",  
           width = 0.3, 
           stat = "summary", 
           fun = "mean") +
  scale_x_discrete(breaks = c("Jan","Apr","Jul","Oct")) +
  scale_fill_brewer(palette = "Dark2")

bar.plot.EMP.RA +
  facet_wrap(Year ~ ., ncol = 4) +
  labs(x = NULL,
       y = "Relative Abundance (%)",
       fill = "Genus")

ggsave(path = output,
       filename = "phyto_EMP_RA.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=3.5,
       width=6.5, 
       dpi="print")

# Read in EDI Data -------------------------------------------------------------
df_phyto_EDI <- read_csv("EMP_Phyto_Data_2008-2021.csv")

df_phyto_EDI <- df_phyto_EDI %>%
  filter(StationCode == "D19")

df_phyto_EDI <- df_phyto_EDI %>%
  select(Lab:SampleDate,StationCode,Name,Genus,Organisms_per_mL)

df_phyto_EDI$SampleDate <- mdy(df_phyto_EDI$SampleDate )

df_phyto_EDI <- df_phyto_EDI %>%
  mutate(Year = year(df_phyto_EDI$SampleDate)) %>%
  mutate(Month = month(df_phyto_EDI$SampleDate, label = T))

df_phyto_EDI$Genus <- gsub("Raphidiopsis","Cylindrospermopsis",df_phyto_EDI$Genus)

tox2 <- read_csv("toxin_producer_list_short.csv")

df_phyto_EDI <- left_join(df_phyto_EDI, tox2)

# Highlight potentially toxic genera
df_phyto_EDI <- df_phyto_EDI %>%
  mutate(Toxic = case_when(Toxingenic == "Potential Toxin Producer" ~ "Toxic",
                           TRUE ~ 'Non-toxic'))

df_phyto_EDI$Toxingenic <- NULL

# Highlight most abundant genera (avg abundance > 1%)
df_abund <- df_phyto_EDI %>%
  mutate(Type = case_when(Toxic == 'Toxic' ~ Genus,
                          TRUE ~ 'Non-toxic'))

df_phyto_type <- df_abund %>% select(Genus,Type)

df_phyto_type <- unique(df_phyto_type)

# Combine with complete phyto dataset
df_phyto_EDI <- left_join(df_phyto_EDI,df_phyto_type, by = "Genus")

# Relative abundance calcs
df_phyto_EDI_RA <- df_phyto_EDI %>%
  group_by(SampleDate, Year, Month, Genus, Type) %>%
  summarize(Mean.Units.per.mL = mean(Organisms_per_mL)) %>%
  ungroup()

df_phyto_EDI_RA <- df_phyto_EDI_RA %>%
  group_by(SampleDate, Year, Month) %>%
  mutate(MeanRelAbund = Mean.Units.per.mL/sum(Mean.Units.per.mL)*100) %>%
  ungroup

df_phyto_EDI_RA <- df_phyto_EDI_RA %>%
  group_by(SampleDate, Year, Month, Type) %>%
  summarize(MeanRelAbund = sum(MeanRelAbund)) %>%
  ungroup()

unique(df_phyto_EDI_RA$Type)

# Compare taxonomy (relative abundance)
bar.plot.EDI.RA <- ggplot(df_phyto_EDI_RA, aes(x = Month, y = MeanRelAbund, fill = Type)) +
  geom_bar(position = "stack",  
           width = 0.3, 
           stat = "summary", 
           fun = "mean") +
  scale_fill_brewer(palette = "Set1")

bar.plot.EDI.RA +
  facet_wrap(Year ~ ., ncol = 2)

# Generate NMDS data with metaMDS ----------------------------------------------
df_NMDS <- df_phyto %>%
  group_by(DateTime, Month, SampleType, Genus) %>%
  summarize(Total.BV = sum(BV.um3.per.mL)) %>%
  ungroup()

genw <- pivot_wider(df_NMDS, 
                    names_from = "Genus", 
                    values_from = "Total.BV",
                    values_fill = 0)
  
# Calculate the nMDS using vegan 
# A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions,
# < 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.
phyto.NMDS <- metaMDS(
  comm = genw[c(4:39)],
  distance = "bray",
  k = 3,
  trymax = 50
  #trace = F,
  #autotransform = F
)
  

#look at Shepard plot which shows scatter around the regression between the interpoint distances 
#in the final configuration (i.e., the distances between each pair of communities) against their 
#original dissimilarities.
stressplot(phyto.NMDS)
  
# Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores <- as_tibble(scores(phyto.NMDS, display = "sites"))
  
# Combine metadata with NMDS data scores to plot in ggplot
meta <- genw %>% select(DateTime:SampleType)
meta <- cbind(meta, data.scores)

# Create NMDS plots for each year by Region
NMDS.plot <- ggplot(meta, aes(x = NMDS1, y = NMDS2, color = SampleType, label = Month)) +
  geom_point(size = 5) +
  geom_text(hjust = .5, vjust = -1, color = "black") +
  #stat_ellipse(level = 0.9, type = "t") + 
  #labs(title = "Phytoplankton Community Comparison - Summer 2022 - D19") +
  labs(color = "Sample Type") +
  scale_color_brewer(palette = "Set2") +
  theme_bw()

NMDS.plot

ggsave(path = output,
       filename = "phyto_NMDS.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Calculate ANOSIM
anosim(genw[4:39], genw$SampleType, distance = "bray")

# Import WQ data ---------------------------------------------------------------
df_WQ <- read_csv(file = "SACSJ_delta_water_quality_1975_2022.csv")

regions <- read_csv(file = "regions.csv")

regions <- regions %>%
  rename("Station" = "StationCode")

# Select only data needed
df_WQ <- df_WQ %>%
  select(Station:Date,Chla_Sign:Chla,Microcystis)

df_WQ <- left_join(df_WQ, regions)

df_WQ$Date <- mdy(df_WQ$Date, 
                      tz = "US/Pacific")

# Check for missing dates
df_WQ %>% filter(is.na(Date)) # No missing dates

# Add month and year columns
df_WQ <- df_WQ %>%
  mutate(Month = month(Date, label = T)) %>%
  mutate(Year = year(Date))

# Order month in calendar order rather than (default) alphabetical
df_WQ$Month = factor(df_WQ$Month, levels = month.abb)

# Round up fractional MC scores
df_WQ$Microcystis <- round_half_up(df_WQ$Microcystis)

df_WQ_NA <- df_WQ %>%
  filter(is.na(Region))

table(df_WQ_NA$Station)

# Plot Chlorophyll a concentrations at D19 -------------------------------------

# Filter out non-D19 samples
df_WQ_D19 <- df_WQ %>%
  filter(Station == "D19")

Chla.plot <- ggplot(df_WQ_D19, aes(x = Year, y = Chla, color = Month)) +
  geom_point()

Chla.plot

# Plot MC Scores ---------------------------------------------------------------

# Remove samples that don't have MC scores
df_MC_scores <- df_WQ %>%
  filter(!is.na(Microcystis)) %>%
  filter(Year > 2015)

# Make MC Scores factors
df_MC_scores$Microcystis <- as.factor(df_MC_scores$Microcystis)

df_MC_scores <- df_MC_scores %>% count(Region, Year, Microcystis, sort = TRUE)

df_MC_scores <- df_MC_scores %>%
  group_by(Region, Year) %>%
  mutate(Proportion = n / sum(n) * 100) %>%
  ungroup

# Make years factors for plotting
df_MC_scores$Year <- as.factor(df_MC_scores$Year)

MC.plot <- ggplot(df_MC_scores, aes(y = Proportion, x = Year, fill = Microcystis)) +
  geom_bar(position = "stack",  
           width = 0.5, 
           stat = "summary", 
           fun = "sum") +
  scale_x_discrete(breaks = c("2016","2018","2020","2022")) +
  scale_fill_brewer(palette = "Set1")

MC.plot +
  facet_wrap(Region ~ ., ncol = 4) +
  labs(x = "Year",
       y = "Relative Proportion (%)",
       fill = "MC Index")

ggsave(path = output,
       filename = "MC_Index_scores_by_Region.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=8, 
       dpi="print")

# Make plot for D19 Only
df_MC_scores_D19 <- df_WQ %>%
  filter(!is.na(Microcystis)) %>%
#  filter(Year > 2015) %>%
  filter(Station == "D19")

#df_MC_scores_D19 <- df_MC_scores_D19 %>% count(Station, Year, Microcystis, sort = TRUE)

# df_MC_scores_D19 <- df_MC_scores_D19 %>%
#   group_by(Year) %>%
#   mutate(Proportion = n / sum(n) * 100) %>%
#   ungroup

# Plot MC Index at D19 ---------------------------------------------------------

MC.plot.D19 <- ggplot(df_MC_scores_D19, aes(y = Microcystis, x = Month)) +
  geom_col() +
  scale_x_discrete(breaks = c("Jan","Mar","May","Jul","Sep","Nov")) 
  #scale_fill_brewer(palette = "Set1")

MC.plot.D19 +
  facet_wrap(Year ~ ., ncol = 4) +
  labs(x = "Year",
       y = "Microcystis Visual Index")

ggsave(path = output,
       filename = "MC_Index_scores_D19.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=3.5,
       width=6.5, 
       dpi="print")

# Plot Chla concentration at D19 -----------------------------------------------
df_WQ_D19 <- df_WQ_D19 %>%
  filter(Year >= 2014 & Year < 2022)

Chla.plot.D19 <- ggplot(df_WQ_D19, aes(y = Chla, x = Month)) +
  geom_point(size = 2) +
  scale_x_discrete(breaks = c("Jan","Apr","Jul","Oct")) 
#scale_fill_brewer(palette = "Set1")

Chla.plot.D19 +
  facet_wrap(Year ~ ., ncol = 4, scale = "free_y") +
  labs(x = NULL,
       y = "Chlorophyll a (ug/L)")

ggsave(path = output,
       filename = "Chla_conc_D19.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=3.5,
       width=6.5, 
       dpi="print")

# Plot WQ from D19 in Summer 2022 only
df_WQ <- read_csv(file = "SACSJ_delta_water_quality_1975_2022.csv")

# Select only data needed
df_WQ <- df_WQ %>%
  select(Station:Date,Chla_Sign:Chla,Microcystis,DOSurface,WTSurface,pHSurface)

df_WQ <- left_join(df_WQ, regions)

df_WQ$Date <- mdy(df_WQ$Date, 
                  tz = "US/Pacific")

# Add month and year columns
df_WQ <- df_WQ %>%
  mutate(Month = month(Date, label = T)) %>%
  mutate(Year = year(Date))

# Order month in calendar order rather than (default) alphabetical
df_WQ$Month = factor(df_WQ$Month, levels = month.abb)

# Round up fractional MC scores
df_WQ$Microcystis <- round_half_up(df_WQ$Microcystis)

df_WQ_D19 <- df_WQ %>%
  filter(Station == "D19" & Year == 2022) %>%
  filter(Month %in% c("May","Jun","Jul","Aug","Sep","Oct")) %>%
  select(Station,Date,Month,Chla,Microcystis,DOSurface,WTSurface,pHSurface) 

df_WQ_D19 <- pivot_longer(df_WQ_D19, 
                          names_to = "Analyte", 
                          values_to = "Conc",
                          cols = Chla:pHSurface)

plot.D19.2022 <- ggplot(df_WQ_D19, aes(y = Conc, x = Month)) +
  geom_col() +
  scale_x_discrete(breaks = c("May","Jul","Sep")) 
#scale_fill_brewer(palette = "Set1")

plot.D19.2022 +
  facet_wrap(Analyte ~ ., ncol = 5, scale = "free_y") +
  labs(x = NULL)

ggsave(path = output,
       filename = "WQ_D19_2022.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=2,
       width=8.5, 
       dpi="print")
