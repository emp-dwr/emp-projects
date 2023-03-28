# Extracting phyto data from EMP datasheets ------------------------------------
# 01/03/2023 -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(ggpubr)
library(rstatix)
library(car)
library(visreg)
library(emmeans)

# Set working directory --------------------------------------------------------
setwd("./03_Phyto/surface-phyto-study")
getwd()

# Set directory for storing plots ----------------------------------------------
output <- "plots"

# Clean workspace --------------------------------------------------------------
rm(list=ls())

# Set graphing theme -----------------------------------------------------------
theme_set(theme_bw())

# Import data files ------------------------------------------------------------
phyto_files <- dir(path = "data/", pattern = "\\.csv", full.names = T)

df_phyto <- map_dfr(phyto_files, ~read_csv(.x))

# Clean up column names --------------------------------------------------------
df_phyto <- df_phyto %>% clean_names(case = "big_camel")

df_phyto <- df_phyto %>% rename("GALD" = "Gald1")

# Format date column -----------------------------------------------------------
df_phyto$SampleDate <- mdy(df_phyto$SampleDate)

# Calculate Unit Density & Biovolume Density -----------------------------------
df_phyto <- df_phyto %>%
  mutate(Units.per.mL = UnitAbundance * Factor) %>%
  mutate(BV.um3.per.mL= TotalCellsCounted * Biovolume1 * Factor)

# Fix EMP site names -----------------------------------------------------------
sort(unique(df_phyto$StationCode)) # there are several typos in station names

df_phyto$StationCode <- gsub("D16 Twitchell","D16",df_phyto$StationCode)
df_phyto$StationCode <- gsub("D16-Twitchell","D16",df_phyto$StationCode)
df_phyto$StationCode <- gsub("C3A- Hood","C3A",df_phyto$StationCode)
df_phyto$StationCode <- gsub("C3A-Hood","C3A",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ2SJR","EZ2-SJR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ2 SJR","EZ2-SJR",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ6 SAC","EZ6",df_phyto$StationCode)
df_phyto$StationCode <- gsub("EZ6 SJR","EZ6-SJR",df_phyto$StationCode)

length(unique(df_phyto$StationCode)) # confirm only 28 stations

# Remove unneeded columns ------------------------------------------------------
df_phyto <- df_phyto %>% 
  select(SampleDate:StationCode, Taxon:Genus, SampleType:BV.um3.per.mL)

# Add month data from DateTime column ------------------------------------------
df_phyto <- df_phyto %>% mutate(Month = month(SampleDate, label = T))

# Add in region and season data ------------------------------------------------
regions <- read_csv("regions.csv")
df_phyto <- left_join(df_phyto, regions)

# Fix region data for EZ-SJR samples -------------------------------------------
df_phyto <- df_phyto %>%
  mutate(Region = case_when(is.na(Region) ~ "Low Salinity Zone",
                            TRUE ~ Region))

df_phyto <- df_phyto %>%
  mutate(RegionAbbreviation = case_when(is.na(RegionAbbreviation) ~ "LSZ",
                                        TRUE ~ RegionAbbreviation))

# Set display order for regions ------------------------------------------------
# Display regions E -> W (generally)
region.order <- c("Northern Interior Delta",
                  "Central Delta",
                  "Southern Interior Delta",
                  "Confluence",
                  "Low Salinity Zone",
                  "Grizzly.Suisun Bay",
                  "San Pablo Bay")

df_phyto$Region <- factor(as.character(df_phyto$Region), levels = region.order)

region.abbv.order <- c("NID","CED","SID","CON","LSZ","GSB","SPB")

df_phyto$RegionAbbreviation <- factor(as.character(df_phyto$RegionAbbreviation), levels = region.abbv.order)


# Add month data from DateTime column ------------------------------------------
seasons <- read_csv("seasons.csv")
df_phyto <- left_join(df_phyto, seasons)

# Summarize data by genus ------------------------------------------------------
df_phyto_gen <- df_phyto %>%
  group_by(SampleDate, Season, Month, StationCode, Region, RegionAbbreviation, SampleType, Genus) %>%
  summarize(across(Units.per.mL:BV.um3.per.mL, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

# Summarize data by algal group ------------------------------------------------
df_phyto_grp <- df_phyto %>%
  group_by(SampleDate, Season, Month, StationCode, Region, RegionAbbreviation, SampleType, AlgalType) %>%
  summarize(across(Units.per.mL:BV.um3.per.mL, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

# Create unique column for use in Primer with date and station ID --------------
df_phyto_gen <- df_phyto_gen %>% unite(SampleID, 
                               c("SampleDate", "StationCode","SampleType"),
                               sep = "-",
                               remove = FALSE)

df_phyto_grp <- df_phyto_grp %>% unite(SampleID, 
                                       c("SampleDate", "StationCode","SampleType"),
                                       sep = "-",
                                       remove = FALSE)

# Plot data to make sure the scale is ~approx correct and not orders of --------
# magnitude off (might indicate problematic unit conversion) -------------------
plot <- ggplot(data = df_phyto_gen, aes(x = Month, 
                                        y = BV.um3.per.mL, 
                                        color = SampleType)) +
  geom_jitter(width = 0.2, size = 2)

plot # looks good

# Export data for analysis in PRIMER -------------------------------------------
write_csv(df_phyto_gen, file = "primer/df_phyto_gen.csv")
write_csv(df_phyto_grp, file = "primer/df_phyto_grp.csv")

# Make boxplots for IEP --------------------------------------------------------
df_phyto_tot <- df_phyto %>%
  group_by(SampleDate, Season, Month, StationCode, Region, RegionAbbreviation, SampleType) %>%
  summarize(across(Units.per.mL:BV.um3.per.mL, ~sum(.x, na.rm = TRUE))) %>%
  ungroup

# Remove samples with no region (EZ samples)
boxplot <- ggplot(data = df_phyto_tot, aes(x = SampleType, 
                                           y = log10(BV.um3.per.mL))) +
  geom_boxplot(width = 0.2)

boxplot +
  facet_wrap(Region ~ ., ncol = 4) +
  labs(x = NULL,
       y = "Log10 Biovolume (um^3 per mL)",
       title = "Comparison of Phytoplankton Biovolume by Sampling Method")

ggsave(path = output,
       filename = "phyto_boxplots_by_Region.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=8, 
       dpi="print")

## Create QQ Plot to check for normality
ggqqplot(log10(df_phyto_tot$BV.um3.per.mL))

## View histogram to check for normality
hist(log10(df_phyto_tot$BV.um3.per.mL))

## Run Shapiro-Wilks test to check whether data is normally distributed
shapiro.test(log10(df_phyto_tot$BV.um3.per.mL))

## Rosie Code
t.test(df_phyto_tot$BV.um3.per.mL~df_phyto_tot$SampleType)

L1 <- lm(log10(BV.um3.per.mL) ~ SampleType, data = df_phyto_tot)

summary(L1)
