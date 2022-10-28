## Comparing Microcystis data 
## Collected at EMP sites 2014-2019
## 2/24/2022

library(tidyverse)
library(lubridate)
library(janitor)

# Set working directory
setwd("./03_Phyto/microcystin-score-comparison")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Read in EDI data from Tomo
df_MC <- read_csv("data/San Francisco Estuary cyanoHAB data 2014 to 2019_v1.csv")

# Remove non-EMP sites
df_MC <- df_MC %>%
  filter_at(vars(EMP_Site), all_vars(!is.na(.))) %>%
  filter(EMP_Site != "FieldCont") %>%
  filter(RepCont == 0)



# Remove unneeded cols in MC dataset
df_MC <- df_MC %>%
  select(EMP_Site, Collection_Date:Collection_Month,field.Water.temp:`ucd.elisa.ANA-a.dissolved.ugL`)

# Clean up column names
df_MC <- df_MC %>% clean_names(case = "snake")

## Rename columns to match
df_MC <- df_MC %>%
  rename(date = collection_date) %>%
  rename(station = emp_site) %>%
  rename(month = collection_month)

## Get dates to match
df_MC$date <- parse_date_time(df_MC$date, 
                      c("%Y-%m-%d"))

# Read in EDI from EMP Discrete Water Quality
df_EMP_WQ <- read_csv("data/SACSJ_delta_water_quality_1975_2020.csv")

# Clean up column names
df_EMP_WQ <- df_EMP_WQ %>% clean_names(case = "snake")

df_EMP_WQ$date <- parse_date_time(df_EMP_WQ$date, 
                           c("%m-%d-%Y"))

# Select only MC Score in WQ dataset
df_EMP_WQ <- df_EMP_WQ %>% select(station, date, microcystis) 

## EMP Phyto Data Massaging
## Import Phyto Data

df_EMP_phyto <- read_csv("data/EMP_1975_2020_phyto.csv")


## Remove unneeded columns
df_EMP_phyto[,c(2,4,5,8,9,10,12,13,14,15)] <- NULL

## Rename date column
df_EMP_phyto <- df_EMP_phyto %>%
  rename(Date = SampleDate)

## Convert other dates and times into POSIXct-compliant values
## then remove old values
df_EMP_phyto$Date <- parse_date_time(df_EMP_phyto$Date, 
                           c("%m-%d-%Y"))

## Remove stations apart from modern ones
stations <- c("D22","NZ068","D19","D12","D16","D41A","D41","D28A","D26","NZ325",
              "NZS42","D10","EZ2","EZ6","D8","EZ2-SJR","NZ002","NZ004","NZ032",
              "D7","C9","C3A","MD10A","C10A","D4","P8","D6","EZ6-SJR")

df_EMP_phyto <- df_EMP_phyto %>% filter(StationCode %in% stations)

## Rename Density & Station
df_EMP_phyto <- df_EMP_phyto %>% 
  rename("Unit.Density" = "Organisms_per_mL") %>%
  rename("Station" = "StationCode")

## Summarize by genus
df_EMP_phyto_gen <- df_EMP_phyto %>% 
  group_by(Date, Station, Genus) %>%
  summarize(Unit.Density = sum(Unit.Density, na.rm = T)) %>%
  ungroup

## Filter out everything but Microcystis
df_EMP_MC <- filter(df_EMP_phyto_gen, Genus == "Microcystis")

## Rename Column Header and Remove Genus column
df_EMP_MC <- df_EMP_MC %>% rename("EMP.MC.Unit.Density" = "Unit.Density")
df_EMP_MC$Genus <- NULL

## Join datasets
df_MC <- left_join(df_MC, df_EMP_WQ)
df_MC <- left_join(df_MC, df_EMP_MC)

## Replace NAs with zeros in EMP Unit Density values
df_MC <- df_MC %>%
  replace_na(list(EMP.MC.Unit.Density = 0))

## Rename category with Microcystis score
df_MC <- df_MC %>%
  rename(MC.Score = Microcystis)

## Remove half-score MC scores
df_MC <- df_MC %>%
  filter(MC.Score != 2.5) %>%
  filter(MC.Score != 3.5)

## Convert Microcystis scores to factors
df_MC$MC.Score <- as.factor(df_MC$MC.Score)

## Filter out data pre-2015 (when MC SCore was implemented in EMP)
df_MC <- df_MC %>%
  filter(Year >= 2015)

## Plot Microcystis score vs. Microcystis biovolume
plot1 <- ggplot(df_MC) +
#  geom_boxplot(aes(x = MC.Score,
#                   y = log10(dwr.MIC_totbvL+1)),
#               width = 0.1) +
  geom_jitter(aes(x = MC.Score,
                  y = log10(dwr.MIC_totbvL+1)),
              width = 0.1,
              size = 2,
              color = "darkgreen")

plot1 +
  labs(title = "Microcystis - Visual Score vs. Surface Tow Biovolume (FlowCam)",
       x = "Microcystis Visual Score",
       y = "Biovolume of Microcystis (log10 + 1 um^3/mL)")

ggsave(path="plots",
       filename = "Fig1.visual_score_vs_surface_tow.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

## Plot Microcystis score vs. EMP Microcystis Density
plot2 <- ggplot(df_MC) +
  geom_jitter(aes(x = MC.Score,
                  y = log10(EMP.MC.Unit.Density+1)),
              width = 0.1,
              size = 2,
              color = "darkgreen")

plot2 +
  labs(title = "Microcystis - Visual Score vs. Unit Density (EMP)",
       x = "Microcystis Visual Score",
       y = "Unit Density of Microcystis (log10 + 1 units/mL)")

ggsave(path="plots",
       filename = "Fig2.visual_score_vs_EMP_grab.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

## Plot MC Score vs. qPCR Microcystis
plot3 <- ggplot(df_MC) +
  geom_jitter(aes(x = MC.Score,
                   y = log10(ucd.ppia.MC.total.ugL+1)),
               width = 0.1)

plot3

## Plot MC biovolume vs. qPCR Microcystis
plot4 <- ggplot(df_MC) +
  geom_point(aes(x = log10(dwr.MIC_totbvL+1),
                  y = log10(EMP.MC.Unit.Density+1)))

plot4

## Plot EMP Unit Density vs Surface Tow
plot5 <- ggplot(df_MC) +
  geom_point(aes(x = log10(dwr.MIC_totbvL+1),
                  y = log10(EMP.MC.Unit.Density+1)),
              size = 2,
              color = "darkgreen")

plot5 +
  labs(title = "Surface Tow Biovolume (FlowCam) vs. Unit Density (EMP)",
       x = "Biovolume of Microcystis (log10 + 1 um^3/mL)",
       y = "Unit Density of Microcystis (log10 + 1 units/mL)")

ggsave(path="plots",
       filename = "Fig3.surface_tow_vs_EMP_grab.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Compare chlorophyll in tows vs water grabs
plot6 <- ggplot(df_MC) +
  geom_point(aes(x = log10(dwr.MIC_totbvL+1),
                 y = log10(EMP.MC.Unit.Density+1)),
             size = 2,
             color = "darkgreen")

