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

## Read in data
MC <- read_csv("San Francisco Estuary cyanoHAB data 2014 to 2019_v1.csv")
WQ <- read_csv("SACSJ_delta_water_quality_1975_2020.csv")
phyto <- read_csv("EMP_1975_2020_phyto.csv")

unique(MC$EMP_Site)
table(MC$EMP_Site)

## Remove non-EMP sites
MC <- MC %>%
  filter_at(vars(EMP_Site), all_vars(!is.na(.))) %>%
  filter(EMP_Site != "FieldCont") %>%
  filter(RepCont == 0)

## Select only MC Score in WQ dataset
WQ <- WQ %>% 
  select(Station, Date, Microcystis) 

## Remove unneeded cols in MC dataset
MC$DWR_Site <- NULL
MC$Lat <- NULL
MC$Lon <- NULL
MC$Collection_ID <- NULL
MC[17:36] <- NULL
MC$RepCont <- NULL
MC$field.PST <- NULL

## Rename columns to match
MC <- MC %>%
  rename(Date = Collection_Date) %>%
  rename(Station = EMP_Site) %>%
  rename(Year = Survey_Year) %>%
  rename(Month = Collection_Month)

## Get dates to match
MC$Date <- parse_date_time(MC$Date, 
                      c("%Y-%m-%d"))

WQ$Date <- parse_date_time(WQ$Date, 
                           c("%m-%d-%Y"))

## EMP Phyto Data Massaging
## Import Phyto Data

## Remove unneeded columns
phyto[,c(2,4,5,8,9,10,12,13,14,15)] <- NULL

## Rename date column
phyto <- phyto %>%
  rename(Date = SampleDate)

## Convert other dates and times into POSIXct-compliant values
## then remove old values
phyto$Date <- parse_date_time(phyto$Date, 
                           c("%m-%d-%Y"))

## Remove stations apart from modern ones
stations <- c("D22","NZ068","D19","D12","D16","D41A","D41","D28A","D26","NZ325",
              "NZS42","D10","EZ2","EZ6","D8","EZ2-SJR","NZ002","NZ004","NZ032",
              "D7","C9","C3A","MD10A","C10A","D4","P8","D6","EZ6-SJR")

phyto <- phyto %>% filter(StationCode %in% stations)

## Rename Density & Station
phyto <- phyto %>% 
  rename("Unit.Density" = "Organisms_per_mL") %>%
  rename("Station" = "StationCode")

## Summarize by genus
phyto.gen <- phyto %>% 
  group_by(Date, Station, Genus) %>%
  summarize(Unit.Density = sum(Unit.Density, na.rm = T)) %>%
  ungroup

## Filter out everything but Microcystis
MC.EMP <- filter(phyto.gen, Genus == "Microcystis")

## Rename Column Header and Remove Genus column
MC.EMP <- MC.EMP %>% rename("EMP.MC.Unit.Density" = "Unit.Density")
MC.EMP$Genus <- NULL

## Join datasets
MC <- left_join(MC, WQ)
MC <- left_join(MC, MC.EMP)

## Replace NAs with zeros in EMP Unit Density values
MC <- MC %>%
  replace_na(list(EMP.MC.Unit.Density = 0))

## Rename category with Microcystis score
MC <- MC %>%
  rename(MC.Score = Microcystis)

## Remove half-score MC scores
MC <- MC %>%
  filter(MC.Score != 2.5) %>%
  filter(MC.Score != 3.5)

## Convert Microcystis scores to factors
MC$MC.Score <- as.factor(MC$MC.Score)

## Filter out data pre-2015 (when MC SCore was implemented in EMP)
MC <- MC %>%
  filter(Year >= 2015)

## Plot Microcystis score vs. Microcystis biovolume
plot1 <- ggplot(MC) +
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
plot2 <- ggplot(MC) +
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
plot3 <- ggplot(MC) +
  geom_jitter(aes(x = MC.Score,
                   y = log10(ucd.ppia.MC.total.ugL+1)),
               width = 0.1)

plot3

## Plot MC biovolume vs. qPCR Microcystis
plot4 <- ggplot(MC) +
  geom_point(aes(x = log10(dwr.MIC_totbvL+1),
                  y = log10(EMP.MC.Unit.Density+1)))

plot4

## Plot EMP Unit Density vs Surface Tow
plot5 <- ggplot(MC) +
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
