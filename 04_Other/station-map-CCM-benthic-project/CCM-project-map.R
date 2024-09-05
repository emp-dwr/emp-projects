# Create maps for Phoebe's poster at BDSC 2024
# Ted Flynn
# 9/5/2024

# Load essential packages ------------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(here)))
suppressWarnings(suppressMessages(library(deltamapr)))
suppressWarnings(suppressMessages(library(sf)))
suppressWarnings(suppressMessages(library(maps)))
suppressWarnings(suppressMessages(library(ggspatial)))
suppressWarnings(suppressMessages(library(ggrepel)))

# Set output directory 
output <- here("04_Other",
               "station-map-CCM-benthic-project",
               "output")

# Set visual theme in ggplot
theme_set(theme_bw())

# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

# Load EMP station locations ---------------------------------------------------
# Read in station locations for all compliance monitoring stations
EMP_stations <- read_csv(here("04_Other",
                              "station-map-CCM-benthic-project",
                              "EMP-stations.csv"))

# Convert to sf object
sf_EMP <- st_as_sf(EMP_stations,
                   coords = c("Longitude", "Latitude"), 
                   crs = 4326)

# Filter to stations used in CCM project
sf_EMP <- sf_EMP %>% 
  filter(StationID %in% c("D6-R","D7-C","D24-L","D16-L","P8-R"))

# Create plot ------------------------------------------------------------------
