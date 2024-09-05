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