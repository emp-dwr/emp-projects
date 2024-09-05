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
# Use the black-and-white theme
theme_set(theme_bw())

# Create a ggplot2 theme
theme_update(
  axis.text = element_blank(),  # Hide axis text
  axis.ticks = element_blank(), # Hide axis ticks
  axis.title = element_blank(), 
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.border = element_rect(fill = NA, colour = "black"),
  strip.background = element_rect(fill = "gray", colour = "black"),
  legend.position = "right",
  legend.key = element_rect(fill = "white", colour = NA)
)

# Plot Phoebe's stations
ggplot() +
  geom_sf(data = WW_Delta, fill = "lightblue") +
  geom_sf(data = sf_EMP, 
          aes(),
          size = 3) +
  coord_sf(xlim = c(-122.2, -121.3), ylim = c(37.95, 38.2)) +
  geom_label(data = sf_EMP,
                   aes(label = StationID,
                       geometry = geometry),
                   stat = "sf_coordinates") +
  annotation_scale(location = "bl", 
                   width_hint = 0.5,
                   unit_category = "imperial") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  labs(x = NULL,
       y = NULL)

ggsave(path = output,
       filename = "EMP-station-map.pdf",
       device = "pdf",
       scale=1.0,
       units="in",
       height=5,
       width=11,
       dpi="print")
