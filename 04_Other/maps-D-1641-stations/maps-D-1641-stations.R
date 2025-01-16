# Create maps of all D-1641 compliance stations
# 9-11-2024 (Ted Flynn)

# Load required libraries ------------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(here)))
suppressWarnings(suppressMessages(library(deltamapr)))
suppressWarnings(suppressMessages(library(sf)))
suppressWarnings(suppressMessages(library(maps)))
suppressWarnings(suppressMessages(library(ggspatial)))
suppressWarnings(suppressMessages(library(ggrepel)))
suppressWarnings(suppressMessages(library(cowplot)))
suppressWarnings(suppressMessages(library(ggthemes)))

# Set output directory 
output <- here("04_Other",
               "maps-D-1641-stations",
               "output")

# Load D-1641 station locations ------------------------------------------------
# Read in station locations for all compliance monitoring stations
df_stations <- read_csv(here(
  "04_Other",
  "maps-D-1641-stations",
  "data",
  "D-1641-compliance-stations.csv"
))

# Convert to sf object
sf_EMP <- st_as_sf(df_stations,
                   coords = c("Longitude", "Latitude"),
                   crs = 4326)

# Lat/long limits
xlim_Delta <- c(-121.8, -121.2)
ylim_Delta <- c(37.7, 38.5)

xlim_Bay <- c(-122.5, -121.8)
ylim_Bay <- c(37.95,38.25)

# Create plot ------------------------------------------------------------------
# Use the black-and-white theme
theme_set(theme_bw())

# Create a ggplot2 theme
theme_update(
  axis.text = element_blank(),  # Hide axis text
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "#F1F8ED", colour = NA),
  axis.ticks.length = unit(-0.15, "cm"),
  panel.border = element_rect(fill = NA, colour = "black"),
  legend.position = "none",
  panel.grid.major = element_blank(),  # Set color for major grid lines (latitude and longitude lines)
  panel.grid.minor = element_blank()
)

# Plot Delta stations
ggplot() +
  geom_sf(data = WW_Delta, fill = "#87A7D1", color = "#87A7D1") +
  geom_sf(data = sf_EMP, 
          aes(shape = Component, fill = Agency),
          size = 3) +
  coord_sf(xlim = xlim_Delta, ylim = ylim_Delta) +
  scale_shape_manual(values = c(21,22,23)) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(-121.8, -121.2, by = 0.2)) +
  annotation_scale(location = "bl", 
                   width_hint = 0.5,
                   unit_category = "imperial") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  labs(x = NULL, y = NULL)

ggsave(path = output,
       filename = "compliance-station-map-Delta.pdf",
       device = "pdf",
       scale=1.0,
       units="in",
       height=16,
       width=10,
       dpi="print")

# Plot Bay Stations
ggplot() +
  geom_sf(data = WW_Delta, fill = "#87A7D1", color = "#87A7D1") +
  geom_sf(data = sf_EMP, 
          aes(shape = Component, fill = Agency),
          size = 3) +
  coord_sf(xlim = xlim_Bay, ylim = ylim_Bay) +
  scale_shape_manual(values = c(21,22,23)) +
  scale_fill_brewer(palette = "Set1") +
  #scale_x_continuous(breaks = seq(-121.8, -121.2, by = 0.2)) +
  annotation_scale(location = "bl", 
                   width_hint = 0.5,
                   unit_category = "imperial") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  labs(x = NULL, y = NULL)

ggsave(path = output,
       filename = "compliance-station-map-Bay.pdf",
       device = "pdf",
       scale=1.0,
       units="in",
       height=10,
       width=16,
       dpi="print")
