# Generate a map of Franks Tract showing D19 and FRK ---------------------------
# 3/9/2023

# Load required libraries ------------------------------------------------------

library(tidyverse)
library(deltamapr)
library(sf)
library(ggrepel)
library(maps)

# Prepare workspace ------------------------------------------------------------

setwd("./03_Other/MC-surface-tows")
getwd()

theme_set(theme_bw())

rm(list=ls()) 

# Read in EMP station and city location data -----------------------------------
df_EMP <- read_csv("EMP_Stations_All.csv")

# Filter out unneeded stations -------------------------------------------------
df_EMP <- df_EMP %>%
  filter(StationCode %in% c("D19","FRK","D19A")) %>%
  filter(StationType != "Phytoplankton")

# Import data about city locations
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")

# Plot all EMP station maps together -------------------------------------------
plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_jitter(data = df_EMP,
             aes(x = Longitude,
                 y = Latitude,
                 fill = StationCode,
                 size = 3),
             pch = 21,
             color = "black") +
  scale_fill_manual(values = c("#E41A1C",
                               "#984EA3",
                               "#4DAF4A",
                               "#FF7F00",
                               "#377EB8")) +
  geom_point(data = cities %>% arrange(pop) %>% tail(250),
             aes(x = long,
                 y = lat)) +
  geom_text_repel(data = cities %>% arrange(pop) %>% tail(250), 
                  aes(x = long,
                      y = lat, 
                      label = name)) +
  ylim(38.0, 38.1) +
  xlim(-121.7, -121.55) +
  theme_bw()

plot + labs(x = NULL,
            y = NULL,
            fill = "Station Name") +
  guides(size = "none")


ggsave(path="plots",
       filename = "franks_tract_station_map_2.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=5,
       width=7, 
       dpi="print")
