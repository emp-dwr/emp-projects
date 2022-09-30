## Generate maps of EMP's fixed sampling locations
## 9/23/2022

# Load required libraries ------------------------------------------------------

library(tidyverse)
library(deltamapr)
library(sf)
library(ggrepel)
library(maps)

# Prepare workspace ------------------------------------------------------------

setwd("./04_Other/EMP-station-maps")
getwd()

theme_set(theme_bw())

rm(list=ls()) 

# Read in EMP station and city location data -----------------------------------
df_EMP <- read_csv("EMP_Stations_All.csv")

# Set order that station types appear in 
#unique(df_EMP$StationType)
stations <- c("Benthic","Zooplankton","Phytoplankton","WQ - Continuous",
                   "WQ - Discrete")

df_EMP$StationType <- factor(df_EMP$StationType, levels = stations)

# Import data about city locations
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")

# Plot all EMP station maps together -------------------------------------------
plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_jitter(data = df_EMP,
             aes(x = Longitude,
                 y = Latitude,
                 fill = StationType,
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
  ylim(37.65, 38.6) +
  xlim(-122.41, -121.2) +
  theme_bw()

plot + labs(x = NULL,
            y = NULL,
            fill = "Station Type",
            title = "EMP Monitoring Stations - 2022") +
  guides(size = "none")


ggsave(path="plots",
       filename = "EMP.Monitoring.Stations.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=6,
       width=9, 
       dpi="print")

# Plot stations for each EMP elemtn---------------------------------------------

for (x in stations) {

plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_jitter(data = subset(df_EMP, StationType == x),
              aes(x = Longitude,
                  y = Latitude,
                  fill = StationType,
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
  ylim(37.65, 38.6) +
  xlim(-122.41, -121.2) +
  theme_bw()

plot + labs(x = NULL,
            y = NULL,
            fill = "Station Type",
            title = paste0("EMP ",x," Monitoring Stations - 2022")) +
  guides(size = "none")


ggsave(path="plots",
       filename = paste0("EMP.Monitoring.Stations.",x,".pdf"), 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=6,
       width=9, 
       dpi="print")

}
