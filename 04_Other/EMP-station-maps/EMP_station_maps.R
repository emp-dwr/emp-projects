## Generate maps of EMP's fixed sampling locations
## 9/23/2022 (updated 4/3/2024 TMF)

# Load required libraries ------------------------------------------------------

library(tidyverse)
library(deltamapr)
library(sf)
library(ggrepel)
library(ggspatial)
library(maps)
library(here)

# Set plot themes --------------------------------------------------------------

plots <- here(file = "04_Other","EMP-station-maps","plots")

# Create custom palettes
color_EMP <- c("#E41A1C","#984EA3","#4DAF4A","#FF7F00","#377EB8")

# Read in EMP station and city location data -----------------------------------
df_EMP <- read_csv(here(file = "04_Other",
                        "EMP-station-maps",
                        "EMP_Stations_All.csv"))

# Set order that station types appear in 
#unique(df_EMP$StationType)
stations <- c("Benthic","Zooplankton","Phytoplankton","WQ - Continuous",
                   "WQ - Discrete")

df_EMP$StationType <- factor(df_EMP$StationType, levels = stations)

# Import data about city locations
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")

# Plot EMP stations at Franks Tract together -----------------------------------
theme_set(theme_bw())

plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = df_EMP,
             aes(x = Longitude,
                 y = Latitude,
                 fill = StationType,
                 size = 3),
             pch = 21,
             color = "black") +
  annotation_scale(location = "tl", 
                   width_hint = 0.4, 
                   unit_category = "imperial") +
  annotation_north_arrow(location = "tr") +
  geom_text(data = df_EMP,
            aes(x = Longitude,
                y = Latitude,
                label = Station_ID),
            nudge_y = 0.003) +
  ylim(38.015,38.075) +
  xlim(-121.65,-121.56)

plot + labs(x = NULL,
            y = NULL,
            fill = NULL,
            title = "EMP Monitoring Stations - Franks Tract") +
  theme(legend.position = "none") +
  guides(size = "none")

ggsave(path=plots,
       filename = "EMP-stations-Franks.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4.5,
       width=5.5, 
       dpi="print")

# Plot all EMP station maps together -------------------------------------------
theme_set(theme_bw())

plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = df_EMP,
             aes(x = Longitude,
                 y = Latitude,
                 fill = StationType,
                 size = 3),
             pch = 21,
             color = "black") +
  annotation_scale(location = "bl", 
                   width_hint = 0.4, 
                   unit_category = "imperial") +
  scale_fill_manual(values = color_EMP) +
  geom_point(data = cities %>% arrange(pop) %>% tail(250),
             aes(x = long,
                 y = lat)) +
  geom_text_repel(data = cities %>% arrange(pop) %>% tail(250), 
                  aes(x = long,
                      y = lat, 
                      label = name)) +
  ylim(37.65, 38.6) +
  xlim(-122.41, -121.2)


plot + labs(x = NULL,
            y = NULL,
            fill = NULL,
            title = "EMP Monitoring Stations") +
  guides(size = "none")

ggsave(path=plots,
       filename = "EMP-stations-all.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")

# Plot stations for each EMP element -------------------------------------------

for (x in stations) {

plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = subset(df_EMP, StationType == x),
              aes(x = Longitude,
                  y = Latitude,
                  fill = StationType,
                  size = 3),
              pch = 21,
              color = "black") +
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
