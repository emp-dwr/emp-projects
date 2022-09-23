## Generate maps of EMP's fixed sampling locations
## 7/25/2021

library("tidyverse");packageVersion("tidyverse")
library("deltamapr");packageVersion("deltamapr")
library("sf");packageVersion("sf")
library("ggrepel");packageVersion("ggrepel")
library("maps");packageVersion("maps")

setwd("C:/Users/tflynn/Documents/R/EMP_station_maps")
theme_set(theme_bw())

## Clean workspace
rm(list=ls()) 

## Read FP data
df_EMP <- read_csv("EMP_Stations_All.csv")
df_EMP$Longitude <- df_EMP$Longitude * -1

#Import data about city locations
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")
cities$long <- cities$long * -1



## Plot EMP station maps
plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = subset(df_EMP, StationType == "Benthic"),
             aes(x = Longitude,
                 y = Latitude,
                 fill = StationType,
                 size = 3),
             pch = 21,
             color = "black") +
  #scale_fill_manual(values = c("red","purple")) +
  geom_point(data = cities %>% arrange(pop) %>% tail(250),
             aes(x = long,
                 y = lat)) +
  geom_text_repel(data = cities %>% arrange(pop) %>% tail(250), 
                  aes(x = long,
                      y = lat, 
                      label = name)) +
  ylim(37.65, 38.6) +
  xlim(122.41, 121.2) +
  theme_bw()

plot + labs(x = "Longitude",
            y = "Latitude",
            fill = "Station Type",
            title = "EMP Monitoring Stations - Water Quality") +
  guides(size = "none")


ggsave(path="plots",
       filename = "EMP.WQ.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=6.5,
       width=9, 
       dpi="print")
