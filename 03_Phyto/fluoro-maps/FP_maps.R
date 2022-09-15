## Cleaning and joining FluoroProbe and MOPED data, then generating a map
## 
## 8/25/22 TMF

library(tidyverse)
library(lubridate)
library(deltamapr)
library(sf)
library(ggrepel)
library(maps)
library(janitor)


# Set working directory
setwd("./03_Phyto/fluoro-maps")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

## File names 
FP_data <- "Fluoroprobe_SP_Aug22.txt"
MOPED_data <- "MOPED_Export_92_08232022_SP.csv"

## Map settings
time_unit <- "1 minute"

# read in FluoroProbe data
col_names <- names(read_tsv(FP_data, n_max = 0))
df_FP <- read_tsv(FP_data, skip = 2, col_names = col_names)
## Read in MOPED data
df_MOPED <-  read_csv(MOPED_data, skip = 2)

## Remove unnecessary columns
df_FP[6:9] <- NULL
df_FP[7:28] <- NULL

## Clean headers and remove unnecessary data
df_FP <- df_FP %>%
  clean_names(case = "big_camel")

## Rename headers
df_FP <- df_FP %>%
  rename("GreenAlgae" = "GreenAlgae2") %>%
  rename("Cyanobacteria" = "Bluegreen3") %>%
  rename("Diatoms" = "Diatoms4") %>%
  rename("Cryptophytes" = "Cryptophyta5") %>%
  rename("Total.Fluorescence" = "TotalConc")

## Covert to datetime
df_FP$DateTime <- mdy_hms(df_FP$DateTime, 
                          tz = "US/Pacific")

# Round to nearest 1 min and calc average
df_FP <- df_FP %>%
  mutate(DateTime = round_date(DateTime, unit=time_unit)) %>%
  group_by(DateTime) %>%
  summarize_all(~round(mean(., na.rm = TRUE),2))

## Remove vertical data from MOPED
df_MOPED <- df_MOPED %>% filter(Equipment == "EXO Horizontal")

# clean up (code from Sarah Perry)
if(length(unique(df_MOPED$Extension[!is.na(df_MOPED$Extension)])) > 1) stop('ERROR: more than one extension value in wq data')
run_name <- unique(df_MOPED$Extension[!is.na(df_MOPED$Extension)])

## Covert to datetime
df_MOPED <- df_MOPED %>% rename("DateTime" = "TimeStamp")
df_MOPED$DateTime <- mdy_hm(df_MOPED$DateTime, 
                            tz = "US/Pacific")

## Average data to same time interval as FluoroProbe data
df_MOPED_data <- df_MOPED %>%
  subset(select = c(DateTime, Value, Header)) %>%
  mutate(DateTime = round_date(DateTime, unit = time_unit)) %>%
  group_by(Header, DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE)) 

## Average data to same time interval as FluoroProbe data
df_MOPED_GPS <- df_MOPED %>%
  subset(select = c(DateTime, Latitude, Longitude)) %>%
  mutate(DateTime = round_date(DateTime, unit = time_unit)) %>%
  group_by(DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE))

## Pivot MOPED data wide
df_MOPED_l <- left_join(df_MOPED_GPS, df_MOPED_data)
df_MOPED_w <- pivot_wider(df_MOPED_l, names_from = Header, values_from = Value)

## Combine MOPED and FluoroProbe data
df_FP <- left_join(df_MOPED_w, df_FP)

## Filter out data rows with no FP data
df_FP <- df_FP %>% drop_na()

#df_FP <- df_FP %>% filter(Longitude > 1)

df_FP$Longitude <- df_FP$Longitude

df_FP <- df_FP %>% 
  rename("EXO2.Chla" = "FLUOR") %>%
  rename("FP.Total.Chl" = "Total.Fluorescence")

df_FP.l <- pivot_longer(df_FP, cols = 4:16,
                        names_to = "Group",
                        values_to = "Conc")

## Pull data to plot cities alongside data
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")
cities$long <- cities$long

## Plot maps of FluoroProbe data
groups <- unique(df_FP.l$Group) 

for (group in groups) {
  df_temp <- df_FP.l %>%
    filter(Group == group)
  
  plot <- ggplot(WW_Delta) + 
    geom_sf(fill = "lightblue") + 
    geom_point(data = df_temp,
               aes(x = Longitude,
                   y = Latitude,
                   fill = Conc,
                   size = 1),
               pch = 21,
               color = "black") +
    scale_fill_continuous(type = "viridis", 
                          limits = c(0,80)) +
    geom_point(data = cities %>% arrange(pop) %>% tail(500),
               aes(x = long,
                   y = lat)) +
    geom_text_repel(data = cities %>% arrange(pop) %>% tail(500), 
                    aes(x = long,
                        y = lat, 
                        label = name)) +
    #scale_y_continuous() +
    ylim(38, 38.2) +
    xlim(-122.5, -122.2) +
    theme_bw()
  
  plot + labs(x = "Longitude",
              y = "Latitude",
              fill = "Fluorescence (ug/L)",
              title = paste0("San Pablo Bay Phytoplankton, August 23, 2022 - ", group)) +
    guides(size = "none")
  
  
  ggsave(path="plots",
         filename = paste0("FP.map.Aug.2022.",group,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=6.5,
         width=9, 
         dpi="print")
  
}

## Plot MOPED values

plot.MOPED <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = df_FP,
             aes(x = Longitude,
                 y = Latitude,
                 fill = EXO2.Chla,
                 size = 1),
             pch = 21,
             color = "black") +
  scale_fill_continuous(type = "viridis") +
  #                      limits = c(0,80)) +
  geom_point(data = cities %>% arrange(pop) %>% tail(500),
             aes(x = long,
                 y = lat)) +
  geom_text_repel(data = cities %>% arrange(pop) %>% tail(500), 
                  aes(x = long,
                      y = lat, 
                      label = name)) +
  #scale_y_continuous() +
  ylim(38, 38.2) +
  xlim(-122.5, -122.2) +
  theme_bw()

plot.MOPED

## Make plot comparing different fluorescence values (EXO2 and FluoroProbe)
plot.fluor <- ggplot(df_FP.l, aes(x = DateTime, y = Conc, color = Group)) +
  geom_line(data = subset(df_FP.l, Group == "EXO2.Chla")) +
  geom_line(data = subset(df_FP.l, Group == "FP.Total.Chl")) +
  scale_color_brewer(palette = "Dark2")

plot.fluor +
  labs(x = "Time",
       y = "Concentration (ug/L)",
       fill = "Fluorescence (ug/L)",
       title = "Chlorophyll Fluorescence in San Pablo Bay, August 2022") +
  guides(size = "none")

ggsave(path="plots",
       filename = "FP.map.Aug.2022.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")
