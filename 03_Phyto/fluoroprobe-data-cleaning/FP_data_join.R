## Join together MOPED and FluoroProbe data

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("janitor");packageVersion("janitor")
library(deltamapr)
library(sf)
library(sp)

# Set working directory
setwd("./03_Phyto/fluoroprobe-data-cleaning")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

## Time unit selection for averaging data
time_unit <- "1 minute"

#################################################
######## Importing Raw FluoroProbe Data #########
#################################################

# Import FluoroProbe data files
FP_files <- dir(path = "data_FP/", pattern = "\\.txt", full.names = T)
df_FP <- map_dfr(FP_files, ~read_tsv(.x))

# Remove second row of labels
df_FP <- df_FP %>%
  filter(`Date/Time` != "date")

# Select Columns with concentrations
df_FP <- df_FP %>%
  select(`Date/Time`:Cryptophyta...5,`Total conc.`)

# Rename headers
df_FP <- df_FP %>%
  rename("DateTime" = "Date/Time") %>%
  rename("GreenAlgae.ug.L" = "Green Algae...2") %>%
  rename("Cyanobacteria.ug.L" = "Bluegreen...3") %>%
  rename("Diatoms.ug.L" = "Diatoms...4") %>%
  rename("Cryptophytes.ug.L" = "Cryptophyta...5") %>%
  rename("Total.Conc.FP.ug.L" = "Total conc.")

# change FluoroProbe values to numeric
df_FP <- df_FP %>%
  mutate(across(GreenAlgae.ug.L:Total.Conc.FP.ug.L, ~ as.numeric(.x), .keep = 'unused'))

# Convert to datetime
df_FP$DateTime <- mdy_hms(df_FP$DateTime, tz = "US/Pacific")

# Round to nearest 1 min and calc average
df_FP <- df_FP %>%
  mutate(DateTime = round_date(DateTime, unit=time_unit)) %>%
  group_by(DateTime) %>%
  summarize_all(~round(mean(., na.rm = TRUE),2))

###########################################
######## Importing Raw MOPED Data #########
###########################################
# Import FluoroProbe data files
MOPED_files <- dir(path = "data_MOPED/", pattern = "\\.csv", full.names = T)
df_MOPED <- map_dfr(MOPED_files, ~read_csv(.x, skip = 2))

## Remove vertical data from MOPED
df_MOPED <- df_MOPED %>% filter(Equipment == "EXO Horizontal")

## Covert to datetime
df_MOPED <- df_MOPED %>% rename("DateTime" = "TimeStamp")
df_MOPED$DateTime <- mdy_hms(df_MOPED$DateTime, tz = "US/Pacific")

# Select values needed
df_MOPED <- df_MOPED %>% select(Header:Latitude,DateTime:Value)

# Average data to same time interval as FluoroProbe data
df_MOPED_data <- df_MOPED %>%
  subset(select = c(DateTime, Value, Header)) %>%
  mutate(DateTime = round_date(DateTime, unit = time_unit)) %>%
  group_by(Header, DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE)) 

# Average data to same time interval as FluoroProbe data
df_MOPED_GPS <- df_MOPED %>%
  subset(select = c(DateTime, Latitude, Longitude)) %>%
  mutate(DateTime = round_date(DateTime, unit = time_unit)) %>%
  group_by(DateTime) %>%
  summarize_all(~mean(., na.rm = TRUE))

## Pivot MOPED data wide
df_MOPED_l <- left_join(df_MOPED_GPS, df_MOPED_data)
df_MOPED_w <- pivot_wider(df_MOPED_l, names_from = Header, values_from = Value)

## Combine MOPED and FluoroProbe data
df_all <- left_join(df_MOPED_w, df_FP)

#################################
######## Assign Regions #########
#################################
# import delta sf
sf_delta <- R_EDSM_Subregions_Mahardja

# convert wq to spdf
coords <- df_all[,c('Longitude', 'Latitude')]
data   <- subset(df_all)#, select = -c(Latitude, Longitude))
crs    <- CRS('+init=epsg:4326 +proj=longlat')
spdf_wq <- SpatialPointsDataFrame(coords = coords,
                                  data = data, 
                                  proj4string = crs)

# convert delta to spdf
spdf_delta <- as(sf_delta, 'Spatial')
spdf_delta <- spTransform(spdf_delta, CRS('+init=epsg:4326 +proj=longlat'))

# add subregion to df
col_sr <- sp::over(spdf_wq, spdf_delta[,'SubRegion'])
spdf_wq$SubRegion <- col_sr$SubRegion

# convert to shapefile
sf_wq <- st_as_sf(spdf_wq)
sf_wq <- st_transform(sf_wq, st_crs = sf_delta)
sf_wq <- sf_wq %>% filter(!is.na(SubRegion))

# check data
ggplot() +
  geom_sf(data = sf_delta) +
  geom_sf(data = sf_wq, color = 'red')

# clean up regions in final df
df_final <- as_tibble(sf_wq)
df_regions <- read_csv('supp_files/regions_fluoro.csv')
df_final <- left_join(df_final, df_regions, 'SubRegion')

## Add column for year and month for highlighting data
df_final <- df_final %>% 
  mutate(Year = year(df_final$DateTime)) %>%
  mutate(Month = month(df_final$DateTime, label = T))

## Order month in calendar order rather than (default) alphabetical
df_final$Month = factor(df_final$Month, levels = month.abb)

## Reorder date/time columns
df_final <- df_final %>% 
  relocate(Year, .after = DateTime) %>% 
  relocate(Month, .after = DateTime)  %>%
  relocate(Region, .after = Longitude) %>%
  subset(select = -c(geometry, SubRegion))

# Save CSV to share
write.csv(df_final, file = "data_request_output/FP_data_for_USBR.csv")
save(df_final, file = "data_request_output/FP_data_for_USBR.RData")

sum(is.na(df_final$GreenAlgae.ug.L))

table(df_final$Month)
