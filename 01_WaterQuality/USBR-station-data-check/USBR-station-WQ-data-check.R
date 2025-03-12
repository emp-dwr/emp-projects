## Create plots for USBR-managed D-1641 stations since 2020
# Ted Flynn 2/13/2025

# Load packages ----------------------------------------------------------------
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(janitor)))
suppressWarnings(suppressMessages(library(here)))
suppressWarnings(suppressMessages(library(cder)))

# Basic settings ---------------------------------------------------------------

# Set output directory 
plots <- here("01_WaterQuality","USBR-station-data-check","output")

# Suppress summarise info
options(tidyverse.quiet = TRUE)
options(dplyr.summarise.inform = FALSE)

# Import station data from CDEC ------------------------------------------------

# Create date variables 
start <- as.Date("2020-01-01")
end <- as.Date("2024-12-31")

# List all D-1641 stations managed by USBR 
stations <- c("CLL","SAL","CNT","UNI","PCT","DMC","EMM")

# Load previously downloaded data (if already downloaded )
load(here("01_WaterQuality","USBR-station-data-check","df_WQ_all.RData"))

# Download data directly (use the first time)

# df_WQ_all <- cdec_query(stations, sensors = c(100,25),
#                         start.date = start,
#                         end.date = end)
# 
# # Takes a long time to download from CDEC so save as RData file
# save(df_WQ_all, file = here("01_WaterQuality","D1641-table5-sonde-uptime","df_WQ_all.RData"))

# Clean Up Data ----------------------------------------------------------------

# Create new df for cleaning and remove duplicates
df_WQ <- df_WQ_all %>% distinct() # no rows removed

# Add more detailed description of each station
df_WQ <- df_WQ %>%
   mutate(Description = case_when(StationID == "CLL" ~ "C2 - Sac River @ Collinsville",
                                  StationID == "SAL" ~ "C4 - SJ River @ San Andreas Landing",
                                  StationID == "CNT" ~ "C5 - Contra Costa Canal @ Pumping Plant 1",
                                  StationID == "UNI" ~ "C8 - Old River near Middle River",
                                  StationID == "PCT" ~ "C14 - Sac River @ Port Chicago",
                                  StationID == "DMC" ~ "DMC1 - Delta-Mendota Canal @ Tracy Pumping Plant",
                                  StationID == "EMM" ~ "EMM - Sac River @ Emmaton"
                                  ))

# Check CDEC column values
unique(df_WQ$DataFlag) # no values, not used for these stations

#quick check to see if there are the same number of samples per station per duration
table(df_WQ$SensorType, df_WQ$StationID, df_WQ$Duration)

# Remove unneeded columns
df_WQ <- df_WQ %>% 
  select(!DataFlag) %>% # just a blank column
  select(!SensorNumber) %>% # CDEC codes for each analyte
  select(!SensorUnits) # we know what the units are

# Rename output from CDEC for clarify
df_WQ <- df_WQ %>% 
  mutate(SensorType = str_replace(SensorType, "EL COND", "SpC")) %>% 
  mutate(SensorType = str_replace(SensorType, "TEMP W", "Water Temp"))

# Interpret content in the "Duration" column
unique(df_WQ$Duration) # H = hourly, E = event, D = daily

# Replace duration codes with more verbose descriptions
durations <- c("D" = "Daily","E" = "Event","H" = "Hourly")

df_WQ <- df_WQ %>% 
  mutate(Duration = str_replace_all(Duration, durations))

# Create new data frame with rows for all hourly date-time combinations from 
# 1-1-2020 until 12-31-2024 at each station

df_hourly_datetimes <- tibble(
  DateTime = seq.POSIXt(
    from = as.POSIXct("2020-01-01 00:00:00"),
    to = as.POSIXct("2024-12-31 23:00:00"),
    by = "hour"
  )
)

# Use crossing to make a row at each station
df_blank <- crossing(StationID = stations, df_hourly_datetimes)

# Filter CDEC data for hourly data only
df_WQ_hr <- df_WQ %>% 
  filter(Duration == "Hourly") %>% 
  select(-Duration,-ObsDate)

df_WQ_hr <- pivot_wider(df_WQ_hr, names_from = SensorType, values_from = Value)

# Combine CDEC data with blank data frame of all possible dates
# DateTimes that weren't on CDEC will be NAs

df_WQ_hr <- left_join(df_blank,df_WQ_hr)

df_WQ_hr <- pivot_longer(df_WQ_hr, cols = 4:5, names_to = "Analyte", values_to = "Value")

# Classify data as missing or less than or equal to zero (bad data)
df_WQ_hr <- df_WQ_hr %>% 
  mutate(DataStatus = case_when(is.na(Value) ~ "Missing",
                                Value <= 0 ~ "Zero",
                                TRUE ~ "Present"))

# Add additional date units for grouping and plotting
df_WQ_hr <- df_WQ_hr %>%
  mutate(Year = year(df_WQ_hr$DateTime), .after = DateTime) %>%
  mutate(Month = month(df_WQ_hr$DateTime, label = TRUE), .after = DateTime) %>%
  mutate(Date = date(df_WQ_hr$DateTime), .after = DateTime) %>% 
  mutate(Julian = yday(df_WQ_hr$Date), .after = DateTime)

# Create fractional Julian day
df_WQ_hr <- df_WQ_hr %>%
  mutate(
    fraction_of_day = (hour(DateTime) + minute(DateTime) / 60 + second(DateTime) / 3600) / 24,
    decimal_julian_day = Julian + fraction_of_day
  )

# Plot yearly WQ data presence/absence for each station ------------------------
# Use the black-and-white theme
theme_set(theme_bw())

# Create a ggplot2 theme
theme_update(
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "white", colour = NA),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.border = element_rect(fill = NA, colour = "black"),
  strip.background = element_rect(fill = "gray", colour = "black"),
  legend.position = "bottom",
  legend.key = element_rect(fill = "white", colour = NA)
)

years <- unique(df_WQ_hr$Year) 
years <- sort(years, decreasing = F, na.last = T)

colors <- c("darkred","lightblue","orange2")

# Create facet plot for each analyte 

analytes <- unique(df_WQ_hr$Analyte)

for (x in analytes) {
  df_temp <- df_WQ_hr %>%
    filter(Analyte == x)
  
  p1 <- ggplot(data = df_temp, aes(x = decimal_julian_day,
                                    y = Year,
                                    fill = DataStatus)) +
    geom_tile(height = 0.5) +
    facet_wrap(~ StationID, ncol = 2) +
    #scale_x_datetime(date_labels = "%b %d", date_breaks = "1 month") +
    scale_fill_manual(values = colors) +
    labs(x = "Date",
         y = NULL,
         title = paste0("Continuous WQ Monitoring - ",x))
  
  print(p1) 
  
  ggsave(path = plots,
         filename = paste0(x,"-USBR-data-completeness-CDEC.png"),
         device = "png",
         scale=1.0,
         units="in",
         height=9,
         width=6.5,
         dpi="print")
}
