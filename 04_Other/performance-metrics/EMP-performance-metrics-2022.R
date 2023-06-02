# Load Libraries and Load Data Files -------------------------------------------
# Evaluate #s for EMP's Perfomance Metrics Report
# Started: 3/14/2023

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(janitor)

# Set working directory
setwd("./04_Other/performance-metrics")
getwd()

# Clean workspace
rm(list=ls()) 

# Define negative %in% operator
`%nin%` = Negate(`%in%`)

# Set directory for storing plots
output <- "plots"

# Set visual theme in ggplot
theme_set(theme_bw())

# Import data files 
df_sondes <- read_csv(file = "DEMP_sonde_ratings_2022.csv")

# Rename headers 
df_sondes <- df_sondes %>%
  rename("DO (%)" = "DO.per") %>%
  rename("Temperature" = "Temp") %>%
  rename("Turbidity" = "Turb") %>%
  rename("Conductivity" = "SpCond") %>%
  rename("Chlorophyll" = "Chla")

df_sondes <- pivot_longer(df_sondes, 
                          values_to = "PercentPass",
                          names_to = "Sensor",
                          cols = Conductivity:Turbidity)

# Get ratings to sort in order good -> bad
rating.order <- c("Excellent","Good","Fair","Poor","MAL")

df_sondes$Rating <- factor(as.character(df_sondes$Rating), 
                           levels = rating.order)

# Plot DEMP's sonde ratings from 2022 ------------------------------------------
colors = c("darkgreen","darkblue","yellow3","darkred","purple")

rating.plot <- ggplot(data = df_sondes, aes(x = PercentPass, 
                                            y = Sensor, 
                                            fill = Rating)) +
  geom_bar(position = "stack",  
           width = .6, 
           stat = "summary", 
           fun = "sum") +
  scale_fill_manual(values = colors)

rating.plot +
  labs(x = "Rating of Data Collected (%)",
       y = "Sensor",
       fill = "Sonde Rating")
  
ggsave(path = output,
       filename = "DEMP_sonde_scores_2022.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=5, 
       dpi="print")

# Read in C-EMP sonde data -----------------------------------------------------
# Import data files 
df_CEMP <- read_csv(file = "data/CEMP-sonde-ratings-2019-2022.csv")

# Rename column headers
df_CEMP <- df_CEMP %>% 
  rename("Exchange_Date" = "Field Date") %>% 
  rename("Rating" = "Wagner Rating")

# Convert dates to date format
df_CEMP$Exchange_Date <- mdy(df_CEMP$Exchange_Date)

# Select only needed columns
df_CEMP <- df_CEMP %>% 
  select(Filename,Group,Parameter,Exchange_Date,Rating)

# Filter to C-EMP Stations Only
df_CEMP <- df_CEMP %>% 
  filter(Group == "CEMP")

# Pull StationID out of the filename using regular expressions and stringr
df_CEMP <- df_CEMP %>% 
  mutate(StationCode = str_extract(Filename, pattern = "(?<=DRIFT_)[^_]+"))

# Remove unneeded stations and years
df_CEMP <- df_CEMP %>% 
  filter(StationCode %nin% c("GZB","2022","RRI A","RRI B","RRI C","RRI D")) %>%
  filter(Exchange_Date >= "2022-01-01")

df_CEMP_w <- pivot_wider(df_CEMP, names_from = Parameter, values_from = Rating)

# Sort samples by date (early to late)
df_CEMP_w <- df_CEMP_w %>% 
  group_by(StationCode) %>% 
  arrange(Exchange_Date) %>% 
  ungroup()

df_CEMP_w <- df_CEMP_w %>% 
  group_by(StationCode) %>% 
  mutate(Start_Date = lag(Exchange_Date)) %>%
  ungroup()

df_CEMP_w <- df_CEMP_w %>% 
  mutate(Start_Date = case_when(is.na(Start_Date) ~ "2022-01-01",
                          TRUE ~ as.character(Start_Date)))

# Convert dates back to date format
df_CEMP_w$Start_Date <- ymd(df_CEMP_w$Start_Date)

# Relocate Start Date column
df_CEMP_w <- df_CEMP_w %>% 
  relocate(Start_Date, .before = Exchange_Date)

# Calculate number of days between sonde exchanges
df_CEMP_w <- df_CEMP_w %>%
  mutate(Days_Rated = as.integer(Exchange_Date - Start_Date))

# Summarize rating data for graphing -------------------------------------------
df_CEMP_sum <- pivot_longer(df_CEMP_w, cols = DO:pH,
                            values_to = "Rating",
                            names_to = "Parameter")

# Select needed data
df_CEMP_sum <- df_CEMP_sum %>% 
  select(StationCode:Rating)

df_CEMP_sum <- df_CEMP_sum %>% 
  group_by(StationCode, Parameter, Rating) %>% 
  summarize(Total_Days = sum(Days_Rated)) %>% 
  ungroup()

df_CEMP_sum$Rating <- factor(as.character(df_CEMP_sum$Rating), 
                             levels = rating.order)

# Plot C-EMP rating data -------------------------------------------------------
rating.plot.CEMP <- ggplot(data = df_CEMP_sum, aes(x = Total_Days, 
                                            y = Parameter, 
                                            fill = Rating)) +
  geom_bar(position = "stack",  
           width = .6, 
           stat = "summary", 
           fun = "sum") +
  scale_fill_manual(values = colors)

rating.plot.CEMP +
  labs(x = "Rating of Data Collected (Days)",
       y = "Sensor",
       fill = "Sonde Rating",
       title = "EXO2 Sonde Probe Ratings - C-EMP - 2022 ") +
  facet_wrap(StationCode ~ ., ncol = 5)

ggsave(path = output,
       filename = "CEMP_sonde_scores_2022.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=8, 
       dpi="print")

# Graph EMP position vacancy rates ---------------------------------------------
df_vacancy <- read_csv("data/EMP_vacancies.csv")

# Summarize vacancy numerically
df_vacancy %>% 
  group_by(Classification) %>% 
  summarize(Avg_Vacancy = mean(Days_Vacant))

p_vac <- ggplot(data = df_vacancy,
                aes(x = Position_Type,
                    y = Days_Vacant,
                    fill = Classification)) +
#  geom_point(size = 4) +
  geom_point(size = 4,
             pch = 21,
             color = "black")

p_vac +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL,
       y = "Days Vacant",
       fill = "Classification",
       title = "EMP Position Vacancies 2019 - 2022")

ggsave(path = output,
       filename = "EMP_position_vacancies.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=3,
       width=5, 
       dpi="print")

# Graph EMP position tenure ----------------------------------------------------
df_tenure <- read_csv("data/EMP_tenure_2022.csv")

# Convert to dates
df_tenure <- df_tenure %>%
  mutate(Start_Date = mdy(Start_Date)) %>% 
  mutate(End_Date = mdy(End_Date))

# Calculate years between each date 
df_tenure <- df_tenure %>%
  mutate(Tenure_yr = time_length(interval(Start_Date, End_Date), "year"))

# Summarize tenure numerically
df_tenure %>% 
  group_by(Position_Title) %>% 
  summarize(Avg_Tenure = mean(Tenure_yr))

# ES Tenure = 6.7 yr
# Sci Aide = 0.8 yr
# SES Supv = 4.3 yr

p_ten <- ggplot(data = df_tenure,
                aes(x = Position_Type,
                    y = Tenure_yr,
                    fill = Position_Title)) +
  geom_point(size = 4) +
  # geom_boxplot(width = 0.1)
  geom_point(size = 4,
             pch = 21,
             color = "black") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0.95, 
                                   vjust = 0)) 

p_ten +
  scale_fill_brewer(palette = "Set1") +
  labs(x = NULL,
       y = "Time in Position (yr)",
       fill = "Classification",
       title = "EMP Position Tenure - 2022")

ggsave(path = output,
       filename = "EMP_position_tenure.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=3.5,
       width=5, 
       dpi="print")
