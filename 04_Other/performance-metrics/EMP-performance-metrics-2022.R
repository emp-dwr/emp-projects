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
  rename("Chlorophyl" = "Chla")

df_sondes <- pivot_longer(df_sondes, 
                          values_to = "PercentPass",
                          names_to = "Sensor",
                          cols = Conductivity:Turbidity)

# Get ratings to sort in order good -> bad
rating.order <- c("Excellent","Good","Fair","Poor")

df_sondes$Rating <- factor(as.character(df_sondes$Rating), levels = rating.order)

# Plot DEMP's sonde ratings from 2022 ------------------------------------------
colors = c("darkgreen","darkblue","yellow3","darkred")

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
