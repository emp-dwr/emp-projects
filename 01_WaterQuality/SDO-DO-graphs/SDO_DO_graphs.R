## Graph DO Values at all three depths at RRI (CDEC ID = SDO)
## Started Summer 2021
## Ted Flynn

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("timetk");packageVersion("timetk")
library("cder");packageVersion("cder")

# Set working directory
setwd("./SDO-DO-graphs")
getwd()

# Set Black-and-white color scheme
theme_set(theme_bw())

# Clean workspace
rm(list=ls())

## Create date variables
start <- as.Date("2022-08-01")
end <- Sys.Date()

## Pull non-QA'd data from CDEC
df_DO <- cdec_query("SDO", sensors = c(61, 201, 202), 
                       start.date = start, 
                       end.date = end)

## Remove Single Midnight-only data point
df_DO <- df_DO %>% filter(df_DO$DateTime < end)

## Rename sensors to match
df_DO$SensorType <- gsub("DIS OXY","1m",df_DO$SensorType)
df_DO$SensorType <- gsub("DOXY 3M","3m",df_DO$SensorType)
df_DO$SensorType <- gsub("DOXY 6M","6m",df_DO$SensorType)

df_DO <- df_DO %>% 
  rename("Depth" = "SensorType")

## Remove unneeded columns
df_DO <- df_DO %>% select(Depth, DateTime, Value)

## Calculate daily average, min, and max
df_DO_daily <- df_DO %>%
  mutate(Date = floor_date(DateTime, unit = "day")) %>%
  group_by(Depth, Date) %>%
  summarize(mean_DO = round(mean(Value, na.rm = TRUE), 1), 
            min_DO = min(Value, na.rm = TRUE),
            max_DO = max(Value, na.rm = TRUE)) %>%
  ungroup()

## Remove infinite min values from days with no data
df_DO_daily <- df_DO_daily %>% filter(!is.infinite(min_DO))

## Add column of just the year for highlighting yearly data
## Add Julian date for plotting
df_DO_daily <- df_DO_daily %>%
  mutate(Year = year(df_DO_daily$Date)) %>%
  mutate(Month = month(df_DO_daily$Date, label = TRUE)) %>%
  mutate(Month.Num =month(df_DO_daily$Date)) %>%
  mutate(Julian = yday(df_DO_daily$Date))

## Order month in calendar order rather than (default) alphabetical
df_DO_daily$Month = factor(df_DO_daily$Month, levels = month.abb)

## Convert years to characters for plotting
df_DO_daily <- df_DO_daily %>% mutate(Year = as.character(df_DO_daily$Year))

## Reorder date/time columns
df_DO_daily <- df_DO_daily %>% 
  relocate(Year, .after = Date) %>% 
  relocate(Month, .after = Year) %>%
  relocate(Month.Num, .after = Month) %>%
  relocate(Julian, .after = Month.Num)

## Create Category for higher DO threshold months
df_DO_daily <- df_DO_daily %>% mutate(df_DO_daily, threshold = ifelse(Month.Num %in% 9:11, 6, 5))
df_DO_daily <- df_DO_daily%>% mutate(df_DO_daily, threshold.EMP = ifelse(Month.Num %in% 9:11, 6.5, 5.5))

df_DO_daily <- df_DO_daily %>% mutate(df_DO_daily, passfail.mean = case_when(
  df_DO_daily$mean_DO >= threshold.EMP ~ "Pass",
  df_DO_daily$mean_DO < threshold.EMP & df_DO_daily$mean_DO >= threshold ~ "Threshold",
  df_DO_daily$mean_DO < threshold ~ "Fail"))

df_DO_daily <- df_DO_daily %>% mutate(df_DO_daily, passfail.min = case_when(
  df_DO_daily$min_DO >= threshold.EMP ~ "Pass",
  df_DO_daily$min_DO < threshold.EMP & df_DO_daily$min_DO >= threshold ~ "Threshold",
  df_DO_daily$min_DO < threshold ~ "Fail"
))

unique(df_DO_daily$passfail.min)

df_DO_daily <- df_DO_daily %>% drop_na(passfail.mean)
df_DO_daily <- df_DO_daily %>% drop_na(passfail.min)

## Set DO Criteria order
criteria_order <- c("Pass","Threshold","Fail")

## Re-order criteria 
df_DO_daily$passfail.mean <- factor(as.character(df_DO_daily$passfail.mean), levels = criteria_order)
df_DO_daily$passfail.min <- factor(as.character(df_DO_daily$passfail.min), levels = criteria_order)

## Plot daily mean
daily_mean_plot <- ggplot(df_DO_daily, aes(x = Date, y = mean_DO, color = passfail.mean)) +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.minor = element_blank()) +
  #geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=1,  position=position_dodge(.9)) +
  geom_point(size = 2) +
  scale_color_manual(name = "Basin Plan Criteria", guide = "legend", values = c("darkgreen","yellow3","red")) +
  #geom_hline(aes(yintercept=6.5, linetype="6.5 mg/L"), color = "orange") +
  #geom_hline(aes(yintercept=6.0, linetype="6.0 mg/L"), color = "orange") +
  scale_linetype_manual(name = "Limits", values = c(1,2), 
                      guide = guide_legend(override.aes = list(linetype = c(1,2),
                                                               color = c("orange","orange")))) +
  #scale_x_date(date_breaks = "2 day") +
  #stat_smooth(method="glm", level=0.95, color = "darkred", size = 2) +
  labs(x = "Date",
       y = "[Dissolved Oxygen] (mg/L)", 
       title = "Daily Mean DO at RRI, Summer 2022") 

daily_mean_plot +
  facet_wrap(Depth ~ ., ncol = 1, dir = "h")

ggsave(path="plots",
       filename = "daily.mean.DO.RRI.Summer22.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=7, 
       dpi="print")

## Plot Daily Minimum
daily_min_plot <- ggplot(df_DO_daily, aes(x = Date, y = min_DO, color = passfail.min)) +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.minor = element_blank()) +
  #geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=1,  position=position_dodge(.9)) +
  geom_point(size = 2) +
  scale_color_manual(name = "Basin Plan Criteria", guide = "legend", values = c("darkgreen","yellow3","red")) +
  #geom_hline(aes(yintercept=6.5, linetype="6.5 mg/L"), color = "orange") +
  #geom_hline(aes(yintercept=6.0, linetype="6.0 mg/L"), color = "orange") +
  scale_linetype_manual(name = "Limits", values = c(1,2), 
                        guide = guide_legend(override.aes = list(linetype = c(1,2),
                                                                 color = c("orange","orange")))) +
  #scale_x_date(date_breaks = "2 day") +
  #stat_smooth(method="glm", level=0.95, color = "darkred", size = 2) +
  labs(x = "Date",
       y = "[Dissolved Oxygen] (mg/L)", 
       title = "Daily Minimum DO at RRI, Fall 2021") 

daily_min_plot +
  facet_wrap(Depth ~ ., ncol = 1, dir = "h")

ggsave(path="plots",
       filename = "daily.min.DO.RRI.Summer2022.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=7,
       width=7, 
       dpi="print")

