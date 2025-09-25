# PHYCOPROBE 5-MINUTE INTERVALS ================================================

setwd("C:/Users/pcarpent/OneDrive - California Department of Water Resources/my files/projects/golden mussels/phycoprobe at stations")

library(data.table)
library(dplyr)
library(fuzzyjoin)
library(ggplot2)
library(lubridate)
library(purrr)
library(readr)
library(reshape2)
library(stringi)
library(stringr)
library(tidyr)

# Combine all phycoprobe files -------------------------------------------------

# List files
files <- list.files(
  path = "raw phycoprobe data",                                                 # selects files in folder 'raw phycoprobe data'
  pattern = "^(Fluoroprobe|Phycoprobe)_.*\\.txt$",                              # selects files that start with Fluroprobe or Phycoprobe
  full.names = TRUE,
  recursive = TRUE
)
read_phyco_file <- function(file) {
  raw <- read_delim(file, delim = "\t",                                         # read text file as delimited
                    col_names = FALSE, 
                    show_col_types = FALSE)

  new_names <- paste0(raw[1, ], "_", raw[2, ])                                  # create column names from first two rows ("Green Algae" + "ug/L" = "Green Algae_ug/L)
  new_names <- as.character(new_names)                                          # convert to character strings
 
  new_names <- new_names %>%                                                    # clean column names
    str_replace_all("µ", "u") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("-", "_") %>%
    str_replace_all("\\[|\\]", "")
  
  new_names <- make.unique(new_names)                                           # make names unique
  colnames(raw) <- new_names
  
  raw <- raw[-c(1,2), ]                                                         # drop first two rows
  
  if("Date/Time_date" %in% colnames(raw)){
    raw <- raw %>%
      mutate(datetime = 
               parse_date_time(.data[["Date/Time_date"]],
               orders = c("mdy HMS", "mdy HM", "ymd HMS", "ymd HM")))
  }
  
  return(raw)
}


# Read all files and combine
phyco_all <- map_df(files, read_phyco_file)           

setnames(phyco_all, old = colnames(phyco_all),                                  # standardize column names 
         new = stri_trans_general(colnames(phyco_all), "latin-ascii") %>%       # convert all characters to plain ASCII 
           gsub("[^A-Za-z0-9_]", "_", .))                                       # replace anything that isn't a number, letter, or underscore with _


anyDuplicated(colnames(phyco_all))                                              # check that column names are unique
colnames(phyco_all)[duplicated(colnames(phyco_all))]                            # see which columns are duplicated
which(colnames(phyco_all) == "Temp_Sample__C")                                  # show where the duplicated columns are (in this case, temperature)
all_equal <- all(phyco_all[[13]] == phyco_all[[67]],                            # check if columns are exact duplicates
                 na.rm = TRUE)
all_equal
phyco_all <- phyco_all[, !duplicated(colnames(phyco_all))]                      # remove duplicated columns (if equal = TRUE)
anyDuplicated(colnames(phyco_all))                                              # check again if there are any duplicates

#colnames(phyco_all)                                                            # check column names
#sum(is.na(phyco_all$datetime))                                                 # check that there are 0 rows that failed to parse

# Read water sample file -------------------------------------------------------

samples <- read_csv("EMP_DWQ_1975_2023.csv") %>%
  mutate(                                                                       # combine date + time columns
    sample_datetime = parse_date_time(
      paste(Date, Time),
      orders = c("ymd HMS")
    )
  ) %>%
  filter(!is.na(sample_datetime)) %>%                                           # remove rows where parsing failed
  filter(sample_datetime >= as.Date("2020-01-01"))                              # keep only samples from 2020 onward

 range(samples$sample_datetime)                                                 # check date range after filtering


# Join with 5 min window and average ------------------------------------------

# colnames(phyco_all)                                                           # check column names
# colnames(samples)

dt_phyco <- as.data.table(phyco_all)                                            # convert to data.table
dt_samples <- as.data.table(samples)

dt_samples[, `:=`(start = sample_datetime - 150, end = sample_datetime + 150)]  # create 5-minute interval windows (300 seconds total)
dt_phyco[, `:=`(start = datetime, end = datetime)]

phyco_cols <- setdiff(names(dt_phyco), c("datetime", "start", "end"))           # selects measurement data columns from dt_phyco (excluding datetime, start, end)
char_cols  <- intersect(phyco_cols, names(dt_phyco)[sapply(dt_phyco,            # creates list of character measurement columns
                                                           is.character)]) 

dt_phyco[, (char_cols) := lapply(.SD, as.numeric), .SDcols = char_cols]         # apply as.numeric to the character columns

phyco_numeric_cols <- names(dt_phyco)[sapply(dt_phyco,                          # identify numeric columns for averaging
                                      is.numeric) & !(names(dt_phyco) %in% 
                                      c("start", "end", "datetime"))]

setkey(dt_samples, start, end)                                                  # set keys for interval join
setkey(dt_phyco, start, end)

joined <- foverlaps(dt_phyco, dt_samples, type = "any", nomatch = 0L)           # interval join: get all phyco rows within 5 minutes of sample

fwrite(joined, "phycoprobe_matches_raw.csv")                                    # write out all joined (un-averaged) rows for QA/QC


phyco_avg <- joined[, lapply(.SD, mean, na.rm = TRUE), by = .(sample_datetime), # average numeric phycoprobe columns by sample_datetime
                      .SDcols = phyco_numeric_cols]

final_data <- merge(dt_samples, phyco_avg, by = "sample_datetime", all.x = TRUE)# merge back with samples

fwrite(final_data, "final_data.csv")                                            # save final_data as csv


# Check ------------------------------------------------------------------------

# stacked area plot
phyco_long <- final_data %>%                                                
  select(sample_datetime, Station, Green_Algae__g_l, Bluegreen__g_l, Diatoms__g_l, Cryptophyta__g_l) %>%
  pivot_longer(cols = -c(sample_datetime, Station), names_to = "Taxa", values_to = "Concentration")

ggplot(phyco_long, aes(x = sample_datetime, y = Concentration, fill = Taxa)) +
  geom_area(position = "fill") +
  facet_wrap(~Station) +
  labs(title = "Phycoprobe Taxa Over Time", y = "Concentration (g/L)", x = "Date") +
  theme_minimal()

phyco_long_cells_mL <- final_data %>%                                                
  select(sample_datetime, Station, Green_Algae_cells_ml, Bluegreen_cells_ml, Diatoms_cells_ml, Cryptophyta_cells_ml) %>%
  pivot_longer(cols = -c(sample_datetime, Station), names_to = "Taxa", values_to = "Concentration")

ggplot(phyco_long_cells_mL, aes(x = sample_datetime, y = Concentration, fill = Taxa)) +
  geom_area(position = "stack") +
  facet_wrap(~Station) +
  labs(title = "Phycoprobe Taxa Over Time", y = "Concentration (cells/mL)", x = "Date") +
  theme_minimal()

phyco_long %>%
  filter(Station == "P8") %>%
  ggplot(aes(x = sample_datetime, y = Concentration, fill = Taxa)) +
  geom_area(position = "fill") +
  labs(title = "Phycoprobe Taxa Over Time (Station P8)",
       y = "Concentration (g/L)", 
       x = "Date") +
  theme_minimal()


# boxplot: diatom distribution by station
ggplot(final_data, aes(x = Station, y = Diatoms__g_l)) +
  geom_boxplot(outlier.shape = 1) +
  scale_y_log10() +
  labs(title = "Diatom Distribution by Station (log scale)", y = "Diatoms (g/L)", x = "Station") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# line plot: green algae
ggplot(final_data, aes(x = sample_datetime, y = Green_Algae__g_l, color = Station)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  scale_y_log10() +
  labs(title = "Smoothed Green Algae Trend", y = "Green Algae (g/L, log scale)", x = "Date") +
  theme_minimal()


# line plot split by taxa
taxa_cols <- c("Green_Algae__g_l", "Bluegreen__g_l", "Diatoms__g_l", "Cryptophyta__g_l", "Planktothrix__g_l")

phyco_long <- final_data %>%
  select(sample_datetime, Station, all_of(taxa_cols)) %>%
  pivot_longer(cols = -c(sample_datetime, Station), names_to = "Taxa", values_to = "Concentration") %>%

  group_by(Taxa) %>%                                                            # filter out extreme outliers
  mutate(upper_limit = quantile(Concentration, 0.99, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Concentration <= upper_limit)

ggplot(phyco_long, aes(x = sample_datetime, y = Concentration + 1, color = Station)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  scale_y_log10() +
  facet_wrap(~Taxa) +
  labs(title = "Phycoprobe Taxa Over Time (99th percentile filtered, log scale)",
       y = "Concentration (g/L, log scale)", x = "Date") +
  theme_minimal() +
  theme(legend.position = "bottom")


# 5-minute interval visualization
sample_to_check <- as.POSIXct("2020-08-11 11:50:00", tz = "UTC")

#"2020-08-10 08:25:00"
#"2020-08-11 11:50:00"
#"2020-08-18 14:30:00"
#"2022-08-23 12:05:00"

# Subset the joined table to just that sample
check_rows <- joined[sample_datetime == sample_to_check]

# Choose a few key numeric phycoprobe columns to plot
cols_to_plot <- c("Green_Algae__g_l", "Bluegreen__g_l", "Diatoms__g_l", "Cryptophyta__g_l")

# Melt to long format for plotting
check_long <- melt(check_rows, measure.vars = cols_to_plot,
                   variable.name = "Taxon", value.name = "Value")

# Extract final averaged values from final_data
avg_row <- final_data[sample_datetime == sample_to_check, ..cols_to_plot]
avg_long <- melt(avg_row, variable.name = "Taxon", value.name = "Average_Value")

# Plot raw points and overlay the averaged value as a big red point
ggplot(check_long, aes(x = datetime, y = Value, color = Taxon)) +
  geom_point() +
  geom_line() +
  geom_point(data = avg_long, aes(x = sample_to_check, y = Average_Value),
             color = "red", size = 3, shape = 18) +
  labs(title = paste("Phycoprobe values ±5 min around", sample_to_check),
       subtitle = "Red points = averaged value used in final_data",
       x = "Datetime", y = "Value") +
  theme_minimal()
