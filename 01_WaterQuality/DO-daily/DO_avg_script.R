# DO Daily Average Calculation
# calculates DO average for previous day(s) from continuous sonde and appends to output text file
# data pulled directly from CDEC
# questions: sarah.perry@water.ca.gov

# import packages
suppressWarnings(suppressMessages(library(cder)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(R.utils)))
`%ni%` <- Negate(`%in%`)

# --- Read in Data ---
# filepath for DO data (ie. txt file to write data to)
# assumes connection to S: drive
fp_DOdata <- paste0('S:/M & A BRANCH/Discrete EMP/Code/DO-average-calc/DO_Check.txt')

# import DO Data (to determine which dates avg needs to be calc'd for)
doData <- read.csv(fp_DOdata,
                   header = FALSE,
                   sep = '\t',
                   skip = R.utils::countLines(fp_DOdata) - 1)

# --- Pull Data ---
# define today's date
today <- as.Date(Sys.Date()) # will technically mean its pulled until yesterday

# define last day avg was calc'd
last_day <- as.Date(tail(doData,1)$V1)

# check if up to date
if(Sys.Date()-1 == last_day) {stop('DO values are up to date')
  } else {
  # Pull non-QA'd data from CDEC
  df_SDO <- cdec_query('SDO', sensors = c(61, 201, 202), # 0, 3, 6 depth
                       start.date = last_day, 
                       end.date = today)
  
  # remove data from midnight
  df_SDO <- df_SDO %>% filter(df_SDO$DateTime < today)
  
  # calculate daily average
  high_val_months <- c(9, 10, 11) # for threshold
  
  df_mean <- df_SDO %>%
    mutate(Date = floor_date(DateTime, unit = 'day')) %>%
    group_by(SensorType, Date) %>%
    summarize(mean_DO = round(mean(Value, na.rm = TRUE),2), .groups = 'drop') %>%
    pivot_wider(names_from = SensorType, values_from = mean_DO) %>%
    rename(DO_1m = `DIS OXY`, DO_3m = `DOXY 3M`, DO_6m = `DOXY 6M`)
  
  # add in pass/fail cols
  df_mean <- df_mean %>%
    mutate(thres_1m = case_when(month(Date) %in% c(9, 10, 11) ~ 6.5,
                                month(Date) %ni% high_val_months ~ 5.5),
           thres_3m = thres_1m,
           thres_6m = thres_1m,
           passed_1m = case_when(DO_1m >= thres_1m ~ 'Passed',
                                 DO_1m < thres_1m ~ 'Failed'),
           passed_3m = case_when(DO_3m >= thres_3m ~ 'Passed',
                                 DO_3m < thres_3m ~ 'Failed'),
           passed_6m = case_when(DO_6m >= thres_6m ~ 'Passed',
                                 DO_6m < thres_6m ~ 'Failed'))
  
  # reorder df
  df_mean <- df_mean %>% select(Date, DO_1m, thres_1m, passed_1m, DO_3m, thres_3m, passed_3m, DO_6m, thres_6m, passed_6m)
  
  # export
  write.table(df_mean,
              fp_DOdata,
              append = TRUE,
              sep = '\t',
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE)
  }
