setwd("C:/Users/ahtran/OneDrive - California Department of Water Resources/Documents - DWR Continuous Environmental Monitoring Program/Annual Reports/2022")

library(tidyverse)
library(lubridate)
library(scales)

##Globally assign start and end dates
year <- year(Sys.Date())-1
startdate <- paste0(year,"-01-01")
enddate <- paste0(year,"-12-31")

####Functions####
##Downloads data from WQP and formats
dl_data <- function(df) {
  par <- df[["analyte_name"]]
  id <- trimws(df[["result_id"]])
  station <- df[["cdec_code"]]
  unit <- df[["unit_name"]]
  station_name <- df[["station_name"]]
  
  ## Builds datetime stamps
  startdate <- ymd(startdate)
  start_time <- format("00:00", format = "%H:%M")
  startdt <- paste0(startdate,":",start_time,":00")
  enddate <- ymd(enddate)
  end_time <- format("23:59", format = "%H:%M")
  enddt <- paste0(enddate,":",end_time,":00")
  
  ## builds url based on inputs
  url <- paste("https://dwrmsweb0263.ad.water.ca.gov/TelemetryDirect/api/Results/ResultData?program=100&resultid=",id,"&6&start=",startdt,"&end=",enddt,"&version=1",sep = "")
  data <- read.csv(url(url),sep = '|')
  # data <- readr::read_delim(url(url), delim = '|')
  
  ## Converts columns to correct data types
  data$time <- lubridate::as_datetime(data$time)
  suppressWarnings(data$value <- as.numeric(data$value))
  
  # Adds missing datetime stamps
  ts <- seq(ymd_hms(startdt), ymd_hms(enddt), by = '15 min')
  df <- data.frame(time=ts)
  data <- full_join(df,data,by = "time")
  
  ## changes QAQC flag of null and NA data to "missing"
  data <- data %>%
    mutate(qaqc_flag_id = ifelse(value == 'null' | value == "NA","M", qaqc_flag_id)) %>%
    mutate(qaqc_flag_id = replace(qaqc_flag_id, is.na(qaqc_flag_id),"M"))
  
  ## Checks data for 'Unchecked' flags, warns if true
  if("U" %in% data$qaqc_flag_id == TRUE) {
    warning(paste(station," has unchecked data in ",par,sep = ""))
  }
  
  ##Adds station and parameter names to files
  data$station <- station
  data$site_code <- sub("-","",station_name)
  data$parameter <- sub("_.*","",par)
  names(data) <- tolower(names(data))
  data$unit <- unit
  
  ##Subsets and rearranges columns 
  data <- data[,c("station","site_code","parameter","time","value","unit","qaqc_flag_id")]
  data <- data %>% arrange(data$time)
  return(data)
}

##Function to calculate daily averages for each datafile
avg_data <- function(df) {
  station <- df$station[1]
  station_name <- df$site_code[1]
  par <- df$parameter[1]
  df$time <- lubridate::as_datetime(df$time)
  df <- df %>% arrange(df$time)
  data <-  df %>% mutate(value = ifelse(qaqc_flag_id  != "G", NA, value)) #Removes non-good data
  data$date <- as.Date(data$time)
  
  #Gets daily mean and sample count
  data <- data %>%
    group_by(date) %>%
    summarize(count = sum(!is.na(value)),
              value = mean(value, na.rm = TRUE))            
  
  ## Adds columns 
  data  <- data %>% 
    mutate(site = station, site_code = sub("-","",station_name)) %>%
    mutate(month = month(date)) %>% 
    mutate(par = par) %>% 
    mutate(value = ifelse(count < 68, NA, value)) #omits days with less than 70% data recovery (96*0.7 = 68)
  
  return(data) 
}

####Download metadata table####
  id_data <- readr::read_delim(url('https://dwrmsweb0263.ad.water.ca.gov/TelemetryDirect/api/Results/ResultDetails?program=100'), delim = '|')
  id_data$station_name <- substr(id_data$station_name, 2, regexpr("\\)", id_data$station_name)-1)
  
  #list of sonde WQ constituent IDs in WQP
  cons <- c(102, 103, 104, 105, 107, 686) 
  
  ## Subsets relevant rows
  id_data <- id_data %>% 
    filter(equipment_name == 'YSI Sonde') %>% #surface sonde only
    filter(interval_id == 2) %>% #15-min data only
    filter(station_active == "Y") %>% #removes historical stations
    filter(latitude != 0) %>% #removes test stations
    filter(constituent_id %in% cons) %>% #removes non-WQ data
    filter(cdec_code != "GZB") #gets rid of stupid buoy
  rm(cons)
  
  id_data$analyte_name <- gsub(" ", "", id_data$analyte_name, fixed = TRUE)
  id_data <- id_data[,c("result_id","constituent_id","analyte_name","unit_name","cdec_code","station_name","start_date","end_date")]
  id_data <- arrange(id_data, cdec_code)

###Downloads metadata table, but for bottom data cuz I forgot about it earlier and it's easier to do it this way than think about addressing it properly
  id_data_bottom <- read.csv(url('https://dwrmsweb0263.ad.water.ca.gov/TelemetryDirect/api/Results/ResultDetails?program=100'),sep = "|")
  id_data_bottom$station_name <- substr(id_data_bottom$station_name, 2, regexpr("\\)", id_data_bottom$station_name)-1)
  
  #list of sonde WQ constituent IDs in WQP
  cons <- c(183, 184) 
  
  ## Subsets relevant rows
  id_data_bottom <- id_data_bottom %>% 
    filter(equipment_name == 'YSI Sonde 6m') %>% #bottom sonde only
    filter(interval_id == 2) %>% #15-min data only
    filter(station_active == "Y") %>% #removes historical stations
    filter(latitude != 0) %>% #removes test stations
    filter(constituent_id %in% cons) %>% #removes non-WQ data
    filter(cdec_code != "RRI") #gets rid of P8
  rm(cons)
  
  id_data_bottom$analyte_name <- gsub(" ", "", id_data_bottom$analyte_name, fixed = TRUE)
  id_data_bottom$station_name <- paste0(id_data_bottom$station_name, "_bottom")
  id_data_bottom <- id_data_bottom[,c("result_id","constituent_id","analyte_name","unit_name","cdec_code","station_name","start_date","end_date")]
  id_data_bottom <- arrange(id_data_bottom, cdec_code)

####Download data####
#uses dl_data and metadata table, saves each file to a list
data_files <- setNames(
  lapply(seq_len(nrow(id_data)), function(i) {
    dl_data(id_data[i,])
  }),
  paste0(id_data$cdec_code,"_",id_data$analyte_name)
)
#bottom data
data_files_bot <- setNames(
  lapply(
    seq_len(nrow(id_data_bottom)),
    function(i) {dl_data(id_data_bottom[i,])}
    ),
  paste0(id_data_bottom$cdec_code, '_', id_data_bottom$analyte_name)
)
####Average Data####
#Uses avg_data to calculate averages, saves to list
files <- lapply(data_files, avg_data)
files_bot <- lapply(data_files_bot, avg_data)

#Merges files
files <- c(files,files_bot)
data_avg_all <- do.call(rbind, files)

#Assigns regions
data_avg_all <- data_avg_all %>%
  mutate(region = case_when(
    site %in% c("ANH", "SSI", "MAL") ~ "Confluence",
    site %in% c("FRK", "TWI", "PPT") ~ "Central Delta",
    site %in% c("RRI", "MSD", "SJR") ~ "Southern Interior Delta",
    site %in% c("HON", "RYC", "GZL", "MRZ") ~ "Grizzly and Suisun Bays",
    site %in% c("SRH", "RVB") ~ "Northern Interior Delta",
    TRUE ~ NA
  ))

####Save all data to sharepoint####
lapply(names(data_files), function(x) {
  df <- data_files[[x]]
  filename <- paste0("./data/raw/",x, ".csv")
  write.csv(df, filename, row.names = FALSE)
})

lapply(names(files), function(x) {
  df <- files[[x]]
  filename <- paste0("./data/avg/",x, ".csv")
  write.csv(df, filename, row.names = FALSE)
})

lapply(names(files_bot), function(x) {
  df <- files_bot[[x]]
  filename <- paste0("./data/avg/",x, "_bottom.csv")
  write.csv(df, filename, row.names = FALSE)
})

write.csv(data_avg_all, "./data/data_avg_all.csv", row.names = FALSE)

