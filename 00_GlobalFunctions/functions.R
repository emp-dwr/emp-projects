#' @importFrom magrittr %>%

# import EMP data from EDI
import_emp_data <- function(version = 'newest', show_version = FALSE, show_col_types = FALSE, col_types = NULL, subset_EZs = FALSE){
  if (version != 'newest'){
    url_emp <- paste0('https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.',as.character(version),'&entityid=dfeaee030be901ae00b8c0449ea39e9c')
  } else{
    # url for most recent version
    url_emp <- 'https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.7&entityid=dfeaee030be901ae00b8c0449ea39e9c'
  }
  
  # extract version number
  if(show_version){
    version_num <- str_remove(str_extract(url_emp, 'edi\\..*&'),'&')
    print(paste('EMP data ID:',version_num))
  }
  
  # download file
  download.file(url_emp, file.path(tempdir(), 'EMP_data.csv'), mode='wb')
  
  # import data
  df_wq <- readr::read_csv(file.path(tempdir(), 'EMP_data.csv'), show_col_types = show_col_types, col_types = col_types)
  
  # subsets
  if (subset_EZs){
    df_wq <- df_wq[!grepl('EZ', df_wq$Station),]
  }
  
  return(df_wq)
}

# import EMP data from EDI
import_emp_stations <- function(version = 'newest', show_version = FALSE, show_col_types = FALSE, col_types = NULL, active_only = FALSE, subset_EZs = FALSE){
  if (version != 'newest'){
    url_emp <- paste0('https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.',as.character(version),'&entityid=ecf241d54a8335a49f8dfc8813d75609')
  } else{
    # url for most recent version
    url_emp <- 'https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.7&entityid=ecf241d54a8335a49f8dfc8813d75609'
  }
  
  # extract version number
  if(show_version){
    version_num <- str_remove(str_extract(url_emp, 'edi\\..*&'),'&')
    print(paste('EMP data ID:',version_num))
  }
  
  # download file
  download.file(url_emp, file.path(tempdir(), 'EMP_stations.csv'), mode='wb')
  
  # import data
  df_stations <- readr::read_csv(file.path(tempdir(), 'EMP_stations.csv'), show_col_types = show_col_types, col_types = col_types)
  df_stations$StartDate <- as.Date(df_stations$StartDate, '%m/%d/%Y')
  df_stations$EndDate <- as.Date(df_stations$EndDate, '%m/%d/%Y')
  
  # subsets
  if (active_only){
    df_stations <- df_stations[df_stations$Status == 'Active',]
  }
  
  if (subset_EZs){
    df_stations <- df_stations[!grepl('EZ', df_stations$Station),]
  }
  
  return(df_stations)
}

# region run assigner
assign_run_regions <- function(df_edi, subset_regions = FALSE, subset_EZs = FALSE){
  
  # assign regions
  df_edi <- df_edi %>%
    dplyr::mutate(
      Region = dplyr::case_when(
        Station %in% c('D16', 'D19','D26','D28A') ~ 'Central Delta',
        Station %in% c('D10','D12','D22','D4') ~ 'Confluence',
        Station %in% c('C3A','NZ068') ~ 'Northern Interior Delta',
        Station %in% c('C10A','C9','MD10A','P8') ~ 'Southern Interior Delta',
        Station %in% c('D41', 'D41A','D6','NZ002','NZ004','NZ325') ~ 'San Pablo Bay',
        Station %in% c('D7','D8','NZ032','NZS42') ~ 'Suisun & Grizzly Bays',
        TRUE ~ NA_character_
        )
    )
  
  # subsets
  if(subset_regions){
    df_edi <- df_edi[!is.na(df_edi$Region),]
  }
  
  if (subset_EZs){
    df_edi <- df_edi[!grepl('EZ', df_edi$Station),]
  }
  
  return(df_edi)
}