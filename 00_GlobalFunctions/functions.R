# import EMP data from EDI
import_emp_data <- function(version = 'newest', show_version = FALSE, show_col_types = FALSE, col_types = NULL){
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
  
  return(df_wq)
}

# import EMP data from EDI
import_emp_stations <- function(version = 'newest', show_version = FALSE, show_col_types = FALSE, col_types = NULL, active_only = FALSE, ez_stations = FALSE){
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
  
  # subset active
  if (active_only){
    df_stations <- df_stations[df_stations$Status == 'Active',]
  }
  
  if (!ez_stations){
    df_stations <- df_stations[!grepl('EZ', df_stations$Station),]
  }
  
  return(df_stations)
}
