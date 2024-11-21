#' @importFrom magrittr %>%

# import data from EDI
get_edi_file <- function(pkg_id, fnames, verbose = FALSE){
  # get revision
  revision_url <- glue::glue('https://pasta.lternet.edu/package/eml/edi/{pkg_id}')
  all_revisions <- readLines(revision_url, warn = FALSE) 
  latest_revision <- tail(all_revisions, 1)
  if (verbose) {
    message('Latest revision: ', latest_revision)
  }
  # get entities 
  pkg_url <- glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}')
  all_entities <- readLines(pkg_url, warn = FALSE)
  name_urls <- glue::glue('https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}')
  names(all_entities) <- purrr::map_chr(name_urls, readLines, warn = FALSE)
  if (verbose) {
    message('Package contains files:\n', 
            stringr::str_c('    ', names(all_entities), sep = '', collapse = '\n'))
  }
  # select entities that match fnames
  fname_regex <- stringr::str_c(glue::glue('({fnames})'), collapse = '|')
  included_entities <- all_entities[stringr::str_detect(names(all_entities), fname_regex)]
  if(length(included_entities) != length(fnames)){
    stop('Not all specified filenames are included in package')
  }
  # download data
  if (verbose) {
    message('Downloading files:\n',
            stringr::str_c('    ', names(included_entities), sep = '', collapse = '\n'))
  }
  dfs <- purrr::map(glue::glue('https://portal.edirepository.org/nis/dataviewer?packageid=edi.{pkg_id}.{latest_revision}&entityid={included_entities}'),
                   readr::read_csv, guess_max = 1000000)
  names(dfs) <- names(included_entities)
  dfs
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

# abs path
abs_path_emp <- function(fp_rel = NULL) {
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/'
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_emp))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_emp, fp_rel))
  }
  
  return(fp_abs)
}

# station renaming
replace_station <- function(df_data, df_names) {
  df_joined <- df_data %>%
    left_join(df_names, by = 'Station')
  
  dif_stations_orig <- setdiff(df_data$Station, df_names$Station)
  dif_stations <- setdiff(dif_stations_orig, df_names$Station_new)
  
  if (length(dif_stations) > 0) {
    warning(glue::glue('The following stations are not in the reference list: {toString(dif_stations)}'))
  } else {
    print('All stations in reference list')
  }
  
  df_joined <- df_joined %>%
    mutate(Station = coalesce(Station_new, Station)) %>%
    select(-Station_new)
  
  return(df_joined)
}

# merge csv files
merge_csv_files <- function(folder_path) {
  # list of .csv files
  csv_files <- list.files(folder_path, pattern = '\\.csv$', full.names = TRUE)
  
  # read into list of dfs
  df_list <- map(csv_files, ~ read_csv(.x, show_col_types = FALSE))
  
  # find all unique column names across all dfs
  all_columns <- unique(unlist(lapply(df_list, names)))
  
  # add in missing columns
  add_missing_columns <- function(df, all_columns) {
    missing_columns <- setdiff(all_columns, names(df))
    df[missing_columns] <- NA
    df <- df %>%
      mutate(across(everything(), as.character))
    return(df)
  }
  
  df_list <- lapply(df_list, add_missing_columns, all_columns = all_columns)
  
  # merge dfs
  df_merged <- bind_rows(df_list)
  
  return(df_merged)
}

# remove labdupes
remove_labdupes <- function(df, cols = 'all_cols'){
  if (cols == 'all_cols'){
    df_ret <- df %>%
      mutate(across(everything(), ~ str_extract(., '^[^,]*')))
  } else{ # dunno if this part works
    df_ret <- df %>%
      mutate({{cols}}, ~ str_extract(., '^[^,]*'))
  }
  
  return(df_ret)
}
