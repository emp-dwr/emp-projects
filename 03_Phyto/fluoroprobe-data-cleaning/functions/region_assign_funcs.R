create_fl_dir <- function(year){
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Water Quality/'
  dir_test <- paste0(Sys.getenv('USERPROFILE'),'/',fp_emp)
  dir.create(dir_test, showWarnings = FALSE)
  return(NULL)
}

extract_year <- function(fp){
  year <- str_extract(fp, '\\d{2}')
  year <- paste0('20',year)
  return(year)
}

extract_month <- function(fp){
  month <- str_extract(fp, '\\d{8}')
  month <- str_extract(month, '^..')
  return(month)
}

abs_path <- function(fp_rel, type = NULL, year = NULL) {
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/'
  
  
  if (type == 'fluoro') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        '00 - Monthly Run Docs/Raw Field Data/',
        fp_rel
      ))
    
  } else if (type == 'MOPED') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'MOPED Data/',
        fp_rel
      ))
  }
  else if (type == 'export') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'Cleaned Data/',
        year,
        fp_rel
      ))
  }
  return(fp_abs)
}