create_fl_dir <- function(year){
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Fluoroprobe'
  dir_test <- paste0(Sys.getenv('USERPROFILE'),'/',fp_emp,'/Cleaned Data/',year)
  dir.create(dir_test, showWarnings = FALSE)
  return(NULL)
}

abs_path <- function(fp_rel, type = NULL, year = NULL) {
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Fluoroprobe'
  
  if (type == 'fluoro') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'Fluoroprobe Data/',
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