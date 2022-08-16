# read in data
abs_path <- function(fp_rel = NULL, type = NULL) {
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Phyto/Phyto EDI Data/'

  if (type == 'BSA') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'phyto_data_bsa/',
        fp_rel
      ))
  } else if (type == 'ecoanalysts') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'phyto_data_ea/',
        fp_rel
      ))
  } else if (type == 'general') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        fp_rel
      ))
  } else if (type == 'export') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp
      ))
  } else if (!(type %in% c('BSA', 'ecoanalysts', 'general', 'export'))) {
    stop('type must be either BSA, ecoanalysts, or the general filepath')

  }
  return(fp_abs)
}

read_data <- function(fp, ext){
  if (ext == '.xlsx') {
    list.files(path = fp,
               pattern = '*.xlsx',
               full.names = T) %>%
      map_df(~readxl::read_xlsx(.))
  } else if (ext == '.csv') {
    list.files(path = fp,
               pattern = '*.xlsx',
               full.names = T) %>%
      map_df(~read_csv(., col_types = cols(SampleDate = 'c')))
  } else if (!(type %in% c('xlsx', 'csv '))) {
    stop('file extension must be either .xslx or .csv')
  }
}
