create_fl_dir <- function(year){
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Fluoroprobe'
  dir_test <- paste0(Sys.getenv('USERPROFILE'),'/',fp_emp,'/Cleaned Data/',year)
  dir.create(dir_test, showWarnings = FALSE)
  return(NULL)
}

abs_path_fl <- function(fp_rel, type = NULL, year = NULL) {
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

abs_path <- function(fp_rel, type = NULL) {
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
    
  } else if (!(type %in% c('BSA', 'ecoanalysts'))) {
    stop('type must be either BSA or ecoanalysts')
    
  }
  return(fp_abs)
}

read_file <- function(fp_abs, type = NULL) {
  if (type == 'BSA') {
    if (str_detect(fp_abs, '.xls')){
      df <- readxl::read_xls(fp_abs)
    } else {
      df <- readxl::read_xlsx(fp_abs)
    }
    
  } else if (type == 'ecoanalysts') {
    df <- readr::read_csv(fp_abs, skip = 7)

  } else if (!(type %in% c('BSA', 'ecoanalysts'))) {
    stop('type must be either BSA or ecoanalysts')
    
  }
  return(df)
}

check_codes <- function(df, type = NULL){
  if (type == 'BSA' | type == 'ecoanalysts') {
    df_codes <- df %>%
      count(StationCode) %>%
      na.omit() %>%
      arrange(n)
    
    } else if (!(type %in% c('BSA','ecoanalysts'))){
      stop('type must be either BSA or ecoanalysts')
      
    }
  return(df_codes)
}

check_dates <- function(df, type = NULL){
  if (type == 'BSA') {
    years <- unique(year(df$SampleDate)) %>% na.omit()
    months <- unique(month(df$SampleDate)) %>% na.omit()
    
    cat('Years:', years, '\nMonths:', months)
    
  } else  if (type == 'ecoanalysts') {
    df$SampleDate <- as.Date(df$SampleDate, format = '%m/%d/%Y')
    years <- unique(year(df$SampleDate)) %>% na.omit()
    months <- unique(month(df$SampleDate)) %>% na.omit()
    
    cat('Years:', years, '\nMonths:', months)
    
  } else if (!(type %in% c('BSA','ecoanalysts'))){
    stop('type must be either BSA or ecoanalysts')
    
  }
}

check_times <- function(df, type = NULL){
  if (type == 'BSA') {
    hours <- hour(ymd_hms(df$SampleTime))
    bad_hrs <- hours < 6 | hours > 20
    bad_hrs <- bad_hrs %>% na.omit()
    
    mins <- minute(ymd_hms(df$SampleTime))
    bad_mins <- mins < 0 | mins > 59
    bad_mins <- bad_mins %>% na.omit()
    
    cat('Hours outside range:', hours[bad_hrs], '\nMinutes outside range:', mins[bad_mins])
    
  } else  if (type == 'ecoanalysts') {
    hours <- hour(ymd_hms(df$SampleTime))
    bad_hrs <- hours < 6 | hours > 20
    bad_hrs <- bad_hrs %>% na.omit()
    
    mins <- minute(ymd_hms(df$SampleTime))
    bad_mins <- mins < 0 | mins > 59
    bad_mins <- bad_mins %>% na.omit()
    
    cat('Hours outside range:', hours[bad_hrs], '\nMinutes outside range:', mins[bad_mins])
    
  } else if (!(type %in% c('BSA','ecoanalysts'))){
    stop('type must be either BSA or ecoanalysts')
    
  }
}

check_counts <- function(df, type = NULL) {
  if (type == 'BSA') {
    df_counts <- df %>%
      select(Taxon,
             `Unit Abundance (# of Natural Units)`,
             `Total Number of Cells`,
             `Colony/Filament/Individual Group Code`) %>%
      na.omit() %>%
      arrange(desc(`Unit Abundance (# of Natural Units)`))
    
  } else  if (type == 'ecoanalysts') {
    df_counts <- df %>%
      select(TAXON,
             `Unit Abundance`,
             `Cells/Unit`,
             `Colony/Filament Group Code`) %>%
      na.omit() %>%
      arrange(desc(`Unit Abundance`))
    
  } else if (!(type %in% c('BSA', 'ecoanalysts'))) {
    stop('type must be either BSA or ecoanalysts')
    
  }
  return(df_counts)
}

check_taxa <- function(df, type = NULL){
  if (type == 'BSA') {
    df_taxa <- df %>%
      select(Taxon, Genus, Species) %>%
      na.omit() %>%
      unite(Genus_Species, c('Genus', 'Species'), sep = ' ')
    
    df_taxa$Taxon <- stringr::str_remove(df_taxa$Taxon,'cf. ')
    
    df_taxa <- df_taxa[!(df_taxa$Taxon == df_taxa$Genus_Species),]
    
  } else  if (type == 'ecoanalysts') {
    df_taxa <- df %>%
      select(TAXON, Genus, Species) %>%
      na.omit() %>%
      unite(Genus_Species, c('Genus', 'Species'), sep = ' ')
    
    df_taxa$TAXON <- stringr::str_remove(df_taxa$TAXON,'cf. ')
    df_taxa$TAXON <- stringr::str_remove(df_taxa$TAXON,'(single cell)')
    df_taxa$TAXON <- stringr::str_remove(df_taxa$TAXON,' ()')
    
    df_taxa <- df_taxa[!(df_taxa$TAXON == df_taxa$Genus_Species),]
    
  } else if (!(type %in% c('BSA','ecoanalysts'))){
    stop('type must be either BSA or ecoanalysts')
    
  }
  return(df_taxa)
}

check_genus <- function(df, type = NULL){
  if (type == 'BSA' | type == 'ecoanalysts') {
    df_genus <- df %>%
      count(Genus) %>%
      na.omit() %>%
      arrange(n)
    
  } else if (!(type %in% c('BSA','ecoanalysts'))){
    stop('type must be either BSA or ecoanalysts')
    
  }
  return(df_genus)
}

check_species <- function(df, type = NULL){
  if (type == 'BSA' | type == 'ecoanalysts') {
    df_species <- df %>%
      count(Species) %>%
      na.omit() %>%
      arrange(n)

  } else if (!(type %in% c('BSA','ecoanalysts'))){
    stop('type must be either BSA or ecoanalysts')
    
  }
  return(df_species)
}

check_shape <- function(df, type = NULL) {
  if (type == 'BSA') {
    shape <- unique(df$Shape %>% na.omit())
    
  } else  {
    stop('only for BSA data')
    
  }
  return(shape)
}
