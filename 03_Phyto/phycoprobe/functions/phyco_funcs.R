create_dir <- function(year){
  fp_emp <- '/California Department of Water Resources/Environmental Monitoring Program - Documents/Water Quality/'
  dir_test <- paste0(Sys.getenv('USERPROFILE'),fp_emp,'Phycoprobe/Cleaned Data/',year)
  dir.create(dir_test, showWarnings = FALSE)
  return(dir_test)
}

fp_main_export <- function(year){
  fp_emp <- '/California Department of Water Resources/Environmental Monitoring Program - Documents/Water Quality/'
  fp_full <- paste0(Sys.getenv('USERPROFILE'),fp_emp,'EDI Data/',year,' Data Publishing/EMP_Phycoprobe_',year)
  return(fp_full)
}

month_num <- function(name){
  df_month <- data.frame(name = array(month.name), abbrv = array(month.abb), num = 1:12)
  
  num <- df_month$num[tolower(df_month$name) == tolower(name)]  

  num <- sprintf('%02.0f', num)
  
  return(num)
}

norm_month <- function(month, type = 'name'){
  # create df of months
  df_month <- data.frame(name = array(month.name), abbrv = array(month.abb), num = 1:12)
  
  # extract current month's row
  if(type == 'name'){
    df_cur <- df_month[tolower(df_month$name) == tolower(month),]    
  } else if(type == 'number'){
    df_cur <- df_month[df_month$num == month,]    
  } else if(type == 'abbrv'){
    df_cur <- df_month[tolower(df_month$abbrv) == tolower(month),]    
  } else{
    stop('"type" must be one of c("name", "abbrv", "number"')
  }

  if(nrow(df_cur) == 0){
    stop('wrong input month for norm_month')
  }
  
  # create fp
  fp_month <- glue::glue('/{df_cur$num} - {df_cur$name}/')
  
  return(fp_month)
}

data_path <- function(run, month, year, type = NULL){
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Water Quality/'
  
  if (run %in% df_names$ShortName){
    runname <- run
  } else{
    runname <- df_names$LongName[grepl(run, df_names$LongName)]
  }
  
  if(runname == 0){
    stop('run name not valid')
  }

  if (type == 'phyco') {
    fp <- normalizePath(file.path(Sys.getenv('USERPROFILE'),fp_emp, 'Phycoprobe/Archived Data/', year, norm_month(month)))
  }
  if (type == 'MOPED') {
    fp <- normalizePath(file.path(Sys.getenv('USERPROFILE'),fp_emp, 'MOPED Data/', year, norm_month(month)))
  }

  fp_file <- list.files(fp, full.names = TRUE)[grepl(runname, list.files(fp))]
  
  return(fp_file)
}

clean_path <- function(year){
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Water Quality/'
  
  fp <- normalizePath(file.path(Sys.getenv('USERPROFILE'),fp_emp, 'Phycoprobe/Cleaned Data/', year))
  
  fp_file <- list.files(fp, full.names = TRUE, recursive = TRUE)
  
  return(fp_file)
}

archive_path <- function(year, type = NULL){
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Water Quality/'
  
  if (type == 'phyco'){
    fp <- normalizePath(file.path(Sys.getenv('USERPROFILE'),fp_emp, 'Phycoprobe/Archived Data/', year))    
  }
  if(type == 'MOPED'){
    fp <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_emp, 'MOPED Data', year))
  }
  
  fp_file <- list.files(fp, recursive = TRUE)
  
  return(fp_file)
}

abs_path <- function(fp_rel, type = NULL, year = NULL) {
  fp_emp <- 'California Department of Water Resources/Environmental Monitoring Program - Documents/Water Quality/'
  
  
  if (type == 'phyco') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'Phycoprobe/Archived Data/',
        year,
        fp_rel
      ))
    
  } else if (type == 'MOPED') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'MOPED Data/',
        year,
        norm_month(month),
        fp_rel
      ))
  }
  else if (type == 'export') {
    fp_abs <-
      normalizePath(file.path(
        Sys.getenv('USERPROFILE'),
        fp_emp,
        'Phycoprobe/Cleaned Data/',
        fp_rel
      ))
  }
  return(fp_abs)
}

create_combo_df <- function(fp_data){
  # two steps to lower df size
  df_one <- crossing(month = month.name, fp = fp_data)
  
  df_two <- df_one[mapply(function(x,y) grepl(x,y), df_one$month, df_one$fp),]
  
  df_three <- crossing(monthfp = paste0(df_two$month,'RRR',df_two$fp), run = df_names$ShortName) %>%
    separate(monthfp, sep = 'RRR', into = c('month','fp'))
  
  df_four <- df_three[mapply(function(x,y) grepl(x,y), df_three$run, df_three$fp),]
}
