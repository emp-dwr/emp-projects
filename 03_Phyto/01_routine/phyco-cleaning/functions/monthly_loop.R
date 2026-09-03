# clean and combine one month/run of phyco + WQ data into one summary csv

library(lubridate)
library(deltamapr)
library(data.table)
library(readxl)
library(sf)
library(sp)
source(here::here('03_Phyto/01_routine/phyco-cleaning/functions/phyco_funcs.R'))

combine_files <- function(year){
  fp_phyco <- clean_path(year)
  
  df_comb <- rbindlist(lapply(fp_phyco, fread), fill = TRUE) %>%
    arrange(DateTime) %>%
    as_tibble() %>%
    distinct()
  
  df_comb <- df_comb %>%
    mutate(
      DateTime_parsed = as.POSIXct(DateTime),
      Date = as.Date(DateTime_parsed),
      Time = format(DateTime_parsed, format = '%H:%M:%S')
    ) %>%
    select(-any_of(c('DateTime', 'DateTime_parsed', 'Year'))) %>%
    relocate(Date, Time, Region, Latitude, Longitude)
  
  df_comb
}

clean_phyco_wq_summaries <- function(year){
  options(warn = 1)
  
  # read in run names and regions
  df_names <- read_csv(here::here('03_Phyto/01_routine/phyco-cleaning/supp_files/run_names.csv'), show_col_types = FALSE)
  df_regions <- read_csv(here::here('03_Phyto/01_routine/phyco-cleaning/supp_files/regions_fluoro.csv'), show_col_types = FALSE)
  
  # obtain all filepaths and create combo df
  fp_all_phy <- archive_path(year, 'phyco')
  fp_all_wq <- archive_path(year, 'MOPED')
  df_combo_phy <- create_combo_df(fp_all_phy, df_names)
  df_combo_wq <- create_combo_df(fp_all_wq, df_names)
  df_combo <- inner_join(df_combo_wq, df_combo_phy, by = c('month','run'))
  
  
  wq_only <- anti_join(df_combo_wq, df_combo_phy, by = c('month','run'))
  phy_only <- anti_join(df_combo_phy, df_combo_wq, by = c('month','run'))
  
  if(nrow(phy_only) > 0){
    for(j in 1:nrow(phy_only)){
      message(glue::glue('{phy_only$month[j]} {phy_only$run[j]}: no MOPED data, no file created'))
    }
  }
  
  # every WQ run gets processed, whether or not phyco matched
  df_combo <- df_combo_wq %>% select(month, run) %>% distinct()
  
  results <- vector('list', nrow(df_combo))
  
  # for all combos, run cleaning code
  for(i in 1:nrow(df_combo)){
    results[[i]] <- clean_phyco_wq_run(
      month = df_combo[i,]$month,
      run = df_combo[i,]$run,
      year = year,
      df_regions = df_regions,
      df_names = df_names
    )
  }
  
  issues <- list(
    parse_probs_phy = map_dfr(results, 'parse_probs_phy'),
    parse_probs_wq = map_dfr(results, 'parse_probs_wq'),
    bad_report_phy = map_dfr(results, 'bad_report_phy'),
    bad_report_wq = map_dfr(results, 'bad_report_wq')
  )
  
  issues <- issues[sapply(issues, nrow) > 0]

  issues
}

clean_phyco_wq_run <- function(month, run, year, df_regions, df_names){
  
  # message(glue::glue('month: {month} and run: {run}'))
  
  # Read in Phyto Data ------------------------------------------------------
  
  # READ IN PHYTO DATA
  fp_phy <- tryCatch(
    data_path(run, month, year, type = 'phyco', df_names = df_names),
    error = function(e) NA_character_
  )
  
  has_phyco <- !is.na(fp_phy) && length(fp_phy) > 0 && file.exists(fp_phy)
  
  parse_probs_phy <- tibble()
  bad_report_phy <- tibble()
  
  if(!has_phyco){
    message(glue::glue('{month} {run}: no phyco data'))
    df_phy <- tibble(DateTime = character(0), phyco_match = logical(0))
  } else {
    # read in phyco data
    df_phy <- read_tsv(fp_phy, show_col_types = FALSE, name_repair = 'unique_quiet')
    
    # find parsing issues
    parse_probs_phy <- problems(df_phy) %>%
      mutate(Month = month, Run = run, file = tools::file_path_sans_ext(basename(fp_phy)), .before = 1)
    
    if(nrow(parse_probs_phy) > 0){
      message(glue::glue('{month} {run}: {nrow(parse_probs_phy)} Phyco parsing issue(s); see dataframe at end'))
    }
    
    # rename datetime to no slash
    df_phy <- df_phy %>% rename(DateTime = `Date/Time`)
    
    # save first rows
    first_row <- df_phy[1,]
    df_phy <- df_phy[-1,]
    
    raw_datetime <- df_phy$DateTime
    df_phy$DateTime <- suppressWarnings(parse_date_time(df_phy$DateTime, c('mdY HMS', 'mdY IMS p')))
    
    unparsed <- unique(raw_datetime[is.na(df_phy$DateTime) & !is.na(raw_datetime)])
    if(length(unparsed) > 0){
      stop(glue::glue('month {month}, run {run}: {length(unparsed)} bad DateTime value(s) in Phyco data: {paste(unparsed, collapse = \', \')}'))
    }
    
    char_cols <- names(df_phy)[sapply(df_phy, is.character)]
    raw_vals <- df_phy[char_cols]
    
    df_phy <- df_phy %>%
      mutate(across(where(is.character), ~na_if(., '--')))
    
    na_before <- is.na(df_phy[char_cols])
    
    df_phy <- df_phy %>%
      mutate(across(where(is.character), as.numeric))
    
    bad_vals_phy <- is.na(df_phy[char_cols]) & !na_before
    
    if(any(bad_vals_phy)){
      bad_phy_idx <- which(bad_vals_phy, arr.ind = TRUE)
      
      bad_report_phy <- tibble(
        Month = month,
        Run = run,
        file = tools::file_path_sans_ext(basename(fp_phy)),
        column = char_cols[bad_phy_idx[, 'col']],
        row = bad_phy_idx[, 'row'],
        raw_value = raw_vals[cbind(bad_phy_idx[, 'row'], bad_phy_idx[, 'col'])]
      )
      
      message(glue::glue('{month} {run}: {nrow(parse_probs_phy)} Phyco parsing issue(s); see dataframe at end'))
    }
    
    # aggregate phyto data
    # # select closest data to a minute up-to-and-including that minute
    df_phy <- df_phy %>%
      mutate(minute_mark = ceiling_date(`DateTime`, unit = '1 minute', change_on_boundary = TRUE)) %>%
      group_by(minute_mark) %>%
      slice_max(`DateTime`, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(`DateTime` = minute_mark) %>%
      select(-minute_mark) %>%
      mutate(across(where(is.numeric), ~round(., 2))) # round to 2 decimals
    
    # get units corresponding to each column
    units <- first_row[1, colnames(df_phy)] %>%
      unlist(use.names = FALSE) %>%
      as.character()
    
    # construct cleaned column names
    new_names <- str_remove(colnames(df_phy), '\\.\\.\\.\\d+$')
    new_names <- paste(new_names, units, sep = '_')
    new_names <- new_names %>%
      str_replace_all('µg|�g', 'ug') %>%
      str_replace_all('°C|�C', 'degC') %>%
      str_replace_all('\\.|\\[|\\]|#|/', '')
    
    # resolve duplicated final column names
    dup_names <- unique(new_names[duplicated(new_names)])
    drop_cols <- integer(0)
    
    for(nm in dup_names){
      idx <- which(new_names == nm)
      all_zero <- sapply(idx, function(j){
        x <- df_phy[[j]]
        is.numeric(x) &&
          any(!is.na(x)) &&
          all(x[!is.na(x)] == 0)
      })
      
      # more than one non-zero version means we do not know which is correct
      if(sum(!all_zero) > 1){
        stop(
          glue::glue(
            'duplicate column "{nm}" has more than one non-zero version ',
            'for month {month}, run {run}'
          )
        )
      }
      # if one is non-zero, remove the zero version(s)
      if(sum(!all_zero) == 1){
        drop_cols <- c(drop_cols, idx[all_zero])
      }
      # if all versions are zero, keep the first one
      if(all(all_zero)){
        drop_cols <- c(drop_cols, idx[-1])
      }
    }
    
    if(length(drop_cols) > 0){
      df_phy <- df_phy[, -drop_cols]
      new_names <- new_names[-drop_cols]
    }
    
    colnames(df_phy) <- new_names
    
    df_phy <- df_phy %>%
      rename(DateTime = `DateTime_date`)
    
    # keep relevant columns
    keep_cols <- c(
      colnames(df_phy)[grepl('Green Algae|Bluegreen|Diatoms|Cryptophyta', colnames(df_phy))],
      'DateTime'
    )
    # add a phyco_match column so phyco data can be found later on (for labels)
    df_phy <- df_phy %>%
      select(all_of(keep_cols)) %>%
      mutate(
        DateTime = as.character(DateTime),
        phyco_match = TRUE
      )
  }
  
  # Read in WQ Data ---------------------------------------------------------
  fp_wq <- data_path(run, month, year, df_names, type = 'MOPED')
  df_wq <- suppressWarnings(read_csv(fp_wq, skip = 2, show_col_types = FALSE))
  
  parse_probs_wq <- problems(df_wq) %>%
    mutate(Month = month, Run = run, file = tools::file_path_sans_ext(basename(fp_wq)), .before = 1)
  
  if(nrow(parse_probs_wq) > 0){
    message(glue::glue('{month} {run}: {nrow(parse_probs_wq)} MOPED parsing issue(s); see dataframe at end'))
  }
  
  raw_timestamp <- df_wq$TimeStamp
  df_wq$TimeStamp <- suppressWarnings(parse_date_time(df_wq$TimeStamp, c('mdY HMS', 'mdY HM')))
  df_wq$TimeStamp <- as.POSIXct(df_wq$TimeStamp, format = '%m/%d/%Y %H:%M:%S')
  
  unparsed_wq <- unique(raw_timestamp[is.na(df_wq$TimeStamp) & !is.na(raw_timestamp)])
  if(length(unparsed_wq) > 0){
    stop(glue::glue('month {month}, run {run}: {length(unparsed_wq)} bad TimeStamp value(s) in MOPED data:  {paste(unparsed_wq, collapse = \', \')}'))
  }
  
  df_wq$Analyte <- paste0(df_wq$Header,'_',df_wq$Unit)
  
  raw_value <- df_wq$Value
  df_wq$Value <- as.numeric(df_wq$Value)
  
  bad_vals_wq <- is.na(df_wq$Value) & !is.na(raw_value)
  
  bad_report_wq <- tibble()
  if(any(bad_vals_wq)){
    bad_report_wq <- tibble(
      Month = month,
      Run = run,
      file = tools::file_path_sans_ext(basename(fp_wq)),
      row = which(bad_vals_wq),
      raw_value = raw_value[bad_vals_wq]
    )
  
    message(glue::glue('{month} {run}: {nrow(parse_probs_wq)} MOPED parsing issue(s); see dataframe at end'))
  }
  
  # aggregate WQ data to nearest minute in similar way to phyto
  # # treat all analyte data within the same aggregated DateTime bin as one sampling event
  df_wq <- df_wq %>%
    subset(select = c(Longitude, Latitude, TimeStamp, Analyte, Value)) %>%
    mutate(
      row_id = row_number(),
      minute_mark = ceiling_date(TimeStamp, unit = '1 minute', change_on_boundary = TRUE)
    )
  
  # if multiple lat/lon values per sampling event, take one closest to minute mark overall
  df_wq_scan <- df_wq %>%
    group_by(minute_mark) %>%
    slice_max(row_id, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(DateTime = minute_mark, Longitude, Latitude)
  
  # each analyte's value comes from its own closest reading within the
  # bucket, independent of which row lat/lon was pulled from
  df_wq <- df_wq %>%
    group_by(minute_mark, Analyte) %>%
    slice_max(row_id, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(DateTime = minute_mark, Analyte, Value) %>%
    pivot_wider(names_from = Analyte, values_from = Value) %>%
    left_join(df_wq_scan, by = 'DateTime')
  
  # rename WQ analytes
  expected_cols <- c(
    'DOSAT_% SAT', 'DO_mg/L', 'FLUORRFU_RFU', 'FLUOR_ug/L',
    'PH_pH Units', 'SPC_uS/cm', 'WT_C', 'FNU_FNU'
  )
  missing_cols <- setdiff(expected_cols, colnames(df_wq))
  if(length(missing_cols) > 0){
    stop(
      glue::glue(
        'missing expected WQ column(s): {paste(missing_cols, collapse = \', \')} ',
        'for month {month}, run {run}'
      )
    )
  }
  
  df_wq <- df_wq %>%
    rename(
      DOSat_perc = `DOSAT_% SAT`,
      Fluor_RFU = FLUORRFU_RFU,
      Fluor_ugL = `FLUOR_ug/L`,
      pH = `PH_pH Units`,
      SpC_uScm = `SPC_uS/cm`,
      WaterTemp_degC = WT_C,
      Turbidity_FNU = FNU_FNU
    )
  
  # add in Year/Month
  df_wq <- df_wq %>%
    mutate(
      Year = year(DateTime),
      Month = month.abb[month(DateTime)]
    )
  
  # convert date/time col back to character
  df_wq$DateTime <- as.character(df_wq$DateTime)
  
  
  # Join Data ---------------------------------------------------------------
  df_comb <- left_join(df_wq, df_phy, by = 'DateTime')
  df_comb <- df_comb %>% filter(!is.na(Longitude) | !is.na(Latitude))
  
  # build phyco label
  df_comb <- df_comb %>%
    mutate(phyco_match = !is.na(phyco_match)) %>%
    arrange(DateTime) %>%
    mutate(
      PhycoLabel = if_else(
        phyco_match,
        paste(Month, Year, run, cumsum(phyco_match)),
        NA_character_
      )
    ) %>%
    relocate(PhycoLabel)
  
  
  # Add Regions -------------------------------------------------------------
  # import delta sf
  sf_delta <- R_EDSM_Subregions_Mahardja
  # convert coordinates to numeric
  df_comb <- df_comb %>%
    mutate(
      Longitude = as.numeric(Longitude),
      Latitude = as.numeric(Latitude)
    )
  
  # filter out rows around Antioch
  lat_min <- 38.016249
  lat_max <- 38.023349
  lon_min <- -121.759586
  lon_max <- -121.746626

  df_comb <- df_comb %>%
    filter(!(Latitude >= lat_min & Latitude <= lat_max &
               Longitude >= lon_min & Longitude <= lon_max))

  # filter out rows with 0 Lat and 0 Lon
  df_comb <- df_comb %>%
    filter(!(Latitude == 0 & Longitude == 0))
  
  if(nrow(df_comb) == 0){
    message(glue::glue('{month} {run}: no valid rows remain after removing data around Antioch, no file created'))
    return(list(
      df_final = tibble(),
      parse_probs_phy = parse_probs_phy,
      parse_probs_wq = parse_probs_wq,
      bad_report_phy = bad_report_phy,
      bad_report_wq = bad_report_wq
    ))
  }
  
  # convert wq to spdf
  coords <- df_comb[, c('Longitude', 'Latitude')]
  data <- subset(df_comb, select = -c(Latitude, Longitude))
  crs <- CRS('EPSG:4326')

  spdf_wq <- SpatialPointsDataFrame(
    coords = coords,
    data = data,
    proj4string = crs
  )
  
  # convert delta to spdf
  spdf_delta <- as(sf_delta, 'Spatial')
  spdf_delta <- spTransform(
    spdf_delta,
    CRS('EPSG:4326')
  )
  
  # add subregion to df
  col_sr <- sp::over(
    spdf_wq,
    spdf_delta[, 'SubRegion']
  )
  spdf_wq$SubRegion <- col_sr$SubRegion
  
  # convert to sf
  sf_wq <- st_as_sf(spdf_wq)
  sf_wq <- st_transform(sf_wq, crs = st_crs(sf_delta))
  sf_wq <- sf_wq %>%
    filter(!is.na(SubRegion))
  
  # check data
  # ggplot() +
  #   geom_sf(data = sf_delta) +
  #   geom_sf(data = sf_wq, color = 'red')
  # add final regions
  
  df_final <- sf_wq %>%
    left_join(df_regions, by = 'SubRegion')
  
  # extract longitude/latitude in EPSG:4326
  coords <- df_final %>%
    st_transform(4326) %>%
    st_coordinates()
  df_final <- df_final %>%
    mutate(
      Longitude = coords[, 'X'],
      Latitude = coords[, 'Y']
    ) %>%
    st_drop_geometry() %>%
    select(-any_of(c('SubRegion', 'VPOSITION_ft', 'SONDEDEPTH_ft', 'Date', 'Month', 'Year', 'phyco_match'))) %>%
    relocate(DateTime, Region, Latitude, Longitude) %>%
    relocate(PhycoLabel, .after = WaterTemp_degC)
  
  id_cols <- c('PhycoLabel', 'DateTime', 'Latitude', 'Longitude', 'Region')
  data_cols <- setdiff(colnames(df_final), id_cols)
  
  # Export ------------------------------------------------------------------
  mon_abb <- month.abb[match(month, month.name)]
  yr_short <- str_sub(as.character(year), -2)
  
  fn_exp <- str_extract(tools::file_path_sans_ext(basename(fp_wq)), '[^_]*$')
  fp_folder <- create_dir(year)
  fp_exp <- paste0(fp_folder, '/', 'Phycoprobe_', fn_exp, '_', mon_abb, yr_short, '_summary.csv')

  write_csv(df_final, fp_exp)
  
  list(
    df_final = df_final,
    parse_probs_phy = parse_probs_phy,
    parse_probs_wq = parse_probs_wq,
    bad_report_phy = bad_report_phy,
    bad_report_wq = bad_report_wq
  )
}