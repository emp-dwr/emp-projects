# process VR --------------------------------------------------------------

process_VR <- function(df_raw) {
  df <- df_raw %>% 
    as_tibble() %>% 
    pivot_longer(everything(), names_to = 'param', values_to = 'value') %>% 
    filter(!is.na(value) & value != '')
  
  df_clean <- df %>%
    filter(!param %in% c('Crew', 'Run Name')) %>%
    mutate(
      is_blank = str_detect(param, '^Blank'),
      is_global_meta = str_detect(param, '^(Sonde ID|Date)$'),
      
      # Batch assignment
      batch_id = case_when(
        is_blank ~ 999,
        is_global_meta ~ 0,
        TRUE ~ str_extract(param, '_\\d+$') %>% str_remove('_') %>% as.integer()
      ),
      batch_id = replace_na(batch_id, 1),
      
      # Surface/bottom flag
      SurfBot = case_when(
        str_detect(param, 'Bottom') ~ 'bottom',
        str_detect(param, 'Surface') ~ 'surface',
        TRUE ~ NA_character_
      ),
      
      # QC type assignment
      `QC: Type` = case_when(
        is_blank ~ 'Blank',
        str_detect(param, '^Dup') ~ 'Replicate',
        TRUE ~ NA_character_
      ),
      
      # Base param cleaning
      param_base_raw = param %>%
        str_remove('^Blank ') %>%
        str_remove('^Dup Check Box_\\d+$') %>%
        str_remove('_[0-9]+$') %>%
        str_squish(),
      
      # Normalize param names
      param_base = case_when(
        str_detect(param_base_raw, 'Lab ID') ~ 'Lab ID',
        str_detect(param_base_raw, 'Churn Bucket') ~ 'Churn Bucket',
        TRUE ~ param_base_raw
      )
    )
  
  station_vals <- df_clean %>%
    filter(str_detect(param, '^Station_\\d+$')) %>%
    transmute(batch_id, `Location ID` = value)
  
  df_out <- df_clean %>%
    left_join(station_vals, by = 'batch_id') %>%
    mutate(
      `Location ID` = case_when(
        `QC: Type` == 'Blank' ~ 'Equipment Blank',
        batch_id == 0 & is_global_meta ~ NA_character_,
        batch_id == 0 ~ 'Equipment Blank',
        TRUE ~ `Location ID`
      )
    ) %>%
    filter(!str_detect(param, '^Station_\\d+$')) %>%
    filter(!(param_base == 'Dup Check Box')) %>%
    select(batch_id, param = param_base, value, SurfBot, `QC: Type`, `Location ID`)
  
  return(df_out)
}

# Format VR ---------------------------------------------------------------

format_VR <- function(df_final) {
  # Date and sonde ID from batch 0
  date_value <- df_final %>%
    filter(batch_id == 0, param == 'Date') %>%
    pull(value) %>%
    mdy()
  
  sonde_id <- df_final %>%
    filter(batch_id == 0, param == 'Sonde ID') %>%
    pull(value)
  
  # Assign times
  df_final <- df_final %>%
    group_by(batch_id) %>%
    mutate(
      batch_time = first(value[param == 'Time' & !is.na(value)]),
      `Observed DateTime` = date_value + hms(paste0(batch_time, ':00'))
    ) %>%
    select(-batch_time) %>%
    ungroup()
  
  # Assign device ID
  df_final <- df_final %>%
    mutate(`Field: Device ID` = sonde_id)
  
  # Extract station names
  station_vals <- df_final %>%
    filter(param == 'Station') %>%
    select(batch_id, `Location ID` = value)
  
  # Get Lab IDs
  lab_ids <- df_final %>%
    filter(param == 'Lab ID') %>%
    select(batch_id, `QC: Type`, `Activity Name` = value)
  
  df_final <- df_final %>%
    left_join(station_vals, by = c('batch_id','Location ID')) %>%
    left_join(lab_ids, by = c('batch_id', 'QC: Type'))
  
  # Depth
  df_final <- df_final %>%
    mutate(water_depth = case_when(
      param == 'Depth' ~ as.numeric(value),
      TRUE ~ NA_real_
    )) %>%
    group_by(batch_id) %>%
    fill(water_depth, .direction = 'downup') %>%
    ungroup() %>%
    mutate(Depth = case_when(
      SurfBot == 'bottom' ~ water_depth - 3,
      SurfBot == 'surface' ~ 3,
      param == 'MC Score' ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(-water_depth)

  # extract jug IDs from Notes
  jug_and_notes <- df_final %>%
    filter(param == 'Notes') %>%
    mutate(
      standard_jug = str_match(value, '(?i)filter jug:?\\s*([^;\\s]+)')[,2],
      duplicate_jug = str_match(value, '(?i)duplicate filter jug:?\\s*([^;\\s]+)')[,2],
      blank_jug     = str_match(value, '(?i)blank filter jug:?\\s*([^;\\s]+)')[,2],
      `Field: Comment` = str_remove_all(
        value,
        '(?i)filter jug:?[ ]*[^;\\s]+;?\\s*|duplicate filter jug:?[ ]*[^;\\s]+;?\\s*|blank filter jug:?[ ]*[^;\\s]+;?\\s*'
      ) %>%
        str_remove('^;\\s*') %>%
        str_remove(';\\s*$') %>%
        str_trim()
    ) %>%
    select(batch_id, standard_jug, duplicate_jug, blank_jug, `Field: Comment`)
  
  # join jug IDs back to main df
  df_final <- df_final %>%
    left_join(jug_and_notes, by = 'batch_id')
  
  # lookup activity metadata
  activity_ids <- df_final %>%
    distinct(batch_id, `QC: Type`, `Location ID`, `Activity Name`, `Observed DateTime`)
  
  # identify the blank sample metadata (Equipment Blank)
  blank_ref <- activity_ids %>%
    filter(`QC: Type` == 'Blank') %>%
    slice(1) %>%
    rename(blank_batch = batch_id,
           blank_loc = `Location ID`,
           blank_act = `Activity Name`,
           blank_dt  = `Observed DateTime`)
  
  # create jug rows
  jug_rows <- df_final %>%
    filter(!is.na(standard_jug) | !is.na(duplicate_jug) | !is.na(blank_jug)) %>%
    rename(orig_value = value) %>%
    distinct(batch_id, .keep_all = TRUE) %>%
    pivot_longer(
      cols = c(standard_jug, duplicate_jug, blank_jug),
      names_to = 'jug_type',
      values_to = 'value',
      values_drop_na = TRUE
    ) %>%
    mutate(
      `QC: Type` = case_when(
        jug_type == 'duplicate_jug' ~ 'Replicate',
        jug_type == 'blank_jug'     ~ 'Blank',
        jug_type == 'standard_jug'  ~ NA_character_
      ),
      param = 'Filter Container ID',
      SurfBot = NA_character_
    ) %>%
    # safely bring in activity name by suffix control
    left_join(activity_ids, by = c('batch_id', 'QC: Type'),
              suffix = c('', '_act')) %>%
    mutate(
      batch_id = case_when(`QC: Type` == 'Blank' ~ blank_ref$blank_batch, TRUE ~ batch_id),
      `Location ID` = case_when(`QC: Type` == 'Blank' ~ blank_ref$blank_loc, TRUE ~ `Location ID_act`),
      `Activity Name` = case_when(`QC: Type` == 'Blank' ~ blank_ref$blank_act, TRUE ~ `Activity Name_act`),
      `Observed DateTime` = case_when(`QC: Type` == 'Blank' ~ blank_ref$blank_dt, TRUE ~ `Observed DateTime_act`)
    ) %>%
    select(batch_id, param, value, SurfBot, `QC: Type`, `Location ID`,
           `Observed DateTime`, `Activity Name`)
  
  df_final <- df_final %>%
    bind_rows(jug_rows) %>%
    select(-standard_jug, -duplicate_jug, -blank_jug)
  
  # Remove metadata
  df_final <- df_final %>%
    filter(!param %in% c('Date', 'Sonde ID', 'Station', 'Time', 'Lab ID', 'Notes')) %>%
    relocate(SurfBot, .after = param)
  
  # Add churn bucket
  churn_rows <- df_final %>%
    group_by(batch_id, `QC: Type`) %>%
    filter(param == 'Churn Bucket #') %>%
    select(-value) %>%
    ungroup()
  
  # Remap weather codes
  df_final <- df_final %>%
    mutate(value = case_when(
      param == 'Sky' & value == 'S' ~ 'Sunny',
      param == 'Sky' & value == 'PC' ~ 'Partly Cloudy',
      param == 'Sky' & value == 'C' ~ 'Cloudy',
      param == 'Sky' & value == 'O' ~ 'Overcast',
      param == 'Sky' & value == 'F' ~ 'Foggy',
      param == 'Rain' & value == 'N' ~ 'None',
      param == 'Rain' & value == 'L' ~ 'Light',
      param == 'Rain' & value == 'M' ~ 'Medium',
      param == 'Rain' & value == 'H' ~ 'Heavy',
      TRUE ~ value
    ))
  
  # Round values
  df_final <- df_final %>%
    mutate(value = case_when(
      param == 'Turbidity (FNU)' ~ as.character(round(as.numeric(value), 1)),
      param == 'DO (mg/L)' ~ as.character(round(as.numeric(value), 2)),
      TRUE ~ value
    ))
  
  # Add parent sample IDs
  df_final <- df_final %>%
    group_by(`Location ID`) %>% 
    mutate(
      `EA_Parent Sample ID` = case_when(
        `QC: Type` == 'Replicate' ~ first(`Activity Name`[is.na(`QC: Type`)]),
        TRUE ~ NA_character_),
      `QC: Source Sample ID` = `EA_Parent Sample ID`
    ) %>%
    ungroup() %>%
    rename(`Result Value` = value)
  
  df_final <- df_final %>%
    filter(!is.na(`Result Value`))
  
  # More depth
  df_final <- df_final %>% 
    rename(`EA_Sampling Depth` = Depth) %>%
    mutate(Depth = if_else(str_detect(param, '(?i)Depth'), `Result Value`, NA)) %>%
    group_by(`Location ID`, `Observed DateTime`) %>%
    fill(Depth, .direction = 'downup') %>%
    ungroup()
  
  # Merge with metadata
  fp_meta <- abs_path_emp('Water Quality/AQUARIUS Samples Database/Database Migration/Lists to Import/Final Imports/Import_FDS Metadata.csv')
  df_meta <- read_csv(fp_meta, show_col_types = FALSE) %>%
    mutate(param = str_replace_all(param, '[\\u00A0\\u2000-\\u200B\\uFEFF]', ' ') %>% str_squish())
  
  df_final <- df_final %>%
    mutate(param = str_replace_all(param, '[\\u00A0\\u2000-\\u200B\\uFEFF]', ' ') %>% str_squish())
  
  df_final <- right_join(df_meta, df_final, by = c('param', 'SurfBot')) %>%
    select(-c(param, SurfBot))
  
  # Format DateTimes
  # df_final <- df_final %>%
  #   mutate(across(contains('DateTime'), ~ format(mdy_hm(.x), '%Y-%m-%d %H:%M:%S')))
  
  # df_final <- df_final %>%
  #   mutate(across(contains('DateTime'), ~ as.POSIXct(round_date(mdy_hm(.x), unit = '5 minutes'))))
  
  df_final <- df_final %>%
    mutate(across(contains('DateTime'), ~ ifelse(is.na(.x), NA, paste0(.x, ' [PST]'))))
  
  df_final <- df_final %>%
    filter(is.na(`Observed Property ID`) == FALSE | `Result Value` != 'Yes')
  
  # Assign depth unit
  df_final <- df_final %>%
    mutate(`Depth Unit` = if_else(`Location ID` == 'Equipment Blank', NA, 'ft')) %>%
    mutate(Depth = as.character(round(as.numeric(Depth), 1)))
  
  col_order <- c(
    'Observation ID', 'Location ID', 'Observed Property ID', 'Observed DateTime', 'Analyzed DateTime',
    'Depth', 'Depth Unit', 'Data Classification', 'Result Value', 'Result Unit', 'Result Status', 
    'Result Grade', 'Medium', 'Activity Name', 'Activity ID', 'Collection Method', 'Field: Device ID', 
    'Field: Device Type', 'Field: Comment', 'Lab: Specimen Name', 'Lab: Analysis Method', 
    'Lab: Detection Condition', 'Lab: Limit Type', 'Lab: MDL', 'Lab: MRL', 'Lab: Quality Flag', 
    'Lab: Received DateTime', 'Lab: Prepared DateTime', 'Lab: Sample Fraction', 'Lab: From Laboratory', 
    'Lab: Sample ID', 'Lab: Dilution Factor', 'Lab: Comment', 'QC: Type', 'QC: Source Sample ID', 
    'EA_Modified Method', 'EA_Profile Type', 'EA_Project Type', 'EA_Reports To', 
    'EA_Parent Sample ID', 'EA_Field Quality Flag', 'EA_Sampling Depth'
  )
  
  df_final <- df_final %>%
    select(all_of(col_order)) %>%
    arrange(`Observed DateTime`, `Location ID`, desc(is.na(`QC: Type`)))
  
  # Format DateTimes
  df_final <- df_final %>%
    mutate(across(
      .cols = matches('DateTime') & !all_of('Observed DateTime'),
      .fns = \(x) {
        x <- as.POSIXct(round_date(mdy_hm(x), unit = '5 minutes'))
        ifelse(is.na(x), NA, paste0(x, ' [PST]'))
      }
    ))
  
  return(df_final)
}

# process FDS -------------------------------------------------------------

process_FDS_excel <- function(fp) {
  df <- suppressMessages(read_excel(fp))
 
  # Main batch
  df_head <- df %>%
    select('...7', '...9', '...18', '...24') %>%
    rename(param1 = '...7', value1 = '...9', param2 = '...18', value2 = '...24') %>%
    pivot_longer(cols = c(param1, param2), values_to = 'param', names_to = 'param_col') %>%
    pivot_longer(cols = c(value1, value2), values_to = 'value', names_to = 'value_col') %>%
    mutate(
      param_suffix = str_extract(param_col, '\\d+$'),
      value_suffix = str_extract(value_col, '\\d+$')
    ) %>%
    filter(param_suffix == value_suffix) %>%
    select(param, value) %>%
    filter(!is.na(param) & !is.na(value)) %>%
    mutate(value = case_when(
      param == 'Date:' ~ format(as.Date(as.numeric(value), origin = '1899-12-30'), '%m/%d/%Y'),
      TRUE ~ value
    ),
    batch_id = 0)
  
  # Blank batch
  df_blank <- df %>%
    select(...38, ...39, ...40, ...41, ...42, ...43, ...44) %>%
    filter(!is.na(...38) | ...38 == 'Equipment Blank') %>%
    pivot_longer(cols = c(...39, ...40, ...41, ...42, ...43, ...44), values_to = 'value', names_to = 'value_col') %>%
    filter(!is.na(value) | ...38 == 'Equipment Blank') %>%
    mutate(
      param = case_when(
        ...38 == 'Equipment Blank' ~ 'Station',
        TRUE ~ ...38
      ),
      value = case_when(
        ...38 == 'Equipment Blank' ~ 'Equipment Blank',
        TRUE ~ value
      ),
      `QC: Type` = 'Blank',
      batch_id = 999,
    ) %>%
    filter(!(duplicated(param))) %>%
    select(param, value, batch_id, `QC: Type`) %>%
    mutate(
      param = case_when(
        param == 'Churn Bucket #:' ~ 'Churn Bucket #',
        param == 'Time:' ~ 'Time',
        param == 'Lab ID:' ~ 'Lab ID',
        TRUE ~ param
      )
    )
  
  # Station batches
  df_batch <- df %>%
    mutate(across(everything(), ~ str_replace_all(., '[\r\n]', ' '))) %>%
    mutate(
      batch_start = ifelse(!is.na(...1) & str_detect(...1, 'Station:'), TRUE, FALSE),
      batch_end = ifelse(!is.na(...1) & str_detect(...1, 'Notes:'), TRUE, FALSE),
      batch_id = cumsum(batch_start)
    ) %>%
    fill(batch_id, .direction = 'down') %>%
    group_by(batch_id) %>%
    mutate(
      in_batch = cumsum(batch_start) > cumsum(batch_end) | batch_end
    ) %>%
    ungroup() %>%
    filter(in_batch | batch_start | batch_end) %>%
    select(-batch_start, -batch_end, -in_batch) %>%
    select(where(~ any(!is.na(.)))) %>%
    filter(rowSums(!is.na(select(., -batch_id))) > 0)
  
  rm_batches <- df_batch %>%
    filter(str_detect(...1, 'Station:') & str_detect(...1, 'Check')) %>%
    pull(batch_id) %>%
    unique()
  
  df_batch <- df_batch %>%
    filter(!batch_id %in% rm_batches)
  
  df_batch <- df_batch %>%
    group_by(batch_id) %>%
    mutate(
      is_header1 = str_detect(...1, 'Station:'),
      is_horz = str_detect(...1, 'Horizontal'),
      is_pretow_surf = str_detect(...1, 'Pre-Tow Surf|No-Tow Surf|Post-Tow Surf'), #TODO: GIVE WARNING IF POST-TOW OR IF MULTIPLE
      is_pretow_bot = str_detect(...1, 'Pre-Tow Bot|No-Tow Bot|Post-Tow Bot'),
      is_header2 = str_detect(...1, 'Lab ID'),
      is_discval = case_when(lag(is_header2, default = FALSE) & !is.na(...1) ~ TRUE, TRUE ~ FALSE),
      is_notes = str_detect(...1, 'Notes:'),
      is_lat = str_detect(...39, 'Lat:'),
      is_long = str_detect(...39, 'Long:')
    ) %>%
    ungroup()
  
  df_long <- df_batch %>%
    pivot_longer(cols = -c(batch_id, is_header1, is_horz, is_pretow_surf,
                           is_pretow_bot, is_header2, is_discval, is_notes,
                           is_lat, is_long),
                 names_to = 'col', values_to = 'cell_value') %>%
    filter(!is.na(cell_value))  
  
  df_values <- df_long %>%
    filter(is_horz | is_pretow_surf | is_pretow_bot | is_discval) %>%
    mutate(
      SondeType = case_when(is_horz ~ 'horizontal', is_pretow_surf ~ 'vertical',
                            is_pretow_bot ~ 'vertical', TRUE ~ NA_character_),
      SurfBot = case_when(is_pretow_surf ~ 'surface', is_pretow_bot ~ 'bottom', TRUE ~ NA_character_)
    ) %>%
    left_join(df_long %>% filter(is_header2) %>% select(batch_id, col, header_param = cell_value),
              by = c('batch_id', 'col')) %>%
    left_join(df_long %>% filter(is_header1) %>% select(batch_id, col, header1_param = cell_value),
              by = c('batch_id', 'col')) %>%
    mutate(
      param = case_when(
        is_discval ~ header_param,
        !is.na(SondeType) | !is.na(SurfBot) ~ header1_param,
        TRUE ~ col
      )
    ) %>%
    filter(!(cell_value %in% c('Horizontal', 'Pre-Tow Surf.', 'Pre-Tow Bot.'))) %>%
    select(batch_id, param, value = cell_value, SondeType, SurfBot)
  
  df_station <- df_long %>%
    filter(is_header1) %>%
    mutate(
      station_value = str_extract(cell_value, '(?<=Station: )[^ ]+'),
      time_value = str_extract(cell_value, '(?<=Time: )\\S+') 
    ) %>%
    select(batch_id, station_value, time_value) %>%
    pivot_longer(cols = c(station_value, time_value), names_to = 'param', values_to = 'value') %>%
    mutate(param = if_else(param == 'station_value', 'Station', 'Time')) %>%
    filter(!is.na(value))
  
  df_notes <- df_long %>%
    filter(is_notes) %>%
    mutate(param = 'Notes', value = cell_value) %>%
    filter(value != 'Notes:') %>%
    select(batch_id, param, value)
  
  df_latlong <- df_long %>%
    filter(is_lat | is_long) %>%
    mutate(
      param = if_else(is_lat, 'latitude', 'longitude'),
      value = str_extract(cell_value, '-?\\d+\\.\\d+')
    ) %>%
    select(batch_id, param, value)
  
  df_dup <- df_long %>%
    filter(is_header1) %>%
    mutate(
      duplicate_value = str_extract(cell_value, '(?<=Duplicate\\? )\\S+'),
      churn_bucket_value = str_extract(cell_value, '(?<=Churn Bucket #: )\\S+'),
      lab_id_value = str_extract(cell_value, '(?<=Lab ID )\\S+')
    ) %>%
    filter(duplicate_value == 'True') %>%
    select(batch_id, churn_bucket_value, lab_id_value) %>%
    pivot_longer(
      cols = c(churn_bucket_value, lab_id_value),
      names_to = 'param',
      values_to = 'value'
    ) %>%
    mutate(
      param = recode(param,
                     churn_bucket_value = 'Churn Bucket #',
                     lab_id_value = 'Lab ID'),
      `QC: Type` = 'Replicate'
    ) %>%
    filter(!is.na(value))
  
  # Combine
  df_final <- bind_rows(df_head, df_station, df_values, df_notes, df_latlong, df_dup, df_blank) %>%
    arrange(batch_id) %>%
    relocate(batch_id)
  
  return(df_final)
}

# format FDS --------------------------------------------------------------

format_FDS_excel <- function(df_final) {
  # Remove extra params
  df_final <- df_final %>%
    filter(!(param %in% c('Vessel:','Crew:','Run Name:','Operator:','Run Type:','Sonde ID (H):','ChloroVol(ml)'))) %>%
    filter(SondeType == 'vertical' | is.na(SondeType)) %>%
    mutate(param = str_squish(param))
  
  # --- Extract relevant values ---
  date_value <- df_final %>%
    filter(batch_id == 0, param == 'Date:') %>%
    pull(value) %>%
    mdy()
  
  sonde_id <- df_final %>%
    filter(batch_id == 0, param == 'Sonde ID (V):') %>%
    pull(value)
  
  station_vals <- df_final %>%
    filter(param == 'Station') %>%
    select(batch_id, `Location ID` = value)
  
  lab_ids <- df_final %>%
    filter(param == 'Lab ID') %>%
    select(batch_id, `QC: Type`, `Activity Name` = value)

  df_final <- df_final %>%
    mutate(`Field: Device ID` = case_when(
      SondeType == 'vertical' ~ sonde_id,
      TRUE ~ NA_character_
    ))

  df_final <- df_final %>%
    left_join(station_vals, by = 'batch_id') %>%
    left_join(lab_ids, by = c('batch_id', 'QC: Type'))

  # Depth
  df_final <- df_final %>%
    mutate(water_depth = case_when(
      param == 'Water Depth (ft)' ~ as.numeric(value),
      TRUE ~ NA_real_
    )) %>%
    group_by(batch_id) %>%
    fill(water_depth, .direction = 'downup') %>%
    ungroup() %>%
    mutate(Depth = case_when(
      SurfBot == 'bottom' ~ as.numeric(water_depth) - 3,
      SurfBot == 'surface' ~ 3,
      param == 'MC Score (1-5)' ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(-water_depth)

  # Filter jugs
  df_final <- df_final %>%
    mutate(
      standard_jug = case_when(
        param == 'Notes' ~ str_match(value, '(?i)filter jug:?\\s*([^;\\s]+)')[,2]
      ),
      duplicate_jug = case_when(
        param == 'Notes' ~ str_match(value, '(?i)duplicate filter jug:?\\s*([^;\\s]+)')[,2]
      ),
      blank_jug = case_when(
        param == 'Notes' ~ str_match(value, '(?i)blank filter jug:?\\s*([^;\\s]+)')[,2]
      )
    ) %>%
    group_by(batch_id) %>%
    fill(standard_jug, .direction = 'downup') %>%
    ungroup() %>%
    fill(duplicate_jug, blank_jug, .direction = 'downup') %>%
    mutate(
      `EA_Filter Container ID` = case_when(
        `QC: Type` == 'Replicate' ~ duplicate_jug,
        `QC: Type` == 'Blank' ~ blank_jug,
        is.na(`QC: Type`) ~ standard_jug
      ),
      value = case_when(
        param == 'Notes' ~ str_remove_all(
          value,
          '(?i)filter jug:?\\s*[^;\\s]+;?\\s*|duplicate filter jug:?\\s*[^;\\s]+;?\\s*|blank filter jug:?\\s*[^;\\s]+;?\\s*'
        ) %>%
          str_remove('^;\\s*') %>%
          str_remove(';\\s*$') %>%
          str_trim(),
        TRUE ~ value
      )
     ) %>%
    select(-standard_jug, -duplicate_jug, -blank_jug)

  # Notes
  notes_vals <- df_final %>%
    filter(param == 'Notes') %>%
    select(batch_id, `Field: Comment` = value)

  df_final <- df_final %>%
    left_join(notes_vals, by = 'batch_id')

  # Remove metadata
  df_final <- df_final %>%
    filter(!(param %in% c('Date:','Sonde ID (V):','Station','Time','Lab ID','Notes')))

  # LSZ lat/lons
  df_final <- df_final %>%
    mutate(
      `Location ID` = if_else(grepl('EZ', `Location ID`), str_replace(`Location ID`, 'EZ', 'LSZ'), `Location ID`),
      `Field: Comment` = str_remove(`Field: Comment`, ';\\s*$')
    ) %>%
    filter(!(param %in% c('latitude', 'longitude') & !grepl('LSZ', `Location ID`))) %>%
    relocate(SurfBot, .after = param)

  # Churn bucket
  churn_rows <- df_final %>%
    group_by(batch_id, `QC: Type`) %>%
    filter(param == 'Churn Bucket #') %>%
    select(-value) %>%
    ungroup()

  filter_vals <- df_final %>%
    filter(!is.na(`EA_Filter Container ID`)) %>%
    distinct(batch_id, `QC: Type`, `EA_Filter Container ID`)

  new_rows <- churn_rows %>%
    left_join(filter_vals, by = c('batch_id', 'QC: Type', 'EA_Filter Container ID')) %>%
    mutate(param = 'Filter Container ID', value = `EA_Filter Container ID`) %>%
    select(-`EA_Filter Container ID`)

  df_final <- bind_rows(df_final, new_rows) %>%
    select(-`EA_Filter Container ID`) %>%
    arrange(batch_id, `QC: Type`, SurfBot)

  # Remap weather codes
  df_final <- df_final %>%
    mutate(value = case_when(
      param == 'Sky' & value == 'S' ~ 'Sunny',
      param == 'Sky' & value == 'PC' ~ 'Partly Cloudy',
      param == 'Sky' & value == 'C' ~ 'Cloudy',
      param == 'Sky' & value == 'O' ~ 'Overcast',
      param == 'Sky' & value == 'F' ~ 'Foggy',
      param == 'Rain' & value == 'N' ~ 'None',
      param == 'Rain' & value == 'L' ~ 'Light',
      param == 'Rain' & value == 'M' ~ 'Medium',
      param == 'Rain' & value == 'H' ~ 'Heavy',
      TRUE ~ value
    ))

  # --- Exclude unwanted stations ---
  # read in active stations
  df_stations <- read_csv('00_GlobalFunctions/station_names.csv', show_col_types = FALSE) %>%
    filter(Status == 'active')

  valid_stations <- c(unique(df_stations$Station_new), 'Equipment Blank')

  # alert to excluded stations
  excluded_ids <- df_final %>%
    filter(!(`Location ID` %in% valid_stations)) %>%
    distinct(`Location ID`) %>%
    pull(`Location ID`)

  if (length(excluded_ids) > 0) {
    message('Excluded ', length(excluded_ids), ' Location IDs not in active station list: ',
            paste(excluded_ids, collapse = ', '))
  }

  # filter df_final to only active station matches
  df_final <- df_final %>%
    filter(`Location ID` %in% valid_stations)

  # --- Format data ---
  # Round values
  df_final <- df_final %>%
    mutate(value = case_when(
      param == 'Turbidity (FNU)' ~ as.character(round(as.numeric(value), 1)),
      param == 'DO (mg/L)' ~ as.character(round(as.numeric(value), 2)),
      TRUE ~ value)
    )

  # Add parent sample ID
  df_final <- df_final %>%
    group_by(`Location ID`) %>%
    mutate(
      `EA_Parent Sample ID` = case_when(
        `QC: Type` == 'Replicate' ~ first(`Activity Name`[is.na(`QC: Type`)]),
        TRUE ~ NA_character_),
      `QC: Source Sample ID` = `EA_Parent Sample ID`
    ) %>%
    ungroup()

  df_final <- df_final %>%
    select(-c(batch_id, SondeType)) %>%
    rename(`Result Value` = value)

  df_final <- df_final %>%
    filter(!is.na(`Result Value`))

  # Depth again
  df_final <- df_final %>%
    rename(`EA_Sampling Depth` = Depth) %>%
    mutate(Depth = case_when(
      grepl('Water Depth', param, ignore.case = TRUE) ~ `Result Value`,
      TRUE ~ NA)) %>%
    group_by(`Location ID`, `Observed DateTime`) %>%
    fill(Depth, .direction = 'downup') %>%
    ungroup()

  # combine with metadata
  fp_meta <- abs_path_emp('Water Quality/AQUARIUS Samples Database/Database Migration/Lists to Import/Final Imports/Import_FDS Metadata.csv')
  df_meta <- read_csv(fp_meta, show_col_types = FALSE)
  
  # check for params in df_final not present in df_meta
  missing_params <- setdiff(unique(df_final$param), unique(df_meta$param))
  if (length(missing_params) > 0) {
    message('Missing ', length(missing_params), ' param(s) not in metadata: ',
            paste(missing_params, collapse = ', '))
  }

  df_final <- right_join(df_meta, df_final, by = c('param', 'SurfBot')) %>%
    select(-c(param, SurfBot))

  # Fix times
  df_final <- df_final %>%
    mutate(`Analyzed DateTime` = format(mdy_hm(`Analyzed DateTime`), '%Y-%m-%d %H:%M:%S'))

  df_final <- df_final %>%
    mutate(across(contains('DateTime'), ~ ifelse(is.na(.x), NA, paste0(.x, ' [PST]'))))

  # Depth unit
  df_final <- df_final %>%
    mutate(`Depth Unit` = case_when(
      `Location ID` == 'Equipment Blank' ~ NA,
      TRUE ~ 'ft'
    )) %>%
    mutate(Depth = as.character(round(as.numeric(Depth), 1)))

  # Remove depth observed property
  df_final <- df_final %>%
    filter(`Observed Property ID` != 'Water Depth')

  # Round some more (TODO: make more efficient)
  df_final <- df_final %>%
    mutate(
      `Result Value` = case_when(
        `Observed Property ID` == 'Turbidity' ~ as.character(round(as.numeric(`Result Value`), 1)),
        `Observed Property ID` == 'Chlorophyll Fluorescence RFU' ~ as.character(round(as.numeric(`Result Value`), 2)),
        TRUE ~ `Result Value`
        ),
      `EA_Sampling Depth` = round(as.numeric(`EA_Sampling Depth`), 1)
      )

  # Remove Device ID for unneeded properties (TODO: make more efficient)
  df_final <- df_final %>%
    mutate(
      `Field: Device ID` = case_when(
        `Observed Property ID` %in% c('Filter Container ID','Sky Conditions','Rain','Microcystis aeruginosa','Churn Bucket ID','Air Temperature','Wind Velocity','Wave Scale') ~ NA,
        TRUE ~ `Field: Device ID`
      )
    )

  # Rename MC
  df_final <- df_final %>%
    mutate(
      `Observed Property ID` = case_when(
        `Observed Property ID` == 'Microcystis aeruginosa' ~ 'MVI',
        TRUE ~ `Observed Property ID`
        )
    )

  # Rearrange
  col_order <- c(
    'Observation ID', 'Location ID', 'Observed Property ID', 'Observed DateTime', 'Analyzed DateTime',
    'Depth', 'Depth Unit', 'Data Classification', 'Result Value', 'Result Unit', 'Result Status',
    'Result Grade', 'Medium', 'Activity Name', 'Activity ID', 'Collection Method', 'Field: Device ID',
    'Field: Device Type', 'Field: Comment', 'Lab: Specimen Name', 'Lab: Analysis Method',
    'Lab: Detection Condition', 'Lab: Limit Type', 'Lab: MDL', 'Lab: MRL', 'Lab: Quality Flag',
    'Lab: Received DateTime', 'Lab: Prepared DateTime', 'Lab: Sample Fraction', 'Lab: From Laboratory',
    'Lab: Sample ID', 'Lab: Dilution Factor', 'Lab: Comment', 'QC: Type', 'QC: Source Sample ID',
    'EA_Modified Method', 'EA_Profile Type', 'EA_Project Type', 'EA_Reports To',
    'EA_Parent Sample ID', 'EA_Field Quality Flag', 'EA_Sampling Depth'
  )

  df_final <- df_final %>% select(all_of(col_order)) %>%
    arrange(`Activity Name`, `Data Classification`)

  df_final <- df_final %>%
    filter(!(`Observed Property ID` %in% c('Latitude', 'Longitude', 'Filter Container ID') & is.na(`Result Value`)))

  df_final <- df_final %>%
    filter(!(is.na(`Observed Property ID`)))

  return(df_final)
}

format_FDS_pdf <- function(df_final) {
  
  # remove extra data/metadata, fix formatting
  df_final <- df_final %>%
    filter(!(param %in% c('Vessel:','Crew:','Run Name:','Operator:','Run Type:','Sonde ID (H):','ChloroVol(ml)'))) %>%
    filter(SondeType == 'vertical' | is.na(SondeType)) %>%
    mutate(param = str_squish(param))
  
  # add per-run metadata
  
  # per-run variables
  run_dates <- df_final %>%
    filter(param == 'Date:') %>%
    group_by(run_id) %>%
    summarize(run_date = mdy(first(value)), .groups = 'drop')
  
  run_sonde <- df_final %>%
    filter(param == 'Sonde ID (V)') %>%
    group_by(run_id) %>%
    summarize(sonde_id = first(value), .groups = 'drop')
  
  station_vals <- df_final %>%
    filter(param == 'Station') %>%
    select(run_id, station_id, `Location ID` = value)
  
  lab_ids <- df_final %>%
    filter(param == 'Lab ID') %>%
    select(run_id, station_id, `QC_Type`, `Activity Name` = value)
  
  # add them in
  
  # datetime
  df_final <- df_final %>%
    left_join(run_dates, by = 'run_id') %>%
    mutate(
      `Observed DateTime` = case_when(
        param == 'Time' & !is.na(run_date) ~ as.POSIXct(
          paste(run_date, value),
          format = '%Y-%m-%d %H:%M',
          tz = 'UTC'
        ),
        TRUE ~ as.POSIXct(NA)
      )
    ) %>%
    group_by(run_id, station_id) %>%
    tidyr::fill(`Observed DateTime`, .direction = 'downup') %>%
    ungroup() %>%
    select(-run_date)
  
  # device ID
  df_final <- df_final %>%
    left_join(run_sonde, by = 'run_id') %>%
    mutate(`Field: Device ID` = case_when(
      SondeType == 'vertical' ~ sonde_id,
      TRUE ~ NA_character_
    )) %>%
    select(-sonde_id)

  # run/station ID
  df_final <- df_final %>%
    left_join(station_vals, by = c('run_id','station_id')) %>%
    left_join(lab_ids, by = c('run_id','station_id','QC_Type'))

  # water and sampling depth
  df_final <- df_final %>%
    mutate(Depth = case_when(
      param == 'Water Depth(ft)' ~ suppressWarnings(as.numeric(value)),
      TRUE ~ NA_real_
    )) %>%
    group_by(run_id, station_id) %>%
    fill(Depth, .direction = 'downup') %>%
    ungroup() %>%
    mutate(`EA_Sampling Depth` = case_when(
      SurfBot == 'bottom' ~ as.numeric(Depth) - 3,
      SurfBot == 'surface' ~ 3,
      param == 'MC Score(1-5)' ~ 0,
      TRUE ~ NA_real_
    ))

  # filter jugs
  df_final <- df_final %>%
    mutate(
      standard_jug  = case_when(param == 'Notes' ~ str_match(value, '(?i)filter jug:?[ ]*([^;]+)')[,2]),
      duplicate_jug = case_when(param == 'Notes' ~ str_match(value, '(?i)duplicate filter jug:?[ ]*([^;]+)')[,2]),
      blank_jug     = case_when(param == 'Notes' ~ str_match(value, '(?i)blank filter jug:?[ ]*([^;]+)')[,2])
    ) %>%
    group_by(run_id, station_id) %>%
    fill(standard_jug, .direction = 'downup') %>%
    ungroup() %>%
    fill(duplicate_jug, blank_jug, .direction = 'downup') %>%
    mutate(
      `EA_Filter Container ID` = case_when(
        `QC_Type` == 'Replicate' ~ duplicate_jug,
        `QC_Type` == 'Blank' ~ blank_jug,
        is.na(`QC_Type`) ~ standard_jug
      ),
      value = case_when(
        param == 'Notes' ~ value %>%
          str_remove_all('(?i)filter jug:?[ ]*[^;\\s]+;?\\s*|duplicate filter jug:?[ ]*[^;\\s]+;?\\s*|blank filter jug:?[ ]*[^;\\s]+;?\\s*') %>%
          str_remove('^;\\s*') %>%
          str_remove(';\\s*$') %>%
          str_trim(),
        TRUE ~ value
      )
    ) %>%
    select(-standard_jug, -duplicate_jug, -blank_jug)

  # add field comments from notes
  notes_vals <- df_final %>%
    filter(param == 'Notes') %>%
    select(run_id, station_id, `Field: Comment` = value)

  df_final <- df_final %>%
    left_join(notes_vals, by = c('run_id','station_id'))

  # remove metadata columns that are no longer needed
  df_final <- df_final %>%
    filter(!(param %in% c('Date:','Sonde ID (V):','Station','Time','Lab ID','Notes')))

  # add in lat/lons for LSZs only
  df_final <- df_final %>%
    mutate(
      `Location ID`   = if_else(grepl('EZ', `Location ID`), str_replace(`Location ID`, 'EZ', 'LSZ'), `Location ID`),
      `Field: Comment` = str_remove(`Field: Comment`, ';\\s*$')
    ) %>%
    filter(!(param %in% c('latitude', 'longitude') & !grepl('LSZ', `Location ID`))) %>%
    relocate(SurfBot, .after = param)

  # get churn bucket
  churn_rows <- df_final %>%
    group_by(run_id, station_id, `QC_Type`) %>%
    filter(param == 'Churn Bucket #') %>%
    select(-value) %>%
    ungroup()

  # add in filter container
  filter_vals <- df_final %>%
    filter(!is.na(`EA_Filter Container ID`)) %>%
    distinct(run_id, station_id, `QC_Type`, `EA_Filter Container ID`)

  # fix up churn bucket
  new_rows <- churn_rows %>%
    dplyr::left_join(
      filter_vals,
      by = c('run_id', 'station_id', 'QC_Type', 'EA_Filter Container ID')
    ) %>%
    dplyr::mutate(param = 'Filter Container ID', value = `EA_Filter Container ID`) %>%
    dplyr::select(-`EA_Filter Container ID`)

  # add in churn bucket
  df_final <- bind_rows(df_final, new_rows) %>%
    select(-`EA_Filter Container ID`) %>%
    arrange(run_id, station_id, `QC_Type`, SurfBot)

  # remap weather codes
  df_final <- df_final %>%
    mutate(value = case_when(
      param == 'Sky' & value == 'S'  ~ 'Sunny',
      param == 'Sky' & value == 'PC' ~ 'Partly Cloudy',
      param == 'Sky' & value == 'C'  ~ 'Cloudy',
      param == 'Sky' & value == 'O'  ~ 'Overcast',
      param == 'Sky' & value == 'F'  ~ 'Foggy',
      param == 'Rain' & value == 'N' ~ 'None',
      param == 'Rain' & value == 'L' ~ 'Light',
      param == 'Rain' & value == 'M' ~ 'Medium',
      param == 'Rain' & value == 'H' ~ 'Heavy',
      TRUE ~ value
    ))
  
  # add in parent sample IDs for replicate
  df_final <- df_final %>%
    group_by(`Location ID`) %>%
    mutate(
      `EA_Parent Sample ID` = case_when(
        `QC_Type` == 'Replicate' ~ first(`Activity Name`[is.na(`QC_Type`)]),
        TRUE ~ NA_character_
      ),
      `QC: Source Sample ID` = `EA_Parent Sample ID`
    ) %>%
    ungroup()

  # remove other helper cols, rename result values
  df_final <- df_final %>%
    select(-c(SondeType)) %>%
    rename(`Result Value` = value) %>%
    filter(!is.na(`Result Value`))

  # combine with Aquarius metadata
  fp_meta <- abs_path_emp('Water Quality/AQUARIUS Samples Database/Database Migration/Lists to Import/Final Imports/Import_FDS Metadata.csv')
  df_meta <- read_csv(fp_meta, show_col_types = FALSE)
  
  # check for params in df_final not present in df_meta
  missing_params <- setdiff(unique(df_final$param), unique(df_meta$param))
  missing_params <- setdiff(missing_params, c('Run Name', 'Sonde ID (V)', 'Sonde ID (H)'))
  if (length(missing_params) > 0) {
    message(length(missing_params), ' unexpected parameters: ',
            paste(missing_params, collapse = ', '))
  }

  df_final <- right_join(df_meta, df_final, by = c('param', 'SurfBot')) %>%
    select(-c(param, SurfBot))

  # fix times
  df_final <- df_final %>%
    mutate(`Analyzed DateTime` = format(mdy_hm(`Analyzed DateTime`), '%Y-%m-%d %H:%M:%S')) %>%
    mutate(across(contains('DateTime'), ~ ifelse(is.na(.x), NA, paste0(.x, ' [PST]'))))

  # add in depth unit
  df_final <- df_final %>%
    mutate(`Depth Unit` = case_when(
      `Location ID` == 'Equipment Blank' ~ NA_character_,
      TRUE ~ 'ft'
    )) %>%
    mutate(Depth = as.character(round(suppressWarnings(as.numeric(Depth)), 1))) %>%
    filter(`Observed Property ID` != 'Water Depth')
  
  # --- Exclude unwanted stations ---
  # read in active stations
  df_stations <- read_csv('00_GlobalFunctions/station_names.csv', show_col_types = FALSE) %>%
    filter(Status == 'active')
  
  valid_stations <- c(unique(df_stations$Station_new), 'Equipment Blank')
  
  # alert to excluded stations
  excluded_ids <- df_final %>%
    filter(!(`Location ID` %in% valid_stations)) %>%
    distinct(`Location ID`) %>%
    pull(`Location ID`)
  
  if (length(excluded_ids) > 0) {
    message('Excluded ', length(excluded_ids), ' Location IDs not in active station list: ',
            paste(excluded_ids, collapse = ', '))
  }
  
  # filter df_final to only active station matches
  df_final <- df_final %>%
    filter(`Location ID` %in% valid_stations)

  # --- Rounding ---
  # turbidity
  idx <- df_final$`Observed Property ID` == 'Turbidity'
  df_final$`Result Value`[idx] <-
    as.character(round(as.numeric(df_final$`Result Value`[idx]), 1))
  
  # DO
  idx <- df_final$`Observed Property ID` == 'DO (mg/L)'
  df_final$`Result Value`[idx] <-
    as.character(round(as.numeric(df_final$`Result Value`[idx]), 2))
  
  # chlorophyll
  idx <- df_final$`Observed Property ID` == 'Chlorophyll Fluorescence RFU'
  df_final$`Result Value`[idx] <-
    as.character(round(as.numeric(df_final$`Result Value`[idx]), 2))
  
  # sampling depth
  idx <- df_final$`Observed Property ID` == 'EA_Sampling Depth'
  df_final$`Result Value`[idx] <-
    as.character(round(as.numeric(df_final$`Result Value`[idx]), 1))

  # --- Final Things ---
  # remove device ID for un-needed params
  df_final <- df_final %>%
    mutate(
      `Field: Device ID` = case_when(
        `Observed Property ID` %in% c('Filter Container ID','Sky Conditions','Rain','MVI','Churn Bucket ID','Air Temperature','Wind Velocity','Wave Scale') ~ NA_character_,
        TRUE ~ `Field: Device ID`
      )
    )

  # rename QC Type
  df_final <- df_final %>%
    rename(`QC: Type` = QC_Type)

  # remove Secchi depth rows for C9, C3A, and C10A
  df_final <- df_final %>%
    filter(!(grepl('^C9$|^C3A$|^C10A$', `Location ID`) &
               grepl('Secchi', `Observed Property ID`, ignore.case = TRUE)))

  # final ordering
  col_order <- c(
    'Observation ID', 'Location ID', 'Observed Property ID', 'Observed DateTime', 'Analyzed DateTime',
    'Depth', 'Depth Unit', 'Data Classification', 'Result Value', 'Result Unit', 'Result Status',
    'Result Grade', 'Medium', 'Activity Name', 'Activity ID', 'Collection Method', 'Field: Device ID',
    'Field: Device Type', 'Field: Comment', 'Lab: Specimen Name', 'Lab: Analysis Method',
    'Lab: Detection Condition', 'Lab: Limit Type', 'Lab: MDL', 'Lab: MRL', 'Lab: Quality Flag',
    'Lab: Received DateTime', 'Lab: Prepared DateTime', 'Lab: Sample Fraction', 'Lab: From Laboratory',
    'Lab: Sample ID', 'Lab: Dilution Factor', 'Lab: Comment', 'QC: Type', 'QC: Source Sample ID',
    'EA_Modified Method', 'EA_Profile Type', 'EA_Project Type', 'EA_Reports To',
    'EA_Parent Sample ID', 'EA_Field Quality Flag', 'EA_Sampling Depth'
  )

  df_final <- df_final %>% select(any_of(col_order)) %>%
    arrange(`Activity Name`, `Data Classification`) %>%
    filter(!(`Observed Property ID` %in% c('Latitude','Longitude','Filter Container ID') & is.na(`Result Value`))) %>%
    filter(!is.na(`Observed Property ID`))

  return(df_final)
}

format_bryte <- function(fp){
  # Read in lab data
  df_lab <- suppressMessages(read_excel(fp, sheet = 'Lab_Results'))
  
  # Rename lab columns
  rename_map <- c(
    'Location ID' = 'Station Name',
    'param' = 'Analyte',
    'Result Value' = 'Sample Result',
    'Analyzed DateTime' = 'Analyzed',
    'Activity Name' = 'Sample Code',
    'Lab: Dilution Factor' = 'Dilution',
    'Lab: Comment' = 'Result Note',
    'Lab: Received DateTime' = 'Received',
    'Lab: MRL' = 'RL',
    'Lab: Quality Flag' = 'Result Flag',
    'EA_Parent Sample ID' = 'Parent Sample',
    'QC: Type' = 'Sample Type'
  )
  
  df_lab <- rename_cols(df_lab, rename_map, verbose = FALSE)
  
  # Filter out lab duplicates
  df_lab <- df_lab %>%
    filter(!grepl('Lab Dup', `Sample Lab Code`, ignore.case = TRUE))
  
  # Filter out unneeded parameters
  df_lab <- df_lab %>% filter(!(param %in% c('Specific Conductance','pH')))
  
  # Format station names
  df_lab <- df_lab %>%
    mutate(
      `Location ID` = str_remove(`Location ID`, ' -.*'),
      `Location ID` = case_when(
        `Location ID` == 'Blank; Equipment' ~ 'Equipment Blank',
        `Location ID` == 'EZ6' ~ 'LSZ6',
        `Location ID` == 'EZ2' ~ 'LSZ2',
        `Location ID` == 'EZ6-SJR' ~ 'LSZ6-SJR',
        `Location ID` == 'EZ2-SJR' ~ 'LSZ2-SJR',
        TRUE ~ `Location ID`
      ),
    )
  
  # Add a Date column (needed for merge with FDS)
  df_lab <- df_lab %>%
    mutate(ObsDate = as.Date(Collected, format = '%m/%d/%Y %H:%M'))
  
  # Add in depth and Observed DateTime (from formatted Field Data Sheet)
  year <- year_month <- format(as.Date(df_lab$Collected[1], format = '%m/%d/%Y %H:%M'), '%Y')
  year_month <- format(as.Date(df_lab$Collected[1], format = '%m/%d/%Y %H:%M'), '%Y-%m')
  
  fp_fds <- abs_path_emp(glue::glue('Water Quality/00 - Monthly Run Docs/Database Import Data/{year}/{year_month}_FDS_EMP-DWQ.csv'))
  df_fds <- read_csv(fp_fds, show_col_types = FALSE)

  df_fds <- df_fds %>%
    select(`Location ID`, `Observed DateTime`, Depth) %>%
    distinct() %>%
    mutate(ObsDate = str_extract(`Observed DateTime`, '\\d{4}-\\d{2}-\\d{2}') %>% as.Date()) # for merging Equipment Blanks

  df_final <- left_join(df_lab, df_fds, by = c('Location ID', 'ObsDate'))
  
  # Combine with metadata
  fp_meta <- abs_path_emp('Water Quality/AQUARIUS Samples Database/Database Migration/Lists to Import/Final Imports/Import_Bryte Metadata.csv')
  df_meta <- read_csv(fp_meta, show_col_types = FALSE)
  
  # check for missing params
  missing_params <- setdiff(df_final$param, df_meta$param)
  
  if (length(missing_params) > 0) {
    stop('The following param values are not in df_meta: ',
         paste(missing_params, collapse = ', '))
  }
  
  df_final <- right_join(df_meta, df_final, by = c('param')) %>%
    select(-c(param))

  # Format columns
  df_final <- df_final %>%
    mutate(
      `EA_Parent Sample ID` = case_when(
        `EA_Parent Sample ID` == '0' ~ NA_character_,
        TRUE ~ `EA_Parent Sample ID`
      ),
      `QC: Source Sample ID` = `EA_Parent Sample ID`,
      `QC: Type` = case_when(
        `QC: Type` == 'Normal Sample' ~ NA_character_,
        `QC: Type` == 'Blank; Equipment' ~ 'Blank',
        `QC: Type` == 'Duplicate, Specific Analyte(s)' ~ 'Replicate'
      ),
      `Lab: Detection Condition` = case_when(
        `Result Value` == '< R.L.' ~ 'Not detected',
        TRUE ~ NA_character_),
      `Result Value` = case_when(
        `Result Value` == '< R.L.' ~ NA_character_,
        TRUE ~ `Result Value`),
      `Depth Unit` = case_when(
        `Location ID` == 'Equipment Blank' ~ NA_character_,
        TRUE ~ 'ft')
    )
  
  # Format DateTimes
  df_final <- df_final %>%
    mutate(across(
      .cols = matches('DateTime') & !all_of('Observed DateTime'),
      .fns = \(x) {
        x <- as.POSIXct(round_date(mdy_hm(x), unit = '5 minutes'))
        ifelse(is.na(x), NA, paste0(x, ' [PST]'))
      }
    ))
  
  # Rearrange
  col_order <- c(
    'Observation ID', 'Location ID', 'Observed Property ID', 'Observed DateTime', 'Analyzed DateTime', 
    'Depth', 'Depth Unit', 'Data Classification', 'Result Value', 'Result Unit', 'Result Status', 
    'Result Grade', 'Medium', 'Activity Name', 'Activity ID', 'Collection Method', 'Field: Device ID', 
    'Field: Device Type', 'Field: Comment', 'Lab: Specimen Name', 'Lab: Analysis Method', 
    'Lab: Detection Condition', 'Lab: Limit Type', 'Lab: MDL', 'Lab: MRL', 'Lab: Quality Flag', 
    'Lab: Received DateTime', 'Lab: Prepared DateTime', 'Lab: Sample Fraction', 'Lab: From Laboratory', 
    'Lab: Sample ID', 'Lab: Dilution Factor', 'Lab: Comment', 'QC: Type', 'QC: Source Sample ID', 
    'EA_Modified Method', 'EA_Profile Type', 'EA_Project Type', 'EA_Reports To', 
    'EA_Parent Sample ID', 'EA_Field Quality Flag', 'EA_Sampling Depth'
  )
  
  df_final <- df_final %>%
    select(all_of(col_order)) %>%
    arrange(`Observed DateTime`, `Location ID`, desc(is.na(`QC: Type`)))
  
  return(df_final)
}


# Check Functions ---------------------------------------------------------

# no missing stations
check_stations <- function(df){
  # read active stations
  df_stations <- read_csv('00_GlobalFunctions/station_names.csv', show_col_types = FALSE) %>%
    filter(Status == 'active')
  
  valid_stations <- unique(df_stations$Station_new)
  
  # active stations not represented in data
  missing_stations <- setdiff(valid_stations, unique(df$`Location ID`))
  if (length(missing_stations) > 0) {
    message('Active stations not found among Location IDs: ',
            paste(missing_stations, collapse = ', '))
  }
}

check_analytes <- function(df, type) {
  # enforce valid argument
  type <- match.arg(type, choices = c('field', 'lab'))
  
  # read analyte reference list
  df_analytes <- read_csv('00_GlobalFunctions/dwq_analytes.csv', show_col_types = FALSE) %>%
    filter(DataType == type)
  
  # read active stations
  df_stations <- read_csv('00_GlobalFunctions/station_names.csv', show_col_types = FALSE) %>%
    filter(Status == 'active')
  
  valid_analytes <- unique(df_analytes$Analyte)
  valid_stations <- unique(df_stations$Station_new)
  
  # normalize date to YYYY-MM-DD
  df <- df %>%
    mutate(`Observed DateTime` = str_extract(`Observed DateTime`, '\\d{4}-\\d{2}-\\d{2}'))
  
  # join SamplingType
  df_joined <- df %>%
    left_join(df_stations %>% select(Station_new, SamplingType),
              by = c('Location ID' = 'Station_new'))
  
  # gather observed stations/dates/QC
  station_dates <- df_joined %>%
    filter(`Location ID` %in% valid_stations | `Location ID` == 'Equipment Blank') %>%
    distinct(`Location ID`, `Observed DateTime`, `QC: Type`)
  
  # create expected combos (station × analyte × date × QC)
  expected <- expand_grid(
    station_dates,
    `Observed Property ID` = valid_analytes
  )
  
  # --- Rules ---
  
  # 1. Shore stations don’t require Secchi
  shore_stations <- df_stations %>%
    filter(SamplingType == 'shore') %>%
    pull(Station_new)
  
  expected <- expected %>%
    filter(!( `Location ID` %in% shore_stations &
                `Observed Property ID` == 'Secchi Depth'))

  # 2. Equipment Blank — only specific analytes
  expected <- expected %>%
    filter(!( `Location ID` == 'Equipment Blank' &
                !(`Observed Property ID` %in% c('Churn Bucket ID', 'Filter Container ID'))))
  
  # 3. Replicates — only specific analytes
  expected <- expected %>%
    filter(
      case_when(
        `QC: Type` == 'Replicate' ~ `Observed Property ID` %in% c('Churn Bucket ID', 'Filter Container ID'),
        TRUE ~ TRUE
      )
    )
  
  # 4. Non-LSZ stations don’t need Lat/Long
  expected <- expected %>%
    filter(!( !grepl('LSZ', `Location ID`, ignore.case = TRUE) &
                `Observed Property ID` %in% c('Latitude', 'Longitude')))
  
  # 5. Only D7, D16, and D19 have Dissolved Bromide
  expected <- expected %>%
    filter(!( `Observed Property ID` == 'Dissolved Bromide' &
                !(`Location ID` %in% c('D7', 'D16', 'D19'))))
  
  # --- Split blanks vs others ---
  expected_blank <- expected %>% filter(`Location ID` == 'Equipment Blank')
  expected_other <- expected %>%
    filter(`Location ID` != 'Equipment Blank') %>%
    select(-`Observed DateTime`) %>%
    distinct()
  
  # --- Observed combos ---
  observed_blank <- df_joined %>%
    filter(`Location ID` == 'Equipment Blank') %>%
    distinct(`Location ID`, `Observed Property ID`, `Observed DateTime`, `QC: Type`)
  
  observed_other <- df_joined %>%
    filter(`Location ID` != 'Equipment Blank') %>%
    distinct(`Location ID`, `Observed Property ID`, `QC: Type`)
  
  # --- Find missing combinations ---
  missing_blank <- anti_join(
    expected_blank, observed_blank,
    by = c('Location ID', 'Observed Property ID', 'Observed DateTime', 'QC: Type')
  )
  
  missing_other <- anti_join(
    expected_other, observed_other,
    by = c('Location ID', 'Observed Property ID', 'QC: Type')
  ) %>%
    mutate(`Observed DateTime` = as.character(NA))
  
  missing_combos <- bind_rows(missing_blank, missing_other)
  
  # --- Format message ---
  fmt_row <- function(x) {
    parts <- x[!is.na(x) & x != '']
    paste(parts, collapse = ' - ')
  }
  
  if (nrow(missing_combos) > 0) {
    message(
      'Missing ', nrow(missing_combos),
      ' station/analyte combinations:\n',
      paste0(apply(missing_combos, 1, fmt_row), collapse = '\n')
    )
  } else {
    message('All expected station/analyte combinations are represented.')
  }
}


check_extra_data <- function(df) {
  # normalize Observed DateTime to YYYY-MM-DD
  df <- df %>%
    mutate(`Observed DateTime` = str_extract(`Observed DateTime`, '\\d{4}-\\d{2}-\\d{2}'))
  
  # --- Equipment Blank duplicates (include date) ---
  duplicates_blank <- df %>%
    filter(`Location ID` == 'Equipment Blank') %>%
    group_by(`Location ID`, `Observed Property ID`, `Observed DateTime`, `QC: Type`) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    distinct(`Location ID`, `Observed Property ID`, `Observed DateTime`, `QC: Type`)
  
  # --- All other stations (ignore date) ---
  duplicates_other <- df %>%
    filter(`Location ID` != 'Equipment Blank') %>%
    group_by(`Location ID`, `Observed Property ID`, `QC: Type`) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    distinct(`Location ID`, `Observed Property ID`, `QC: Type`) %>%
    mutate(`Observed DateTime` = NA_character_)
  
  duplicates <- bind_rows(duplicates_blank, duplicates_other)
  
  # format message
  fmt_row <- function(x) {
    parts <- x[!is.na(x) & x != '']
    paste(parts, collapse = ' - ')
  }
  
  if (nrow(duplicates) > 0) {
    message(
      'Found ', nrow(duplicates), ' duplicate data entries:\n',
      paste0(apply(duplicates, 1, fmt_row), collapse = '\n')
    )
  } else {
    message('No duplicate entries found in the data.')
  }
}

check_filter_ID <- function(df) {
  df_filter <- df %>%
    filter(`Observed Property ID` == 'Filter Container ID') %>%
    mutate(`Observed DateTime` = str_extract(`Observed DateTime`, '\\d{4}-\\d{2}-\\d{2}'))
  
  # identify invalid entries (not single A–Z)
  bad_rows <- df_filter %>%
    filter(!str_detect(`Result Value`, '^[A-Z]$'))
  
  # helper for message
  fmt_row <- function(x) {
    parts <- x[!is.na(x) & x != '']
    paste(parts, collapse = ' - ')
  }
  
  if (nrow(bad_rows) > 0) {
    bad_combos <- bad_rows %>%
      mutate(`Observed DateTime` = case_when(
        `Location ID` == 'Equipment Blank' ~ as.character(`Observed DateTime`),
        TRUE ~ NA_character_
      )) %>%
      distinct(`Location ID`, `Observed DateTime`, `QC: Type`)
    
    message(
      'Invalid Filter Container ID values found for ',
      nrow(bad_combos), ' stations:\n',
      paste0(apply(bad_combos, 1, fmt_row), collapse = '\n')
    )
  } else {
    message('All Filter Container IDs are correctly formatted.')
  }
  
  invisible(df)
}




  
