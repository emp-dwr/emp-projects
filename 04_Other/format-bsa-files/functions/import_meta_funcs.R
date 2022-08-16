# ---
# add SAM code
# ---
add_SAM_code <- function(wkbk, sheetName, dfAbund) {
  df_SAM <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'A1:K1',
      col_names = F
    )
  )

  SAMStr <- unlist(df_SAM, use.names = FALSE) %>% paste(.,collapse = '')
  
  dfAbund$SAM_code <- trimws(SAMStr)
  
  return(dfAbund)
}

# ---
# add sample ID
# ---
add_samp_id <- function(wkbk, sheetName, dfAbund) {
  df_sampid <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'A9:D9',
      col_names = F
    )
  )
  
  sampid_str <- unlist(df_sampid, use.names = FALSE) %>% paste(.,collapse = '')
  
  dfAbund$samp_id <- trimws(sampid_str)
  
  return(dfAbund)
}

  
# ----
# add datetime cols
# ----
add_date_time <- function(wkbk, sheetName, dfAbund) {
  dfDate <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'F6:M6',
      col_types = 'numeric',
      col_names = F
    )
  )
  
  # collapse date to string
  dateStr <- unlist(dfDate, use.names = FALSE) %>% paste(.,collapse = '')
  
  # convert to datetime and add to df
  dateDate <- as.Date(dateStr, '%m%d%Y')
  dfAbund$date <- dateDate
  
  # extract time from sheet
  dfTime <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'F9:I9',
      col_types = 'text',
      col_names = F
    )
  )
  
  timeStr <- unlist(dfTime, use.names = FALSE) %>% paste(.,collapse = '')
  timeStr <- str_replace_all(timeStr,'NA','')
  
  dfAbund$time <- trimws(timeStr)
  
  return(dfAbund)
}

# ----
# add sample name
# ----
add_station_name <- function(wkbk, sheetName, dfAbund){
  statName <- as.character(
    suppressMessages(
      read_excel(
        wkbk,
        sheet = sheetName,
        range = 'V6',
        col_names = F
      )
    )
  )
  
  # changes spaces to underscores
  if (' ' %in% statName) {
    statName <- gsub(" ", "_", statName)  
  }
  
  # add samp name to df; remove whitespace
  dfAbund$station <- trimws(statName)

  return(dfAbund)
}

# ----
# add project name
# ----
add_proj_name <- function(wkbk, sheetName, dfAbund){
  proj_name <- as.character(
    suppressMessages(
      read_excel(
        wkbk,
        sheet = sheetName,
        range = 'A6',
        col_names = F
      )
    )
  )
  
  # add proj name to df; check if NA and remove whitespace
  if (length(proj_name) == 0){
    proj_name <- NA
  }
  
  dfAbund$project <- trimws(proj_name)

  return(dfAbund)
}

# --- 
# add # of jars
# ---
add_jar_num <- function(wkbk, sheetName, dfAbund){
  jar_num <- as.numeric(
    suppressMessages(
      read_excel(
        wkbk,
        sheet = sheetName,
        range = 'Z6',
        col_names = F
      )
    )
  )
  
  # add jar num to df; check if NA and remove whitespace
  if (length(jar_num) == 0){
    jar_num <- NA
  }
  
  dfAbund$num_of_jars <- trimws(jar_num)
  
  return(dfAbund)
}

# --- 
# add # of vials
# ---
add_vial_num <- function(wkbk, sheetName, dfAbund){
  vial_num <- as.numeric(
    suppressMessages(
      read_excel(
        wkbk,
        sheet = sheetName,
        range = 'Z9',
        col_names = F
      )
    )
  )
  
  # add vial num to df; check if NA and remove whitespace
  if (length(vial_num) == 0){
    vial_num <- NA
  }
  
  dfAbund$num_of_vials <- trimws(vial_num)
  
  return(dfAbund)
}

# --- 
# add tow
# ---
add_tow <- function(wkbk, sheetName, dfAbund){
  tow_val <- as.character(
    suppressMessages(
      read_excel(
        wkbk,
        sheet = sheetName,
        range = 'Q6',
        col_names = F
      )
    )
  )
  
  # add tow val to df; check if NA and remove whitespace
  if (length(tow_val) == 0){
    tow_val <- NA
  }
  
  dfAbund$tow <- trimws(tow_val)
  
  return(dfAbund)
}

# ----
# add sample number
# ----
add_samp_num <- function(wkbk, sheetName, dfAbund){
  df_samp_num <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'V12:AA12',
      col_types = 'numeric',
      col_names = F
    )
  )
  
  # grab value/append to df
  samp_num <- df_samp_num[,colSums(is.na(df_samp_num)) == 0][[1]] # collapse to value
  dfAbund$samp_num <- samp_num # append
  
  return(dfAbund)
}

# ----
# add BSA meta
# ----
add_bsa_meta <- function(wkbk, sheetName, dfAbund){
  df_bsa_meta <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'AB1:AB3',
      col_names = F
    )
  )

dfAbund$scope <- df_bsa_meta[[1]][1]
dfAbund$mag <- df_bsa_meta[[1]][2]
dfAbund$id_by <- df_bsa_meta[[1]][3]

df_id_date <- suppressMessages(
  read_excel(
    wkbk,
    sheet = sheetName,
    range = 'AB4',
    col_types = 'date',
    col_names = F
  )
)

dfAbund$id_date <- df_id_date[[1]]

return(dfAbund)
}

# ----
# add bottom meta
# ----
add_bottom_meta <- function(wkbk, sheetName, df) {
  df_bot <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'E100:Y101',
      col_names = F
    )
  )
  
  df$high_algae <- is.na(df_bot[[1]][1])
  df$high_detritus <- is.na(df_bot[[5]][1])
  df$high_silt <- is.na(df_bot[[9]][1])
  df$no_micro_tally <- is.na(df_bot[[13]][1])
  df$no_meso_tally <- is.na(df_bot[[20]][1])
  df$net_size_um <- ifelse(is.na(df_bot[[5]][2]), '50', '150')
  
  return(df)
}
# ----
# add subsamples
# ----
add_subs <- function(wkbk, sheetName, dfAbund){
  # import sub1 df
  subOneDf <- suppressMessages(
    read_excel(
      wkbk
      ,sheet = sheetName
      ,range = 'U9:W9'
      ,col_types = 'numeric'
      ,col_names = F
    )
  )
  
  # grab value/append to df
  if (sum(subOneDf, na.rm = TRUE) == 0) {
    print(paste('Check sheet',sheetName)) # error in sheet
    dfAbund$sub1_ml <- NA # append NA
  } else {
    subOne <- subOneDf[,colSums(is.na(subOneDf)) == 0][[1]] # collapse to value
    dfAbund$sub1_ml <- subOne # append
  }
  
  # import sub2 df
  subTwoDf <- suppressMessages(
    read_excel(
      wkbk
      ,sheet = sheetName
      ,range = 'H12:I12'
      ,col_types = 'numeric'
      ,col_names = F
    )
  )
  
  # grab value/append to df
  if (sum(subTwoDf, na.rm = TRUE) == 0) {
    print(paste('Check sheet',sheetName)) # error in sheet
    dfAbund$sub2_ml <- NA # append NA
  } else {
    subTwo <- subTwoDf[,colSums(is.na(subTwoDf)) == 0][[1]] # collapse to value
    dfAbund$sub2_ml <- subTwo # append
  }
  
  return(dfAbund)
}

# ----
# add volumes
# ----
add_vols <- function(wkbk, sheetName, dfAbund){
  # import v1 df
  vOneDf <- suppressMessages(
    read_excel(
      wkbk
      ,sheet = sheetName
      ,range = 'M9:Q9'
      ,col_types = 'numeric'
      ,col_names = F
    )
  )
  
  # grab value/append to df
  if (sum(vOneDf, na.rm = TRUE) == 0) {
    print(paste('Check sheet',sheetName)) # error in sheet
    dfAbund$v1_ml <- NA # append NA
  } else {
    vOne <- vOneDf[,colSums(is.na(vOneDf)) == 0][[1]] # collapse to value
    dfAbund$v1_ml <- vOne # append
  }
  
  # import v2 df
  vTwoDf <- suppressMessages(
    read_excel(
      wkbk
      ,sheet = sheetName
      ,range = 'C12:D12'
      ,col_types = 'numeric'
      ,col_names = F
    )
  )
  
  # grab value/append to df
  if (sum(vTwoDf, na.rm = TRUE) == 0) {
    print(paste('Check sheet',sheetName)) # error in sheet
    dfAbund$v2_ml <- NA # append NA
  } else {
    vTwo <- vTwoDf[,colSums(is.na(vTwoDf)) == 0][[1]] # collapse to value
    dfAbund$v2_ml <- vTwo # append
  }
  
  # import v_sed df
  df_vsed <- suppressMessages(
    read_excel(
      wkbk
      ,sheet = sheetName
      ,range = 'M12:R12'
      ,col_names = F
    )
  )
  
  # grab value/append to df
  v_sed <- df_vsed[,colSums(is.na(df_vsed)) == 0][[1]] # collapse to value
  dfAbund$vsed_ml <- v_sed # append

  return(dfAbund)
}

# ---
# add additional comments
# ---
add_comments <- function(wkbk, sheetName, dfAbund) {
  df_comms <- suppressMessages(
    read_excel(
      wkbk,
      sheet = sheetName,
      range = 'E102:X102',
      col_names = F
    )
  )
  
  commStr <- unlist(df_comms, use.names = FALSE) %>% paste(.,collapse = '')
  
  dfAbund$comments <- trimws(commStr)
  
  return(dfAbund)
}
